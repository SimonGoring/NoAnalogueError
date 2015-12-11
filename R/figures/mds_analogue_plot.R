ringed_plot <- function(pol.mds, dists){
  #  This gives us the ringed MDS plot to show points at multiple exclusion levels:
  
  # Two sets of points, one in the colder, more boreal zone:
  #  Point 671 is ID 1568 (cold)
  #  Point 1150 is ID 2868 (warmer)
  
  which(round(pol.mds$points[,1],1) < 0 & rowSums(as.matrix(dists)<0.32)>50)
  which(round(pol.mds$points[,1],1) > 0 & rowSums(as.matrix(dists)<0.32)>110)
  
  quant.min <- data.frame(quant = quantile(min.diss, seq(0, 1, by = 0.01)),
                          val = seq(0, 1, by = 0.01))
  
  mds_ex_plot <- data.frame(x = pol.mds$points[,1],
                            y = pol.mds$points[,2],
                            interval = c(findInterval(as.matrix(dists)[434,],
                                                      quantile(min.diss, c(seq(0.5, .9, by = 0.1),0.95,.99))),
                                         findInterval(as.matrix(dists)[1150,],
                                                      quantile(min.diss, c(seq(0.5, .9, by = 0.1),0.95, .99)))),
                            point = rep(c('Boreal Site', 'Temperate Site'), each = nrow(pol.mds$points)))
  mds_ex_plot <- data.frame(x = as.numeric(pol.mds$points[,1]),
                            y = as.numeric(pol.mds$points[,2]),
                            interval = c(as.numeric(as.matrix(dists)[434,]),
                                         as.numeric(as.matrix(dists)[1150,])),
                            point = factor(rep(c('Boreal Site', 'Temperate Site'), each = nrow(pol.mds$points))))
  
  mds_ex_plot <- mds_ex_plot[order(-mds_ex_plot$interval),]
  
  mds_ex_plot$predicted <- predict(gam(I(interval+0.01) ~ s(x,y,by=point), 
                                       data = mds_ex_plot, family = Gamma), type = 'response') - 0.01
  
  mds_ex_plot <- subset(mds_ex_plot, point == 'Boreal Site')
  
  drop.points <- ggplot(mds_ex_plot, aes(x = x, y = y)) + 
    geom_point(aes(color=(interval)^0.75), size = 4) +#,
                                #labels = c('<50%ile', '50-60%ile', '60-70%ile', 
                                #           '70-80%ile', '80-90%ile', '90-95%ile', '95-99%ile', '>99%ile')))) + 
    scale_alpha(guide = 'none') +
    scale_color_distiller(name="Percentile",type='div',palette=3) +
    geom_point(data = as.data.frame(pol.mds$points)[434,], aes(x = V1, y = V2), size = 5, color = 'red') +
    #geom_point(data = as.data.frame(pol.mds$points)[1150,], aes(x = V1, y = V2), size = 5, color = 'red') +
    #facet_wrap(~point) +
    coord_cartesian(xlim=c(-1.2, 1), ylim=c(-0.75, 1.35)) +
    theme_bw() +
    xlab('MDS Axis One') + ylab('MDS Axis Two') +
    theme(axis.text=element_blank(),
          axis.title = element_text(face='bold', family='serif', size = 18),
          strip.text=element_text(family='serif', face='bold', size=18),
          legend.position ='none')
  
  return(drop.points)

}