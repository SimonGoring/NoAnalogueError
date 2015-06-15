run_figure3 <- function(pol_mds, min.diss, map, climate){
  
  mds <- data.frame(pol_mds$points,
                    non.ana = factor(min.diss > 0.48))
  
  non.ana <- ggplot(data = mds, aes(x = X1, y = X2)) +
    geom_point(data = mds[mds$non.ana == FALSE,], alpha=0.5, size = 2, color='black') +   
    geom_point(data = mds[mds$non.ana == TRUE,], alpha=0.7, size = 4, color='red') +
    theme_bw() +
    xlab('MDS Axis One') + ylab('MDS Axis Two') +
    theme(text = element_text(size=24, family='serif'))
  
  non.ana.map <- ggplot(data = data.frame(map), aes(long, lat)) + 
    geom_polygon(aes(group=group), color='blue', alpha=0.2) +
    geom_point(data = data.frame(climate)[mds$non.ana == FALSE,], 
               aes(x = LONDD, y = LATDD), alpha=0.5) +
    geom_point(data = data.frame(climate)[mds$non.ana == TRUE,], 
               aes(x = LONDD, y = LATDD), color = 'red', size = 3) +
    theme_bw() +
    xlab('Longitude') + ylab('Latitude') +
    theme(axis.text=element_text(size=12, family='serif'),
          panel.background = element_rect(fill = 'lightblue')) +
    coord_map(projection='albers', lat0=35, lat1=55, xlim=c(-100, -60), ylim=c(35, 80))
  
  grid.arrange(non.ana, non.ana.map, ncol=2)
}