
bias <- data.frame(apply(wa.res$bias, 2, mean, na.rm=TRUE),
                   apply(mat.res$bias, 2, mean, na.rm=TRUE),
                   apply(brt.res$bias, 2, mean, na.rm=TRUE),
                   apply(rfor.res$bias, 2, mean, na.rm=TRUE))
        
biased <- apply(bias, 1, which.min)

vars <- data.frame(apply(wa.res$variance,   2, mean, na.rm=TRUE),
                   apply(mat.res$variance,  2, mean, na.rm=TRUE),
                   apply(brt.res$variance,  2, mean, na.rm=TRUE),
                   apply(rfor.res$variance, 2, mean, na.rm=TRUE))

vared <- apply(vars, 1, which.min)

exps <- data.frame(apply(wa.res$expectation,   2, mean, na.rm=TRUE),
                   apply(mat.res$expectation,  2, mean, na.rm=TRUE),
                   apply(brt.res$expectation,  2, mean, na.rm=TRUE),
                   apply(rfor.res$expectation, 2, mean, na.rm=TRUE))

exped <- apply(vars, 1, which.min)

exp <-   data.frame(wa    = as.vector(wa.res$expectation),
                     mat  = as.vector(mat.res$expectation),
                     brt  = as.vector(brt.res$expectation),
                     rfor = as.vector(rfor.res$expectation))
           
reps <- rep(NA, nrow(exp))

rank.min <- t(apply(exp, 1, rank))
rank.min[is.na(exp)] <- NA

aa <- do.call(rbind.data.frame, lapply(1:4, function(x){
  best_fit <- matrix(rank.min[,x],
                     ncol = ncol(mat.res$mean_prediction), 
                     nrow = nrow(mat.res$mean_prediction), 
                     byrow = FALSE)
  
  best_fit[is.na(rank.min[,1]) | is.na(rank.min[,2]) | is.na(rank.min[,3]) | is.na(rank.min[,4])] <- NA
  data.frame(mean.rank = colMeans(best_fit, na.rm=TRUE),
             model = c("WA", "MAT", "BRT", "rFor")[x],
             distance = 0:100)
  
}))


for(i in 1:nrow(exp)){
  min.r <- rank(as.vector(exp[i,]))
  if(!length(min.r) == 0|any(is.na(exp[i,]))) { reps[i] <- min.r }
}

reps[is.na(as.vector(rfor.res$mean_prediction)) | is.na(as.vector(brt.res$mean_prediction))] <- NA

best_fit <- matrix(reps,
                   ncol = ncol(mat.res$mean_prediction), 
                   nrow = nrow(mat.res$mean_prediction), 
                   byrow = FALSE)

best_prop <- do.call(rbind.data.frame,lapply(1:4, function(x){
                            data.frame(dist  = seq(0,1, by = 0.01),
                                       mean  = colMeans(best_fit == x, na.rm=TRUE),
                                       model = c("WA", "MAT", "BRT", "rFor")[x])
                     }))

ggplot(best_prop, aes(x = dist, y = mean, color = model)) + 
  geom_path(size = 1.5) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#ff7f00", "#984ea3")) +
  scale_linetype_manual(values = c(1,1,1,8,8,1)) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title = element_text(family = 'serif', face = 'bold', size = 24),
        strip.text = element_text(family = 'serif', face = 'bold', size = 14),
        axis.text = element_text(family = 'serif', size = 16)) +
  coord_cartesian(xlim=c(0, 1), ylim=c(0, 0.6)) +
  xlab("Analogue Distance") +
  ylab("Proportion of Best Fit Points")


