library(snowfall)

sfStop()
sfInit(parallel = TRUE, cpus = 30)

# Set up the analogue distance exclusion values.
#  This was originally a set of quantiles, but it played havoc on plotting and
#  didn't seem to add much to the understanding of what was going on.
vals <- seq(0, 1, by=0.01)

if('mat.res.RData' %in% list.files('data')){
  load('data/mat.res.RData')
} else {
  
  mat.res <- list(mean_prediction  = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                  sample_size      = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                  bias             = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                  variance         = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                  expectation      = matrix(ncol=length(vals), nrow=nrow(new.pol)))

}  
#  Turn the distance table into a matrix.
diag.dist <- as.matrix(dists); diag(diag.dist) <- NA

i <- 1; j <- 1

#  For MAT to work we don't need to re-calculate the distace matrix, we just need to know the
#  sample of interest and the subset:

fmat <- function(x){
  #  returns the means of the 1:10 closest to calculate the RMSE for analogue selection:
  
  index.used <- order(diag.dist[x,])[keep.pol[x,]]
  ten.or.less <- min(c(10, sum(keep.pol[x,])))
  
  vals <- cumsum(climate[index.used,4][1:ten.or.less]) / 1:ten.or.less
  vars <- c(0, lapply(1:ten.or.less, function(x)var(climate[index.used,4][1:x])))
  
  if(length(vals) < 10) vals <- c(vals, rep(NA, 10-length(vals)))
  
  vals
  
}

mat.fun <- function(z, j, min){
  #  For sample j, pass in a vector of samples to be used for calibration.
  
  #  These are the samples that can be used:
  calib.samples <- (1:nrow(new.pol))[ keep.pol[j,] ]
  
  #  now we resample:
  samples <- sample(calib.samples, replace=TRUE)
  
  distances <- order(diag.dist[j,samples])
  
  #  This gives the closest 10 climate variables to the sample of interest:
  clim.var <- climate[samples, 10][distances[1:10]]
  
  #  And the means of the 1 - 10 closest -
  vals <- cumsum(clim.var) / 1:10
  vals[min]
}

#  Parallelize:

sfExport(list = list('diag.dist'))
sfExport(list = list('mat.fun'))
sfExport(list = list('new.pol'))
sfExport(list = list('climate'))

for(i in 1:length(vals)){
  #  At each quantile, figure out which samples should be acceptable for a calibration set targeting
  #  each sample return 'keep.pol'

  if(any(is.na(mat.res$mean_prediction[,i]))){
    keep.pol <- aaply(diag.dist, 1, 
                      function(x) {x > vals[i]})
    
    diag(keep.pol) <- FALSE
    
    fast.mat <- laply(1:nrow(diag.dist), fmat)
    rmse <- aaply(fast.mat, 2, function(x) sqrt(mean((x - climate[,10])^2, na.rm = T)))
  
    sfExport(list = list('keep.pol'))
  
    for(j in 1:nrow(new.pol)){
  
      if(is.na(mat.res$mean_prediction[j,i])){
        prediction <- unlist(sfLapply(1:30, mat.fun, j = j, min = which.min(rmse)))
        mat.res$mean_prediction[j,i] <- mean(prediction, na.rm=TRUE)
        mat.res$sample_size[j,i] <- sum(keep.pol[j,], na.rm=TRUE)
        mat.res$bias[j, i] <- (climate[j,10] - mean(prediction, na.rm=TRUE))^2
        mat.res$expectation[j, i]  <- mean((climate[j,10] - prediction)^2, na.rm=TRUE)
        mat.res$variance[j, i]  <- mean((mean(prediction, na.rm=TRUE) - prediction)^2, na.rm=TRUE)
      }
  
      
    }
      
    save(mat.res, file = 'data/mat.res.RData')
  }
  cat(i, '\n')
}
