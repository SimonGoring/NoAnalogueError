library(snowfall)

sfInit(parallel = TRUE, cpus = 4)

# Find the quantiles & set up the analogue distance values:
quantiles <- c(0, quantile(ecdf(diag.dist), c(seq(0, 0.2, by=0.001), seq(0.21, 1, by=0.01))))
vals <- c(seq(0, 0.2, by=0.001), seq(0.21, 1, by=0.01))

mat.res <- list(mean_prediction  = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                sample_size      = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                bias             = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                variance         = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                expectation      = matrix(ncol=length(vals), nrow=nrow(new.pol)))

#  Turn the distance table into a matrix.
diag.dist <- as.matrix(dists); diag(diag.dist) <- NA

i <- 1; j <- 1

#  For MAT to work we don't need to re-calculate the distace matrix, we just need to know the
#  sample of interest and the subset:

#  A function 
subset.pol <- function(set){
  
  calib.samples <- sample((1:length(set))[set], replace=TRUE)
  
  list(calib.pol  = new.pol[calib.samples, ],
       calib.clim = climate[calib.samples,4])

}

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
sfExport(list = list('subset.pol'))
sfExport(list = list('climate'))

for(i in i:length(vals)){
  #  At each quantile, figure out which samples should be acceptable for a calibration set targeting
  #  each sample return 'keep.pol'
  keep.pol <- aaply(diag.dist, 1, 
                    function(x) {x > quantiles[i]}, 
                    .progress = "text")

  diag(keep.pol) <- FALSE
  
  if(any(is.na(mat.res$prediction[,i]))){
    fast.mat <- laply(1:nrow(diag.dist), fmat, .progress = "text")
    rmse <- aaply(fast.mat, 2, function(x) sqrt(mean((x - climate[,10])^2, na.rm = T)))
  }
  
  sfExport(list = list('keep.pol'))
  

  for(j in 1:nrow(new.pol)){

    prediction <- unlist(sfLapply(1:100, mat.fun, j = j, min = which.min(rmse)))
    
    if(is.na(mat.res$mean_prediction[j,i])){
      mat.res$mean_prediction[j,i] <- mean(prediction, na.rm=TRUE)
      mat.res$sample_size[j,i] <- sum(keep.pol[j,], na.rm=TRUE)
      mat.res$bias[j, i] <- (climate[j,10] - mean(prediction, na.rm=TRUE))^2
      mat.res$exp[j, i]  <- mean((climate[j,10] - prediction)^2, na.rm=TRUE)
      mat.res$var[j, i]  <- mean((mean(prediction, na.rm=TRUE) - prediction)^2, na.rm=TRUE)
    }
    
    cat('\n', j)  
    
  }
    
  save(mat.res, file = 'data/mat.res.RData')
  cat('\n', i)
}