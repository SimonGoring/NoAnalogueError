library(snowfall)

library(randomForest)

sfStop()
sfInit(parallel = TRUE, cpus = 6)

vals <- seq(0, 1, by=0.01)

if('rfor.res.RData' %in% list.files('data')){
  load('data/rfor.res.RData')
}
if(!'rfor.res.RData' %in% list.files('data')){
  rfor.res <- list(mean_prediction  = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                   sample_size      = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                   bias             = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                   variance         = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                   expectation      = matrix(ncol=length(vals), nrow=nrow(new.pol)))
}

i <- 1; j <- 1

subset.pol <- function(set){
  
  calib.samples <- sample((1:length(set))[set], replace=TRUE)
  
  zeros <- colSums(new.pol[calib.samples,], na.rm = TRUE) == 0
  
  list(calib.pol  = new.pol[calib.samples,!zeros],
       calib.clim = climate[calib.samples,10],
       zeros = zeros)

}

rfor.run <- function(j){
  
  set <- keep.pol[j,]
  
  x <- subset.pol(set)
  
  zeros <- colSums(x$calib.pol, na.rm = TRUE) == 0
  
  rfor <- try(randomForest(x=x$calib.pol[,!zeros], 
                     y = x$calib.clim))
  
  if (length(rfor) > 1){
    output <- predict(rfor, newdata = new.pol[j,!zeros])
    }
  else{
    output <- NA
  }
  return(output)
  
}

#  Parallelize:
sfExport(list = list('rfor.run'))
sfExport(list = list('new.pol'))
sfExport(list = list('subset.pol'))
sfExport(list = list('climate'))
sfLibrary(randomForest)

longlist <- expand.grid(x=1:length(vals), y = 1:nrow(new.pol))

samp <- sample(nrow(longlist))

for(k in samp){
  #  This runs through each analogue distance
  
  i <- longlist[k,1]
  j <- longlist[k,2]
  
  keep.pol <- aaply(diag.dist, 1, 
                    function(x) {x > vals[i]})
  diag(keep.pol) <- FALSE
  
  sfExport(list = list('keep.pol'))
    
  if(is.na(rfor.res$mean_prediction[j,i])){
    #  Run randomForest with raw defaults
    prediction <- unlist(sfLapply(rep(j, 50), fun = rfor.run))
  
    rfor.res$mean_prediction[j,i] <- mean(prediction, na.rm=TRUE)
    rfor.res$sample_size[j,i] <- sum(keep.pol[j,], na.rm=TRUE)
    rfor.res$bias[j, i] <- (climate[j,10] - mean(prediction, na.rm=TRUE))^2
    rfor.res$expectation[j, i]  <- mean((climate[j,10] - prediction)^2)
    rfor.res$variance[j, i]  <- mean((mean(prediction) - prediction)^2)
  }

  save(rfor.res, file = 'data/rfor.res.RData')
  cat(round(which(k==samp)/length(samp) * 100, 4), '% at', as.character(Sys.time()), '\n')
}
