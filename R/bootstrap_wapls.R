library(snowfall)

sfStop()
sfInit(parallel = TRUE, cpus = 6)

# Set up the analogue distance exclusion values.
#  This was originally a set of quantiles, but it played havoc on plotting and
#  didn't seem to add much to the understanding of what was going on.
vals <- c(seq(0, 1, by=0.01))

if('wapls.res.RData' %in% list.files('data')){
  load('data/wapls.res.RData')
}
if(!'wapls.res.RData' %in% list.files('data')){
  wapls.res <- list(mean_prediction  = matrix(ncol=length(vals), nrow=nrow(new.pol)),
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

wapls.run <- function(j){
  
  set <- keep.pol[j,]
  
  x <- subset.pol(set)
  
  zeros <- colSums(x$calib.pol, na.rm = TRUE) == 0
  
  wapls <- try(WAPLS(y=x$calib.pol[,!zeros], 
                     x = x$calib.clim))
  
  if (length(wapls) > 1){
    pred.wapls <- predict(wapls, newdata = new.pol[j,!zeros])
    output    <- pred.wapls$fit[which.min(performance(wapls)$object[,1])]
    }
  else{
    output <- NA
  }
  return(output)
  
}

#  Parallelize:
sfExport(list = list('wapls.run'))
sfExport(list = list('new.pol'))
sfExport(list = list('subset.pol'))
sfExport(list = list('climate'))
sfLibrary(rioja)


for(i in 1:length(vals)){
  #  This runs through each analogue distance
  keep.pol <- aaply(diag.dist, 1, 
                    function(x) {x > vals[i]})
  diag(keep.pol) <- FALSE
  
  sfExport(list = list('keep.pol'))
  
  for(j in 1:nrow(new.pol)){
    
    if(is.na(wapls.res$mean_prediction[j,i])){
      #  Run WA with monotonic deshrinking.
      prediction <- unlist(sfLapply(rep(j, 100), fun = wapls.run))
    
      wapls.res$mean_prediction[j,i] <- mean(prediction, na.rm=TRUE)
      wapls.res$sample_size[j,i] <- sum(keep.pol[j,], na.rm=TRUE)
      wapls.res$bias[j, i] <- (climate[j,10] - mean(prediction, na.rm=TRUE))^2
      wapls.res$exp[j, i]  <- mean((climate[j,10] - prediction)^2)
      wapls.res$var[j, i]  <- mean((mean(prediction) - prediction)^2)
    }
    
  }
    
  save(wapls.res, file = 'data/wapls.res.RData')
  cat(i)
}

  