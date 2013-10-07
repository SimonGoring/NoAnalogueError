library(snowfall)

sfStop()
sfInit(parallel = TRUE, cpus = 6)

# Set up the analogue distance exclusion values.
#  This was originally a set of quantiles, but it played havoc on plotting and
#  didn't seem to add much to the understanding of what was going on.
vals <- seq(0, 1, by=0.01)

wa.res <- list(mean_prediction  = matrix(ncol=length(vals), nrow=nrow(new.pol)),
               sample_size      = matrix(ncol=length(vals), nrow=nrow(new.pol)),
               bias             = matrix(ncol=length(vals), nrow=nrow(new.pol)),
               variance         = matrix(ncol=length(vals), nrow=nrow(new.pol)),
               expectation      = matrix(ncol=length(vals), nrow=nrow(new.pol)))

if('wa.res.RData' %in% list.files('data/')) {
  load('data/wa.res.RData')
}

i <- 1; j <- 1

subset.pol <- function(set){
  
  calib.samples <- sample((1:length(set))[set], replace=TRUE)
  
  zeros <- colSums(new.pol[calib.samples,], na.rm = TRUE) == 0
  
  list(calib.pol  = new.pol[calib.samples,!zeros],
       calib.clim = climate[calib.samples,10],
       zeros = zeros)

}

wa.run <- function(j){
  
  set <- keep.pol[j,]
  
  x <- subset.pol(set)
  
  zeros <- colSums(x$calib.pol, na.rm = TRUE) == 0
  
  pred.wa <- try(predict(wa(x=x$calib.pol, 
                            env = x$calib.clim, 
                            deshrinking='monotonic'), 
                         newdata = new.pol[j,!x$zeros]))
  
  if(length(pred.wa) > 1){
    
    output <- pred.wa$pred$pred
    
  }
  else{
    output <- NA
  }
  return(output)
  
}

#  Parallelize:
sfExport(list = list('wa.run'))
sfExport(list = list('new.pol'))
sfExport(list = list('subset.pol'))
sfExport(list = list('climate'))
sfLibrary(analogue)


for(i in i:length(vals)){
  #  This runs through each analogue distance
  keep.pol <- aaply(diag.dist, 1, 
                    function(x) {x > vals[i]})

  sfExport(list = list('keep.pol'))
  
  diag(keep.pol) <- FALSE
  
  for(j in 1:nrow(new.pol)){
    
    if(is.na(wa.res$mean_prediction[j,i])){
      #  Run WA with monotonic deshrinking.
      prediction <- unlist(sfLapply(rep(j, 100), fun = wa.run))
    
      wa.res$mean_prediction[j,i] <- mean(prediction, na.rm=TRUE)
      wa.res$sample_size[j,i] <- sum(keep.pol[j,], na.rm=TRUE)
      wa.res$bias[j, i] <- (climate[j,4] - mean(prediction, na.rm=TRUE))^2
      wa.res$expectation[j, i]  <- mean((climate[j,4] - prediction)^2, na.rm=TRUE)
      wa.res$variance[j, i]  <- mean((mean(prediction, na.rm=TRUE) - prediction)^2, na.rm=TRUE)
    }
        
  }
  if(ncol(wa.res$bias) > i){
    if(!sum(is.na(wa.res$bias[,i+1])) == 0){
      save(wa.res, file = 'data/wa.res.RData')
    }
  }
  
  cat(i)  
  
}
