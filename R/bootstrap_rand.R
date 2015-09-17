library(snowfall)

sfStop()
sfInit(parallel = TRUE, cpus = 2)

# Set up the analogue distance exclusion values.
#  This was originally a set of quantiles, but it played havoc on plotting and
#  didn't seem to add much to the understanding of what was going on.
vals <- seq(0, 1, by=0.01)

if('rand.res.RData' %in% list.files('data')){
  load('data/rand.res.RData')
} else {
  
  rand.res <- list(mean_prediction  = matrix(ncol=length(vals), nrow=nrow(new.pol)),
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

rand.run <- function(j){
  
  set <- keep.pol[j,]
  
  x <- subset.pol(set)
  
  bad.clim <- is.na(x$calib.clim)
  
  rand <- sample(x$calib.clim[!bad.clim],1)
  
  output <- rand
  
  return(output)
  
}

#  Parallelize:
sfExport(list = list('rand.run'))
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
    
    if(is.na(rand.res$mean_prediction[j,i])){
      #  Run WAPLS
      prediction <- unlist(sfLapply(rep(j, 30), fun = rand.run))
      
      rand.res$mean_prediction[j,i] <- mean(prediction, na.rm=TRUE)
      rand.res$sample_size[j,i] <- sum(keep.pol[j,], na.rm=TRUE)
      rand.res$bias[j, i] <- (climate[j,10] - mean(prediction, na.rm=TRUE))^2
      rand.res$exp[j, i]  <- mean((climate[j,10] - prediction)^2)
      rand.res$variance[j, i]  <- mean((mean(prediction) - prediction)^2)
    }
    
  }
  
  save(rand.res, file = 'data/rand.res.RData')
  cat(i)
}
