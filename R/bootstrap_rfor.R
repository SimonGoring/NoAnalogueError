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

#  Create a set of all possible xy pairs in the rfor tables, and then look to see if
#  they've been sampled yet.
longlist <- expand.grid(row = 1:nrow(new.pol), col=1:length(vals))
longlist <- data.frame(longlist, calc = is.na(as.vector(rfor.res$bias)))

#  Now to sample in an efficient way:  This expanded grid gives the probability
#  that a cell is NA in a row or in a column.  If we multiply them together then
#  we get a vector of values for each cell that gives its importance in terms of
#  sampling, with higher values indicating lower rates of sampling in the row/column
#  pair.
prob.vec <- expand.grid(row = rowSums(is.na(rfor.res$bias))/length(vals),
						col = colSums(is.na(rfor.res$bias))/nrow(new.pol))

prob.vec <- prob.vec[,1] * prob.vec[,2]
#  Just to get it to range from ~0 - 1
prob.vec <- (prob.vec - min(prob.vec) + 0.001) / max(prob.vec - min(prob.vec) + 0.01)

#  A simple t-test shows that NA cells have a mean sampling probability of 0.58 and
#  already sampled cells have a probability of 0.49.  We're going to drop already
#  sampled cells anyway, but it helps support our decision to sample this way.

#  Re-order, based on the probability vector defined above:						
samp <- sample(nrow(longlist), 
               prob = prob.vec)

longlist <- longlist[samp,]
longlist <- longlist[longlist[,3],]

for(k in samp){
  #  This runs through each analogue distance
  run.start <- proc.time()
  
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
  
  end.time <- proc.time()
  
  st <- Sys.time()
  
  cat(round(sum(!is.na(rfor.res$bias))/length(rfor.res$bias) * 100, 4), '% done on', 
      weekdays(st), format(st, '%d'), months(st), format(st, '%Y'),
      'at', format(st, '%H:%M'),
      'in', (end.time - run.time)[3], 'seconds.\n')
}
