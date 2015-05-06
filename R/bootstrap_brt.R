# This is a modification of bootstrap_rfor.R, with the following changes:
# - instead of "randomForest", loads (and passes to snowfall)
#   the "gbm" library
# - all references to "rfor.res" replaced with "brt.res"
# - modified subset.pol() to return the climate values as a single-column 
#   data frame (and not as vector)
# - wrote function brt.run(), which replaced rfor.run()

library(snowfall)

library(gbm)

sfStop()
sfInit(parallel = TRUE, cpus = 30)

vals <- seq(0, 1, by=0.01)

if('brt.res.RData' %in% list.files('data')){
  load('data/brt.res.RData')
}
if(!'brt.res.RData' %in% list.files('data')){
  brt.res <- list(mean_prediction = matrix(ncol=length(vals), nrow=nrow(new.pol)),   
                   sample_size = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                   bias = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                   variance = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                   expectation = matrix(ncol=length(vals), nrow=nrow(new.pol)))
}

i <- 1; j <- 1

subset.pol <- function(set){
  
  calib.samples <- sample((1:length(set))[set], replace=TRUE)
  
  zeros <- colSums(new.pol[calib.samples,], na.rm = TRUE) == 0

  # Modified the following to return the climate values as a 
  # single-column data frame.
  
  list(calib.pol = new.pol[calib.samples,!zeros],
       calib.clim = climate[calib.samples,10,drop=FALSE], 
       zeros = zeros)   

}

brt.run <- function(j){
  
  set <- keep.pol[j,]
  
  x <- subset.pol(set)
  
  zeros <- colSums(x$calib.pol, na.rm = TRUE) == 0
  bad.clim <- is.na(x$calib.clim)

  # Construct the brt formula based on x$calib.clim and x$calib.pol column 
  # names, omitting the zero-sum taxa which won't be used.
  
  brt.formula <- as.formula(paste (colnames(x$calib.clim)[1], "~", 
                                   paste(colnames(x$calib.pol)[!zeros], collapse=" + ")))

  # Construct brt model. x$calib.clim and x$calib.pol combined into a single
  # data frame and passed as the "data" parameter.
  
  brt.model <- gbm(brt.formula, distribution="gaussian", 
                   n.trees=500, shrinkage=0.005, 
                   interaction.depth=6, cv.folds=4, 
                   verbose=FALSE, 
                   data=cbind(x$calib.clim, 
                              x$calib.pol[,!zeros])[!bad.clim,])

  # This probably always returns 500
  brt.ntrees <- gbm.perf(brt.model, method="cv", plot.it=FALSE)   
  
  if (length(brt.model) > 1){
    output <- predict(brt.model, new.pol[j,!zeros], brt.ntrees, type="response")
    }
  else{
    output <- NA
  }
  return(output)
  
}

# Parallelize:
sfExport(list = list('brt.run'))
sfExport(list = list('new.pol'))
sfExport(list = list('subset.pol'))
sfExport(list = list('climate'))
sfLibrary(gbm) 

# Create a set of all possible xy pairs in the brt tables, and then look to see if
# they've been sampled yet.
longlist <- expand.grid(row = 1:nrow(new.pol), col=1:length(vals))
longlist <- data.frame(longlist, calc = is.na(as.vector(brt.res$bias)))

# Now to sample in an efficient way: This expanded grid gives the probability
# that a cell is NA in a row or in a column. If we multiply them together then
# we get a vector of values for each cell that gives its importance in terms of
# sampling, with higher values indicating lower rates of sampling in the row/column
# pair.
prob.vec <- expand.grid(row = rowSums(is.na(brt.res$bias))/length(vals),
                                                col = colSums(is.na(brt.res$bias))/nrow(new.pol))

prob.vec <- prob.vec[,1] * prob.vec[,2]
# Just to get it to range from ~0 - 1
prob.vec <- (prob.vec - min(prob.vec) + 0.001) / max(prob.vec - min(prob.vec) + 0.01)

# A simple t-test shows that NA cells have a mean sampling probability of 0.58 and
# already sampled cells have a probability of 0.49. We're going to drop already
# sampled cells anyway, but it helps support our decision to sample this way.

# Re-order, based on the probability vector defined above:
samp <- sample(nrow(longlist),
               prob = prob.vec)

longlist <- longlist[samp,]
longlist <- longlist[longlist[,3],]

for(k in samp){
  # This runs through each analogue distance
  run.time <- proc.time()
  
  i <- longlist[k,2]
  j <- longlist[k,1]
  
  keep.pol <- aaply(diag.dist, 1,
                    function(x) {x > vals[i]})
  
  diag(keep.pol) <- FALSE
  
  sfExport(list = list('keep.pol'))
    
  if(is.na(brt.res$mean_prediction[j,i])){

    prediction <- unlist(sfLapply(rep(j, 30), fun = brt.run))
  
    brt.res$mean_prediction[j,i] <- mean(prediction, na.rm=TRUE)
    brt.res$sample_size[j,i] <- sum(keep.pol[j,], na.rm=TRUE)
    brt.res$bias[j, i] <- (climate[j,10] - mean(prediction, na.rm=TRUE))^2
    brt.res$expectation[j, i] <- mean((climate[j,10] - prediction)^2)
    brt.res$variance[j, i] <- mean((mean(prediction) - prediction)^2)
  }

  save(brt.res, file = 'data/brt.res.RData')
  
  end.time <- proc.time()
  
  st <- Sys.time()
  
  cat(round(sum(!is.na(brt.res$bias))/length(brt.res$bias) * 100, 4), '% done on',
      weekdays(st), format(st, '%d'), months(st), format(st, '%Y'),
      'at', format(st, '%H:%M'),
      'in', round((end.time - run.time)[3]/60, 1), 'minutes.\n')
}
