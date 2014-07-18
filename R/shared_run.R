#  Sakari, make sure that the default directory is set as the main 'NoAnalogueError' directory,
#  and not the R directory.

source('R/data_setup.R')

load('data/total.output.RData')

vals <- seq(0, 1, by=0.01)

#  Sakari, I doubt it will, but you need to make sure that 'goring' isn't anywhere in your
#  working directory path!
#analyst <- ifelse(regexpr('goring', tolower(getwd()))>0, 'Simon', 'Sakari')
analyst <- 'Simon'

if(paste('longlist.', analyst, '.RData', sep='') %in% list.files('data')){
  load(paste0('data/longlist.', analyst, '.RData'))
}

if(!exists('longlist')){
  longlist <- data.frame(total.output, 
                         calc = !is.na(total.output[,3]))
}
if(exists('longlist')){
  if(any(is.na(longlist$bias) & !is.na(total.output$bias))){
    longlist[is.na(longlist$bias) & !is.na(total.output$bias), colnames(total.output)] <-
      total.output[is.na(longlist$bias) & !is.na(total.output$bias), ]
  }
}

save(longlist, file=paste('data/longlist.', analyst, '.RData', sep=''))

library(snowfall)
library(gbm)

sfStop()
sfInit(parallel = TRUE, cpus = 30)

subset.pol <- function(set){
  
  calib.samples <- sample((1:length(set))[set], replace=TRUE)
  
  zeros <- colSums(new.pol[calib.samples,], na.rm = TRUE) == 0
  
  # Modified the following to return the climate values as a 
  # single-column data frame.
  
  list(calib.pol = new.pol[calib.samples,!zeros],
       calib.clim = climate[calib.samples,10,drop=FALSE], 
       zeros = zeros)   
  
}

brt.run <- function(px, pa){
  
  set <- keep.pol
  
  x <- subset.pol(set)
  
  zeros <- colSums(x$calib.pol, na.rm = TRUE) == 0
  bad.clim <- is.na(x$calib.clim)
  
  # Construct the brt formula based on x$calib.clim and x$calib.pol column 
  # names, omitting the zero-sum taxa which won't be used.
  
  brt.formula <- as.formula(paste (colnames(x$calib.clim)[1], "~", paste(colnames(x$calib.pol)[!zeros], collapse=" + ")))
  
  # Construct brt model. x$calib.clim and x$calib.pol combined into a single
  # data frame and passed as the "data" parameter.
  
  brt.model <- try(gbm(brt.formula, distribution="gaussian", 
                   n.trees=500, shrinkage=0.005, interaction.depth=6, 
                   cv.folds=4, verbose=FALSE, 
                   data=cbind(x$calib.clim, x$calib.pol[,!zeros])[!bad.clim,]))
  
  if(length(brt.model) > 1){
  # This probably always returns 500
    brt.ntrees <- gbm.perf(brt.model, method="cv", plot.it=FALSE)   
    output <- predict(brt.model, new.pol[px,!zeros], brt.ntrees, type="response")
  }
  else{
    output <- NA
  }
  return(output)
  
}

rfor.run <- function(px, pa){

  set <- keep.pol
  
  x <- subset.pol(set)
  
  zeros <- colSums(x$calib.pol, na.rm = TRUE) == 0
  bad.clim <- is.na(x$calib.clim)
  
  rfor <- try(randomForest(x = x$calib.pol[!bad.clim, !zeros], 
                           y = x$calib.clim$tjul[!bad.clim]))
  
  if (length(rfor) > 1){
    output <- predict(rfor, newdata = new.pol[px, !zeros])
  }
  else{
    output <- NA
  }
  return(output)
  
}

# Parallelize:
sfExport(list = list('brt.run'))
sfExport(list = list('rfor.run'))
sfExport(list = list('new.pol'))
sfExport(list = list('subset.pol'))
sfExport(list = list('climate'))
sfLibrary(gbm) 
sfLibrary(randomForest)

# Create a set of all possible xy pairs in the brt tables, and then look to see if
# they've been sampled yet.

calc.probvec <- function(x = longlist){
  #  Every once in a while, resample the probability data to make sure we're sampling in a
  #  reasonable way from the dataset.
  
  #  Default is to sample it:
  prob.vec <- rep(1, nrow(x))
  
  #  If rfor had been sampled then brt should be given preference.
  rfor.sampled <- which(x$method == 'rfor' & x$bias == FALSE)
  prob.vec[x$method == 'brt'][rfor.sampled] <- 1
  
  #  Then we want to fill in points based on how rarely they've been sampled:
  x1.sampled <- rep(rep(table(factor(x[,1])[is.na(x$bias)]) / 202, each = 101)^2, 2)
  x2.sampled <- rep(rep(table(factor(x[,2])[is.na(x$bias)]) / 3874, 1937)^2, 2)
  
  prob.vec <- (prob.vec + x1.sampled + x2.sampled) / 3
    
  prob.vec[x$calc == TRUE] <- 0
  
  prob.vec

}

longlist$prob.vec <- calc.probvec(longlist)

while(sum(!longlist$calc[longlist$who == analyst]) > 0){
  # This runs through each analogue distance
  run.time <- proc.time()
  
  #  sampling with a huge long probability vector is insanely slow:
  i <- sample(nrow(longlist), 1)
  
  if(rbinom(1, 1, longlist$prob.vec[i]) == 1 & longlist$who[i] == analyst){
  
    cat('\nStarting', longlist$method[i], 'run. \n')
    keep.pol <- aaply(diag.dist, 1, 
                    function(x) {x[longlist$Var1[i]] > vals[longlist$Var2[i]]})
     
    keep.pol[longlist$Var1[i]] <- FALSE
    
    sfExport(list = list('keep.pol'))
    
    px <- longlist[i,1]
    pa <- longlist[i,2]
    
    if(longlist$method[i] == 'brt'){
            
      prediction <- unlist(sfLapply(rep(px, 30), fun = brt.run, pa = pa))
      
      longlist$mean_prediction[i] <- mean(prediction, na.rm=TRUE)
      longlist$sample_size[i] <- sum(keep.pol, na.rm=TRUE)
      longlist$bias[i] <- (climate[px,10] - mean(prediction, na.rm=TRUE))^2
      longlist$expectation[i] <- mean((climate[px,10] - prediction)^2)
      longlist$variance[i] <- mean((mean(prediction) - prediction)^2)
      longlist$calc[i] <- TRUE
    }
    if(longlist$method[i] == 'rfor'){
      
      prediction <- unlist(sfLapply(rep(px, 30), fun = rfor.run, pa = pa))
      
      longlist$mean_prediction[i] <- mean(prediction, na.rm=TRUE)
      longlist$sample_size[i] <- sum(keep.pol, na.rm=TRUE)
      longlist$bias[i] <- (climate[px,10] - mean(prediction, na.rm=TRUE))^2
      longlist$expectation[i] <- mean((climate[px,10] - prediction)^2)
      longlist$variance[i] <- mean((mean(prediction) - prediction)^2)
      longlist$calc[i] <- TRUE
    }
    
    save(longlist, file=paste('data/longlist.', analyst, '.RData', sep=''))
    
    end.time <- proc.time()
    
    st <- Sys.time()
    
    cat('\n',round(sum(!is.na(longlist$bias))/length(longlist$bias) * 100, 4), '% done on',
        weekdays(st), format(st, '%d'), months(st), format(st, '%Y'),
        'at', format(st, '%H:%M'),
        'in', round((end.time - run.time)[3]/60, 1), 'minutes.\n')
  }
  
  #  Every once in a while make sure we update which samples have been sampled & stuff.
  if(i%%50 == 0){longlist$prob.vec <- calc.probvec(longlist)}
  cat('.')
}
