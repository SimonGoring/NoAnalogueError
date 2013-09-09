library(analogue)
library(MASS)
library(plyr)
library(rioja)
library(randomForest)
library(vegan)

climate <- read.csv('data/Clim_Pollen_Zirbel.csv', row.names=1)
mod_pol <- read.csv('data/ModP35c_East.csv', row.names=1, sep='\t')

new.pol <- mod_pol[,7:ncol(mod_pol)] / rowSums(mod_pol[,7:ncol(mod_pol)])

climate <- climate[rownames(mod_pol),]

dists <- dist(new.pol^(1/2))

dists[dists==0] <- min(dists[dists > 0], na.rm=TRUE)

if(!'pol.mds.RData' %in% list.files('data/')){
  pol.mds <- isoMDS(dists, k=2)
  save(pol.mds, file='data/pol.mds.RData')
}
else{
  load('data/pol.mds.RData')
}

#  Find the density of minimum distances:
diag.dist <- as.matrix(dists); diag(diag.dist) <- NA

min.diss <- apply(diag.dist, 2, min, na.rm=TRUE)
max.diss <- apply(diag.dist, 2, max, na.rm=TRUE)

min.dist.dens <- density(min.diss, from=0, to=1.42, n=100)
max.dist.dens <- density(max.diss, from=0, to=1.42, n=100)

quantiles <- c(0, quantile(ecdf(diag.dist), c(seq(0, 0.2, by=0.001), seq(0.21, 1, by=0.01))))

vals <- c(seq(0, 0.2, by=0.001), seq(0.21, 1, by=0.01))

mat.res <- list(prediction     = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                r.squared      = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                closest.n      = matrix(ncol=length(vals), nrow=nrow(new.pol)))

wa.res <- list(prediction      = matrix(ncol=length(vals), nrow=nrow(new.pol)),
               r.squared       = matrix(ncol=length(vals), nrow=nrow(new.pol)))

wapls.res <- list(prediction   = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                  r.squared    = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                  components.n = matrix(ncol=length(vals), nrow=nrow(new.pol)))

rfor.res <- list(prediction     = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                 r.squared      = matrix(ncol=length(vals), nrow=nrow(new.pol)),
                 closest.n      = matrix(ncol=length(vals), nrow=nrow(new.pol)))


fmat <- function(x){
  #  returns the means of the 1:10 closest:
  
  index.used <- order(diag.dist[x,])[keep.pol[x,]]
  ten.or.less <- min(c(10, sum(keep.pol[x,])))
  
  vals <- cumsum(climate[index.used,4][1:ten.or.less]) / 1:ten.or.less
  vars <- c(0, lapply(1:ten.or.less, function(x)var(climate[index.used,4][1:x])))
  
  if(length(vals) < 10) vals <- c(vals, rep(NA, 10-length(vals)))
  
  vals
  
}

i <- 1

for(i in 1:length(vals)){
  #  This runs through each analogue distance
  keep.pol <- aaply(diag.dist, 1, 
                    function(x) {x > quantiles[i]}, 
                    .progress = "text")
  
  diag(keep.pol) <- FALSE
  
  #  This is waaaay faster.
  if(any(is.na(mat.res$prediction[,i]))){
    fast.mat <- laply(1:nrow(diag.dist), fmat, .progress = "text")
    rmse <- aaply(fast.mat, 2, function(x) sqrt(mean((x - climate[,4])^2, na.rm = T)))
  }
  
  for(j in 1:nrow(new.pol)){
    
    #Too slow running MAT natively in the analogue package.
    #pred.mat <- predict(mat(x=new.pol[keep.pol,], y = climate[keep.pol,4]), newdata = new.pol[j,])

    zeros <- colSums(new.pol[keep.pol[j,],]) == 0
    
    #  Run WA with monotonic deshrinking.
    if(is.na(wa.res$prediction[j,i])){
      pred.wa <- try(predict(wa(x=new.pol[keep.pol[j,],!zeros], env = climate[keep.pol[j,],4], deshrinking='monotonic'), newdata = new.pol[j,!zeros]))
      if(length(pred.wa) > 1){
        
        wa.res$prediction[j,i]  <- pred.wa$pred$pred
        wa.res$r.squared[j,i]   <- pred.wa$performance$r.squared
        
      }
      
    }
    
    if(is.na(wapls.res$prediction[j,i])){
      wapls <- try(WAPLS(y=new.pol[keep.pol[j,],!zeros], x = climate[keep.pol[j,],4]))
      
      if (length(wapls) > 1){
        pred.wapls <- predict(wapls, newdata = new.pol[j,!zeros])
        wapls.res$prediction[j,i]    <- pred.wapls$fit[which.min(performance(wapls)$object[,1])]
        wapls.res$r.squared[j,i]     <- performance(wapls)$object[which.min(performance(wapls)$object[,1]), 2]
        wapls.res$components.n[j, i] <- which.min(performance(wapls)$object[,1])
      }
    }
    
    if(is.na(mat.res$prediction[j,i])){
      mat.res$prediction[j,i]  <- fast.mat[j, which.min(rmse)]
      mat.res$r.squared[j,i]   <- fast.mat[j, which.min(rmse)]
      mat.res$closest.n[j,i]   <- which.min(rmse)
    }
    
    if(is.na(rfor.res$prediction[j,i])){
      rfor <- try(randomForest(x=new.pol[keep.pol[j,],!zeros], y = climate[keep.pol[j,],4]))
      
      if(length(rfor) > 1){
        pred.rfor <- predict(rfor, newdata = new.pol[j,!zeros])
        rfor.res$prediction[j,i] <- pred.rfor
        rfor.res$r.squared[j,i]  <- mean(rfor$rsq)
      }
      
    }
            
    if(j %% 120 == 0) cat('.')
  }
  cat('\n', i)
}

getRMSEP <- function(x, n){
  vals <- sqrt(colMeans((x - climate[,n])^2, na.rm=TRUE))
  errs <- sqrt(apply((x - climate[,n])^2, 2, sd, na.rm=TRUE))
  rbind(vals, errs)
}


mat.rmsep <- getRMSEP(mat.res$prediction, 4)
wa.rmsep <- getRMSEP(wa.res$prediction, 4)
wapls.rmsep <- getRMSEP(wapls.res$prediction, 4)

plot(vals, mat.rmsep[1,], 
     ylim=range(1, mat.rmsep, wa.rmsep, wapls.rmsep, na.rm=T), 
     type='l', log='y',
     yaxt='n', bty='n', xlab='Analog Distance (sq. chord)', ylab='Winter Temperature RMSEP')
axis(2, at = 1:25)
lines(vals, wapls.rmsep[2,])
lines(vals, wa.rmsep[2,])
