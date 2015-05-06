#if(install.it == TRUE){
#  install.packages(c('gridExtra', 'ggplot2', 'MASS', 'plyr', 'rioja', 'randomForest', 'vegan', 'maps', 'maptools', 'ggmap'))
#}

library(gridExtra)
library(ggplot2)
library(MASS)
library(plyr)
library(rioja)
library(randomForest)
library(vegan)
library(maps)
library(maptools)
library(ggmap)
library(reshape2)
library(analogue)

climate <- read.csv('data/Clim_Pollen_Zirbel.csv', row.names=1)
mod_pol <- read.csv('data/ModP35c_East.csv', row.names=1, sep='\t')

mod_pol <- mod_pol[mod_pol$LONDD > -100, ]
climate <- climate[climate$LONDD > -100, ]

bad.pol <- c("LATDD", "LONDD", "XXX", "XXX.1", "ELEVATION", "PolSum", 
             'SPHAGNUM', 'CYPERACE', 'PTERIDIUM')

new.pol <- mod_pol[, !colnames(mod_pol)%in% bad.pol]
  
new.pol <- new.pol / rowSums(new.pol, na.rm=TRUE)

climate <- climate[rownames(mod_pol),]

dists <- dist(new.pol^(1/2))

dists[dists==0] <- min(dists[dists > 0], na.rm=TRUE)

if(!'pol.mds.RData' %in% list.files('data/')){
  pol.mds <- isoMDS(dists, k=2)
  save(pol.mds, file='data/pol.mds.RData')
}

if('pol.mds.RData' %in% list.files('data/')){
  load('data/pol.mds.RData')
}

#  Find the density of minimum distances:
diag.dist <- as.matrix(dists); diag(diag.dist) <- NA

min.diss <- apply(diag.dist, 2, min, na.rm=TRUE)
max.diss <- apply(diag.dist, 2, max, na.rm=TRUE)

min.dist.dens <- density(min.diss, from=0, to=1.5, n=100)
max.dist.dens <- density(max.diss, from=0, to=1.5, n=100)
