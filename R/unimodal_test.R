install.packages('diptest')

library(diptest)
library(mgcv)

dips <- matrix(ncol = ncol(new.pol)+1,
               nrow = ncol(climate))

for(i in 1:ncol(new.pol)){
  
  #  First row is to see whether the pollen data itself has a unimodal distribution
  dips[1,i]    <- dip.test(new.pol[,i], simulate.p.value = TRUE)$p.value  
  
  for(j in 4:ncol(climate)){
    
    samples <- data.frame(y = new.pol[,i], x = climate[,j])
    
    mod.test <- gam(round(cbind(y*1000, 1000),0) ~ s(x, k = 10), data = samples, family = binomial)
    output <- predict(mod.test, 
                      newdata = data.frame(x = seq(min(samples$x, na.rm=TRUE), 
                                                    max(samples$x, na.rm=TRUE), 
                                                   length.out = 100)),
                      type = 'response')
    
    dips[j,1]    <- dip.test(climate[,j], simulate.p.value = TRUE)$p.value  
    dips[j, i+1] <- dip.test(output, simulate.p.value = TRUE)$p.value
    
    cat(ifelse(dips[j, i]<0.05, 't', 'f'))
  }
  cat(i, '\n')
}