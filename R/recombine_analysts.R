#  Taking the shared run files and stitching them back into the respective boot files:
library(reshape2)

source('R/data_setup.R')

load('data/total.output.RData')
load('data/longlist.Simon.RData')

dump.it <- function(x, y, set){
  
  dump.set <- x[x$method == set,]
  
  y$mean_prediction <- acast(data=dump.set, Var1 ~ Var2, fun.aggregate=sum, value.var='mean_prediction')
  y$bias <- acast(data=dump.set, Var1 ~ Var2, fun.aggregate=sum, value.var='bias')
  y$expectation <- acast(data=dump.set, Var1 ~ Var2, fun.aggregate=sum, value.var='expectation')
  y$variance <- acast(data=dump.set, Var1 ~ Var2, fun.aggregate=sum, value.var='variance')
  y$sample_size <- acast(data=dump.set, Var1 ~ Var2, fun.aggregate=sum, value.var='sample_size')
  
  y
}

rfor.res <- dump.it(longlist, rfor.res, 'rfor')
brt.res <- dump.it(longlist, brt.res, 'brt')
