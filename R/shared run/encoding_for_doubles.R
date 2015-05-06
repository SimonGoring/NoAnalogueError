#  To run parallel versions of rFor and BRT on Sakari and Simon's computers at the same time.
#  This is just the setup.

source('R/data_setup.R')

simon <- ifelse(regexpr('goring', tolower(getwd()))>0, TRUE, FALSE)

#  We assume that all the other files have been run (because they have!).

library(snowfall)

library(gbm)

sfStop()
sfInit(parallel = TRUE, cpus = 6)

vals <- seq(0, 1, by=0.01)

load('data/brt.res.RData')
load('data/rfor.res.RData')

total.rfor <- dcast(melt(rfor.res), Var1 + Var2 ~ L1)
total.brt <- dcast(melt(brt.res), Var1 + Var2 ~ L1)

total.output <- rbind(total.rfor, total.brt)
total.output$method <- rep(c('rfor', 'brt'), each = nrow(total.rfor))
total.output$who <- rep(c('Simon', 'Sakari'), nrow(total.rfor))

save(total.output, file='data/total.output.RData')
