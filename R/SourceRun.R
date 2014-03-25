#  Run all bootstraps:
source('R/data_setup.R')


par <- FALSE

file.lead <- ifelse(par == TRUE, 'R/', 'R/nonPar/')

source(paste(file.lead, 'bootstrap_mat.R', sep ='')
cat('Done MAT/n')

source(paste(file.lead, 'bootstrap_wa.R', sep ='')
cat('Done WA/n')

source(paste(file.lead, 'bootstrap_wapls.R', sep ='')
cat('Done WAPLS/n')

source(paste(file.lead, 'bootstrap_rfor.R', sep ='')
cat('Done rFor/n')

source(paste(file.lead, 'bootstrap_brt.R', sep ='')
cat('Done rFor/n')