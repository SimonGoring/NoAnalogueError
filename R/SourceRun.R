
#  Run all bootstraps:
source('R/data_setup.R')

source('R/bootstrap_mat.R')
cat('Done MAT/n')
source('R/bootstrap_wa.R')
cat('Done WA/n')
source('R/bootstrap_wapls.R')
cat('Done WAPLS/n')
source('R/bootstrap_rfor.R')
cat('Done rFor/n')
