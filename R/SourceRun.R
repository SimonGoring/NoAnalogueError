#  Run all bootstraps:
source('R/data_setup.R')

source('R/nonPar/bootstrap_mat.R')
cat('Done MAT/n')
source('R/nonPar/bootstrap_wa.R')
cat('Done WA/n')
source('R/nonPar/bootstrap_wapls.R')
cat('Done WAPLS/n')
source('R/nonPar/bootstrap_rfor.R')
cat('Done rFor/n')
source('R/nonPar/bootstrap_brt.R')
cat('Done rFor/n')