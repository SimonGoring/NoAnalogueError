# Test version of the BRT cross-validation code.
#
# This version does not save the results, but only tests the time requirement of the BRT
# cross validation. 23 samples (i.e. 1% of total) are selected at random, and 50 bootstrap
# iterations are performed for each sample. Predicted value and used time are printed
# in each iteration.

library (gbm)

# Read the data from Excel. Note that the data must be organised as in NA_pollen_for_BRT.csv
# (same column names and column order)

cal_data <- sqrt(read.table("clipboard", header=TRUE))
attach (cal_data)

# Construct the formula string for gbm

long_formula <- as.formula(paste("tjul ~ ", paste(paste("v", 1:56, sep = ""), collapse = " + "), sep = ""))

# Select 23 random samples to test with

test_samples <- sample(x=2289, size=23, replace=FALSE)

# Run 50 bootstrap cycles with 23 random samples. The predicted values are not stored: 
# they are over-written in each iteration in variable "pred".

start_time <- Sys.time()
pred <- numeric()

for (i in 1:23) {

   for (j in 1:50) {
   
      iter_start_time <- Sys.time()
      calibration_set <- cal_data[sample(x=2289, size=2289, replace=TRUE),]
      attach (calibration_set, warn.conflicts = FALSE)
      xval_model <- gbm(long_formula , distribution="gaussian", n.trees=500, shrinkage=0.005, interaction.depth=6, cv.folds = 4, verbose=FALSE)  # Constructs the BRT model
      xval_best_iter <- gbm.perf(xval_model,method="cv",plot.it=FALSE)  # Returns the number of trees that gives the best performance
      test_set <- cal_data[test_samples[i],4:59]
      attach (test_set, warn.conflicts = FALSE)
      pred <- predict(xval_model,test_set,xval_best_iter, type="response") # Predicts for the test-set sample
      iter_end_time <- Sys.time()
      print(paste("Sample ",test_samples[i]," (",i,"/23), bootstrap cycle ",j,"/50, predicted value ",pred,".", sep=""))
      print (iter_end_time - iter_start_time)
	  
   }
   
}

end_time <- Sys.time()
print (end_time-start_time)
