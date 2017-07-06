## R commands for STAT 494 HW1
## Special Topics in Statistics, Probability and Operations Research:
## Statistical Techniques for Machine Learning and Data Mining
## Fall Semester 2016
## Version: 2016.08.28
## Reference: http://homepages.math.uic.edu/~jyang06/stat494/stat494.html


## R package: ElemStatLearn, version 2015.6.26
## Data Sets, Functions and Examples from the Book (ESL): 
## "The Elements of Statistical Learning, Data Mining, Inference, and Prediction" 
## by Trevor Hastie, Robert Tibshirani and Jerome Friedman, Second Edition, 2009

## At Cran: https://cran.r-project.org/web/packages/ElemStatLearn/index.html
## Reference: Section 3.2.1 in the ESL book

# load the package  "ElemStatLearn" 
library("ElemStatLearn")

# load the data set "prostate"
data(prostate)

# repeat the procedure for 100 times
crossvalidation <- function(i) {
  # randomly partition the original data into training and testing datasets
  train_ind <- sample(seq_len(nrow(prostate)), size = 67)
  train <- prostate[train_ind, ]
  test <- prostate[-train_ind, ]
  
  # fit linear model on training dataset using LS method
  trainst <- train
  for(i in 1:8) {
    trainst[,i] <- trainst[,i] - mean(prostate[,i]);
    trainst[,i] <- trainst[,i]/sd(prostate[,i]);
  }
  fitls <- lm( lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=trainst )
  
  # fit the reduced model:
  fitlsr <- lm( lpsa ~ lcavol+lweight+lbph+svi, data=trainst )
  
  ## check testing errors
  testst <- test
  for(i in 1:8) {
    testst[,i] <- testst[,i] - mean(prostate[,i]);
    testst[,i] <- testst[,i]/sd(prostate[,i]);
  }
  
  # mean prediction error on testing data using mean training value
  mean(trainst[,9])                             # 2.453891
  # mean (absolute) prediction error
  mean(abs(testst[,9]-mean(trainst[,9])))       # 0.7114861
  # mean (squared) prediction error
  mean((testst[,9]-mean(trainst[,9]))^2)        # 0.9511531
  
  # mean prediction error based on full model
  test.fitls=predict(fitls, newdata=testst)  
  # mean (absolute) prediction error
  mean(abs(test[,9]-test.fitls))                # 0.6002249
  # mean (squared) prediction error
  mean((test[,9]-test.fitls)^2)                 # 0.5738685
  
  # mean prediction error based on reduced model
  test.fitlsr=predict(fitlsr, newdata=testst)  
  # mean (absolute) prediction error
  mean(abs(test[,9]-test.fitlsr))                # 0.5513764
  # mean (squared) prediction error
  mean((test[,9]-test.fitlsr)^2)                 # 0.4703254
  
  return(c(mean(abs(testst[,9]-mean(trainst[,9]))), 
           mean((testst[,9]-mean(trainst[,9]))^2),
           mean(abs(test[,9]-test.fitls)), 
           mean((test[,9]-test.fitls)^2),
           mean(abs(test[,9]-test.fitlsr)),
           mean((test[,9]-test.fitlsr)^2)))
}

# mean (absolute) prediction errors for three approaches after 100 repeats
set.seed(1)
result <- t(sapply(1:100,crossvalidation))
colnames(result) <- c("mape","mspe","mapef","mspef","maper","msper")
head(result, n=3)
tail(result, n=3)

# paired t-test for comparing mape and mapef
t.test(result[,"mape"], result[,"mapef"], paired=TRUE, alt="greater") #result indicates that mapef is smaller than mape

# paired t-test for comparing mapef and maper
t.test(result[,"mapef"], result[,"maper"], paired=TRUE, alt="greater") #result indicates that mapef and maper are almost the same

# paired t-test for comparing mape and maper
t.test(result[,"mape"], result[,"maper"], paired=TRUE, alt="greater") #result indicates that maper is smaller than mape

# paired t-test for comparing mspe and mspef
t.test(result[,"mspe"], result[,"mspef"], paired=TRUE, alt="greater") #result indicates that mspef is smaller than mspe

# paired t-test for comparing mspef and msper
t.test(result[,"mspef"], result[,"msper"], paired=TRUE, alt="greater") #result indicates that msper is smaller than mspef

# paired t-test for comparing mspe and msper
t.test(result[,"mspe"], result[,"msper"], paired=TRUE, alt="greater") #result indicates that msper is smaller than map