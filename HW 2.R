## R commands for STAT 494 HW2
## Special Topics in Statistics, Probability and Operations Research:
## Statistical Techniques for Machine Learning and Data Mining
## Fall Semester 2016
## Version: 2016.09.06
## Reference: http://homepages.math.uic.edu/~jyang06/stat494/stat494.html

## R package: ElemStatLearn, version 2015.6.26
## Data Sets, Functions and Examples from the Book (ESL): 
## "The Elements of Statistical Learning, Data Mining, Inference, and Prediction" 
## by Trevor Hastie, Robert Tibshirani and Jerome Friedman, Second Edition, 2009

## At Cran: https://cran.r-project.org/web/packages/ElemStatLearn/index.html
## Reference: Section 3.4 in the ESL book

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
  
  # standardization of predictors
  trainst <- train
  for(i in 1:8) {
    trainst[,i] <- trainst[,i] - mean(prostate[,i]);
    trainst[,i] <- trainst[,i]/sd(prostate[,i]);
  }
  testst <- test
  for(i in 1:8) {
    testst[,i] <- testst[,i] - mean(prostate[,i]);
    testst[,i] <- testst[,i]/sd(prostate[,i]);
  }
  
  # fit linear model on training dataset using LS method
  fitls <- lm( lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=trainst )
  
  # fit the reduced model
  fitlsr <- lm( lpsa ~ lcavol+lweight+lbph+svi, data=trainst )
  
  # ridge regression
  library(glmnet)
  # use 10-fold cross-validation to choose best lambda, reproducing the Ridge Regression part of Figure 3.7 on page 62
  #set.seed(331)
  cv.out=cv.glmnet(x=as.matrix(trainst[,1:8]), y=as.numeric(trainst[,9]), nfolds=10, alpha=0, standardize=F)
  # the best lambda chosen by 10-fold cross-validation
  lambda.10fold=cv.out$lambda.1s   
  # apply Ridge regression with chosen lambda
  fitridge=glmnet(x=as.matrix(trainst[,1:8]),y=as.numeric(trainst[,9]),alpha=0,lambda=lambda.10fold,standardize=F,thresh=1e-12)
  
  #Lasso
  library(glmnet)
  # use 10-fold cross-validation to choose best lambda, reproducing the Lasso part of Figure 3.7 on page 62
  #set.seed(321)
  cv.out=cv.glmnet(x=as.matrix(train[,1:8]), y=as.numeric(train[,9]), nfolds=10, alpha=1)
  # the best lambda chosen by 10-fold cross-validation
  lambda.10fold=cv.out$lambda.1s   
  # apply Lasso with chosen lambda
  fitlasso=glmnet(x=as.matrix(trainst[,1:8]),y=as.numeric(trainst[,9]),alpha=1,lambda=lambda.10fold,standardize=F,thresh=1e-12)
  
  #LARS
  library(lars)
  prostate.lar <- lars(x=as.matrix(trainst[,1:8]), y=as.numeric(trainst[,9]), type="lar", trace=TRUE, normalize=F)
  # choose k using 10-fold cross-validation, "cv.lars" also generates a graph similar to Figure 3.7 on page 62
  #set.seed(32)   # initial random seed for 10-fold CV
  cv.out <- cv.lars(x=as.matrix(trainst[,1:8]), y=as.numeric(trainst[,9]), K=10, plot.it=T, type="lar", trace=TRUE, normalize=F)
  itemp=which.min(cv.out$cv) 
  k.lars = min(cv.out$index[cv.out$cv < cv.out$cv[itemp]+cv.out$cv.error[itemp]])  # the chosen k = 6
  
  ## check testing errors
  # mean prediction error on testing data using mean training value
  mean(trainst[,9])                             
  # mean (absolute) prediction error
  mean(abs(testst[,9]-mean(trainst[,9])))       
  # mean (squared) prediction error
  mean((testst[,9]-mean(trainst[,9]))^2)        
  
  # mean prediction error based on full model
  test.fitls=predict(fitls, newdata=testst)  
  # mean (absolute) prediction error
  mean(abs(test[,9]-test.fitls))                
  # mean (squared) prediction error
  mean((test[,9]-test.fitls)^2)                 
  
  # mean prediction error based on reduced model
  test.fitlsr=predict(fitlsr, newdata=testst)  
  # mean (absolute) prediction error
  mean(abs(test[,9]-test.fitlsr))                
  # mean (squared) prediction error
  mean((test[,9]-test.fitlsr)^2)                
  
  # mean prediction error based on ridge regression
  test.ridge=predict(fitridge,newx=as.matrix(testst[,1:8]))
  # mean (absolute) prediction error
  mean(abs(test[,9]-test.ridge))                
  # mean (squared) prediction error
  mean((test[,9]-test.ridge)^2)                 
  
  # mean prediction error based on lasso
  test.lasso=predict(fitlasso,newx=as.matrix(testst[,1:8]))
  # mean (absolute) prediction error
  mean(abs(test[,9]-test.lasso))                
  # mean (squared) prediction error
  mean((test[,9]-test.lasso)^2)                 
  
  # mean prediction error based on larS
  test.lars=predict(prostate.lar, newx=as.matrix(testst[,1:8]), s=k.lars, type="fit", mode=cv.out$mode)$fit
  # mean (absolute) prediction error
  mean(abs(test[,9]-test.lars))                
  # mean (squared) prediction error
  mean((test[,9]-test.lars)^2)                
  
  return(c(mean(abs(testst[,9]-mean(trainst[,9]))), 
           mean((testst[,9]-mean(trainst[,9]))^2),
           mean(abs(test[,9]-test.fitls)), 
           mean((test[,9]-test.fitls)^2),
           mean(abs(test[,9]-test.fitlsr)),
           mean((test[,9]-test.fitlsr)^2),
           mean(abs(test[,9]-test.ridge)),
           mean((test[,9]-test.ridge)^2),
           mean(abs(test[,9]-test.lasso)),
           mean((test[,9]-test.lasso)^2),
           mean(abs(test[,9]-test.lars)),
           mean((test[,9]-test.lars)^2)))
}

# mean prediction errors for six approaches after 100 repeats
set.seed(1)
result <- t(sapply(1:100,crossvalidation))
colnames(result) <- c("mape","mspe","mapef","mspef","maper","msper","maperidge","msperidge","mapelasso","mspelasso","mapelars","mspelars")
head(result, n=3)
tail(result, n=3)

# plot the output
boxplot(result[,"mape"],result[,"mapef"],result[,"maper"],result[,"maperidge"],result[,"mapelasso"],result[,"mapelars"], names = c("mape","mapef","maper","maperidge","mapelasso","mapelars"))
boxplot(result[,"mspe"],result[,"mspef"],result[,"msper"],result[,"msperidge"],result[,"mspelasso"],result[,"mspelars"], names = c("mspe","mspef","msper","msperidge","mspelasso","mspelars"))

# paired t-test for comparing mape and mapef
t.test(result[,"mape"], result[,"mapef"], paired=TRUE, alt="greater") #result indicates that mapef is smaller than mape

# paired t-test for comparing mape and maper
t.test(result[,"mape"], result[,"maper"], paired=TRUE, alt="greater") #result indicates that maper is smaller than mape

# paired t-test for comparing mape and maperidge
t.test(result[,"mape"], result[,"maperidge"], paired=TRUE, alt="greater") #result indicates that maperidge is smaller than mape

# paired t-test for comparing mape and mapelasso
t.test(result[,"mape"], result[,"mapelasso"], paired=TRUE, alt="greater") #result indicates that mapelasso is smaller than mape

# paired t-test for comparing mape and mapelars
t.test(result[,"mape"], result[,"mapelars"], paired=TRUE, alt="greater") #result indicates that maperlars is smaller than mape

# paired t-test for comparing mapef and maper
t.test(result[,"mapef"], result[,"maper"], paired=TRUE, alt="two.sided") #result indicates that mapef and maper are almost the same

# paired t-test for comparing mapef and maperidge
t.test(result[,"mapef"], result[,"maperidge"], paired=TRUE, alt="less") #result indicates that mapef is smaller than maperidge

# paired t-test for comparing mapef and mapelasso
t.test(result[,"mapef"], result[,"mapelasso"], paired=TRUE, alt="less") #result indicates that mapef is smaller than mapelasso

# paired t-test for comparing mapef and mapelars
t.test(result[,"mapef"], result[,"mapelars"], paired=TRUE, alt="less") #result indicates that mapef is smaller than mapelars

# paired t-test for comparing maper and maperidge
t.test(result[,"maper"], result[,"maperidge"], paired=TRUE, alt="less") #result indicates that maper is smaller than maperidge

# paired t-test for comparing maper and mapelasso
t.test(result[,"maper"], result[,"mapelasso"], paired=TRUE, alt="less") #result indicates that maper is smaller than mapelasso

# paired t-test for comparing maper and mapelars
t.test(result[,"maper"], result[,"mapelars"], paired=TRUE, alt="less") #result indicates that maper is smaller than mapelars

# paired t-test for comparing maperidge and mapelasso
t.test(result[,"maperidge"], result[,"mapelasso"], paired=TRUE, alt="two.sided") #result indicates that maperidge and mapelasso are almost the same

# paired t-test for comparing maperidge and mapelars
t.test(result[,"maperidge"], result[,"mapelars"], paired=TRUE, alt="greater") #result indicates that mapelars is smaller than maperidge

# paired t-test for comparing mapelasso and mapelars
t.test(result[,"mapelasso"], result[,"mapelars"], paired=TRUE, alt="greater") #result indicates that mapelars is smaller than mapelasso




# paired t-test for comparing mspe and mspef
t.test(result[,"mspe"], result[,"mspef"], paired=TRUE, alt="greater") #result indicates that mspef is smaller than mspe

# paired t-test for comparing mspe and msper
t.test(result[,"mspe"], result[,"msper"], paired=TRUE, alt="greater") #result indicates that msper is smaller than mspe

# paired t-test for comparing mspe and msperidge
t.test(result[,"mspe"], result[,"msperidge"], paired=TRUE, alt="greater") #result indicates that msperidge is smaller than mspe

# paired t-test for comparing mspe and mspelasso
t.test(result[,"mspe"], result[,"mspelasso"], paired=TRUE, alt="greater") #result indicates that mspelasso is smaller than mspe

# paired t-test for comparing mspe and mspelars
t.test(result[,"mspe"], result[,"mspelars"], paired=TRUE, alt="greater") #result indicates that msperlars is smaller than mspe

# paired t-test for comparing mspef and msper
t.test(result[,"mspef"], result[,"msper"], paired=TRUE, alt="two.sided") #result indicates that mspef and msper are almost the same

# paired t-test for comparing mspef and msperidge
t.test(result[,"mspef"], result[,"msperidge"], paired=TRUE, alt="less") #result indicates that mspef is smaller than msperidge

# paired t-test for comparing mspef and mspelasso
t.test(result[,"mspef"], result[,"mspelasso"], paired=TRUE, alt="less") #result indicates that mspef is smaller than mspelasso

# paired t-test for comparing mspef and mspelars
t.test(result[,"mspef"], result[,"mspelars"], paired=TRUE, alt="less") #result indicates that mspef is smaller than mspelars

# paired t-test for comparing msper and msperidge
t.test(result[,"msper"], result[,"msperidge"], paired=TRUE, alt="less") #result indicates that msper is smaller than msperidge

# paired t-test for comparing msper and mspelasso
t.test(result[,"msper"], result[,"mspelasso"], paired=TRUE, alt="less") #result indicates that msper is smaller than mspelasso

# paired t-test for comparing msper and mspelars
t.test(result[,"msper"], result[,"mspelars"], paired=TRUE, alt="less") #result indicates that msper is smaller than mspelars

# paired t-test for comparing msperidge and mspelasso
t.test(result[,"msperidge"], result[,"mspelasso"], paired=TRUE, alt="two.sided") #result indicates that msperidge and mspelasso are almost the same

# paired t-test for comparing msperidge and mspelars
t.test(result[,"msperidge"], result[,"mspelars"], paired=TRUE, alt="greater") #result indicates that mspelars is smaller than msperidge

# paired t-test for comparing mspelasso and mspelars
t.test(result[,"mspelasso"], result[,"mspelars"], paired=TRUE, alt="greater") #result indicates that mspelars is smaller than mspelasso