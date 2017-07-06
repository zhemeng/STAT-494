## R commands for STAT 494 HW10
## Special Topics in Statistics, Probability and Operations Research:
## Statistical Techniques for Machine Learning and Data Mining
## Fall Semester 2016
## Version: 2016.11.30
## Reference: http://homepages.math.uic.edu/~jyang06/stat494/stat494.html


## R package: ElemStatLearn, version 2015.6.26
## Data Sets, Functions and Examples from the Book (ESL): 
## "The Elements of Statistical Learning, Data Mining, Inference, and Prediction" 
## by Trevor Hastie, Robert Tibshirani and Jerome Friedman, Second Edition, 2009

## At Cran: https://cran.r-project.org/web/packages/ElemStatLearn/index.html


## Example: Trauma clinical trial
## Reference: Chuang-Stein, C. and Agresti, A. (1997). 
## Tutorial in Biostatistics-A Review of tests for detecting a monotone dose-response relationship with ordinal response data, 
## Statistics in Medicine, 16, 2599-2618.

## Five ordered response categories: death, vegetative state, major disability, minor disability, and good recovery
## Often called the Glasgow Outcome Scale (GOS) in the literature on critical care
## Treatment groups:  placebo, low dose, medium dose, and high dose
set.seed(105)
trauma <- read.table("trauma.txt", header=TRUE)
# generate the raw dataframe
dataframe <- matrix(0,802,2)
colnames(dataframe) <- c("response category", "Dose")
a <- c(rep(1,59),rep(2,25),rep(3,46),rep(4,48),rep(5,32),
       rep(1,48),rep(2,21),rep(3,44),rep(4,47),rep(5,30),
       rep(1,44),rep(2,14),rep(3,54),rep(4,64),rep(5,31),
       rep(1,43),rep(2,4),rep(3,49),rep(4,58),rep(5,41))
dataframe[,1]<- a
dataframe[,2]<- c(rep(1,59+25+46+48+32),rep(2,48+21+44+47+30),rep(3,44+14+54+64+31),rep(4,43+4+49+58+41))
# generate the partition table
train.index <- matrix(0,5,802)
for(i in 1:10) train.index[i,]=sample(x=c(rep(1,floor(802*0.8)),rep(0,ceiling(802*0.2))), size=802, replace=F)

train <- matrix(0,4,6)
train[,1] <- c(1,2,3,4)
colnames(train) <- c("dose", "y1","y2","y3","y4","y5")
test <- matrix(0,4,6)
test[,1] <- c(1,2,3,4)
colnames(test) <- c("dose", "y1","y2","y3","y4","y5")
output <- list()

library(VGAM) 
for (isimu in 1:5) {
  
  train0 <- subset(dataframe,train.index[isimu,]==1)[,1:2]
  test0 <- subset(dataframe, train.index[isimu,]==0)[,1:2]     
  
  for (i in 1:4) { 
    for (j in 1:5) { 
      train[i,j+1] <- nrow(subset(train0, train0[,1]==j & train0[,2]==i))
      test[i,j+1]  <- nrow(subset(test0, test0[,1]==j & test0[,2]==i))
    }
  }
  train <- data.frame(train)
  test <- data.frame(test)
  
  fit.cumpo1 <- vglm(cbind(y1,y2,y3,y4,y5) ~ dose,family=cumulative(parallel=TRUE), data=train)
  fit.cumnpo1 <- vglm(cbind(y1,y2,y3,y4,y5) ~ dose,family=cumulative, data=train)
  fit.crpo1 <- vglm(cbind(y1,y2,y3,y4,y5) ~ dose, family=sratio(parallel=T), train)
  fit.crnpo1 <- vglm(cbind(y1,y2,y3,y4,y5) ~ dose, family=sratio, train)
  fit.acpo1 <- vglm(cbind(y1,y2,y3,y4,y5) ~ dose, family=acat(parallel=T), train)
  fit.acnpo1 <- vglm(cbind(y1,y2,y3,y4,y5) ~ dose, family=acat(reverse=T), train)
  fit.nompo1 <- vglm(cbind(y1,y2,y3,y4,y5) ~ dose, family=multinomial(parallel=TRUE), train)
  fit.nomnpo1 <- vglm(cbind(y1,y2,y3,y4,y5) ~ dose, family=multinomial, train)
  # model selection
  output[[isimu]]=rbind(
    c(AIC(fit.cumpo1),AIC(fit.cumnpo1),AIC(fit.crpo1),AIC(fit.crnpo1),AIC(fit.acpo1),AIC(fit.acnpo1),AIC(fit.nompo1),AIC(fit.nomnpo1)),
    c(BIC(fit.cumpo1),BIC(fit.cumnpo1),BIC(fit.crpo1),BIC(fit.crnpo1),BIC(fit.acpo1),BIC(fit.acnpo1),BIC(fit.nompo1),BIC(fit.nomnpo1))
  )
}

output <- apply(simplify2array(output), 1:2, mean)
rownames(output)=c("AIC","BIC")
colnames(output)=c("Cumulative po", "Cumulative npo", "Continuation-ratio po", "Continuation-ratio npo", "Adjacent-categories po", "Adjacent-categories npo", "Baseline-category po", "Baseline-category npo")
output
# Best: Cumulative npo
#     Cumulative po Cumulative npo Continuation-ratio po Continuation-ratio npo Adjacent-categories po
# AIC     101.15038       95.91689             101.95776               97.08651              101.20885
# BIC      98.08185       91.00725              98.88923               92.17686               98.14032
#     Adjacent-categories npo Baseline-category po Baseline-category npo
# AIC                97.23168             183.3806              97.23168
# BIC                92.32203             182.1532              92.32203
