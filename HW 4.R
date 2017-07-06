## R commands for STAT 494 HW4
## Special Topics in Statistics, Probability and Operations Research:
## Statistical Techniques for Machine Learning and Data Mining
## Fall Semester 2016
## Version: 2016.09.25
## Reference: http://homepages.math.uic.edu/~jyang06/stat494/stat494.html

library(class)
set.seed(199)
## data
y <- c(rep(1,25),rep(2,25))
x <- rnorm(50*5000)
## wrong procedure
# Reference: Prof. Jie Yang's code in class
wrong <- function(i){
  # select top 100 predictors
  dim(x) <- c(50,5000)
  x.order <- order(-abs(cor(x,y)))[1:100]
  x100 <- x[,x.order]
  # indicators
  temp <- sample(1:50, size = 50, replace = F)
  fold.ind <- rep(0,50)
  for(i in 1:5) fold.ind[temp[(i-1)*10+(1:10)]] = i;
  y.label <- rep(0,50)
  # use 5-fold cross-validation to estimate the error rate of
  # 1-nearest neighbor classifier with the selected 100 predictors
  for(i in 1:5){
    ytemp <- knn(train = x100[fold.ind != i,],
                 test = x100[fold.ind == i,],
                 cl = y[fold.ind != i], k = 1);
    y.label[fold.ind == i] = ytemp;
  }
  return(sum(y != y.label)/50) # wrong procedure predict error rate per run
}
mean(t(sapply(1:50, wrong))) # average wrong procedure predict error rate: 1.72%

## right procedure
right <- function(i){
  dim(x) <- c(50,5000)
  dim(y) <- c(50,1)
  ind = sample(rep(1:5, each = 10))
  # top 100 predictors in fold 1
  foldx1 <- x[ind==1,]
  foldy1 <- y[ind==1,]
  fold1.order <- order(-abs(cor(foldx1,foldy1)))[1:100]
  x1_100 <- foldx1[,fold1.order]
  # top 100 predictors in fold 2
  foldx2 <- x[ind==2,]
  foldy2 <- y[ind==2,]
  fold2.order <- order(-abs(cor(foldx2,foldy2)))[1:100]
  x2_100 <- foldx2[,fold2.order]
  # top 100 predictors in fold 3
  foldx3 <- x[ind==3,]
  foldy3 <- y[ind==3,]
  fold3.order <- order(-abs(cor(foldx3,foldy3)))[1:100]
  x3_100 <- foldx3[,fold3.order]
  # top 100 predictors in fold 4
  foldx4 <- x[ind==4,]
  foldy4 <- y[ind==4,]
  fold4.order <- order(-abs(cor(foldx4,foldy4)))[1:100]
  x4_100 <- foldx4[,fold4.order]
  # top 100 predictors in fold 5
  foldx5 <- x[ind==5,]
  foldy5 <- y[ind==5,]
  fold5.order <- order(-abs(cor(foldx5,foldy5)))[1:100]
  x5_100 <- foldx5[,fold5.order]
  # predicted y in fold 1
  ytemp1 <- knn(train = rbind(x2_100,x3_100,x4_100,x5_100),
                test = x1_100,
                cl = y[fold.ind != 1], k = 1)
  # predicted y in fold 2
  ytemp2 <- knn(train = rbind(x1_100,x3_100,x4_100,x5_100),
                test = x2_100,
                cl = y[fold.ind != 2], k = 1)
  # predicted y in fold 3
  ytemp3 <- knn(train = rbind(x1_100,x2_100,x4_100,x5_100),
                test = x3_100,
                cl = y[fold.ind != 3], k = 1)
  # predicted y in fold 4
  ytemp4 <- knn(train = rbind(x1_100,x2_100,x3_100,x5_100),
                test = x4_100,
                cl = y[fold.ind != 4], k = 1)
  # predicted y in fold 5
  ytemp5 <- knn(train = rbind(x1_100,x2_100,x3_100,x4_100),
                test = x5_100,
                cl = y[fold.ind != 5], k = 1)
  # right procedure predict error rate per run
  return((sum(y[fold.ind == 1] != ytemp1)+sum(y[fold.ind == 2] != ytemp2)
          +sum(y[fold.ind == 3] != ytemp3)+sum(y[fold.ind == 4] != ytemp4)
          +sum(y[fold.ind == 5] != ytemp5))/50)
}
mean(t(sapply(1:50, right))) # average right procedure predict error rate: 49.52%