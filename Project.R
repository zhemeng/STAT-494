#### Generating data ###
set.seed(19)
x <- seq(-3,3,length.out = 60)
beta_true <- c(2,1.6,-0.12,0.15)
X<- as.matrix(cbind(1,x,x^2,x^3))
M <- t(X) %*% X
y <- as.vector(beta_true%*%t(as.matrix(cbind(1,x,x^2,x^3))))
noise <-  rnorm(60,0,1)
noisy.y <- y + noise
plot(x,noisy.y,col='deepskyblue4',xlab='x',main='95% Confidence Band - Approach I')
lines(x,y,col='firebrick1',lwd=3)

#### Method 1 ####
#### Fitting polynomial regression model on the data ####
model <- lm(noisy.y ~ poly(x,3,raw = 1))
#### Generate 95% confidence intervals for predictions using the fitted model
y_predict <- predict(model,data.frame(x),interval = "confidence",level = 0.95)
#### Draw the 95% confidence band for the linear model curve
lines(x,y_predict[,1],col='gold',lwd=3)
lines(x,y_predict[,2],col='black',lwd=1)
lines(x,y_predict[,3],col='black',lwd=1)
dev.new()


#### Method 2 ####
#### We sample beta_head values from the edge of the 95% confidence ellipsoid of beta_head
#### Then we draw several curves with the sampled beta_head
#### Calculate the radius of the ellipsoid
rad <- sqrt(qchisq(0.95,df=4))
nCurves <- 60
#### Sampling for beta_head
beta_head <- matrix(runif(nCurves*4,min=-3,max=3),ncol=4)
fac <- sqrt(apply(beta_head,1,function(x){t(x) %*% M %*% x}))
betaBase <- matrix(rep(beta_true,nCurves),ncol=4,byrow=T)
beta_head <- betaBase + beta_head*rad/fac
#### Draw graph
plot(x,noisy.y,col='deepskyblue4',xlab='x',main='95% Condifence Band - Approach II')
apply(beta_head,1,function(u){lines(x,X %*% u,col='green')})
plot(x,noisy.y,col='deepskyblue4',xlab='x',main='95% Condifence Band - Compare')