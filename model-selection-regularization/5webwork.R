# Linear Model Selection and Regularization
# Webwork

heart <- read.csv('Projects/data-mining/data/Heart_disease_Cleveland.csv')
# remove rows with missing values
heart <- heart[-c(88, 167, 193, 267, 288, 303),]

heart$log.STdepress <- log(heart$STdepress + 1)
heart$STdepress <- NULL
# convert select vars to factor
catVars <- c('Sex', 'ChestPain', 'HighBloodSugar','ECG', 'ExerAngina', 'Slope', 
             'Thal', 'DiseaseStatus')
heart[,catVars] <- lapply(heart[,catVars], as.factor)

# compute corr matrix
options(scipen=999)
allcors = round(cor(heart[,c(1,4,5,8,11)]),3)
abs(allcors)

require(MASS)
BostonNew <- Boston

BostonNew$log.crim <- log(Boston$crim)
BostonNew$log.zn <- log(Boston$zn + 1)
BostonNew$chas <- as.factor(Boston$chas)
BostonNew$crim <- NULL
BostonNew$zn <- NULL

library(glmnet)  # may need to download package glmnet

# define model matrix
x <- model.matrix(log.crim~.,data=BostonNew)[,-1]
y <- BostonNew$log.crim
n = dim(x)[1]
p = dim(x)[2]

lambdalist = 1:1000/1000  # order large to small

#fit ridge regression - need alpha = 0
RRfit = glmnet(x, y, lambda=lambdalist, alpha = 0)
coef(RRfit,s=0.05)
plot(RRfit,xvar="lambda",xlim=c(-12,12)); abline(v=log(0.1))

#fit LASSO - need alpha =1
LASSOfit = glmnet(x, y, alpha = 1,lambda=lambdalist)
coef(LASSOfit,s=0.05)
plot(LASSOfit,xvar="lambda"); abline(v=log(0.5))

#fit ENET
ENETfit = glmnet(x, y, alpha = 0.5,lambda=lambdalist)
coef(ENETfit,s=0.05)
plot(ENETfit,xvar="lambda"); abline(v=log(0.4))


# penalized regression with cross-validation ####
#cross-validation
#cross-validation
ncv = 10
groups=c(rep(1:ncv,floor(n/ncv)),(1:(n-ncv*floor(n/ncv))))
set.seed(5)
cvgroups = sample(groups,n)

#lambdalist = exp((1200:-1200)/100)  # order large to small

#RR cross-validation
cvRRglm = cv.glmnet(x, y, lambda=lambdalist, alpha = 0, nfolds=ncv, foldid=cvgroups)
plot(cvRRglm$lambda,cvRRglm$cvm,type="l",lwd=2,col="red",xlab="lambda",ylab="CV(10)",
     xlim=c(0,10),ylim = c(20,30))
whichlowestcvRR = order(cvRRglm$cvm)[1]; min(cvRRglm$cvm)
bestlambdaRR = lambdalist[whichlowestcvRR]; bestlambdaRR
abline(v=bestlambdaRR)

#LASSO cross-validation
cvLASSOglm = cv.glmnet(x, y, lambda=lambdalist, alpha = 1, nfolds=ncv, foldid=cvgroups)
plot(cvLASSOglm$lambda,cvLASSOglm$cvm,type="l",lwd=2,col="red",xlab="lambda",ylab="CV(10)",
     xlim=c(0,3),ylim = c(20,30))
whichlowestcvLASSO = order(cvLASSOglm$cvm)[1]; min(cvLASSOglm$cvm)
bestlambdaLASSO = lambdalist[whichlowestcvLASSO]; bestlambdaLASSO
abline(v=bestlambdaLASSO)

#ENET alpha=0.95 cross-validation
cvENET95glm = cv.glmnet(x, y, lambda=lambdalist, alpha = 0.5, nfolds=ncv, foldid=cvgroups)
plot(cvENET95glm$lambda,cvENET95glm$cvm,type="l",lwd=2,col="red",xlab="lambda",ylab="CV(10)",
     xlim=c(0,3),ylim = c(20,30))
whichlowestcvENET95 = order(cvENET95glm$cvm)[1]; min(cvENET95glm$cvm)
bestlambdaENET95 = lambdalist[whichlowestcvENET95]; bestlambdaENET95; abline(v=bestlambdaENET95)


# Bootstrapping SE of coefficients ####

#Estimating standard deviations with bootstrap
library(boot) 
#define functions that output coefficients (parameters to be estimated)

beta.fn.RR = function(inputdata,index,s=0.016) {
  yboot = inputdata[index,1]
  xboot = inputdata[index,-1]
  fitboot = glmnet(xboot, yboot, alpha = 0,lambda=lambdalist)
  return(coef(fitboot,s=s)[,1])
}
beta.fn.LASSO = function(inputdata,index,s=.021) {
  yboot = inputdata[index,1]
  xboot = inputdata[index,-1]
  fitboot = glmnet(xboot, yboot, alpha = 1,lambda=lambdalist)
  return(coef(fitboot,s=s)[,1])
}
beta.fn.ENET = function(inputdata,index,s=.035) {
  yboot = inputdata[index,1]
  xboot = inputdata[index,-1]
  fitboot = glmnet(xboot, yboot, alpha = 0.5,lambda=lambdalist)
  return(coef(fitboot,s=s)[,1])
}

#run the boot function to simulate re-samples (with replacement)
#and obtain the coefficients for each re-sample
#partial model bootstrap
set.seed(5)
RR.bootoutput = boot(cbind(y,x),beta.fn.RR,R=1000)
print(RR.bootoutput)
#(Fullbootoutput$t)[,1] is all 1000 coefficient estimates for intercept (1st term)

set.seed(5)
lasso.bootoutput = boot(cbind(y,x),beta.fn.LASSO,R=1000)
print(lasso.bootoutput)

set.seed(5)
enet.bootoutput = boot(cbind(y,x),beta.fn.ENET,R=1000)
print(enet.bootoutput)

