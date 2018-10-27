bodyfat = read.csv("bodyfat.csv")

dim(bodyfat)
n = dim(bodyfat)[1]

names(bodyfat)  # using BodyFatSiri as response, versus Density or BodyFatBrozek
# fit models
x = model.matrix(BodyFatSiri~.,data=bodyfat)[,-(1:4)]
y = bodyfat[,3]
p = dim(x)[2]

#fit response on all predictors, using multiple regression
REGfit = lm(y~x)  
summary(REGfit)  

#cross-validation
set.seed(5)
ncv = 10
groups=c(rep(1:ncv,floor(n/ncv)),(1:(n-ncv*floor(n/ncv)))); cvgroups = sample(groups,n)

library(glmnet)
lambdalist = exp((1200:-1200)/100)  # order large to small

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
cvENET95glm = cv.glmnet(x, y, lambda=lambdalist, alpha = 0.95, nfolds=ncv, foldid=cvgroups)
plot(cvENET95glm$lambda,cvENET95glm$cvm,type="l",lwd=2,col="red",xlab="lambda",ylab="CV(10)",
     xlim=c(0,3),ylim = c(20,30))
whichlowestcvENET95 = order(cvENET95glm$cvm)[1]; min(cvENET95glm$cvm)
bestlambdaENET95 = lambdalist[whichlowestcvENET95]; bestlambdaENET95; abline(v=bestlambdaENET95)

#ENET alpha=0.5 cross-validation
cvENET50glm = cv.glmnet(x, y, lambda=lambdalist, alpha = 0.50, nfolds=ncv, foldid=cvgroups)
plot(cvENET50glm$lambda,cvENET50glm$cvm,type="l",lwd=2,col="red",xlab="lambda",ylab="CV(10)",
     xlim=c(0,3),ylim = c(20,30))
whichlowestcvENET50 = order(cvENET50glm$cvm)[1]; min(cvENET50glm$cvm)
bestlambdaENET50 = lambdalist[whichlowestcvENET50]; bestlambdaENET50; abline(v=bestlambdaENET50)

#fit selected model
Bestfit = glmnet(x, y, alpha = 0.50,lambda=lambdalist)
coef(Bestfit,s=bestlambdaENET50)
plot(Bestfit,xvar="lambda"); abline(v=log(bestlambdaENET50))
plot(Bestfit)




#Estimating standard deviations with bootstrap
library(boot)  
	#define functions that output coefficients (parameters to be estimated)

beta.fn.Full = function(inputdata,index) {
  yboot = inputdata[index,1]
  xboot = inputdata[index,-1]
  lmfitboot = lm(yboot~xboot)
  return(lmfitboot$coef)
}
	#run the boot function to simulate re-samples (with replacement)
	#and obtain the coefficients for each re-sample
	#partial model bootstrap
set.seed(5)
Fullbootoutput = boot(cbind(y,x),beta.fn.Full,R=10000)
print(Fullbootoutput)
	#(Fullbootoutput$t)[,1] is all 1000 coefficient estimates for intercept (1st term)

	
	
	
	

# do the same for the best-fitting (Elastic Net) model
beta.fn.ENET = function(inputdata,index) {
  yboot = inputdata[index,1]
  xboot = inputdata[index,-1]
  ENETfitboot = glmnet(xboot, yboot, alpha = 0.50,lambda=lambdalist)
  return(coef(ENETfitboot,s=bestlambdaENET50)[,1])
}

set.seed(5)
ENETbootoutput = boot(cbind(y,x),beta.fn.ENET,R=10000)
print(ENETbootoutput)
	#(Fullbootoutput$t)[,1] is all 1000 coefficient estimates for intercept (1st term)

# compare variability of coefs
data.frame(cbind(round(apply(Fullbootoutput$t,2,sd),4),round(apply(ENETbootoutput$t,2,sd),4)),row.names=c("intercept",names(bodyfat)[5:18]))

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  