
#############Part 3#############
bodyfat = read.csv("bodyfat.csv")

dim(bodyfat)
n = dim(bodyfat)[1]

attach(bodyfat)
names(bodyfat)  # using BodyFatSiri as response, versus Density or BodyFatBrozek
pairs(bodyfat[,-c(1,2,4)])

#fit response on all predictors
Fullmodel = (BodyFatSiri ~ Abs + Weight + Wrist + Forearm + Neck + Biceps + Age
			+ Thigh + Hip + Ankle + BMI + Height + Chest + Knee)
Fullfit = lm(Fullmodel,data=bodyfat)
summary(Fullfit)  

Partialmodel = (BodyFatSiri ~ Abs + Weight + Wrist + Forearm)
Partialfit = lm(Partialmodel,data=bodyfat)
summary(Partialfit)  











#Estimating standard deviations with bootstrap
library(boot)  #need to install package
	#define functions that output coefficients (parameters to be estimated)
beta.fn.Full = function(inputdata,index) {
  lmfitboot = lm(formula = Fullmodel,data=inputdata[index,])
  return(lmfitboot$coef)
}
beta.fn.Partial = function(inputdata,index) {
  lmfitboot = lm(formula = Partialmodel,data=inputdata[index,])
  return(lmfitboot$coef)
}
	#run the boot function to simulate re-samples (with replacement)
	#and obtain the coefficients for each re-sample
	#partial model bootstrap
set.seed(2)
Partialbootoutput = boot(bodyfat,beta.fn.Partial,R=1000)
print(Partialbootoutput)

sd((Partialbootoutput$t)[,1])  #standard error as estimated via simulation
hist((Partialbootoutput$t)[,1])
	#(Partialbootoutput$t)[,1] is all 1000 coefficient estimates for intercept (1st term)
summary(Partialfit)$coef[1,2]  #standard error as estimated mathematically

sd((Partialbootoutput$t)[,2])  #standard error as estimated via simulation
hist((Partialbootoutput$t)[,2])
	#(Partialbootoutput$t)[,2] is all 1000 coefficient estimates for "Abs" (2nd term)
summary(Partialfit)$coef[2,2]   #standard error as estimated mathematically

sd((Partialbootoutput$t)[,3])  #standard error as estimated via simulation
hist((Partialbootoutput$t)[,3])
	#(Partialbootoutput$t)[,3] is all 1000 coefficient estimates for "Weight" (3rd term)
summary(Partialfit)$coef[3,2]   #standard error as estimated mathematically





#full model bootstrap
set.seed(2)
Fullbootoutput = boot(bodyfat,beta.fn.Full,R=1000)
print(Fullbootoutput)

sd((Fullbootoutput$t)[,1])  #standard error as estimated via simulation
hist((Fullbootoutput$t)[,1])
	#(Fullbootoutput$t)[,1] is all 1000 coefficient estimates for intercept (1st term)
summary(Fullfit)$coef[1,2]  #standard error as estimated mathematically

sd((Fullbootoutput$t)[,2])  #standard error as estimated via simulation
hist((Fullbootoutput$t)[,2])
	#(Fullbootoutput$t)[,2] is all 1000 coefficient estimates for "Abs" (2nd term)
summary(Fullfit)$coef[2,2]   #standard error as estimated mathematically

sd((Fullbootoutput$t)[,3])  #standard error as estimated via simulation
hist((Fullbootoutput$t)[,3])
	#(Fullbootoutput$t)[,3] is all 1000 coefficient estimates for "Weight" (3rd term)
summary(Fullfit)$coef[3,2]   #standard error as estimated mathematically

