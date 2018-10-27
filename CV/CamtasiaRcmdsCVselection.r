#############Part 2#############
bodyfat = read.csv("bodyfat.csv")

dim(bodyfat)
n = dim(bodyfat)[1]

attach(bodyfat)
names(bodyfat)  # using BodyFatSiri as response, versus Density or BodyFatBrozek
pairs(bodyfat[,-c(1,2,4)])

#fit response on all predictors
Fullmodel = (BodyFatSiri ~ Age + Weight + Height + BMI + Neck + Chest + Abs + Hip
			+ Thigh + Knee + Ankle + Biceps + Forearm + Wrist)
Fullfit = lm(Fullmodel,data=bodyfat)
summary(Fullfit)  















#forwardregression
nullfit = lm(BodyFatSiri ~ 1,data=bodyfat)  #fit response on intercept only
summary(nullfit)
anova(nullfit)

step(nullfit, scope=list(lower=nullfit, upper=Fullfit), direction="forward",k=.1) 
	#forcing the selection to go through all the variables
	#order: Abs, Weight, Wrist, Forearm, Neck, Biceps, Age, Thigh, Hip, Ankle, BMI, Height, Chest, Knee)
	#introducing predictor (x) variables in order as selected by forward regression
Model1 = (BodyFatSiri ~ Abs)
Model2 = (BodyFatSiri ~ Abs+Weight)
Model3 = (BodyFatSiri ~ Abs+Weight+Wrist)
Model4 = (BodyFatSiri ~ Abs+Weight+Wrist+Forearm)
Model5 = (BodyFatSiri ~ Abs+Weight+Wrist+Forearm+Neck)
Model6 = (BodyFatSiri ~ Abs+Weight+Wrist+Forearm+Neck+Biceps)
Model7 = (BodyFatSiri ~ Abs+Weight+Wrist+Forearm+Neck+Biceps+Age)
Model8 = (BodyFatSiri ~ Abs+Weight+Wrist+Forearm+Neck+Biceps+Age+Thigh)
Model9 = (BodyFatSiri ~ Abs+Weight+Wrist+Forearm+Neck+Biceps+Age+Thigh+Hip)
Model10 = (BodyFatSiri ~ Abs+Weight+Wrist+Forearm+Neck+Biceps+Age+Thigh+Hip+Ankle)
Model11 = (BodyFatSiri ~ Abs+Weight+Wrist+Forearm+Neck+Biceps+Age+Thigh+Hip+Ankle+BMI)
Model12 = (BodyFatSiri ~ Abs+Weight+Wrist+Forearm+Neck+Biceps+Age+Thigh+Hip+Ankle+BMI+Height)
Model13 = (BodyFatSiri ~ Abs+Weight+Wrist+Forearm+Neck+Biceps+Age+Thigh+Hip+Ankle+BMI+Height+Chest)
Model14 = (BodyFatSiri ~ Abs+Weight+Wrist+Forearm+Neck+Biceps+Age+Thigh+Hip+Ankle+BMI+Height+Chest+Knee)
allModels = list(Model1,Model2,Model3,Model4,Model5,Model6,Model7,
				 Model8,Model9,Model10,Model11,Model12,Model13,Model14)	

				 
		
		
#applying 10-fold cross-validation for model selection
k = 10 
groups = c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))  #produces list of group labels
set.seed(2)
cvgroups = sample(groups,n)  #orders randomly, with seed (2) to determine starting point

allmodelMSE = rep(NA,14)  #place-holder for results
allmodelMSEadj = rep(NA,14)  #place-holder for results
allmodelCV = rep(NA,14) #place-holder for results
	#fits for each model m, including predictors 1:m in order of forward step regression selection
for (m in 1:14) {
  mPredfit = lm(formula = allModels[[m]],data=bodyfat)
  allmodelMSE[m] = sum((mPredfit$fitted.values-BodyFatSiri)^2)/n
  allmodelMSEadj[m] = sum((mPredfit$fitted.values-BodyFatSiri)^2)/(n-1-m)

  #prediction via cross-validation
  allpredictedCV = rep(0,n)
  for (i in 1:k)  {
    groupi = (cvgroups == i)
	lmfitCV = lm(formula = allModels[[m]],data=bodyfat,subset=!groupi)
    allpredictedCV[groupi] = predict.lm(lmfitCV,bodyfat[groupi,])
  }
  allmodelCV[m] = sum((allpredictedCV-BodyFatSiri)^2)/n
}







#comparison
plot(1:14,allmodelMSE,ylim = c(17,25),xlab="number of predictors",ylab="")
	# MSE for each model fit plotted against number of predictors ("complexity" of model)
points(1:14,allmodelMSEadj,col="blue",pch=8)
	# MSE, adjusted for the number of predictors, plotted against number of predictors
points(1:14,allmodelCV,col="red",pch=20)
	# CV(10), adjusted for the number of predictors, plotted against number of predictors























