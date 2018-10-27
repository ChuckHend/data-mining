
############# Cross-Validation for Assessment #############

# viewing the data
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
#note the reasonably high R-squared value, 0.7365 after adjustment for multiple predictors

k = 10 #using 10-fold cross-validation
groups = c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))  #produces list of group labels

set.seed(2)
cvgroups = sample(groups,n)  #orders randomly, with seed (2) to determine starting point

#prediction via cross-validation
allpredictedCV = rep(0,n)
for (i in 1:k)  {
  groupi = (cvgroups == i)
  lmfitCV = lm(formula = Fullmodel,data=bodyfat,subset=!groupi)
  allpredictedCV[groupi] = predict.lm(lmfitCV,bodyfat[groupi,])
}

plot(Fullfit$fitted.values,BodyFatSiri)
points(allpredictedCV,BodyFatSiri,pch=20,col="red")

#the cross-validation assessment part
CV10 = sum((allpredictedCV-BodyFatSiri)^2)/n

#comparison
MSE = sum((Fullfit$fitted.values-BodyFatSiri)^2)/n; MSE
sum((Fullfit$fitted.values-BodyFatSiri)^2)/(n-15)  #lower, even when adjusted for number of predicted coefficients
CV10 = sum((allpredictedCV-BodyFatSiri)^2)/n; CV10  #"true" assessment
1 - CV10*n/sum((BodyFatSiri-mean(BodyFatSiri))^2)  #cross-validated analogy of R-squared











