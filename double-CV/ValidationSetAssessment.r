#open data
bodyfat = read.csv("bodyfat.csv")
dim(bodyfat)
n = dim(bodyfat)[1]
names(bodyfat) 































##############################################################################################
##### Validation set assessment with a pre-defined cv function for inner model selection #####				 
##############################################################################################

library(glmnet)  # use LASSO model from package glmnet 
lambdalist = exp((-1200:1200)/100)  # defines models to consider


##### model selection ##### reference - PenalizedModelAssessment.R from Lesson 5
fulldata.in = bodyfat
x.in = model.matrix(BodyFatSiri~.,data=fulldata.in)[,-(1:4)]
y.in = fulldata.in[,3]
k.in = 10 
n.in = dim(fulldata.in)[1]
groups.in = c(rep(1:k.in,floor(n.in/k.in)),1:(n.in%%k.in))  #produces list of group labels
set.seed(8)
cvgroups.in = sample(groups.in,n.in)  #orders randomly, with seed (8) 
#LASSO cross-validation
cvLASSOglm.in = cv.glmnet(x.in, y.in, lambda=lambdalist, alpha = 1, nfolds=k.in, foldid=cvgroups.in)
plot(cvLASSOglm.in$lambda,cvLASSOglm.in$cvm,type="l",lwd=2,col="red",xlab="lambda",ylab="CV(10)",
     xlim=c(0,3),ylim = c(20,30))
whichlowestcvLASSO.in = order(cvLASSOglm.in$cvm)[1]; min(cvLASSOglm.in$cvm)
bestlambdaLASSO = (cvLASSOglm.in$lambda)[whichlowestcvLASSO.in]; bestlambdaLASSO
abline(v=bestlambdaLASSO)
bestlambdaLASSO  # this is the lambda for the best LASSO model
LASSOfit.in = glmnet(x.in, y.in, alpha = 1,lambda=lambdalist)  # fit the model across possible lambda
LASSObestcoef = coef(LASSOfit.in, s = bestlambdaLASSO); LASSObestcoef # coefficients for the best model fit








##### model assessment outer validation shell #####
fulldata.out = bodyfat
k.out = 10 
n.out = dim(fulldata.out)[1]
#define the split into training set (of size 152) and testing set (of size 100)
trainn.out = 152
testn.out = 100
set.seed(8)
test.out = sample(1:n.out,testn.out)  #produces list of data to exclude
testinclude.out = is.element(1:n.out,test.out)  # sets up a T-F vector to be used similarly as group T-F vectors

#no outer loop, just one split
traindata.out = bodyfat[!testinclude.out,]
trainx.out = model.matrix(BodyFatSiri~.,data=traindata.out)[,-(1:4)]
trainy.out = traindata.out[,3]
testdata.out = bodyfat[testinclude.out,]
testx.out = model.matrix(BodyFatSiri~.,data=testdata.out)[,-(1:4)]
testy.out = testdata.out[,3]
  ### entire model-fitting process  ###
  ###	:	:	:	:	:	:	:   ###
  ###INCLUDING ALL CONSIDERED MODELS###				 
  ###   :	:	:	:	:	:	:   ###
  ### resulting in bestlambdaLASSO ###
LASSOtrainfit.out = glmnet(trainx.out, trainy.out, alpha = 1,lambda=lambdalist)
allpredictedValid.out = predict(LASSOtrainfit.out,newx=testx.out,s=bestlambdaLASSO)
plot(allpredictedValid.out,testy.out)
Valid.out = sum((allpredictedValid.out-testy.out)^2)/testn.out; CV.out
R2.out = 1-sum((allpredictedValid.out-testy.out)^2)/sum((testy.out-mean(testy.out))^2); R2.out







##### model assessment with inner entire model-fitting process (including cross-validation for model selection) #####
fulldata.out = bodyfat
k.out = 10 
n.out = dim(fulldata.out)[1]
#define the split into training set (of size 152) and testing set (of size 100)
trainn.out = 152
testn.out = 100
set.seed(8)
test.out = sample(1:n.out,100)  #produces list of data to exclude
testinclude.out = is.element(1:n.out,test.out)  # sets up a T-F vector to be used similarly as group T-F vectors

#no outer loop, just one split
traindata.out = bodyfat[!testinclude.out,]
trainx.out = model.matrix(BodyFatSiri~.,data=traindata.out)[,-(1:4)]
trainy.out = traindata.out[,3]
testdata.out = bodyfat[testinclude.out,]
testx.out = model.matrix(BodyFatSiri~.,data=testdata.out)[,-(1:4)]
testy.out = testdata.out[,3]
  ### entire model-fitting process ###
    fulldata.in = traindata.out  # only input the data used to fit the model
    x.in = model.matrix(BodyFatSiri~.,data=fulldata.in)[,-(1:4)]
    y.in = fulldata.in[,3]
    k.in = 10 
    n.in = dim(fulldata.in)[1]
    groups.in = c(rep(1:k.in,floor(n.in/k.in)),1:(n.in%%k.in))  #produces list of group labels
#    set.seed(8)   # do not reset seed for each internal loop
    cvgroups.in = sample(groups.in,n.in)  #orders randomly, with seed (8) 
    #LASSO cross-validation
    cvLASSOglm.in = cv.glmnet(x.in, y.in, lambda=lambdalist, alpha = 1, nfolds=k.in, foldid=cvgroups.in)
    plot(cvLASSOglm.in$lambda,cvLASSOglm.in$cvm,type="l",lwd=2,col="red",xlab="lambda",ylab="CV(10)",
     xlim=c(0,3),ylim = c(15,25))
    whichlowestcvLASSO.in = order(cvLASSOglm.in$cvm)[1]; min(cvLASSOglm.in$cvm)
    bestlambdaLASSO = (cvLASSOglm.in$lambda)[whichlowestcvLASSO.in]; bestlambdaLASSO
    abline(v=bestlambdaLASSO)
  ### resulting in bestlambdaLASSO ###
LASSOtrainfit.out = glmnet(trainx.out, trainy.out, alpha = 1,lambda=lambdalist)
allpredictedValid.out = predict(LASSOtrainfit.out,newx=testx.out,s=bestlambdaLASSO)
plot(allpredictedValid.out,testy.out)
Valid.out = sum((allpredictedValid.out-testy.out)^2)/testn.out; CV.out
R2.out = 1-sum((allpredictedValid.out-testy.out)^2)/sum((testy.out-mean(testy.out))^2); R2.out





























				 
###############################################################################################
##### Validation set assessment with inner loop for selection and outer set for assessment #####				 
###############################################################################################

#model list specification
nmodels = 14
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

				 
				 
				 
				 
				 
				 
				 
				 
				 
				 
				 
				 
##### model selection ##### reference - RcmdsCVselection from lesson 2
fulldata.in = bodyfat
k.in = 10 
n.in = dim(fulldata.in)[1]
groups.in = c(rep(1:k.in,floor(n.in/k.in)),1:(n.in%%k.in))  #produces list of group labels
set.seed(8)
cvgroups.in = sample(groups.in,n.in)  #orders randomly, with seed (8) 

allpredictedCV.in = matrix(rep(NA,n.in*nmodels),ncol=nmodels)
for (i in 1:k.in)  {
  groupi.in = (cvgroups.in == i)
  #fit the model
  #placing various model fits INSIDE cross-validation loop makes the program easier-to-adapt to new model fits
  for (m in 1:nmodels) {
    lmfitCV.in = lm(formula = allModels[[m]],data=bodyfat,subset=!groupi.in)
    allpredictedCV.in[groupi.in,m] = predict.lm(lmfitCV.in,fulldata.in[groupi.in,])
  }
}
allmodelCV.in = rep(NA,nmodels) #place-holder for results
for (m in 1:nmodels) { allmodelCV.in[m] = sum((allpredictedCV.in[,m]-fulldata.in$BodyFatSiri)^2)/n.in}
plot(1:nmodels,allmodelCV.in,col="red",pch=20)
bestmodel.in = (1:nmodels)[order(allmodelCV.in)[1]]  # actual selection

REGRbestcoef = coef(lm(formula = allModels[[bestmodel.in]], data=fulldata.in)); REGRbestcoef # coefficients for the best model fit
bestmodel.in










##### model assessment outer validation shell #####
fulldata.out = bodyfat
k.out = 10 
n.out = dim(fulldata.out)[1]
#define the split into training set (of size 152) and testing set (of size 100)
trainn.out = 152
testn.out = 100
set.seed(8)
test.out = sample(1:n.out,100)  #produces list of data to exclude
testinclude.out = is.element(1:n.out,test.out)  # sets up a T-F vector to be used similarly as group T-F vectors

#no outer loop, just one split
traindata.out = bodyfat[!testinclude.out,]
trainx.out = model.matrix(BodyFatSiri~.,data=traindata.out)[,-(1:4)]
trainy.out = traindata.out[,3]
testdata.out = bodyfat[testinclude.out,]
testx.out = model.matrix(BodyFatSiri~.,data=testdata.out)[,-(1:4)]
testy.out = testdata.out[,3]
  ### entire model-fitting process  ###
  ###	:	:	:	:	:	:	:   ###
  ###INCLUDING ALL CONSIDERED MODELS###				 
  ###   :	:	:	:	:	:	:   ###
  ###  resulting in bestmodel.in    ###
lmfitCV.out = lm(allModels[[bestmodel.in]],traindata.out)
allpredictedValid.out = predict.lm(lmfitCV.out,testdata.out)
#assessment
plot(allpredictedCV.out,testy.out)
Valid.out = sum((allpredictedCV.out-testy.out)^2)/testn.out; CV.out
R2.out = 1-sum((allpredictedCV.out-testy.out)^2)/sum((testy.out-mean(testy.out))^2); R2.out






##### model assessment outer validation shell #####
fulldata.out = bodyfat
k.out = 10 
n.out = dim(fulldata.out)[1]
#define the split into training set (of size 152) and testing set (of size 100)
trainn.out = 152
testn.out = 100
set.seed(8)
test.out = sample(1:n.out,100)  #produces list of data to exclude
testinclude.out = is.element(1:n.out,test.out)  # sets up a T-F vector to be used similarly as group T-F vectors

#no outer loop, just one split
traindata.out = bodyfat[!testinclude.out,]
trainx.out = model.matrix(BodyFatSiri~.,data=traindata.out)[,-(1:4)]
trainy.out = traindata.out[,3]
testdata.out = bodyfat[testinclude.out,]
testx.out = model.matrix(BodyFatSiri~.,data=testdata.out)[,-(1:4)]
testy.out = testdata.out[,3]
  ### entire model-fitting process ###
    fulldata.in = traindata.out   # only input the data used to fit the model
    k.in = 10 
    n.in = dim(fulldata.in)[1]
    groups.in = c(rep(1:k.in,floor(n.in/k.in)),1:(n.in%%k.in))  #produces list of group labels
#    set.seed(8)   # do not reset seed for each internal loop
    cvgroups.in = sample(groups.in,n.in)  #orders randomly, with seed (8) 

    allpredictedCV.in = matrix(rep(NA,n.in*nmodels),ncol=nmodels)  
    for (i in 1:k.in)  {
      groupi.in = (cvgroups.in == i)
      for (m in 1:nmodels) {
        lmfitCV.in = lm(formula = allModels[[m]],data=bodyfat,subset=!groupi.in)
        allpredictedCV.in[groupi.in,m] = predict.lm(lmfitCV.in,fulldata.in[groupi.in,])
      }
    }
    allmodelCV.in = rep(NA,nmodels) #place-holder for results
    for (m in 1:nmodels) { allmodelCV.in[m] = sum((allpredictedCV.in[,m]-fulldata.in$BodyFatSiri)^2)/n.in}
    plot(1:nmodels,allmodelCV.in,col="red",pch=20)
    bestmodel.in = (1:nmodels)[order(allmodelCV.in)[1]]  # actual selection
    ###  resulting in bestmodel.in   ###
lmfitCV.out = lm(allModels[[bestmodel.in]],traindata.out)
allpredictedValid.out = predict.lm(lmfitCV.out,testdata.out)
#assessment
plot(allpredictedCV.out,testy.out)
Valid.out = sum((allpredictedCV.out-testy.out)^2)/testn.out; CV.out
R2.out = 1-sum((allpredictedCV.out-testy.out)^2)/sum((testy.out-mean(testy.out))^2); R2.out





