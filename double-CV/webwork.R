# double cross-validation webwork

d <- read.csv('data-mining/data/Heart_disease_Cleveland.csv')

# create var HD 1=has HD, 0 = no HD
d$HD <- ifelse(d$DiseaseStatus == 0, 0, 1)

heart <- d[,c('Age','BloodPressure','Chol','MaxHeartRate','STdepress','HD')]

set.seed(8)
library(MASS)

# CV Approach ####
##entire model-fitting process##
xy.in = heart
n.in = dim(xy.in)[1]
ncv = 10
if ((n.in%%ncv) == 0) {
  groups.in= rep(1:ncv,floor(n.in/ncv))} else {
    groups.in=c(rep(1:ncv,floor(n.in/ncv)),(1:(n.in%%ncv)))
  }

cvgroups.in = sample(groups.in,n.in)
# with model selection 
allpredictedcv10 = matrix(rep(0,n.in*6),ncol=6)
for (i in 1:ncv) {
  newdata.in = xy.in[cvgroups.in==i,]
  lda2fit = lda(HD ~ MaxHeartRate + STdepress, data=xy.in, subset=(cvgroups.in!=i))
  allpredictedcv10[cvgroups.in==i,1] = predict(lda2fit,newdata.in)$class
  lda5fit = lda(HD ~., data= xy.in, subset=(cvgroups.in!=i))
  allpredictedcv10[cvgroups.in==i,2] = predict(lda5fit,newdata.in)$class
  
  qda2fit = qda(HD ~ MaxHeartRate + STdepress, data=xy.in, subset=(cvgroups.in!=i))
  allpredictedcv10[cvgroups.in==i,3] = predict(qda2fit,newdata.in)$class
  qda5fit = qda(HD ~., data= xy.in, subset=(cvgroups.in!=i))
  allpredictedcv10[cvgroups.in==i,4] = predict(qda5fit,newdata.in)$class
  
  log2fit = glm(HD ~ MaxHeartRate + STdepress, data=xy.in, subset=(cvgroups.in!=i), family=binomial)
  log2prob = predict(log2fit,newdata.in,type="response")
  log2fact = rep(0,dim(newdata.in)[1]); log2fact[log2prob > 0.5] = 1
  allpredictedcv10[cvgroups.in==i,5] = log2fact
  
  log5fit = glm(HD ~., data= xy.in, subset=(cvgroups.in!=i),family=binomial)
  log5prob = predict(log5fit,newdata.in,type="response")
  log5fact = rep(0,dim(newdata.in)[1]); log5fact[log5prob > 0.5] = 1
  allpredictedcv10[cvgroups.in==i,6] = log5fact
}
allpredictedcv10[,1:4] = allpredictedcv10[,1:4]-1
allcv10 = rep(0,6)
for (m in 1:6) allcv10[m] = sum(xy.in$HD!=allpredictedcv10[,m])/n.in
bestmodels = (1:6)[allcv10 == min(allcv10)]
##############################


## Validation Set Approach ####


##### model assessment OUTER shell #####
nvalid = 100
xy.out = heart
n.out = dim(xy.out)[1]
#define the validation set
set.seed(8)
validset = sample(1:n.out,nvalid)

trainxy.out = xy.out[-validset,]
testxy.out = xy.out[validset,]
###        inputs trainxy.out       ###
xy.in = xy.out
n.in = dim(xy.in)[1]
ncv = 10
if ((n.in%%ncv) == 0) {
  groups.in= rep(1:ncv,floor(n.in/ncv))} else {
    groups.in=c(rep(1:ncv,floor(n.in/ncv)),(1:(n.in%%ncv)))
  }

cvgroups.in = sample(groups.in,n.in)
# with model selection 
allpredictedcv10 = matrix(rep(0,n.in*6),ncol=6)
for (i in 1:ncv) {
  newdata.in = xy.in[cvgroups.in==i,]
  lda2fit = lda(HD ~ MaxHeartRate + STdepress, data=xy.in, subset=(cvgroups.in!=i))
  allpredictedcv10[cvgroups.in==i,1] = predict(lda2fit,newdata.in)$class
  lda5fit = lda(HD ~., data= xy.in, subset=(cvgroups.in!=i))
  allpredictedcv10[cvgroups.in==i,2] = predict(lda5fit,newdata.in)$class
  
  qda2fit = qda(HD ~ MaxHeartRate + STdepress, data=xy.in, subset=(cvgroups.in!=i))
  allpredictedcv10[cvgroups.in==i,3] = predict(qda2fit,newdata.in)$class
  qda5fit = qda(HD ~., data= xy.in, subset=(cvgroups.in!=i))
  allpredictedcv10[cvgroups.in==i,4] = predict(qda5fit,newdata.in)$class
  
  log2fit = glm(HD ~ MaxHeartRate + STdepress, data=xy.in, subset=(cvgroups.in!=i), family=binomial)
  log2prob = predict(log2fit,newdata.in,type="response")
  log2fact = rep(0,dim(newdata.in)[1]); log2fact[log2prob > 0.5] = 1
  allpredictedcv10[cvgroups.in==i,5] = log2fact
  
  log5fit = glm(HD ~., data= xy.in, subset=(cvgroups.in!=i),family=binomial)
  log5prob = predict(log5fit,newdata.in,type="response")
  log5fact = rep(0,dim(newdata.in)[1]); log5fact[log5prob > 0.5] = 1
  allpredictedcv10[cvgroups.in==i,6] = log5fact
}
allpredictedcv10[,1:4] = allpredictedcv10[,1:4]-1
allcv10 = rep(0,6)
for (m in 1:6) allcv10[m] = sum(xy.in$HD!=allpredictedcv10[,m])/n.in
bestmodels = (1:6)[allcv10 == min(allcv10)]
###      resulting in bestmodels     ###

bestmodel = ifelse(length(bestmodels)==1,bestmodels,sample(bestmodels,1))
if (bestmodel == 1)  {
  lda2fit.train = lda(HD ~ MaxHeartRate + STdepress, data=trainxy.out)
  predictvalid = as.numeric(predict(lda2fit.train, testxy.out)$class)-1
}
if (bestmodel == 2)  {
  lda5fit.train = lda(HD ~ ., data=trainxy.out)
  predictvalid = as.numeric(predict(lda5fit.train, testxy.out)$class)-1
}
if (bestmodel == 3)  {
  qda2fit.train = qda(HD ~ MaxHeartRate + STdepress, data=trainxy.out)
  predictvalid = as.numeric(predict(qda2fit.train, testxy.out)$class)-1
}
if (bestmodel == 4)  {
  qda5fit.train = qda(HD ~ ., data=trainxy.out)
  predictvalid = as.numeric(predict(qda5fit.train, testxy.out)$class)-1
}
if (bestmodel == 5)  {
  log2fit.train = glm(HD ~ MaxHeartRate + STdepress, data= trainxy.out, family=binomial)
  log2prob.test = predict(log2fit.train,testxy.out,type="response")
  predictvalid = rep(0,dim(testxy.out)[1]); predictvalid[log2prob.test > 0.5] = 1
}
if (bestmodel == 6)  {
  log5fit.train = glm(HD ~ ., data= trainxy.out, family=binomial)
  log5prob.test = predict(log5fit.train,testxy.out,type="response")
  predictvalid = rep(0,dim(testxy.out)[1]); predictvalid[log5prob.test > 0.5] = 1
}

#assessment
CV.valid = sum(testxy.out$HD!=predictvalid)/nvalid
p.valid = 1-CV.valid


  