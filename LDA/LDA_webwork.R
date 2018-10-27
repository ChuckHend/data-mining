# read in the data
d <- read.csv('Documents/Academics/data-mining/LDA/Dividends.csv')
str(d)
# Let Y denote the outcome, Y=1 for "yes" (dividend issued) and Y=0 for "no" (dividend not issued). 
dim(d)

library(MASS)  #help(lda)
# use the lda() to compute the priors
ldafit1 = lda(y~x, data = d)
ldafit1

# Compute the (posterior) probability that a company will issue a dividend this 
# year given that its percentage profit was 2% (X=2) last year,
pred <- predict(ldafit1, newdata = data.frame(x=2))
pred


mu <- 10
sigma <- 6

goal_fun <- function(mu, sigma, pi){
  t1 <- mu/sigma^2
  t2 <- log(pi, exp(1)) - ((mu^2) / (2*(sigma^2)))
  return(paste(t1, t2, sep = ','))
}

# When Y = 1, using π1 = 0.8 (80% of companies issue dividends), compute the constants for the goal function δ1(x)
goal_fun(10, 6, 0.8)

# companies that do not issue dividend
goal_fun(0, 6, 0.2)


# ROC and AUC
require(pROC)
myroc <- roc(response=d$y, predictor = d$x)
plot(myroc)
myroc$auc

# predict on original data
pred <- predict(ldafit1, data.frame(x=d$x))

require(dplyr)
tab <- table(d$y, pred$class); tab
tpr <- tab['1', '1'] / sum(tab['1',]); tpr
tnr <- tab['0','0'] / sum(tab['0',]); tnr

# Cleveland data set
heart <- read.csv('Documents/Academics/data-mining/LDA/Heart_disease_Cleveland.csv')
heart <- heart[,c('Age', 'BloodPressure', 'Chol', 'MaxHeartRate', 'STdepress',  'DiseaseStatus')]


# buil model
mod1 <- lda(DiseaseStatus ~ MaxHeartRate + STdepress, data = heart)
mod1$means

# pred on training data
pred <- predict(mod1, newdata = heart[,c('MaxHeartRate','STdepress')])
tab <- table(heart$DiseaseStatus, pred$class); tab

# predict on a asingle patient
patient <- data.frame(MaxHeartRate=130, STdepress=2)
result <- predict(mod1, newdata=patient); result

# fit on all vars and compare
mod2 <- lda(DiseaseStatus ~ ., data=heart)
keep <- names(heart)[!names(heart) %in% 'DiseaseStatus']
pred2 <- predict(mod2, heart[,keep])
tab2 <- table(heart$DiseaseStatus, pred2$class); tab2

# parameter estimationm, number of
K=5; p=5; K+K*p+p*(p+1)/2
# where k= number of predictor classes
# p is number of predictors

# CV Example
cvk <- 10
n <- nrow(heart)
groups <- c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))
set.seed(4)
cvgroups = sample(groups,n)
#prediction via cross-validation
allpredictedCV1 = rep("NA",n)

for (i in 1:cvk)  {
  ldafit1i = lda(DiseaseStatus ~ ., data=heart, subset=(cvgroups!=i))
  newdata1i = data.frame(heart[cvgroups==i,])
  allpredictedCV1[cvgroups==i] = as.character(predict(ldafit1i,newdata1i)$class)
}
table(heart$DiseaseStatus,allpredictedCV1)
CVallmodel = sum(allpredictedCV1!=heart$DiseaseStatus)/n; CVallmodel

for (i in 1:cvk)  {
  ldafit1i = lda(DiseaseStatus ~ MaxHeartRate + STdepress, data = heart,subset=(cvgroups!=i))
  newdata1i = data.frame(heart[cvgroups==i,c('MaxHeartRate','STdepress')])
  allpredictedCV1[cvgroups==i] = as.character(predict(ldafit1i,newdata1i)$class)
}
table(heart$DiseaseStatus,allpredictedCV1)
CVmodelsmall = sum(allpredictedCV1!=heart$DiseaseStatus)/n; CVmodelsmall

#### QDA ####
mod3 <- qda(DiseaseStatus ~ MaxHeartRate + STdepress, data = heart); mod3
pred3 <- predict(mod3, heart)
tab3 <- table(heart$DiseaseStatus, pred3$class); tab3

# predict on a asingle patient
patient <- data.frame(MaxHeartRate=130, STdepress=2)
result <- predict(mod3, newdata=patient); result

# fit all model
mod4 <- qda(DiseaseStatus ~ ., data=heart)
pred4 <- predict(mod4, heart)
table(heart$DiseaseStatus, pred4$class)

# which group has highest variance?
aggregate(heart$STdepress, by=list(heart$DiseaseStatus), FUN=sd)

# get number of parameters
get.params <- function(k, p){
  # where k= number of predictor classes
  # p is number of predictors
  params <- (k*(p+1)*(p/2 + 1))
  return(params)
}

# light mod
get.params(5, 2)
# full mod
get.params(5, 5)

cvk <- 10
n <- nrow(heart)
groups <- c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))
set.seed(4)
cvgroups = sample(groups,n)
#prediction via cross-validation
allpredictedCV1 = rep("NA",n)

# full model
for (i in 1:cvk)  {
  ldafit1i = qda(DiseaseStatus ~ ., data=heart, subset=(cvgroups!=i))
  newdata1i = data.frame(heart[cvgroups==i,])
  allpredictedCV1[cvgroups==i] = as.character(predict(ldafit1i,newdata1i)$class)
}
table(heart$DiseaseStatus,allpredictedCV1)
CVallmodel = sum(allpredictedCV1!=heart$DiseaseStatus)/n; CVallmodel

# partial model
for (i in 1:cvk)  {
  ldafit1i = qda(DiseaseStatus ~ MaxHeartRate + STdepress, data = heart,subset=(cvgroups!=i))
  newdata1i = data.frame(heart[cvgroups==i,c('MaxHeartRate','STdepress')])
  allpredictedCV1[cvgroups==i] = as.character(predict(ldafit1i,newdata1i)$class)
}
table(heart$DiseaseStatus,allpredictedCV1)
CVmodelsmall = sum(allpredictedCV1!=heart$DiseaseStatus)/n; CVmodelsmall
