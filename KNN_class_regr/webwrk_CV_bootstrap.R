require(MASS)
require(FNN)
?Boston

set.seed(100)
row <- sample(nrow(Boston), 350)

train.x <- Boston[row, c('age','rad')]
valid.x <- Boston[-row, c('age','rad')]

train.y <- Boston[row, c('crim')]
valid.y <- Boston[-row, c('crim')]

train.x.std = scale(train.x)

#valid.x.std = (valid.x - mean(train.x)) / sd(train.x)

valid.x.std = scale(valid.x, center = attr(train.x.std, 'scaled:center'), 
                    scale = attr(train.x.std, 'scaled:scale'))

pred <- knn.reg(train.x.std, valid.x.std, train.y,k = 25)

mse <- function(predicted, actual){
  return(mean((predicted - actual)^2))
}

mse(pred$pred, valid.y)


bos.std <- Boston[,c('age','rad','crim')]
bos.std['age.std'] <- scale(Boston$age)
bos.std['rad.std'] <- scale(Boston$rad)

write.csv('/Users/ahendel1/Documents/Academics/data-mining/KNN_class_regr/bos.std.csv',row.names = F)

x.std <- bos.std[,c('age.std','rad.std')]
y.tng <- bos.std$crim

pred <- knn.reg(x.std, x.std, y = y.tng, k=25)
mse(pred$pred, y.tng)

## LOOCV ####
kf <- sample(seq(nrow(Boston)))
res <- rep(0,nrow(Boston))
for(f in 1:nrow(Boston)){
  train.x <- Boston[-f,c('age','rad')]
  train.x <- scale(train.x)
  train.y <- Boston[-f,c('crim')]
  
  valid.x <- Boston[f,c('age','rad')]
  valid.x <- scale(valid.x, center = attr(train.x, 'scaled:center'), 
                  scale = attr(train.x, 'scaled:scale'))
  valid.y <- Boston[f,c('crim')]
  pred <- knn.reg(train.x, valid.x, train.y, k=25)
  
  res[f] <- mse(pred$pred, valid.y)
}
mean(res)

#### kfold cross validation ####
n <- nrow(Boston)
k = 10 #using 10-fold cross-validation
groups = c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))  #produces list of group labels
set.seed(100)
cvgroups = sample(groups,n) 
allpredictedCV = rep(0,n)
for(i in 1:k){
  groupi <- (cvgroups == i)
  train.x <- Boston[!groupi,c('age','rad')]
  train.x <- scale(train.x)
  train.y <- Boston[!groupi,c('crim')]
  
  valid.x <- Boston[groupi,c('age','rad')]
  valid.x <- scale(valid.x, center = attr(train.x, 'scaled:center'), 
                   scale = attr(train.x, 'scaled:scale'))
  valid.y <- Boston[groupi,c('crim')]
  pred <- knn.reg(train.x, valid.x, train.y, k=25)
  
  allpredictedCV[groupi] <- pred$pred
}
CV10 <- sum((allpredictedCV-Boston$crim)^2)/n; CV10

#### Bootstrapping Standard Errors in Linear Regression ####
BostonTrans <- Boston[,c('age', 'rad', 'crim')]
names(BostonTrans) <- c('age', 'rad', 'log.crim')
BostonTrans$log.crim <- log(BostonTrans$log.crim)

mod <- lm(log.crim ~ age + rad, data = BostonTrans)
summary(mod)
library(boot)
beta.fn <- function(inputdata,index) {
  lmfitboot = lm(log.crim ~ age + rad,
                 data=inputdata[index,])
  return(lmfitboot$coef)
}
set.seed(100)
bootoutput <- boot(BostonTrans,beta.fn,R=5000); bootoutput
