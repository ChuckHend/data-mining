# webwork
# generate a sin wave
x <- seq(from=0, to = 2*pi, by = 0.01)
y = sin(x)

library(nnet)
set.seed(20)
# this is equivalent to a linear model
fit0 = nnet(y~x, size = 0, linout = T, skip = T)
summary(fit0)
# this is a neural net
fit1 <- nnet(y~x, size = 1, linout = T)
summary(fit1)
fit2 <- nnet(y~x, size = 2, linout = T)
summary(fit2)

mse <- function(y, yhat){
  return (mean((y - yhat)**2))
}

mse(y, fit0$fitted.values)
mse(y, fit1$fitted.values)
mse(y, fit2$fitted.values)

# try to make fit2  converge
fit2 <- nnet(y~x, size = 2, linout = T, maxit = 200)

set.seed(16)
fit2 <- nnet(y~x, size = 2, linout = T, maxit = 500)
summary(fit2)
fit2$convergence


#### Loan Predict Ex. ####
require(ISLR)
summary(Default)
# form a numerical category
Default$student01 <- ifelse(Default$student=='Yes', 1,0)
Default$student <- NULL

set.seed(4)
nnet.mod <- nnet(default ~ ., data=Default, 
                 size = 1)
summary(nnet.mod$fitted.values)

# check the math on a prediction
yhat <- function(x_i, b, w){
  return(1/(1 + 1/exp(b + w*x_i)))
}
yhat(x_i=100000, b=0.01, w=0.02)

# experment with scaling

Default.std <- data.frame(lapply(Default[,c('balance', 'income', 'student01')], scale))
Default.std$default <- Default$default

fit3 <- nnet(default ~ ., data=Default.std, size = 1, maxit = 200)
library(NeuralNetTools)
plotnet(fit3)
text(47, 78, round(fit3$wts[1], 2))
text(32, 73, round(fit3$wts[2], 2))
text(32, 53, round(fit3$wts[3], 2))
text(32, 36, round(fit3$wts[4], 2))
text(84, 75, round(fit3$wts[5], 2))
text(70, 53, round(fit3$wts[6], 2))
summary(fit3)

fit3$wts


fit3$fitted.values[28]
zH1 = fit3$wts[1] + sum(fit3$wts[1:3] * Default.std[28, 1:3])
zH1 = 0.007841263
sigmaH1 = 1/(1+exp(-zH1))
zOut = fit3$wts[5] + sigmaH1 * fit3$wts[6]
1/(1+exp(-zOut))


# prob 8
DefaultClass = predict(fit3, Default.std, type = "class")
table(DefaultClass, Default.std$default)
# misclass rate
(40 + 228) / (9627 + 228 + 40 + 105)

DefaultClass.8 = rep(NA, length(Default.std$default))
DefaultClass.8[which(fit3$fitted.values > .8)] = "Yes"
DefaultClass.8[which(fit3$fitted.values < .2)] = "No"
table(DefaultClass.8, Default.std$default)
# misclass rate
confusion = table(predvals = DefaultClass.8, truth = Default.std$default)
(confusion[1,2] + confusion[2,1])/sum(confusion)
# how many fail to make any prediciton
sum(is.na(DefaultClass.8))

# variable importance
require(NeuralNetTools)
garson(fit3)
lekprofile(fit3)

# ANN EXAMPLE ####
library(nnet)
set.seed(100)
# CV to tune number of hidden nodes
n = dim(Default)[1]
k = 10 # using 10-fold cross-validation
groups = c(rep(1:k,floor(n/k)))

sizes = 1:8
misclassError = matrix( , nr = k, nc = length(sizes) )
conv = matrix( , nr = k, nc = length(sizes) ) 

set.seed(4)
cvgroups = sample(groups,n) 

# Cross-Validation


# CV to choose # of hidden nodes
n = dim(Default)[1]
k = 10 #using 10-fold cross-validation
groups = c(rep(1:k,floor(n/k)))

sizes = 1:8
misclassError = matrix( , nr = k, nc = length(sizes) )
conv = matrix(, nr = k, nc = length(sizes) ) 

set.seed(4)
cvgroups = sample(groups,n) 

for(i in 1:k){
  groupi = (cvgroups == i)
  myDefault.train = scale(Default[!groupi, 2:4])
  myDefault.valid = scale(Default[groupi, 2:4], center = attr(myDefault.train, "scaled:center"), 
                          scale = attr(myDefault.train, "scaled:scale"))
  
  myDefault.train = data.frame(default=Default[!groupi, 1], myDefault.train)
  myDefault.valid = data.frame(default=Default[groupi, 1], myDefault.valid)
  
  for(j in 1:length(sizes)){
    fit = nnet(default ~ ., data=myDefault.train, size = sizes[j], trace = F, maxit = 1000) 
    
    predictions = predict(fit, myDefault.valid, type = "class")
    misclassError[i, j] = length(which(predictions != myDefault.valid[ , 1])) / length(predictions)
    conv[i, j] = fit$convergence
  } # end iteration over j
} # end iteration over i
colSums(conv)


plot(colMeans(misclassError), sizes)
colMeans(misclassError)[which(colMeans(misclassError) == min (colMeans(misclassError)))]
colMeans(misclassError)

# 13
set.seed(4)
train = sample(1:10000, 8000, replace = F)
myDefault.train = scale(Default[train, 2:4])
myDefault.valid = scale(Default[-train, 2:4], center = attr(myDefault.train, "scaled:center"), 
                        scale = attr(myDefault.train, "scaled:scale"))

myDefault.train = data.frame(default=Default[train, 1], myDefault.train)
myDefault.valid = data.frame(default=Default[-train, 1], myDefault.valid)
fit <- nnet(default ~ . , data=myDefault.train, size=4, maxit=1000, trace=T, linout=F)
# number of weights
length(fit$wts)

# misclass rate on train
train.predict = predict(fit, myDefault.train, type = "class")
length(which(train.predict != myDefault.train[ , 1]))/length(train.predict)

# misclass rate on validation
valid.predict <- predict(fit, newdata = myDefault.valid, type='class')
confusion = table(predvals = valid.predict, truth = myDefault.valid$default)
(confusion[1,2] + confusion[2,1])/sum(confusion)

# 14
fit2 = nnet(default ~ ., data=myDefault.train, size = 4, maxit=1000, decay = .5)
valid.predict <- predict(fit2, newdata = myDefault.valid, type='class')
confusion = table(predvals = valid.predict, truth = myDefault.valid$default)
(confusion[1,2] + confusion[2,1])/sum(confusion)

# find max of abs weights
max(abs(fit2$wts))
# compared to non-decay model
max(abs(fit$wts))
