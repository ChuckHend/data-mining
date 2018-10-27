# Homework 11 - 

library(ISLR)
library(nnet)
set.seed(10)
fit <- nnet(Purchase ~ LoyalCH + SalePriceMM + PriceDiff, 
            data=OJ, 
            size=1)

# 2 make a plot
library(NeuralNetTools)
plotnet(fit)

# 3 explain the weights, why the model is predicting whether its a MM sale, not a CH sale
# Based on the plot, we see strong positive weight associated with customer loyalty 
# but a negative value for the weight going to the output unit. PriceDiff is 
# the price of MM less CH - and it would make sense that a negative value here 
# would be in favor of MM (price of CH greater than MM would be in favor or MM sales).
# Also stated, positive LoyalCH and positive PriceDiff are both in favor of CH sales. 
# Since the weight going to the output node is negative, it is flipping this relationship.

# 4 predicted prob that first person will purchase MM
fit$fitted.values[1]

# 5 Compute σ(z), the output of the hidden node, for the first person in the data set.
zH1 <- fit$wts[1] + sum(fit$wts[2:length(fit$wts)] + OJ[1, 2:length(fit$wts)])
sigmaH1 <- 1/(1+exp(-zH1))

# 6
oj.pred <- predict(fit, OJ, type='class')
confusion <- table(oj.pred, OJ$Purchase)
(confusion[1,2] + confusion[2,1])/sum(confusion)

# 7
oj.pred.9 <- rep(NA, length(OJ$Purchase))
oj.pred.9[which(fit$fitted.values > .9)] <- "Yes"
oj.pred.9[which(fit$fitted.values < .1)] <- "No"
confusion <- table(oj.pred.9, OJ$Purchase)
(confusion[1,2] + confusion[2,1])/sum(confusion)

# 9
length(which(is.na(oj.pred.9)))


#10
lekprofile(fit)



#11
library(ISLR)
library(nnet)
# drop where salary is na
hit <- Hitters[!is.na(Hitters$Salary),]
#hit <- na.omit(Hitters)
# confirm there are 2 levels of all we'll manipulate
levels(hit$League)
levels(hit$Division)
levels(hit$NewLeague)
# create new vars
hit$League01 <- ifelse(hit$League == 'A', 0, 1)
hit$Division01 <- ifelse(hit$Division  == 'E', 0, 1)
hit$NewLeague01 <- ifelse(hit$NewLeague  == 'A', 0, 1)
# remove old vars
hit$Division  <- NULL
hit$League <- NULL
hit$NewLeague <- NULL


# 12
# CV to choose decay parameter
n = dim(hit)[1]
k = 10 #using 10-fold cross-validation
groups <- c(rep(1:k, floor(n / k)), 1:(n - floor(n / k) * k))

decayRate = seq(.1, 3, by = .1)
misclassError = matrix( , nr = k, nc = length(decayRate) )

set.seed(10)
cvgroups = sample(groups,n) 

mse <- function(predicted, actual){
  # compute mean squared error
  return(mean((predicted - actual)^2))
}

mean.sq.er <- matrix(, nr=n, nc = length(decayRate))

for (i in 1:k) {
  training <- hit[which(cvgroups != i), ]
  validation <- hit[which(cvgroups == i), ]
  training.std <- scale(training)
  validation.std <- scale(
    validation, 
    center=attr(training.std, 'scaled:center'),
    scale=attr(training.std, 'scaled:scale'))
  
  for (j in 1:length(decayRate)) {
    print(paste('Fold/Rate: ',i,'/',j))
    fit <- nnet(
      Salary ~ .,
      data=training.std, 
      size=10,
      decay=decayRate[j],
      linout=T,
      trace=F,
      maxit=1000) 
    predictions <- predict(fit, validation.std)
    misclass.error <- mse(predictions, validation.std[,'Salary'])
    misclassError[i,j] <- misclass.error
    mean.sq.er[(cvgroups == i), j] <- misclass.error
  }
}
mean(mean.sq.er[,1])

error <- apply(misclassError, 2, mean)
plot(decayRate, error, type = "l", lwd = 2, las = 1)
min(error)
decayRate[which(error == min(error))]

# last
set.seed(10)
hit_std <- scale(hit)
fit <- nnet(
  Salary ~ .,
  data=hit_std,
  decay=1.5,
  size=10,
  linout=T,
  trace=T,
  maxit=300) 
library(NeuralNetTools)
garson(fit)
