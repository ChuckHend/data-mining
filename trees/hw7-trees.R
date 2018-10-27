## Homework 7 DS  740-data mining

# Q1
# load required data
require(ISLR)
data(OJ)
set.seed(7)
# define indices for training data
train <- sample(1:nrow(OJ), 800)
# assign training data
# training data can be accessed via OJ[train,]

# Q2
# fit a tree model
require(tree)
oj.tree <- tree(Purchase~., data = OJ[train,])
# which variables were used?
summary(oj.tree) # "LoyalCH"     "SalePriceMM" "PriceDiff" 

# Q3
# error rate is 0.1788, per summary()

# Q4 Plot the decision tree with labels
jpeg('decision-tree.jpg')
plot(oj.tree)
text(oj.tree, pretty=FALSE)
dev.off()
# the model will classify whether a customer purchase Citris Hill or Minute Maid Orange Juice, based
# on the customer brand layalty to CH, the listed sale price for Minute Maid, an the difference between
# the price of Minute Maid and Citrus Hill. 
# if the loyalty factor to citrus hill is less than 0.48, they purchase MM. Otherwise, they purchase 
# MM if the Loyalty is below 0.76 and PriceDiff is less than -0.165, meaning MM is cheaper than CH.

# Q5 compute confusion matrix on validation set
oj.pred <- predict(oj.tree, newdata = OJ[-train,], type = 'class')
table(oj.pred, OJ$Purchase[-train])
# error rate = 0.1444444
(21 + 18) / (21 + 18 + 147 + 84)

# Q6 Use 10 fold CV to choose number of leaves that minimizes error rate
set.seed(7)
oj.cv <- cv.tree(oj.tree, FUN = prune.misclass)
# index the minimum deviance with the index of tree size to return optimum
leaves <- oj.cv$size[which(oj.cv$dev == min(oj.cv$dev))]; leaves

# Q7
# Create a pruned tree with 5 leaves.  
# What is the error rate of the pruned tree on the validation set
oj.prune <- prune.misclass(oj.tree, best=5)
plot(oj.prune)
text(oj.prune, pretty=0)
oj.prune.pred <- predict(oj.prune, 
                         newdata = OJ[-train,], 
                         type = 'class')
table(oj.prune.pred, OJ$Purchase[-train])
# misclass error rate
(18 + 21) / (147 + 18 + 21 + 84)

# Q8
# Load Hitters
data("Hitters")
# select rows where Salary is not NA
Hitters.transform <- Hitters[!is.na(Hitters$Salary),]
# log transform salary
Hitters.transform$log.Salary <- log(Hitters.transform$Salary)
# remove non-transformed salary from dataframe
Hitters.transform$Salary <- NULL

# Q9 Boosting to predict log.Salary
require(gbm)
# gaussian for gaussian for regression
# bernoulli for classification
# outcome variable must be numeric
set.seed(7)
boost <- gbm(log.Salary~., data=Hitters.transform, 
             distribution='gaussian', 
             n.trees = 5000, 
             shrinkage = 0.001,
             interaction.depth = 4)
# most important variable?
summary(boost) # CAtBat w/ highest rel.inf

# Q10 - 10Fold CV

# setup for CV
n <- nrow(Hitters.transform)
k <- 10 
groups <- c(rep(1:k, floor(n / k)), 1:(n - floor(n / k) * k))
set.seed(7)
cv.groups <- sample(groups, n)

# placeholders for results we want to compare
boost.predict <- rep(-1, n)
lm.predict <- rep(0, n)

for (i in 1:k) {
  group_i <- (cv.groups == i)
  
  # fit the gb model
  set.seed(7)
  boost <- gbm(log.Salary ~ ., 
               data = Hitters.transform[!group_i, ],
               distribution = 'gaussian',
               n.trees = 5000,
               shrinkage = 0.001,
               interaction.depth = 4)
  
  # fit multi-linear model
  set.seed(7)
  lm.fit <- lm(log.Salary ~ .,
               data=Hitters.transform[!group_i, ])
  
  # predict boost
  boost.predict[group_i] <- predict(boost,
                                    newdata = Hitters.transform[group_i, ],
                                    n.trees = 5000)
  # predict multi-linear
  lm.predict[group_i] <- predict(lm.fit,
                                 newdata = Hitters.transform[group_i, ])
}

# Q11-13
# Compute MSEs
mse <- function(yhat, y){
  return(mean((yhat - y) ^ 2))
}
# boost MSE
mse(boost.predict, Hitters.transform$log.Salary)
# lm MSE
mse(lm.predict, Hitters.transform$log.Salary)
# boosting is better

# Q14
# Why random forest for Hitters dataset?
# use when strong correlations between predictors
# or, when one or a handful of predictors are much more informative than others
pairs(Hitters)
summary(Hitters)
pairs(Hitters[,c('AtBat', 'Hits', 'HmRun', 'RBI', 'CHmRun')])
# There is high collinearity in the data set. For example, at bats, hits, 
# home runs, and RBIs are all highly correlated. Other variables are also collinearity. 
# If were used bagging, we would have higher variance due to the high correlation. 
# Instead, we can select a random subset of the predictors to use at each split by using RandomForest.

# Q15
# Bagging
require(randomForest)
tree.bag <- randomForest(log.Salary~., 
                         data=Hitters.transform,
                         importance = T)

# Q16
# compare the explanation of variance
tree.bag
summary(lm.fit)
# We see that the the linear model explains approximately 54% of the variance, 
# hereas the bagged model explains about 77%. Thus the bagged model is a better 
# representation of the variance in the data set.

# Q16
# variable most important?
tree.bag$importance
importance(tree.bag)
varImpPlot(tree.bag)

# Q18
# 10fold CV to compare bagging random forest with 6 vars with resutls from 
# setup for CV

n <- nrow(Hitters.transform)
k <- 10 
groups <- c(rep(1:k, floor(n / k)), 1:(n - floor(n / k) * k))
set.seed(7)
cv.groups <- sample(groups, n)

# placeholders for results we want to compare vs Q8-10
bag.predict <- rep(-1, n)

for (i in 1:k) {
  group_i <- (cv.groups == i)
  # fit the bagged model
  set.seed(7)
  bag.fit <- randomForest(log.Salary~., 
              data=Hitters.transform[!group_i, ],
              mtry=6,
              importance=T)
  # predict multi-linear
  bag.predict[group_i] <- predict(bag.fit,
                                 newdata = Hitters.transform[group_i, ])
}
# bag MSE
mse(bag.predict, Hitters.transform$log.Salary)
# boost MSE
mse(boost.predict, Hitters.transform$log.Salary)
# Under the same 10-fold cross-validation conditions, bagging with random forests is 
# producing a lower (better) MSE than boosting. We can see a MSE of about 22% with boosting, 
# compared to about 18% under bagging with random forests. We noted collinearity earlier, 
# so as expected we saw improved performance under bagging with random forests compared to boosting.