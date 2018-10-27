# Decision Trees Webwork

d <- read.csv('data-mining/data/CancerSurvival01.csv')

# d$Survival01 <- as.factor(d$Survival01)

# DECISION TREE ####
require(tree)

mytree <- tree(as.factor(Survival01) ~ ., data=d)

plot(mytree)
text(mytree, pretty = 0)

pred <- predict(mytree, data=d[,c('Age','Year','Nodes')], type='class')
table(pred, d$Survival01)

(33 + 35) / (306)
dim(d)

mytree

set.seed(400)
tree.cv <- cv.tree(mytree)
# which minimizes deviance?
min.leaf <- tree.cv$size[which(tree.cv$dev == min(tree.cv$dev))]; min.leaf

prune.tree <- prune.misclass(mytree, best=min.leaf)
plot(prune.tree)
text(prune.tree, pretty = 0)

# BOOSTING ####
require(gbm)
set.seed(400)
# gaussian for gaussian for regression
# bernoulli for classification
# outcome variable must be numeric
boost <- gbm(Survival01~., data=d, 
             distribution='bernoulli', 
             n.trees = 5000, 
             shrinkage = 0.001,
             interaction.depth = 2)
boost
summary(boost)

# view most influencial variable
# Plot the marginal effect of the Nodes variable
plot(boost, i='Nodes')


# Bagging ####
require(randomForest)
set.seed(400)
tree.bag <- randomForest(as.factor(Survival01)~., 
                         data=d,
                         mtry = 3,
                         importance = T)
tree.bag

#  Plot the out-of-bag error rates as a function of the number of trees. 
# Is it appropriate to use the default of 500 trees
plot(tree.bag)
legend("topright",
       colnames(tree.bag$err.rate),
       col = 1:3,
       lty = 1:3)
# 500 trees is fine. it levels off around 50-75, no risk of overfitting trees
# but can slow down compute


importance(tree.bag)
varImpPlot(tree.bag)

partialPlot(tree.bag, pred.data=d, x.var = Age)
