# Cross Validation Examples

require(mosaic)   # Load additional packages here 
require(datasets)

# Basic CV ####
set.seed(2)
groups = c(rep(1:5,6),1)
n <- nrow(trees)
cvgroups <- sample(groups, n)

# Questions 11-12
k <- 5
allpredictedCV = rep(0,n)
for (i in 1:k)  {
  groupi = (cvgroups == i)
  lmfitCV <- lm(Volume ~ Girth + Height + I(Girth*Height) + I(Girth**2) + I((Girth**2)*Height), 
                data = trees,
                subset = !groupi)
  allpredictedCV[groupi] <- predict.lm(lmfitCV,trees[groupi,])
}
allpredictedCV

# Boostrap CV ####
library(boot)
beta.fn <- function(inputdata,index) {
  lmfitboot = lm(Volume ~ Girth + Height + I(Girth*Height) + I(Girth**2) + I((Girth**2)*Height),
                 data=inputdata[index,])
  return(lmfitboot$coef)
}

set.seed(2)
bootoutput <- boot(trees,beta.fn,R=1000)
bootoutput


# Multi-Model CV ####

model1 <- (Volume ~ Girth + Height + I(Girth*Height) + I(Girth**2) + I((Girth**2)*Height))
model2 <- (Volume ~ Girth + Height)
model3 <- (Volume ~ Girth + Height + I(Girth*Height))
model4 <- (Volume ~ Girth + Height + I(Girth**2) + I((Girth**2)*Height))
model5 <- (Volume ~ I(Girth**2) + I((Girth**2)*Height))
model6 <- (Volume ~ I(Girth**2)*Height)
allModels <- list(model1, model2, model3, model4, model5, model6)

n <- nrow(trees)
k <- 5
groups = c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))  #produces list of group labels
set.seed(2)
cvgroups <- sample(groups, n)

# Questions 11-12
for (m in 1:numMods) {
  #prediction via cross-validation
  allpredictedCV = rep(0,n)
  for (i in 1:k)  {
    groupi = (cvgroups == i)
    lmfitCV = lm(formula = allModels[[m]],data=trees[!groupi,])
    pred = predict.lm(lmfitCV,trees[groupi,])
    allpredictedCV[groupi] <- pred
  }
  allmodelCV[m] = sum((allpredictedCV-trees$Volume)^2)/n
}
allmodelCV
which(allmodelCV == min(allmodelCV))


# CV10 KNN ####
require(ISLR)
require(FNN)
require(dplyr)

k <- 10
n <- nrow(Auto)
groups = c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))
set.seed(2)
cvgroups <- sample(groups, n)

CVresults <- rep(0, n)
for (i in 1:k)  {
  groupi = (cvgroups == i)
  weight.std <- Auto$weight[!groupi] %>% scale()
  year.std   <- Auto$year[!groupi] %>% scale()
  train.x <- cbind(weight.std, year.std)
  train.y <- Auto$mpg[!groupi]
  
  valid.weight.std <- (Auto$weight[groupi] - mean(Auto$weight[!groupi])) / sd(Auto$weight[!groupi])
  valid.year.std <- (Auto$year[groupi] - mean(Auto$year[!groupi])) / sd(Auto$year[!groupi])
  valid.x <- cbind(valid.weight.std, valid.year.std)
  valid.y <- Auto$mpg[groupi]
  
  knnCV <- knn.reg(train.x, valid.x, train.y, k = 1)
  
  CVresults[groupi] <- knnCV$pred
}
CV10 <- sum((CVresults-Auto$mpg)^2)/n; CV10

#CV10 <- sum((results$valid.y - results$knnCV.pred)^2)/nrow(results)

# CV 10 Evaluate K in KNN ####

require(ISLR)
require(FNN)
require(dplyr)
require(ggplot2)
kf <- 10
KNN = seq(30)
n <- nrow(Auto)
groups = c(rep(1:kf,floor(n/kf)),1:(n-floor(n/kf)*kf))  #produces list of group labels
all.results <- data.frame()
set.seed(2)
cvgroups <- sample(groups, n)
for(kn in KNN){
  CVresults = rep(NA,n)
  for (i in 1:kf)  {
    groupi = (cvgroups == i)
    weight.std <- Auto$weight[!groupi] %>% scale()
    year.std   <- Auto$year[!groupi] %>% scale()
    train.x <- cbind(weight.std, year.std)
    train.y <- Auto$mpg[!groupi]
    
    valid.weight.std <- (Auto$weight[groupi] - mean(Auto$weight[!groupi])) / sd(Auto$weight[!groupi])
    valid.year.std <- (Auto$year[groupi] - mean(Auto$year[!groupi])) / sd(Auto$year[!groupi])
    valid.x <- cbind(valid.weight.std, valid.year.std)
    
    knnCV <- knn.reg(train.x, valid.x, train.y, k = kn)
    
    CVresults[groupi] <- knnCV$pred
  }
  CV10 <- sum((CVresults - Auto$mpg)^2)/n
  all.results <- rbind(all.results, data.frame(k=kn, CV10=CV10))
}
colVec <- all.results$CV10 == min(all.results$CV10)
ggplot(all.results, aes(x=k,y=CV10)) +
  geom_point(aes(col=!colVec)) +
  scale_x_continuous(name='K-Neighbors') +
  guides(col=F) +
  annotate('text',x = which(all.results$CV10 == min(all.results$CV10)),y=9, label='K=20')

ggsave('cvPlot.png')

