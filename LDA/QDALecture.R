
 
##### data #####
data(iris)
attach(iris)
n = dim(iris)[1]; n

#explore the data
names(iris)
levels(Species)
K=3

xvar = Petal.Length  # try different options: Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
boxplot(xvar ~ Species)
#both Petal.Length and Petal.Width look like good candidates for predictors

##### QDA with one predictor #####
library(MASS)  #help(lda)
qdafit2 = qda(Species~Petal.Length)
qdafit2

fittedclass2 = predict(qdafit2,data=iris)$class
table(Species,fittedclass2)
Error2 = sum(Species != fittedclass2)/n; Error2

#prediction via cross-validation
allpredictedCV2 = rep("NA",n)
cvk = 10
groups = c(rep(1:cvk,floor(n/cvk)))
set.seed(4)
cvgroups = sample(groups,n)

for (i in 1:cvk)  {
  qdafit2i = qda(Species~Petal.Length,subset=(cvgroups!=i))
  newdata2i = data.frame(iris[cvgroups==i,])
  allpredictedCV2[cvgroups==i] = as.character(predict(qdafit2i,newdata2i)$class)
}
table(Species,allpredictedCV2)
CVmodel2 = sum(allpredictedCV2!=Species)/n; CVmodel2


##### LDA with multiple predictors #####
ldafit3 = lda(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width)
ldafit3

fittedclass3 = predict(ldafit3,data=iris)$class
table(Species,fittedclass3)
Error3 = sum(Species != fittedclass3)/n; Error3


#prediction via cross-validation
allpredictedCV3 = rep("NA",n)

for (i in 1:cvk)  {
  ldafit3i = lda(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,subset=(cvgroups!=i))
  newdata3i = data.frame(iris[cvgroups==i,])
  allpredictedCV3[cvgroups==i] = as.character(predict(ldafit3i,newdata3i)$class)
}
table(Species,allpredictedCV3)
CVmodel3 = sum(allpredictedCV3!=Species)/n; CVmodel3
table(Species,fittedclass3)





#assumptions
xvar = cbind(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)
xSetosa = xvar[Species == "setosa",]
xVersicolor = xvar[Species == "versicolor",]
xVirginica = xvar[Species == "virginica",]

library(MVN)
hzTest(xSetosa)
hzTest(xVersicolor)
hzTest(xVirginica)
#multivariate normality of the predictors is reasonable, 
#  so it is not surprising that cross-validation gives precisely the same 
#  classification counts as assessing the model on the same data

library(biotools)
boxM(xvar,Species)
#equal covariance matrices are definitely NOT reasonable, 
#  so QDA is the better option







##### QDA with multiple predictors #####
qdafit4 = qda(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width)
qdafit4

fittedclass4 = predict(qdafit4,data=iris)$class
table(Species,fittedclass4)
Error4 = sum(Species != fittedclass4)/n; Error4

#prediction via cross-validation
allpredictedCV4 = rep("NA",n)

for (i in 1:cvk)  {
  qdafit4i = lda(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,subset=(cvgroups!=i))
  newdata4i = data.frame(iris[cvgroups==i,])
  allpredictedCV4[cvgroups==i] = as.character(predict(qdafit4i,newdata4i)$class)
}
table(Species,allpredictedCV4)
CVmodel4 = sum(allpredictedCV4!=Species)/n; CVmodel4
table(Species,fittedclass4)






##### Model comparison #####
# CVmodel1 was 0.06, lda with 1 predictor
CVmodel2 #qda with 1 predictor
CVmodel3  #lda
CVmodel4   #qda

#equal covariance matrices are definitely NOT reasonable, 
#  so we prefer models 2 and 4 (QDA) to models 1 and 3 (LDA)
#  and model 4 has a marginally lower CV than does model 2, 
#  so we select model 4














