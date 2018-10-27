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


##### LDA #####

library(MASS)  #help(lda)

ldafit1 = lda(Species~Petal.Length)
ldafit1

fittedclass1 = predict(ldafit1,data=iris)$class
table(Species,fittedclass1)
Error1 = sum(Species != fittedclass1)/n; Error1

#prediction via cross-validation
allpredictedCV1 = rep("NA",n)

cvk = 10
groups = c(rep(1:cvk,floor(n/cvk)))
set.seed(4)
cvgroups = sample(groups,n)

for (i in 1:cvk)  {
  ldafit1i = lda(Species~Petal.Length,subset=(cvgroups!=i))
  newdata1i = data.frame(iris[cvgroups==i,])
  allpredictedCV1[cvgroups==i] = as.character(predict(ldafit1i,newdata1i)$class)
}
table(Species,allpredictedCV1)
CVmodel1 = sum(allpredictedCV1!=Species)/n; CVmodel1


#assumptions
xvar = Petal.Length
xSetosa = xvar[Species == "setosa"]
xVersicolor = xvar[Species == "versicolor"]
xVirginica = xvar[Species == "virginica"]

shapiro.test(xSetosa); qqnorm(xSetosa)
shapiro.test(xVersicolor); qqnorm(xVersicolor)
shapiro.test(xVirginica); qqnorm(xVirginica)
#normality of the Petal.Length is reasonable, 
#  so it is not surprising that Error and CV are reasonably close

bartlett.test(xvar,Species)
sd(xSetosa)
sd(xVersicolor)
sd(xVirginica)
#equal variances are definitely NOT reasonable, so QDA is the better option -- see next Lecture




#decision boundaries
pi.hat1 = length(xSetosa)/n
pi.hat2 = length(xVersicolor)/n
pi.hat3 = length(xVirginica)/n
mu.hat1 = mean(xSetosa)
mu.hat2 = mean(xVersicolor)
mu.hat3 = mean(xVirginica)
sigma2 = 1/(n-K)*(sum((xSetosa-mu.hat1)^2)+sum((xVersicolor-mu.hat2)^2)+sum((xVirginica-mu.hat3)^2))

slope1 = (mu.hat1/sigma2); int1 = (-(1/2)*mu.hat1^2/sigma2 + log(pi.hat1))
slope2 = (mu.hat2/sigma2); int2 = (-(1/2)*mu.hat2^2/sigma2 + log(pi.hat2))
slope3 = (mu.hat3/sigma2); int3 = (-(1/2)*mu.hat3^2/sigma2 + log(pi.hat3))
curve(slope1*x+int1,col="black",xlim=c(0,max(Petal.Length)),ylim=c(0,80),lwd=2)
abline(int2,slope2,col="red",lwd=2)
abline(int3,slope3,col="blue",lwd=2)
legend(0,60,c("setosa","versicolor","virginica"),col=c("black","red","blue"),lwd=2)

bound12 = (int1-int2)/(slope2-slope1); bound12; abline(v=bound12,lty=2)
bound23 = (int2-int3)/(slope3-slope2); bound23; abline(v=bound23,lty=2)
#so classify 1="setosa" for Petal.Length < 2.861; 2="versicolor" for Petal.Length from 2.861 to 4.906;
#  and 3="virginica" for Petal.Length > 2.906












