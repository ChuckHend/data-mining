#make data frame from original data set
data(trees)
#help(trees)

origfit = lm(Volume ~ .,data=trees)
summary(origfit)

names(trees)
Trees = trees[,c(3,1,2)]  #define a new dataframe with the variables we want, response at the beginning
names(Trees)
GirthHeight=trees$Girth*trees$Height
Girth2=trees$Girth^2
Girth2Height=trees$Girth^2*trees$Height
Trees[,"GirthHeight"] = GirthHeight
Trees[,"Girth2"] = Girth2
Trees[,"Girth2Height"] = Girth2Height

#labels
x = model.matrix(Volume~.,data=Trees)[,-1]
y = Trees[,1]
n = dim(x)[1]
p = dim(x)[2]


#fit multiple regression using all predictors
REGfit = lm(y~x)  
summary(REGfit)  

#fit penalized regression using all predictors
library(glmnet)  # may need to download package glmnet
lambdalist = exp((1200:-1200)/100)  # order large to small
  #fit ridge regression - need alpha = 0
RRfit = glmnet(x, y, alpha = 0,lambda=lambdalist)
coef(RRfit,s=0.1)
plot(RRfit,xvar="lambda",xlim=c(-12,12)); abline(v=log(0.1))
  #fit LASSO - need alpha =1
LASSOfit = glmnet(x, y, alpha = 1,lambda=lambdalist)
coef(LASSOfit,s=0.5)
plot(LASSOfit,xvar="lambda"); abline(v=log(0.5))
  #fit ENET
ENETfit = glmnet(x, y, alpha = 0.75,lambda=lambdalist)
coef(ENETfit,s=0.4)
plot(ENETfit,xvar="lambda"); abline(v=log(0.4))

cbind(coef(RRfit,s=.1)[,1],coef(LASSOfit,s=.5)[,1],coef(ENETfit,s=.4)[,1])
RRyhat = predict(RRfit,newx=x,s=.1); LASSOyhat = predict(LASSOfit,newx=x,s=.5); ENETyhat = predict(ENETfit,newx=x,s=.4)
round(cbind(RRyhat,LASSOyhat,ENETyhat,y),2)

