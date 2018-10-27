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

fullfit = lm(Volume ~ .,data=Trees)

summary(fullfit)




#calculating standard error of coefficients
x = model.matrix(Volume~.,data=trees)[,-1]
x1 = model.matrix(Volume~.,data=trees)  # includes column of ones to fit intercept
xfull = model.matrix(Volume~.,data=Trees)[,-1]
x1full = model.matrix(Volume~.,data=Trees) 
y = Trees[,1]

cor(x)
cor(x,y)
s2 = sum(origfit$residuals^2)/(31-2-1)
round((solve(t(x1)%*%x1)),5)
s.e.beta = sqrt(s2*diag(solve(t(x1)%*%x1))); s.e.beta
summary(lm(y~x))$coef

cor(xfull)
cor(xfull,y)
s2full = sum(fullfit$residuals^2)/(31-5-1)
round((solve(t(x1full)%*%x1full)),5)
s.e.betafull = sqrt(s2full*diag(solve(t(x1full)%*%x1full))); s.e.betafull
summary(lm(y~xfull))$coef




