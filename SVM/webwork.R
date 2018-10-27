# webwork SVMs

# data


d <- read.csv('data-mining/data/bank-additional.csv', sep=';')

require(ggplot2)
ggplot(d, aes(x=scale(emp.var.rate), y=scale(duration), col=y)) +
  geom_point()
require(e1071)

svm.mod <- svm(y ~ scale(duration) + scale(emp.var.rate), data=d, cost = 1, kernel="linear",type = "C-classification")
summary(svm.mod)

my.d <- d[,c('y','duration','emp.var.rate')]


# new plot
colors <- ifelse(d$y=='yes', 'green','red')
plot(scale(my.d$duration) ~ scale(my.d$emp.var.rate), col = colors)
b <- svm.mod$rho
w <- colSums(svm.mod$coefs[,1] * svm.mod$SV) 

# draw lines
abline(b/w[2], -w[1]/w[2]) # y-int and slope of svm mod

# margins
abline((b+1)/w[2], -w[1]/w[2],lty=2)
abline((b-1)/w[2], -w[1]/w[2],lty=2)

# highlight support vectors
points(svm.mod$SV, cex=2)

# analyze results
table(svm.mod$fitted, my.d$y)

table(my.d$y)
16/(3668)

417/(451)

library(e1071)
colors <- ifelse(my.d$y=='yes', 'green','red')
plot(scale(my.d$duration) ~ scale(my.d$emp.var.rate), col = colors)
svmfit = svm(y~emp.var.rate+duration, data = my.d, kernel = 'linear', cost = 1)
w = colSums(svmfit$coefs[,1] * svmfit$SV)
b = svmfit$rho
# Optimal line
abline(b/w[2],-w[1]/w[2]) # y-int, slope
# Margin lines
abline((b-1)/w[2],-w[1]/w[2], lty=2) # y-int, slope; 
# as cost decreases, w[2] tends to decrease, resulting in a larger margin
abline((b+1)/w[2],-w[1]/w[2], lty=2) 




# cv to test several parameters
set.seed(999)
tune.out <- tune(svm, y ~ duration + emp.var.rate,
                 data = my.d,
                 kernel='radial',
                 type='C-classification',
                 ranges= list( cost = c(0.001, 0.01, .1, 1, 5, 10, 100),
                               gamma = c(0.5, 1, 2, 3, 4)))

summary(tune.out)

# extract best model
best.model <- tune.out$best.model
summary(best.model)


# predict on new data
newClient <- data.frame(duration=250,emp.var.rate=1)
predict(best.model, newdata = newClient)

# plot the decision boundaries
xgrid <- 
# 