# Homework Lesson 8
# SVMs

# Problem 1: Support Vector Classification
# 1
# load data
oak <- read.csv('data-mining/data/Oak_log.csv')
str(oak)

#Make a scatterplot that shows 
# the region, standardized log(acorn size),
#and standardized log(range) all in the same plot.  
# Include a legend. 
oak$l
require(ggplot2)
b <- svc.mod$rho
w <- colSums(svc.mod$coefs[ ,1] * svc.mod$SV) #beta_1, ..., beta_p

ggplot(oak, aes(x=scale(logSize), 
                y=scale(logRange),
                col=Region)) +
  geom_point() +
  geom_abline(intercept = b / w[2], slope=-w[1] / w[2]) + 
  geom_abline(intercept = (b + 1) / w[2], slope = -w[1] / w[2], linetype=2) +
  geom_abline(intercept = (b - 1) / w[2], slope = -w[1] / w[2], linetype=2) +
  scale_color_brewer(palette = 'Set1')
ggsave('hyperplane.png')


# use a support vector classifier to fit this data
require(e1071)
svc.mod <- svm(Region ~ logSize + logRange, 
               data = oak, 
               cost=1,
               type='C-classification',
               kernel='linear')
summary(svc.mod)

# 4
table(svc.mod$fitted, oak$Region)

# make hyperplane plot
colors <- ifelse(oak$Region=='Atlantic', 'black', 'red')
b <- svc.mod$rho
w <- colSums(svc.mod$coefs[ ,1] * svc.mod$SV) #beta_1, ..., beta_p



jpeg('hyperplane.jpeg')
plot(scale(oak$logSize), scale(oak$logRange), 
     col=colors,
     xlab='log(size)',
     ylab='log(range)',
     pch=16)
abline(b / w[2], -w[1] / w[2])
abline((b + 1) / w[2], -w[1] / w[2], lty=2)
abline((b - 1) / w[2], -w[1] / w[2], lty=2)
legend('bottomright', 
       legend=c('Atlantic', 'California'), 
       col=c('black', 'red'),
       pch=16)
dev.off()


# Problem 2: Using Support Vector Machines for Classification
# load data
require(ISLR)
data(Auto)
Auto$high.mpg <- as.factor(ifelse(Auto$mpg > median(Auto$mpg), 1, 0))
Auto$origin <- as.factor(Auto$origin)

require(e1071)
# drop the continuous mpg form dataset
Auto$mpg <- NULL
# also drop car's name
Auto$name <- NULL
svm.lin <- svm(high.mpg ~ ., data = Auto,
    cost = 1,
    type = 'C-classification',
    kernel = 'linear')

# 10
# test parameters in svc
set.seed(9)
svm.lin <- tune(high.mpg ~ ., 
                method = svm,
                data = Auto,
                ranges = list( cost = c(0.001, 0.01, .1, 1, 5, 10, 100)),
                type = 'C-classification',
                kernel = 'linear')
summary(svm.lin)

# 12
set.seed(9)
svm.rad <- tune(high.mpg ~ ., 
                method = svm,
                data = Auto,
                ranges = list( cost = c(0.001, 0.01, .1, 1, 5, 10, 100),
                               gamma = c(0.5, 1, 2, 3, 4)),
                type = 'C-classification',
                kernel = 'radial')
summary(svm.rad)
best.mod <- svm.rad$best.model
table(best.mod$fitted, Auto$high.mpg)
# predict on new data
new.car <- data.frame(cylinders=4, 
                      displacement=132.5, 
                      horsepower=155, 
                      weight=2910, 
                      acceleration=8.3, 
                      year=77,
                      origin=factor(1, levels=c(1,2,3)))
predict(best.mod, new.car)
