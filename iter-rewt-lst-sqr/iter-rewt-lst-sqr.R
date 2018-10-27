# Iteratively Reweighted Least Squares

require(mosaic)

library(smss)
data(crime2005)

mod.lm <- lm(VI2 ~ ME + PO, data = crime2005)

# check for outliers
plot(mod.lm)

crime2005$STATE[c(8,40, 51)]
crime2005[c(8,40, 51),]
#Delaware, South Carolina, Washington D.C.

# IRWLS with Huber weights ####
library(MASS)
# fit base model
fit.w <- lm(VI2 ~ ME + PO, data = crime2005)

# setup placeholder var and extract old coefficients
oldcoef <- rep(0, length(fit.w$coef))
newcoef <- fit.w$coef
iter <- 0
# iterate through model fit with new weights
while(sum(abs(oldcoef - newcoef)) > .0001 & iter < 100){
  # new weights
  w <- 1/(fit.w$fitted.values^2) 
  fit.w <- lm(VI2 ~ ME + PO, 
              data = crime2005, 
              weights = w)  # fit using new weights
  
  iter <- iter + 1
  oldcoef <- newcoef
  newcoef <- fit.w$coef
}


# Tukey's Method ####

library(MASS)
mod.tukey <- rlm(VI2 ~ ME + PO, 
                 data=crime2005, 
                 psi = psi.bisquare)

require(car) # for showLabels
labs <- which(mod.tukey$w < 0.8)

plot(1:length(mod.tukey$w), mod.tukey$w, xlab = 'Index',ylab='weight')
showLabels(1:length(mod.tukey$w), mod.tukey$w, crime2005[,'STATE'], labs)

# Dealing Correlated Errors in Least Squre Model ####

elnino <- read.csv('../data/elnino.csv')
# remove records with missing data
elnino <- elnino[complete.cases(elnino),]
# fit unweighted model
mod.uw <- lm(air.temp ~ zon.winds + mer.winds + humidity + s.s.temp,
             data = elnino)
# plot residuals vs. fitted values
plot(mod.uw$fitted.values, mod.uw$residuals)
# plot qqnorm
qqnorm(mod.uw$residuals)


jpeg('appropriateness-of-linear-model.jpg')
par(mfrow=c(2, 1))
plot(mod.uw, which=c(1, 2))
dev.off()

# OLS the residuals are generally random around the horizontal line
# generally linear in QQ, so OLS is a good option

# use gls() to fit model with uncorrelated errors
library(nlme)
mod.gls <- gls(air.temp ~ zon.winds + mer.winds + humidity + s.s.temp,
               data = elnino, correlation = corCompSymm(form = ~1 | buoy))

mod.uw$coefficients
mod.gls$coefficients

summary(mod.gls)

res.buoy3 <- mod.gls$residuals[elnino$buoy==3]

plot(res.buoy3[-length(res.buoy3)], 
     res.buoy3[-1],
     xlab=paste('Residuals 1 - ',length(res.buoy3)-1),
     ylab=paste('Residuals 2 - ',length(res.buoy3)))


mod.ac <- gls(air.temp ~ zon.winds + mer.winds + humidity + s.s.temp,
              data = elnino, correlation = corAR1(form = ~1 | buoy))
summary(mod.gls)
summary(mod.ac)

mod.gls$coefficients/mod.ac$coefficients