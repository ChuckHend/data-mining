# iteratively reweighted least squares
# use IRLS when the variance of each error term ϵi is exactly equal to

# Robust Regression
# Tukey Bisquare places lower weights on outliers

# use weighted regression when some data is more reliable than others
pmin( c( 7, 3, 8 ), c(3, 9, 8) ) 


pmax( c( 1, -5, 2 ), 0 ) 
1 - (4/3)

r <- -6:6
Tukey = (pmax(1-(r/4.685)^2,0))^2
plot(Tukey)


fat <- read.csv('data-mining/data/bodyfat.csv')
require(MASS)
fit <- rlm(BodyFatBrozek ~ Weight,data=fat, psi=psi.bisquare)
fit


fit.w <- lm(BodyFatBrozek ~ Weight,data=fat)

iter = 0
newcoef = fit.w$coef
oldcoef = rep(0, length(fit.w$coef))



while(sum(abs(oldcoef-newcoef)) > .0001 & iter < 100){
  w = pmax(1-abs(fit.w$residuals)/3, 0)
  fit.w = lm(fat$BodyFatBrozek~fat$Weight, weights=w)
  
  
  iter = iter + 1
  oldcoef = newcoef
  newcoef = fit.w$coef 
}

plot(x=1:length(w), y=w)



n = 100
Sigmax = matrix(,nr=n, nc=n) # initialize the covariance matrix
Sigmax[1,1] = 1
for(i in 2:n){
  Sigmax[i,i] = 1
  for(j in 1:(i-1)){
    Sigmax[i, j] = .9^abs(i-j)
    Sigmax[j, i] = Sigmax[i, j] # make the covariance matrix 
    # symmetric across the diagonal
  } #end iter over j
} # end iter over i

View(Sigmax)

require(MASS)
set.seed(15)
x = runif(n, 0, 1)
# generate 1 random vector 
# of n noise terms
y = 2*x + 3 + mvrnorm(1, rep(0,n), Sigmax) 

require(nlme)
m <- gls(y~x)
summary(m)
plot(m)


cor(m$residuals[1:99], m$residuals[2:100])

acf(m$residuals, ci.type='ma')
plot(m$residuals)


m2 = gls(y ~ x, correlation = corAR1(form = ~1))
summary(m2)
