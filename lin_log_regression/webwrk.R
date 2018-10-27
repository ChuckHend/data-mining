# linear and logistic regression


d <- read.csv('/mnt/ubudata/projects/data-mining/lin_log_regression/BadRegression.csv')

require(pROC)

myroc <- roc(response=d$y, predictor = d$predictvals)
plot(myroc)
myroc$auc

# not doing good job predicting when y=1
boxplot(d$predictvals, x = d$y)

# The boxplot shows that this model is not doing a decent job of predicting y--
#   it’s actually doing worse than assigning probabilities randomly! 
#   This can happen when the training set used to build the regression model is 
# not a good representation of validation set used to build the ROC curve. 
myroc = roc(response=d$y, predictor=d$predictvals, direction='<')
plot(myroc)
myroc$auc


heart <- read.csv('/mnt/ubudata/projects/data-mining/lin_log_regression/Heart_disease_Cleveland.csv')
summary(heart)

# store the numerical categoricals as factors
cats <- c('Sex', 'ChestPain', 'HighBloodSugar', 'ECG', 'ExerAngina', 'Slope', 'Thal')
heart[,cats] <- lapply(heart[,cats], as.factor)

# create variable for if patient has heart disease, 1 if does have heart disease
heart$HD <- ifelse(heart$DiseaseStatus == 0, 0, 1)

# fit logistic regression
fit = glm(HD~., data = heart, family = "binomial")
summary(fit)

# fit logistic regression with STdepress as outcome variable
mod <- glm(STdepress ~ . -HD, data=heart)
summary(mod)
plot(mod)

boxplot(heart$STdepress)
hist(heart$STdepress)

# correct for right-skewness
# adjusting +1 since cannot take log(0)
mod2 = lm(log(STdepress+1) ~ .-HD, data = heart)
plot(mod2)

AIC(mod); AIC(mod2)
