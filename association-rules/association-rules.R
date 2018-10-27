# association rules

require(arules)

data("Groceries")
str(Groceries)
inspect(Groceries)
summary(Groceries)

# 169 products

# 
itemFrequencyPlot(Groceries, support = 0.05)

#5 #5
rules <- apriori(Groceries, 
                 parameter=list(support=0.001, confidence = 0.5))
summary(rules)

inspect(tail(rules))


# rules with 10 highest ift
inspect(head(rules, n = 10, by = 'lift'))


# Return to working with the original set of rules from the fifth question in this problem.  
# Filter out any rules that have lower confidence than more general versions of the same rules. 
# How many rules remain?
non.redundant <- which(interestMeasure(rules,
                                       measure = 'improvement',
                                       quality_measure = 'confidence') >= 0)
rules.nonred <- rules[non.redundant]
rules.nonred

# Suppose that you work for a baking company, and you want to offer a coupon to customers who are likely to buy pastry.  
# Using your filtered rules from the previous question, identify combination(s) of items that are 
# associated with an increased probability of buying “pastry”.  To whom would you offer your coupon?
rules.pastry = subset( rules, subset = rhs %in% c("pastry") )
inspect(rules.pastry)



# Problem 2: Modeling with Association Rules ####
raw <- read.csv('data-mining/data/HeartDisease.csv')

hasCP <- ifelse(raw$ChestPain != 4, 1, 0)

Age.disc <- discretize(raw$Age, breaks=3, method='interval',ordered=T)

bp.summary <- summary(raw$BloodPressure)
BP.disc <- discretize(raw$BloodPressure, 
                      breaks=c(bp.summary['Min.'], 
                               bp.summary['1st Qu.'],
                               bp.summary['3rd Qu.'],
                               bp.summary['Max.']),
                      method='fixed', 
                      ordered=T)

# create list of the variables we want to exclude
exclude <- c('ChestPain', 'Age', 'BloodPressure')
# use that list to create list that we want to include
cols <- colnames(raw)[!colnames(raw) %in% exclude]
# select the columns to keep from the new df, and add our transformed variables as columns
heart.cln <- cbind(raw[,cols], BP.disc, Age.disc, hasCP)
# convert entire df to factors
heart.cln <- data.frame(lapply(heart.cln, as.factor))
heart.trans <- as(heart.cln, "transactions")
heart.trans


rules = apriori(heart.trans, parameter = list(support = .03, confidence = 0.5), 
                 appearance = list(rhs = c("hasHD=1"), default = "lhs") )
rules

# find rules where antecedent is female
rules.f = subset( rules, subset = lhs %in% c("Sex=0") )

inspect(head(rules.f, n=10, by='lift'))

table(raw$Sex)
names(heart.cln)
