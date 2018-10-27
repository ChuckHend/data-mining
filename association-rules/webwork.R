# association rules

# apriori methods

d <- read.csv('Academics/Data Science/DS740_Data_Mining/data/AmesSimple.csv')
str(d)
summary(d)


# apriori needs all variables to be binary
d$Lot.Shape.bin <- ifelse(d$Lot.Shape=='Reg', 1, 0)

# discretize variables
require(arules)
d$Bedroom.AbvGr.disc <- discretize(d$Bedroom.AbvGr, breaks=2, ordered=T, method = 'interval')

summary(d$Bedroom.AbvGr.disc)

d$Full.Bath.disc <- discretize(d$Full.Bath, breaks = 2, ordered=T, method = 'interval')


d$Gr.Liv.Area.disc <- discretize(d$Gr.Liv.Area, breaks = 3, method = 'interval')
summary(d$Gr.Liv.Area.disc)

hist(d$Gr.Liv.Area)

# there are only 5 houses in the largest category. this will not help much
# instead, try:
d$Gr.Liv.Area.disc <- discretize(d$Gr.Liv.Area, method= "fixed", breaks=c(334, 1126, 1743, 5642), ordered=T)
summary(d$Gr.Liv.Area.disc)



hist(d$SalePrice)

# do same to sale price, with fixed boundaries, quartiles (0, 1st, 3rd, max)
sale.summ <- summary(d$SalePrice)
d$SalePrice.disc <- discretize(d$SalePrice, 
                               method="fixed", 
                               breaks = c(0,129500,
                                          213500,755000), 
                               ordered=T)
summary(d$SalePrice.disc)
# drop a couple per instruction in example
d$Full.Bath.disc <- NULL
d$Bedroom.AbvGr.disc <- NULL
d$Gr.Liv.Area <- NULL # using .disc
d$Lot.Shape <- NULL # using .disc
d$SalePrice <- NULL # using .disc

d[,colnames(d)] <- lapply(d[,colnames(d)], as.factor)

ames.trans = as(d, "transactions")

rules <- apriori(ames.trans, parameter = list(support = 0.05, confidence = 0.5))
summary(rules)


# inspect the rules with highest lift
subrules <- head(rules, n = 5, by = 'lift')
inspect(subrules)

inspect(head(rules, n = 30, by = 'lift'))
# 
rules2 = apriori(ames.trans, parameter = list(support = .05, confidence = 0.5), 
                 appearance = list(rhs = c("SalePrice.disc=[2.14e+05,7.55e+05]"), default = "lhs") )
summary(rules2)


# filter out redundant rules ####
non.redundant <- which(interestMeasure(rules2,
                                measure = 'improvement',
                                quality_measure = 'confidence') >= 0)
rules3 <- rules2[non.redundant]
summary( rules3 )

# or will return indices of the rules that are redundant
sum(!is.redundant(rules2))

# subset of subset of rules
rules4 = subset( rules3, subset = lhs %in% c("Bedroom.AbvGr=3", "Bedroom.AbvGr=4") )
inspect(head(rules4, n = 10, by = 'lift')) # take a look at the ones that are most associated with high sale price

# 
highLift = subset(rules3, subset = lift > 3.5 & confidence > .95)


# filter results with a single antecedent
mylhs = lhs(rules3)
singleAnt = which( size(mylhs) == 1 )
inspect( rules3[singleAnt] )


# What characteristics, combined with having a regular lot shape, are predictive of a high sale price?
# In other words, look at rules with “Lot.Shape.bin=Reg” on the left-hand side. 
RegLot = which(mylhs %in% c("Lot.Shape.bin=1")) # remember, we changed this to binary
inspect( rules3[ RegLot ] )


mylhs.mat = as(mylhs, Class = "matrix")

hist( colSums(mylhs.mat) )
