cancer = read.csv("WI_breastcancer_data.csv")
attach(cancer)

# measurements only (as if we do not have diagnosis)
x = cancer[,-c(1,2)]
xsubset = cancer[,c(4,26,27)]

# scaling visibly required to allow all predictors to contribute to similarity measure
par(mfrow=c(1,3))
plot(Texture.mean, Area.extreme, main="")
plot(Texture.mean, Smoothness.extreme, main="")
plot(Area.extreme, Smoothness.extreme, main="")

x.scale = scale(x)
xsubset.scale = scale(xsubset)












############ fitting non-hierarchical models - K-means with p = 3 variables ############
nclust=2
colused = c("turquoise3", "red", "black", "orange","blue", "slateblue",  "purple","green", "violetred" )

set.seed(12)


#repeat the following a few times
memb = kmeans(xsubset.scale,nclust)$cluster
#plotting clusters
colused = c("turquoise3", "red", "black", "orange","blue", "slateblue",  "purple","green", "violetred" )
par(mfrow=c(1,3))
plot(Texture.mean, Area.extreme ,pch=16,main=paste(nclust," clusters determined by K-means"))
for (i in 1:9)  points(Texture.mean[memb == i],Area.extreme[memb == i],pch=16,col=colused[i])
plot(Texture.mean, Smoothness.extreme ,pch=16,main=paste(nclust," clusters determined by K-means"))
for (i in 1:9)  points(Texture.mean[memb == i],Smoothness.extreme[memb == i],pch=16,col=colused[i])
plot(Area.extreme, Smoothness.extreme ,pch=16,main=paste(nclust," clusters determined by K-means"))
for (i in 1:9)  points(Area.extreme[memb == i],Smoothness.extreme[memb == i],pch=16,col=colused[i])








############ fitting non-hierarchical models - K-means with p = 30 variables ############
nclust=2
colused = c("turquoise3", "red", "black", "orange","blue", "slateblue",  "purple","green", "violetred" )

set.seed(12)


#repeat the following a few times
membfull = kmeans(x.scale,nclust)$cluster
#plotting clusters
colused = c("turquoise3", "red", "black", "orange","blue", "slateblue",  "purple","green", "violetred" )
par(mfrow=c(1,3))
plot(Texture.mean, Area.extreme ,pch=16,main=paste(nclust," clusters joined by average linkage"))
for (i in 1:9)  points(Texture.mean[membfull == i],Area.extreme[membfull == i],pch=16,col=colused[i])
plot(Texture.mean, Smoothness.extreme ,pch=16,main=paste(nclust," clusters joined by average linkage"))
for (i in 1:9)  points(Texture.mean[membfull == i],Smoothness.extreme[membfull == i],pch=16,col=colused[i])
plot(Area.extreme, Smoothness.extreme ,pch=16,main=paste(nclust," clusters joined by average linkage"))
for (i in 1:9)  points(Area.extreme[membfull == i],Smoothness.extreme[membfull == i],pch=16,col=colused[i])

# nonhierarchical selection
nclust=2
set.seed(12)
membfull = kmeans(x.scale,nclust)$cluster
finalnonhierarchicalclusters = membfull





#comparing results

table(finalhierarchicalclusters,finalnonhierarchicalclusters)


# true diagnoses
table(finalhierarchicalclusters,Diagnosis)

table(finalnonhierarchicalclusters,Diagnosis)













