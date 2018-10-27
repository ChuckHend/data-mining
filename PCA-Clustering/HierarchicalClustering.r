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












############ fitting hierarchical models - complete  with p = 3 variables ############
dist.xsubset.scale = dist(xsubset.scale, method="euclidean")
hc.complete = hclust(dist.xsubset.scale,method="complete")
par(mfrow=c(1,1))
plot(hc.complete,cex=0.5,labels=F)
abline(h=c(4.5, 6, 7.5),col=c("green","purple","red","blue"),lty=2)  # selecting 6, 4, and 2 clusters respectively

#plotting clusters
colused = c("turquoise3", "red", "black", "orange","blue", "slateblue",  "purple","green", "violetred" )
par(mfrow=c(1,3))
# practically, 2 clusters makes most sense 
# but 4 clusters might be considered levels of malignancy "risk"
nclust=2  
memb = cutree(hc.complete,k=nclust)
par(mfrow=c(1,3))
plot(Texture.mean, Area.extreme ,pch=16,main=paste(nclust," clusters joined by complete linkage"))
for (i in 1:9)  points(Texture.mean[memb == i],Area.extreme[memb == i],pch=16,col=colused[i])
plot(Texture.mean, Smoothness.extreme ,pch=16,main=paste(nclust," clusters joined by complete linkage"))
for (i in 1:9)  points(Texture.mean[memb == i],Smoothness.extreme[memb == i],pch=16,col=colused[i])
plot(Area.extreme, Smoothness.extreme ,pch=16,main=paste(nclust," clusters joined by complete linkage"))
for (i in 1:9)  points(Area.extreme[memb == i],Smoothness.extreme[memb == i],pch=16,col=colused[i])





############ fitting hierarchical models - single  with p = 3 variables ############
dist.xsubset.scale = dist(xsubset.scale, method="euclidean")
hc.single = hclust(dist.xsubset.scale,method="single")
par(mfrow=c(1,1))
plot(hc.single,cex=0.5,labels=F)  
abline(h=c(1.25,1.5,2),col=c("green","purple","red","blue"),lty=2)  # selecting 7, 4, and 2 clusters respectively

#plotting clusters
colused = c("turquoise3", "red", "black", "orange","blue", "slateblue",  "purple","green", "violetred" )
par(mfrow=c(1,3))
#"string" pattern does not appear to do well here
nclust=5
memb = cutree(hc.single,k=nclust)
par(mfrow=c(1,3))
plot(Texture.mean, Area.extreme ,pch=16,main=paste(nclust," clusters joined by single linkage"))
for (i in 1:9)  points(Texture.mean[memb == i],Area.extreme[memb == i],pch=16,col=colused[i])
plot(Texture.mean, Smoothness.extreme ,pch=16,main=paste(nclust," clusters joined by single linkage"))
for (i in 1:9)  points(Texture.mean[memb == i],Smoothness.extreme[memb == i],pch=16,col=colused[i])
plot(Area.extreme, Smoothness.extreme ,pch=16,main=paste(nclust," clusters joined by single linkage"))
for (i in 1:9)  points(Area.extreme[memb == i],Smoothness.extreme[memb == i],pch=16,col=colused[i])






############ fitting hierarchical models - average with p = 3 variables ############
dist.xsubset.scale = dist(xsubset.scale, method="euclidean")
hc.average = hclust(dist.xsubset.scale,method="average")
par(mfrow=c(1,1))
plot(hc.average,cex=0.5,labels=F)  
abline(h=c(2.8, 3.8, 4.2),col=c("green","purple","red","blue"),lty=2)  # selecting 8, 4, and 3 clusters respectively

#plotting clusters
colused = c("turquoise3", "red", "black", "orange","blue", "slateblue",  "purple","green", "violetred" )
par(mfrow=c(1,3))
# practically, 2 clusters makes most sense 
# but 4 clusters might be considered levels of malignancy "risk"
nclust=4
memb = cutree(hc.average,k=nclust)
par(mfrow=c(1,3))
plot(Texture.mean, Area.extreme ,pch=16,main=paste(nclust," clusters joined by average linkage"))
for (i in 1:9)  points(Texture.mean[memb == i],Area.extreme[memb == i],pch=16,col=colused[i])
#points(Texture.mean[Diagnosis == "M"], Area.extreme[Diagnosis == "M"], pch=8)
plot(Texture.mean, Smoothness.extreme ,pch=16,main=paste(nclust," clusters joined by average linkage"))
for (i in 1:9)  points(Texture.mean[memb == i],Smoothness.extreme[memb == i],pch=16,col=colused[i])
#points(Texture.mean[Diagnosis == "M"], Smoothness.extreme[Diagnosis == "M"], pch=8)
plot(Area.extreme, Smoothness.extreme ,pch=16,main=paste(nclust," clusters joined by average linkage"))
for (i in 1:9)  points(Area.extreme[memb == i],Smoothness.extreme[memb == i],pch=16,col=colused[i])
#points(Area.extreme[Diagnosis == "M"], Smoothness.extreme[Diagnosis == "M"], pch=8)


############ fitting hierarchical models - complete with p = 30 variables, euclidean distance ############
dist.x.scale = dist(x.scale, method="euclidean")  
hc.completefull = hclust(dist.x.scale,method="complete")
par(mfrow=c(1,1))
plot(hc.completefull,cex=0.5,labels=F)  
abline(h=c(16,19),col=c("green","purple","red","blue"),lty=2)  
  # selecting 7 and 4 clusters, for euclidean

#plotting clusters
colused = c("turquoise3", "red", "black", "orange","blue", "slateblue",  "purple","green", "violetred" )
par(mfrow=c(1,3))
nclust=4
membfull = cutree(hc.completefull,k=nclust)
par(mfrow=c(1,3))
plot(Texture.mean, Area.extreme ,pch=16,main=paste(nclust," clusters joined by complete linkage"))
for (i in 1:9)  points(Texture.mean[membfull == i],Area.extreme[membfull == i],pch=16,col=colused[i])
plot(Texture.mean, Smoothness.extreme ,pch=16,main=paste(nclust," clusters joined by complete linkage"))
for (i in 1:9)  points(Texture.mean[membfull == i],Smoothness.extreme[membfull == i],pch=16,col=colused[i])
plot(Area.extreme, Smoothness.extreme ,pch=16,main=paste(nclust," clusters joined by complete linkage"))
for (i in 1:9)  points(Area.extreme[membfull == i],Smoothness.extreme[membfull == i],pch=16,col=colused[i])






############ fitting hierarchical models - complete with p = 30 variables, manhattan distance ############
dist.x.scale = dist(x.scale, method="manhattan")  # try manhattan distance with so many predictors
hc.completefull = hclust(dist.x.scale,method="complete")
par(mfrow=c(1,1))
plot(hc.completefull,cex=0.5,labels=F)  
abline(h=c(85,110),col=c("green","purple","red","blue"),lty=2)  
  # selecting 4 and 2 clusters, for manhattan

#plotting clusters
colused = c("turquoise3", "red", "black", "orange","blue", "slateblue",  "purple","green", "violetred" )
par(mfrow=c(1,3))
nclust=2
membfull = cutree(hc.completefull,k=nclust)
par(mfrow=c(1,3))
plot(Texture.mean, Area.extreme ,pch=16,main=paste(nclust," clusters joined by complete linkage"))
for (i in 1:9)  points(Texture.mean[membfull == i],Area.extreme[membfull == i],pch=16,col=colused[i])
plot(Texture.mean, Smoothness.extreme ,pch=16,main=paste(nclust," clusters joined by complete linkage"))
for (i in 1:9)  points(Texture.mean[membfull == i],Smoothness.extreme[membfull == i],pch=16,col=colused[i])
plot(Area.extreme, Smoothness.extreme ,pch=16,main=paste(nclust," clusters joined by complete linkage"))
for (i in 1:9)  points(Area.extreme[membfull == i],Smoothness.extreme[membfull == i],pch=16,col=colused[i])

# hierarchical selection
nclust=2
membfull = cutree(hc.completefull,k=nclust)
finalhierarchicalclusters = membfull







