set.seed(15)
clust = sample(1:2,6, replace=T)

clust


d<-data.frame(x=c(2,1,0,5,6,4),
              y=c(4,5,4,2,2,1),
              c=c(2,1,2,2,1,2))

c1 <- colMeans(d[d$c==1,c('x','y')])
c2 <- colMeans(d[d$c==2,c('x','y')])

d$c <- c(2,2,2,1,1,2)



d <- data.frame(x1=c(-1,-2,-3,2,3,1),
                x2=c(1,2,1,-1,-1,-2),
                x3=c(-2,5,4,-1,-3,-3))

pc.info <- prcomp(d)
summary(pc.info)
vjs <- pc.info$sdev**2
pve = vjs/sum(vjs)
cumsum(pve)

plot(cumsum(pve), type = "o", ylab="Cumulative PVE", xlab="Principal Comp")

biplot(pc.info,scale=0)
pc.info$rotation[,1]  # loadings for first principal component
pc.info$rotation[,2]  # loadings for second principal component
pc1scores = pc.info$x[,1]  # first principal component score vector
pc2scores = pc.info$x[,2]  # second principal component score vector

# get coordinates of point #1, from the first principal components
pc1scores[1]
# get coordinates of point #1, from the second principal components
pc2scores[1]


library(MASS)
x = UScereal[1:7,c(2:10)]
x.scale = scale(x)

dim(x)
# b. Cluster these seven observations using complete-linkage (with Euclidean distance) hierarchical clustering. 
# Using this to select two clusters, which observations are in those two clusters? 
dist.x.scale = dist(x.scale, method="euclidean")
hc.complete = hclust(dist.x.scale,method="complete")
abline(h=c(4.5, 6, 7.5),col=c("green","purple","red","blue"),lty=2)  # selecting 6, 4, and 2 clusters respectively

membfull = cutree(hc.complete,k=nclust)
plot(membfull)




# doing kmeans
set.seed(12)
membfull = kmeans(x.scale,2)$cluster
membfull

x.scale.pca <- prcomp(x.scale,center=T,scale=T)
summary(x.scale.pca)
.552 + 0.2265 + 0.1311
