
# Problem 1 uses clustering methods on wine data.
# Problem 2 uses PCA methods to identify meaningful variables in the wine data set.
# Problem 3 explores genetics application via clustering and PCA methods.

require(rattle)
x <- read.csv('Academics/Data Science/DS740_Data_Mining/data/wine.csv')
str(x)
dim(x)
summary(x)
means <- colMeans(x)
sds <- apply(x, MARGIN = 2, FUN = sd)
# visualize the means and standard deviations
plot(means)
plot(sds)




# 2 # using euclidean distance, fit hierarchical model using complete linkage
x.scale <- scale(x)
dist.x.scale = dist(x.scale, method="euclidean")
complete = hclust(dist.x.scale,method="complete")
plot(complete,cex=0.5)
abline(h = 9, lty = 2, lwd=2, col = 'firebrick')

# 3 List an appropriate  height (corresponding to the value of the distance measure) 
# on the dendrogram for complete linkage that would produce three clusters.
# height of 9, by visual inspection
nclust=3

# Using Euclidean distance, fit the hierarchical model using each of single linkage and average linkage, as well as
# complete linkage.  Which of the three linkage methods appears to produce the most similarly-sized clusters?
single <- hclust(dist.x.scale,method="single")
average <- hclust(dist.x.scale,method="average")
plot(single,cex=0.5)
plot(average,cex=0.5)
# complete seems to provide most similarly sized

# 6
nclust=3  
cutree(average,k=nclust)
cutree(single,k=nclust)
cutree(complete,k=nclust)

# 7
cols <- cutree(complete,k=nclust)
plot(Alcohol ~ Dilution, data = x, col=cols, pch=16, main = 'Clusters of Wine Types')


# 8
nclusts <- c(2, 3, 4, 5)
par(mfrow=c(1,length(nclusts)))
par(mar=c(1,1,1,1))
iters <- 20
n <- dim(x.scale)[1]
k2 <- matrix(nrow = iters, ncol = n)
k3 <- matrix(nrow = iters, ncol = n)
k4 <- matrix(nrow = iters, ncol = n)
k5 <- matrix(nrow = iters, ncol = n)
results <- list(k2, k3, k4, k5)
for(i in 1:iters){
  for(j in 1:length(nclusts)){
    k <- nclusts[j]
    memb <- kmeans(x.scale, centers = k)
    plot(Alcohol ~ Dilution, data = x.scale, 
         pch=16,
         main=paste('K=',k),
         col=memb$cluster)
    results[[j]][i,] <- memb$cluster
  }
  Sys.sleep(3)
}
# iterate through results
# calc the SD of each obseration
# if SD != 0, then there is a membership change
for(i in 1:length(nclusts)){
  k <- nclusts[i]
  sds <- apply(results[[i]], 2, sd)
  num.sd.delta <- sum(sds != 0)
  perc <- num.sd.delta / n
  print(paste(k,':',perc))
  }

# 12
set.seed(12)
km <- kmeans(x.scale, 3)
km.memb <- km$cluster
table(km.memb)

complete.memb <- cutree(complete, k=3)
sum(diag(table(km.memb, complete.memb))) / sum(table(km.memb, complete.memb))


# Problem 2: PCA
dev.off()
x <- read.csv('Academics/Data Science/DS740_Data_Mining/data/wine.csv')
pca.info.x <- prcomp(x, scale = T)
pca.info.x$rotation

biplot(pca.info.x)

# percent variance explained by first prin comp?
summary(pca.info.x)
vjs <- pca.info.x$sdev**2
pve = vjs/sum(vjs)
cumsum(pve)

# how many components do we need to reach 80% of explained variance?
# 5
cumsum(pve)

# what are the coordinates for outlier, sample #159?
pca.info.x$x[,1][159]
pca.info.x$x[,2][159]


# Problem 3: Gene Expression Application
d <- read.csv('Academics/Data Science/DS740_Data_Mining/data/GeneExpression.csv',header=F)
# transpose
d.t <- t(d)
# scale
d.t.scale <- scale(d.t)

means1 <- apply(d.t[1:20, ], 2, mean)
means2 <- apply(d.t[21:40, ], 2, mean)
hist(means2)

# 21
dist.d <- dist(d.t.scale, method="euclidean")
complete <- hclust(dist.d,method="complete")
hc.memb <-cutree(complete, k=2)

# 22 How many tissue samples from 21-40 are in the second cluster?
sum(hc.memb[21:40]==2)

#23 How many principle components are able to be computed?
pca.d <- prcomp(d.t.scale)

# percent variance explained by first TWO prin comp?
summary(pca.d)
vjs <- pca.d$sdev**2
pve = vjs/sum(vjs)
cumsum(pve)

# biplot
biplot(pca.d, main='Biplot: GeneExpression prcomp')

plot(means1, pca.d$rotation[,1])
