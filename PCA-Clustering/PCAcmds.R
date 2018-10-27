cancer = read.csv("WI_breastcancer_data.csv")
attach(cancer)

# measurements only (as if we do not have diagnosis)
x = cancer[,-c(1,2)]

# scaling visibly required to allow all predictors to contribute to similarity measure
par(mfrow=c(1,3))
plot(Texture.mean, Area.extreme, main="")
plot(Texture.mean, Smoothness.extreme, main="")
plot(Area.extreme, Smoothness.extreme, main="")

# include scaling due to widely varied magnitudes
pc.info = prcomp(x,center=T,scale=T)
pc.info$rotation  #loadings

par(mfrow=c(1,1))
plot(pc.info)
summary(pc.info)
pc.info$sdev
vjs = pc.info$sdev^2
pve = vjs/sum(vjs)
cumsum(pve)

plot(cumsum(pve), type = "o", ylab="Cumulative PVE", xlab="Principal Component")

biplot(pc.info,scale=0)
pc.info$rotation[,1]  # loadings for first principal component
pc.info$rotation[,2]  # loadings for second principal component
pc1scores = pc.info$x[,1]  # first principal component score vector
pc2scores = pc.info$x[,2]  # second principal component score vector


# mean measurements only 
xmeans = cancer[,c(3:12)]

# include scaling due to widely varied magnitudes
pc.info.mean = prcomp(xmeans,center=T,scale=T)
pc.info.mean$rotation

plot(pc.info.mean)
summary(pc.info.mean)
pc.info.mean$sdev
vjs = pc.info.mean$sdev^2
pve = vjs/sum(vjs)
cumsum(pve)

plot(cumsum(pve), type = "o", ylab="Cumulative PVE", xlab="Principal Coponene")

biplot(pc.info.mean,scale=0)
pc.info.mean$rotation[,1]  # loadings for first principal component
pc.info.mean$rotation[,2]  # loadings for second principal component
pc1scores.mean = pc.info.mean$x[,1]  # first principal component score vector
pc2scores.mean = pc.info.mean$x[,2]  # second principal component score vector











# extreme measurements only 
xextremes = cancer[,c(23:32)]

# include scaling due to widely varied magnitudes
pc.info.extreme = prcomp(xextremes,center=T,scale=T)
pc.info.extreme$rotation

plot(pc.info.extreme)
summary(pc.info.extreme)
pc.info.extreme$sdev
vjs = pc.info.extreme$sdev^2
pve = vjs/sum(vjs)
cumsum(pve)

plot(cumsum(pve), type = "o", ylab="Cumulative PVE", xlab="Principal Coponene")

biplot(pc.info.extreme,scale=0)
pc.info.extreme$rotation[,1]  # loadings for first principal component
pc.info.extreme$rotation[,2]  # loadings for second principal component
pc1scores.extreme = pc.info.extreme$x[,1]  # first principal component score vector
pc2scores.extreme = pc.info.extreme$x[,2]  # second principal component score vector











