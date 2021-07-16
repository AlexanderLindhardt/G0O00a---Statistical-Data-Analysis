
## Load data set
rm(list = ls())
set.seed(0826077)
library(readxl)
Dry_Bean_Dataset <- read_excel("Dry_Bean_Dataset.xlsx")

## Get a sample of size 200 from each type of bean
sample1 <- sample(which(Dry_Bean_Dataset[,17]=='DERMASON'),200)
sample2 <- sample(which(Dry_Bean_Dataset[,17]=='SIRA'),200)
sample3 <- sample(which(Dry_Bean_Dataset[,17]=='SEKER'),200)
mydatafull <- data.frame(Dry_Bean_Dataset[c(sample1,sample2,sample3),])

## Shorten some of the column names
names(mydatafull)[3] <- "MajorAxis"
names(mydatafull)[4] <- "MinorAxis"
names(mydatafull)[8] <- "EquivDiam"
names(mydatafull)[13] <- "shape1"
names(mydatafull)[14] <- "shape2"
names(mydatafull)[15] <- "shape3"
names(mydatafull)[16] <- "shape4"

## Divide into training and test data
mytrainingdata <- mydatafull[c(1:100,201:300,401:500),]
mytestdata <- mydatafull[c(101:200,301:400,501:600),]

## Libraries used
library(corrplot) # to make correlation plot
library(cellWise) # To use box-cox transformation function
library(rrcov) # PCA function
library(factoextra) # Make bi-plots 
library(cluster) # Cluster package
library(dendextend) # For functions involving clustering trees
library(RColorBrewer) # Colormap
library(pheatmap) # To make heatmap

##### 1.1 #####

## Check some of the rows and the dimensions of the data
head(mydatafull)
dim(mydatafull)

## Scatterplot-matrix
colors <- c(rep("blue3",200), rep("orange",200), rep("darkgreen",200))
pairs(mydatafull[,1:16], col=colors, pch=19) # Full scatterplot matrix
par(mfrow=c(3,4)) # We select a useful few scatter plots to use in report
plot(mydatafull[,c(1,2)], col=colors, pch=19)
plot(mydatafull[,c(1,3)], col=colors, pch=19)
plot(mydatafull[,c(1,7)], col=colors, pch=19)
plot(mydatafull[,c(1,8)], col=colors, pch=19)

plot(mydatafull[,c(9,10)], col=colors, pch=19)
plot(mydatafull[,c(16,15)], col=colors, pch=19)
plot(mydatafull[,c(10,6)], col=colors, pch=19)
plot(mydatafull[,c(11,8)], col=colors, pch=19)

plot(mydatafull[,c(12,13)], col=colors, pch=19)
plot(mydatafull[,c(12,14)], col=colors, pch=19)
plot(mydatafull[,c(5,7)], col=colors, pch=19)
plot(mydatafull[,c(6,8)], col=colors, pch=19)
par(mfrow=c(1,1))


## Correlation plot
corrplot.mixed(cor(mydatafull[,1:16]),tl.col="black", lower="circle", upper="number",
               number.cex= 18/ncol(mytrainingdata),tl.cex = 0.6)

## Check normality for each variable
par(mfrow=c(4,4))
for (i in 1:16) qqnorm(mydatafull[,i], pch=19,  main=colnames(mydatafull)[i]) # normal QQ-plots
par(mfrow=c(1,1))
for (i in 1:16) cat(colnames(mydatafull)[i],": ", shapiro.test(mydatafull[,i])$p, "\n") # Shapiro-wilk test

## Squared mahalanobis distance
S = cov(scale(mydatafull[,1:16]))
col_means = colMeans(scale(mydatafull[,1:16]))
mdx <- mahalanobis(scale(mydatafull[,1:16]),col_means ,S)
qqplot(qchisq(ppoints(nrow(mydatafull[,1:16])),df=ncol(mydatafull[,1:16])),mdx,pch=19,
       main="Chi-squared Q-Q Plot", xlab="Theoretical quantiles",
       ylab="Squared Mahalanobis distance")
abline(0,1,col="darkgray")

##### 1.2 #####

## Plot histograms for each variable
par(mfrow=c(4,4))
for (i in 1:16) hist(mydatafull[,i], pch=19, xlab = "", main=colnames(mydatafull)[i])

## Plot histograms for the variables furthest from normality to include in report
par(mfrow=c(3,4))
for (i in c(3,4,5,6,9,10,11,12,13,14,15,16)) hist(mydatafull[,i], pch=19, xlab = "", main=colnames(mydatafull)[i])
par(mfrow=c(1,1))

## Transform the variables that are far from normally distributed
mydatafull.trans = mydatafull
for (i in c(3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16)) {
  mydatafull.trans[,i] <- transfo(mydatafull[,i],type="BC",robust=TRUE)$Xt
}

## Plot all histograms
par(mfrow=c(4,4))
for (i in 1:16) hist(mydatafull.trans[,i], pch=19,xlab = "", main=colnames(mydatafull.trans)[i])

## Perform Shapiro-Wilk test and print the p-values
par(mfrow=c(1,1))
for (i in 1:16) cat(colnames(mydatafull.trans)[i],": ", shapiro.test(mydatafull.trans[,i])$p, "\n")



##### 2.1 #####

## Transform the training data
mytrainingdata <- mydatafull.trans[c(1:100,201:300,401:500),]
rownames(mytrainingdata) <- 1:nrow(mytrainingdata)

## Comparing using covariance and correlation for PCA
mytrainingdata.pca.cov <- PcaClassic(mytrainingdata[,1:16], scale=FALSE)
mytrainingdata.pca.corr <- PcaClassic(mytrainingdata[,1:16], scale=TRUE)

## Print summary of the PCA 
summary(mytrainingdata.pca.cov) # First component has 99% of the variance when using covariance
summary(mytrainingdata.pca.corr)

## Plot the correlations between the variables and the PCs
par(mfrow=c(1,2))
corrplot(mytrainingdata.pca.cov$loadings, title="cov", cl.lim = c(-1, 1), method = "color", tl.col = "black",mar=c(0,0,5,0))
corrplot(mytrainingdata.pca.corr$loadings, title="cor", cl.lim = c(-1, 1), method = "color", tl.col = "black",mar=c(0,0,4,0))

## Choosing the amount of PCs
par(mfrow=c(1,1))
screeplot(mytrainingdata.pca.corr, type = "lines", main="Scree plot") # Scree plot


##### 2.2 #####

## Biplots of the first three scores
colors <- c("blue3","orange","darkgreen")
mytrainingdata.prcomp <- prcomp(mytrainingdata[,1:16], scale=T)

# Bi-plots for components 1 and 2
fviz_pca_biplot(mytrainingdata.prcomp,axes=c(1,2),label="var",col.var="brown",
                habillage=mytrainingdata[,17],palette=colors,
                ellipse.level=0.99) 
# Bi-plots for components 1 and 3
fviz_pca_biplot(mytrainingdata.prcomp,axes=c(1,3),label="var",col.var="brown",
                habillage=mytrainingdata[,17],palette=colors,
                ellipse.level=0.99) 
# Bi-plots for components 2 and 3
fviz_pca_biplot(mytrainingdata.prcomp,axes=c(2,3),label="var",col.var="brown",
                habillage=mytrainingdata[,17],palette=colors,
                ellipse.level=0.99) 


##### 2.3 #####

## Classic PCA
mytrainingdata.pca.3 <- PcaClassic(mytrainingdata[,1:16], k = 3, scale=T, crit.pca.distances = 0.99)

## Robust PCA
mytrainingdata.robpca.3 <- PcaHubert(mytrainingdata[,1:16],k=3,scale=mad,crit.pca.distances=0.99)

## plot outlier maps
par(mfrow=c(1,2))
plot(mytrainingdata.pca.3, pch=19)
plot(mytrainingdata.robpca.3,pch=19)
par(mfrow=c(1,1))

## Scatter plot to show the detected outliers
colors <- rep("grey",nrow(mytrainingdata[,1:16]))
colors[c(5, 205, 11, 104)] <- c("blue", "purple", "orange","red")
pairs(mytrainingdata[,1:16], pch = 16, col = colors, gap = 0)

## Remove these outliers
mytrainingdata <- mytrainingdata[-c(5, 205, 11, 104),]

##### 2.4 #####

## Check amount of components we want after removing outliers
mytrainingdata.pca.corr <- PcaClassic(mytrainingdata[,1:16], scale=TRUE)
summary(mytrainingdata.pca.corr)
screeplot(mytrainingdata.pca.corr, type = "lines", main="Scree plot") 

## We choose three again
mytrainingdata.prcomp.3 <- prcomp(mytrainingdata[,1:16], rank = 3, scale = TRUE)

## Scores and predicted values
mytestdata <- mydatafull.trans[c(101:200,301:400,501:600),]
test.prcomp.scores <- predict(mytrainingdata.prcomp.3, newdata = mytestdata[,1:16])

test.prcomp.fitted <- t(t(test.prcomp.scores%*%t(mytrainingdata.prcomp.3$rotation)) + mytrainingdata.prcomp.3$center/mytrainingdata.prcomp.3$scale)
test.prcomp.fitted <- test.prcomp.fitted%*%diag(mytrainingdata.prcomp.3$scale)


## Means of predicted Area and real Area
mean(test.prcomp.fitted[1:100])
mean(Dry_Bean_Dataset[which(Dry_Bean_Dataset[,17]=='DERMASON'),1]$Area)
mean(test.prcomp.fitted[101:200])
mean(Dry_Bean_Dataset[which(Dry_Bean_Dataset[,17]=='SIRA'),1]$Area)
mean(test.prcomp.fitted[201:300])
mean(Dry_Bean_Dataset[which(Dry_Bean_Dataset[,17]=='SEKER'),1]$Area)

##### 2.5 #####

## Create outlier maps for the classic and robust PCA with both the test and training set
## We color the test and training set differently to be able to distinguish them.
mytrainingdata.pca.3 <- PcaClassic(mytrainingdata[,1:16], k = 3, scale=T, crit.pca.distances = 0.99)
mytrainingdata.pcahub.3 <- PcaHubert(mytrainingdata[,1:16], k=3, scale=mad, crit.pca.distances=0.99)

mytestdata.z <- scale(mytestdata[,1:16], center = mytrainingdata.pca.3$center, scale = mytrainingdata.pca.3$scale)
mytestdata.z.pcahub <- scale(mytestdata[,1:16], center = mytrainingdata.pcahub.3$center, scale = mytrainingdata.pcahub.3$scale)

mytestdata.scores <- mytestdata.z%*%mytrainingdata.pca.3$loadings
mytestdata.scores.pcahub <- mytestdata.z.pcahub%*%mytrainingdata.pcahub.3$loadings

mytestdata.fitted <- t(t(mytestdata.scores%*%t(mytrainingdata.pca.3$loadings)) + mytrainingdata.pca.3$center/mytrainingdata.pca.3$scale)
mytestdata.fitted.pcahub <- t(t(mytestdata.scores.pcahub%*%t(mytrainingdata.pcahub.3$loadings)) + mytrainingdata.pcahub.3$center/mytrainingdata.pcahub.3$scale)

mytestdata.s <- scale(mytestdata[,1:16], center = FALSE, scale = mytrainingdata.pca.3$scale)
mytestdata.s.pcahub <- scale(mytestdata[,1:16], center = FALSE, scale = mytrainingdata.pcahub.3$scale)


euclnorm <- function(y) sqrt(sum(y^2))
mytestdata.od <- apply(mytestdata.s - mytestdata.fitted, 1, euclnorm)
mytestdata.od.pcahub <- apply(mytestdata.s.pcahub - mytestdata.fitted.pcahub, 1, euclnorm)


mytestdata.sd <- sqrt(mahalanobis(mytestdata.scores, center = FALSE, diag(mytrainingdata.pca.3$eigenvalues)))
mytestdata.sd.pcahub <- sqrt(mahalanobis(mytestdata.scores.pcahub, center = FALSE, diag(mytrainingdata.pcahub.3$eigenvalues)))

cutoff.sd <- mytrainingdata.pca.3$cutoff.sd
cutoff.od <- mytrainingdata.pca.3$cutoff.od

cutoff.sd.pcahub <- mytrainingdata.pcahub.3$cutoff.sd
cutoff.od.pcahub <- mytrainingdata.pcahub.3$cutoff.od

ods <- c(mytrainingdata.pca.3$od, mytestdata.od)
sds <- c(mytrainingdata.pca.3$sd, mytestdata.sd)

ods.pcahub <- c(mytrainingdata.pcahub.3$od, mytestdata.od.pcahub)
sds.pcahub <- c(mytrainingdata.pcahub.3$sd, mytestdata.sd.pcahub)

xmax <- max(sds, cutoff.sd)
ymax <- max(ods, cutoff.od)

xmax.pcahub <- max(sds.pcahub, cutoff.sd.pcahub)
ymax.pcahub <- max(ods.pcahub, cutoff.od.pcahub)

colors <- c(rep("blue3",300), rep("orange",300))

par(mfrow=c(1,2))
plot(sds, ods, pch = 19, xlim = c(0, xmax + 1), ylim = c(0, ymax + 0.2), main = "Classical PCA", xlab = "Score distance", ylab = "Orthogonal distance", col = colors)
abline(h = cutoff.od, col = "red", lwd = 1.5)
abline(v = cutoff.sd, col ="red", lwd = 1.5)
legend(5, 1, legend=c("Training", "Test"),
       col=c("blue3", "orange"), pch=c(19,19))

plot(sds.pcahub, ods.pcahub, pch = 19, xlim = c(0, xmax.pcahub + 1), ylim = c(0, ymax.pcahub + 0.2), main = "Robust PCA", xlab = "Score distance", ylab = "Orthogonal distance", col = colors)
abline(h = cutoff.od.pcahub, col = "red", lwd = 1.5)
abline(v = cutoff.sd.pcahub, col ="red", lwd = 1.5)
legend(5, 1, legend=c("Training", "Test"),
       col=c("blue3", "orange"), pch=c(19,19))
par(mfrow=c(1,1))

##### 3.1 #####

## Standardize training data
mytrainingdata.s <- scale(mytrainingdata[,1:16])

## Compare silhouette widths
fviz_nbclust(mytrainingdata.s, kmeans, method = "silhouette")
fviz_nbclust(mytrainingdata.s, kmeans, method = "silhouette")$data$y

fviz_nbclust(mytrainingdata.s, pam, method = "silhouette")
fviz_nbclust(mytrainingdata.s, pam, method = "silhouette")$data$y

## K-means
set.seed(20)
train.kmeans <- kmeans(mytrainingdata.s,3, nstart=50)

## K-medoids / PAM
train.medoids <- pam(mytrainingdata.s, k=3)

## 
par(mfrow=c(1,2))
colors <- c("blue3","orange","darkgreen")
clusplot(mytrainingdata.s, train.kmeans$cluster, main="K-means",color=TRUE,col.clus=c("black","black","black"),
         col.p=colors[train.kmeans$cluster])
clusplot(train.medoids, main="K-medoids",color=TRUE,col.clus=c("black","black","black"),
         col.p=colors[train.medoids$cluster])


##### 3.2. #####
par(mfrow=c(1,1))
## Dissimilarity matrix
train.diss <- daisy(mytrainingdata.s)

## Single linkage
agnes.single <- agnes(train.diss, method="single")
agnes.single.dg <- as.dendrogram(agnes.single)
plot(agnes.single.dg,leaflab="none", main="Single linkage")

## Average linkage
agnes.average <- agnes(train.diss, method="average")
agnes.average.dg <- as.dendrogram(agnes.average)
plot(agnes.average.dg,leaflab="none", main="Average linkage")

## Complete linkage
agnes.complete <- agnes(train.diss, method="complete")
agnes.complete.dg <- as.dendrogram(agnes.complete)
plot(agnes.complete.dg,leaflab="none", main="Complete linkage")

## Plot clustering tree
agnes.complete.dg <- color_branches(agnes.complete.dg,k=3,col=colors)
plot(agnes.complete.dg,leaflab="none", main="Agglomerative clustering - Complete linkage")

## Divisive clustering / DIANA
train.diana <- diana(train.diss)
train.diana.dg <- as.dendrogram(train.diana)
plot(train.diana.dg,leaflab="none", main="Divisive clustering")

## Color the three clusters in clustering tree
train.diana.dg <- color_branches(train.diana.dg, k=3,col=colors)
plot(train.diana.dg,leaflab="none", main="Clustering tree - Divisive algorithm")

## Average silhouette width
agnes.complete.cluster <- cutree(agnes.complete.dg,3)
train.diana.cluster <- cutree(train.diana.dg,3)
cat("Divisive clustering average silhouette width:", mean(silhouette(agnes.complete.cluster,train.diss)[,3]))
cat("Agglomerative clustering average silhouette width:", mean(silhouette(train.diana.cluster,train.diss)[,3]))

## Cluster plots
par(mfrow=c(1,2))
clusplot(mytrainingdata.s, agnes.complete.cluster, main="Agglomerative clustering with complete linkage",
         color=TRUE,col.clus=c("black","black","black"), col.p=colors[agnes.complete.cluster])
clusplot(mytrainingdata.s, train.diana.cluster, main="Divisive clustering",
         color=TRUE,col.clus=c("black","black","black"), col.p=colors[train.diana.cluster])
par(mfrow=c(1,1))

##### 3.3 #####

## Correlation matrix
corrplot(cor(mytrainingdata[,-17]),tl.col="black",tl.cex=0.75)

## Dissimilarity matrix from the correlation matrix
diss.cor <- as.dist((1-cor(mytrainingdata[,-17]))/2)

## Plot clustering trees for agglomerative and divisive clustering
par(mfrow=c(1,2))
train.agnes.var <- agnes(diss.cor, method="complete")
plot(train.agnes.var,which.plots=2,main="Agglomerative clustering - complete linkage")
train.diana.var <- diana(diss.cor)
plot(train.diana.var,which.plots=2,main="Divisive clustering")
par(mfrow=c(1,1))

## Show clustering with correlation plot
corrplot(cor(mytrainingdata[,-17]),tl.col="black",tl.cex=0.75,hclust.method="complete",
          order="hclust",addrect=3,cl.pos="n")

##### 3.4 #####
## Compare accuracy
table(mytrainingdata[,17], train.kmeans$cluster)
accuracy.kmeans <- (82+91+91)/length(mytrainingdata[,1])
cat("Accuracy for the k-means clustering:", accuracy.kmeans)

table(mytrainingdata[,17], train.diana.cluster)
accuracy.diana <- (71+87+94)/length(mytrainingdata[,1])
cat("Accuracy for the divisive clustering:", accuracy.diana)

## Heat map
pheatmap((mytrainingdata.s[order(train.kmeans$cluster),-17]),
         cluster_rows=F, cluster_cols=T, cutree_cols=3,
         gaps_row=cumsum(train.kmeans$size),legend=TRUE,
         show_rownames=FALSE,color=brewer.pal(11, "BrBG"))