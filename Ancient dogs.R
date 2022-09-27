dog <- read.csv(file.choose())  #Use dogs2.csv
dim(dog)
head(dog)
round(cor(dog[,1:9]), 3)
dogpca <- princomp(dog[,1:9],cor=T, scores=T)
dogpca
summary(dogpca, loadings=T, cutoff=0)
screeplot(dogpca)
plot(dogpca, type="lines")
plot(dogpca$scores[,1],dogpca$scores[,2], xlab="PC1", ylab="PC2", pch=as.character(dog$Group2))
biplot(dogpca)
plot(dogpca$scores[,1],dogpca$scores[,2],xlab="PC1",ylab="PC2",pch=as.character(dog$Group2))



# Hirerarchic cluster analysis, Nearest-neighbor
canines <- read.csv("Canines means.csv", header=TRUE, row.names=1)
attach(canines)
# Standardizing the data with scale() 
matstd.can <- scale(canines)
# Creating a (Euclidean) distance matrix of the standardized data 
dist.canine <- dist(matstd.can, method="euclidean")
# Invoking hclust command (cluster analysis by single linkage method)      
cluscanine.nn <- hclust(dist.canine, method = "single") 
# Plotting vertical dendrogram      
# create extra margin room in the dendrogram, on the bottom (Canine species' labels)
par(mar=c(6, 4, 4, 2) + 0.1)
plot(as.dendrogram(cluscanine.nn),ylab="Distance between Canine species",ylim=c(0,2.5),
     main="Dendrogram of six canine species")
#
detach(canines)
#
# End of script





