#2. Clustering
clustering_ds = myDataSet_avg[, -c(1:3)]
View(clustering_ds)


# apply elbow method to find number of optimal clusters for k means
set.seed(123)
k.max <- 13
data <- clustering_ds
wss <- sapply(1:k.max,
function(k){kmeans(data, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
type="b", pch = 19, frame = FALSE,
xlab="Number of clusters K",
ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)

#library("factoextra") find optimal clusters with silhoutte method for both kmeans and hclustering

fviz_nbclust(clustering_ds, kmeans, method = "silhouette")

#K-means clustering with 7 clusters 
km_dataset = kmeans(clustering_ds,7)

#(between_SS / total_SS =  46.2 %)
km_dataset

#K-means clustering with 3 clusters
km_dataset = kmeans(clustering_ds,3)

#(between_SS / total_SS =  32.2 %)
km_dataset


# Apply PCA
pca_km = prcomp(clustering_ds, scale. = TRUE, center = TRUE)
pca_km.var =pca_km$sdev ^2
pve=pca_km.var/sum(pca_km.var)
plot(pve , xlab=" Principal Component ", ylab=" Proportion of
     Variance Explained ", ylim=c(0,1) ,type='b')

# Let's use 3 PCs ...
km_data_pca = kmeans(pca_km$x[,1:3],4)
km_data_pca

# After applying PCA we increased between_SS / total_SS to 50.5%

hclustData = clustering_ds

hclustData.complete = hclust(dist(hclustData), method="complete")
hclustData.average = hclust(dist(hclustData), method="average")
hclustData.single = hclust(dist(hclustData), method="single")

par(mfrow=c(1,3))
plot(hclustData.complete,main="Complete Linkage", xlab="", sub="", cex=.9,labels = FALSE)
plot(hclustData.average, main="Average Linkage", xlab="", sub="", cex=.9,labels = FALSE)
plot(hclustData.single, main="Single Linkage", xlab="", sub="", cex=.9,labels = FALSE)

#library("factoextra") find optimal clusters with elbow method

fviz_nbclust(hclustData, hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

#library("factoextra") find optimal clusters with silhoutte method for hclustering

fviz_nbclust(hclustData, hcut, method = "silhouette",
             hc_method = "complete")