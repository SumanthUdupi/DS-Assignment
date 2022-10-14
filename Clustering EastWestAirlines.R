install.packages("dummies")
install.packages("dendextend")
install.packages("gridExtra")
install.packages("cluster")
install.packages("factoextra")
install.packages("MASS")
install.packages("fpc")
install.packages("clValid")
install.packages("optCluster")
install.packages("dbscan")
airlines_1 <- read.csv("C:\\Users\\suman\\Desktop\\Assignment\\DS Assignment\\EastWestAirlines.csv", header = TRUE)
library("readr")
library("dplyr")
library("ggplot2")
library("stringr")
library("dummies")
library("dendextend")
library("gridExtra")
library("cluster")
library("factoextra")
library("MASS")
library("fpc")
library("clValid")
library("optCluster")
library("dbscan")
head(airlines_1)
airlines_2 <- airlines_1[c(-1,-12)] 
head(airlines_2)
str(airlines_2)
summary(airlines_2)
d.pearson <- get_dist(airlines_1, method = "pearson")
d.pearson
airlines_2$cc1_miles = ifelse(airlines_2$cc1_miles==1,2500,
                              ifelse(airlines_2$cc1_miles==2,7500,
                                     ifelse(airlines_2$cc1_miles==3,17500,
                                            ifelse(airlines_2$cc1_miles==4,32500,
                                                   ifelse(airlines_2$cc1_miles==5,50000,0)))))

airlines_2$cc2_miles = ifelse(airlines_2$cc2_miles==1,2500,
                              ifelse(airlines_2$cc2_miles==2,7500,
                                     ifelse(airlines_2$cc2_miles==3,17500,
                                            ifelse(airlines_2$cc2_miles==4,32500,
                                                   ifelse(airlines_2$cc2_miles==5,50000,0)))))

airlines_2$cc3_miles = ifelse(airlines_2$cc3_miles==1,2500,
                              ifelse(airlines_2$cc3_miles==2,7500,
                                     ifelse(airlines_2$cc3_miles==3,17500,
                                            ifelse(airlines_2$cc3_miles==4,32500,
                                                   ifelse(airlines_2$cc3_miles==5,50000,0)))))

head(airlines_2)
airlines_3 <- scale (airlines_2)
head(airlines_3)
k.max <- 10
wss <- sapply(1:k.max, function(k){ kmeans(airlines_3[,-1], k)$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
k_mean_cluster_3 <- eclust(airlines_3, "kmeans", k = 3, nstart = 25, graph = TRUE)
k_mean_cluster_4 <- eclust(airlines_3, "kmeans", k = 4, nstart = 25, graph = TRUE)
k_mean_cluster_5 <- eclust(airlines_3, "kmeans", k = 5, nstart = 25, graph = TRUE)
fviz_silhouette(k_mean_cluster_3)
fviz_silhouette(k_mean_cluster_4)
fviz_silhouette(k_mean_cluster_5)
airlines_euclidian_dist <- dist(airlines_3, method = "euclidean")
str(airlines_euclidian_dist)
airline_fit <- hclust(airlines_euclidian_dist, method="ward.D2")
airline_fit1 <- as.dendrogram(airline_fit)
plot(airline_fit1, cex = 0.6)
abline(h=85,col="red",lty=2)
hierarchical_cluster <- cutree(airline_fit, k = 3, h=NULL)
table(hierarchical_cluster)
intern <- clValid(airlines_3, 2:6, clMethods=c("hierarchical"),
                  validation="internal", maxitems=nrow(airlines_3))
summary (intern)
stab <- matrix(0,nrow=ncol(airlines_3),ncol=4)
colnames(stab) <- c("APN","AD","ADM","FOM")
for (del in 1:ncol(airlines_3)) {
  matDel <- airlines_3[,-del]               
  DistDel <- dist(matDel,method="euclidean")
  clusterObjDel <- hclust(DistDel, method="ward.D2")
  clusterDel <- cutree(clusterObjDel,3)
  stab[del,] <- stability(airlines_3, airlines_euclidian_dist, del, hierarchical_cluster, clusterDel)
}
colMeans(stab)
dens.clust <- dbscan(d.pearson, minPts = 5, eps = 0.15)
fviz_cluster(dens.clust,data = airlines_1, palette ="jco", geom = "point", ggtheme = theme_classic())
dens.clust.data <- data.frame(airlines_1[,1], "cluster"=dens.clust$cluster)
dens.clust.data
df1 <- data.frame(airlines_3, k_mean_cluster_3$cluster) # append cluster membership
Cluster_mean1 <- aggregate(df1, by=list(k_mean_cluster_3$cluster), FUN=mean)
Cluster_mean1
Cluster_sum <- aggregate(df1, by=list(k_mean_cluster_3$cluster), FUN=sum) 
d <- transform(Cluster_sum, clusterSize = k_mean_cluster_3.cluster / Group.1)
d <- transform(d, k_mean_cluster_3.cluster= k_mean_cluster_3.cluster/ clusterSize)
d
Cluster_mean1$clusterSize <- d$clusterSize
Cluster_mean1$clusterPCT <- (d$clusterSize*100)/3999
temp1 <- t(Cluster_mean1)
round_df <- function(x, digits) {
  numeric_columns    <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
temp2 <- round_df (temp1,2)
temp2
Cluster_mean2 <- aggregate(airlines_3,list(hierarchical_cluster),mean)
df2 <- data.frame(Cluster=Cluster_mean2[,1],Cluster_sum=as.vector(table(hierarchical_cluster)),Cluster_mean2[,-1])
temp3 <- t(df2)

round_df <- function(x, digits) {
  numeric_columns    <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

temp4 <- round_df(temp3, 2)
temp4

