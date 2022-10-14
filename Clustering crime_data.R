install.packages("data.table")
install.packages("ClusterR")
install.packages("cluster")
install.packages("animation")
install.packages("fpc")
install.packages("dbscan")
install.packages("factoextra")
# Hierarchical cluster.

cat("\014")
library(data.table)
crime_data = read.csv("C:\\Users\\suman\\Desktop\\Assignment\\DS Assignment\\crime_data.csv")
str(crime_data)
crime_data_1 <- crime_data[,2:5]
crime_data_2 <- scale(crime_data_1)
x <- dist(crime_data_2, method = "euclidean")
str(x)
crime_clust <- hclust(x, method = "complete")
plot(crime_clust, hang=-1)
rect.hclust(crime_clust,plot(crime_clust,hang=-1),k=4,border="red")
clusters <- cutree(crime_clust,k=4)
crime_data_final <- cbind(crime_data, clusters)
aggregate(crime_data_final[,2:6], by=list(crime_data_final$clusters), FUN = mean)

#K-means
library(ClusterR)
library(cluster)
library(animation)
crime_data_3 <- na.omit(USArrests)
crime <- data.matrix (crime_data_3)
str(crime)
set.seed(666) 
cl <- kmeans(crime_data_3, 4)
class(cl)
cl$centers
cl<- kmeans.ani(crime_data_3, 4)

#DBSCAN

library(factoextra)
library(dbscan)
library(fpc)
crime_data_4 <- crime_data[-1]
str(crime_data_4)
set.seed(666) 
Dbscan_cl <- dbscan(crime_data_4, eps = 0.45, MinPts = 5)
Dbscan_cl
Dbscan_cl$cluster
table(Dbscan_cl$cluster, crime_data$State)
plot(crime_data_4, col=Dbscan_cl$cluster+1, main="DBSCAN")
print("As per summary we can say group 2 have the higher rate of crime.")
