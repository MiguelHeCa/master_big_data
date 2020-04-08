
leucemias<-read.table("leucemias.txt", header=T)
dim(leucemias)
row.names(leucemias)
colnames(leucemias)

install.packages("gplots")
library(gplots)

library(ClusterR) #Kmeans++
library(cluster) #PAM, Clara, Fuzzy Clustering
library(e1071) #Cmeans
library(dbscan)

clasificacion<-c(rep("ALL", 24), rep("MLL", 20), 
                 rep("AML", 28))

coloresleucemias<-c(rep("red", 24), rep("black", 20), 
                    rep("green", 28))

pcaleucemias<-prcomp(leucemias)

plot(pcaleucemias$x[,1:2], pch=19, col=coloresleucemias)

#KMEANS

set.seed(200)
kmeansMacqueen<-kmeans(leucemias, 3, algorithm = "MacQueen")
kmeansMacqueen$cluster

balloonplot(t(table(clasificacion,kmeansMacqueen$cluster)))

par(mfrow=c(1,2))
plot(pcaleucemias$x[,1:2], pch=19, col=coloresleucemias)
plot(pcaleucemias$x[,1:2], pch=19, col=kmeansMacqueen$cluster)
par(mfrow=c(1,1))

set.seed(1)
km.init<-KMeans_rcpp(leucemias, 3, num_init = 1, 
                     initializer = "kmeans++")

balloonplot(t(table(clasificacion, km.init$clusters)))


#PAM

pam.res<-pam(leucemias, 3)
print(pam.res)  

balloonplot(t(table(clasificacion, pam.res$clustering)))

#CLARA

clara.res<-clara(leucemias, 3)
clara.res$clustering

balloonplot(t(table(clasificacion, clara.res$clustering)))


#Cmeans

#fanny (cluster)
#cmeans (e1071)

cmeans.res<-cmeans(leucemias, 3, method="cmeans")
cmeans.res$membership

plot(cmeans.res$membership[,1], pch=19, 
     col=coloresleucemias)

balloonplot(t(table(clasificacion, cmeans.res$cluster)))

cmeans.res$cluster

#DBSCAN
data("DS3")
pca.res<-prcomp(DS3)
plot(pca.res$x[,1:2])

kmeans.res<-kmeans(DS3, 6, algorithm = "Forgy")
kmeans.plus<-KMeans_rcpp(DS3, 6, initializer = "kmeans++")
dbscan.res<-hdbscan(DS3, minPts=50)
dbscan.res$cluster

par(mfrow=c(1,3))
plot(pca.res$x[,1:2], col=kmeans.res$cluster)
plot(pca.res$x[,1:2], col=kmeans.plus$clusters)
plot(pca.res$x[,1:2], col=dbscan.res$cluster+1)

