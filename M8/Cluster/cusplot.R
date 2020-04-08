library(cluster)

coches = readxl::read_excel("M8/Cluster/coches.xls")
coches
coches.diss <- daisy(coches[,-c(1, 2, 5)], stand=TRUE, metric="gower")
coches.clus <- pam(coches.diss,3,diss=TRUE)
clusplot(coches.clus,lines=0,shade=TRUE,col.p="black",color=TRUE,labels=2)
