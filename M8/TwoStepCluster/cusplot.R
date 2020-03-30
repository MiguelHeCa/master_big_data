>library(cluster)
>coches = read.table(file.choose(), header=TRUE, sep="\t")
>coches
>coches.diss <- daisy (coches,stand=TRUE, metric="gower")
>coches.clus <- pam (coches.diss,3,diss=TRUE)
>clusplot (coches.clus,lines=0,shade=TRUE,col.p="black",color=TRUE,labels=2)
