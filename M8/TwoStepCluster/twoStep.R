library(factoextra)
library(cluster)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

datos = foreign::read.spss("anticuerpos.sav", to.data.frame = T)
dat = datos[, 2:ncol(datos)]

datos.dis = daisy(dat, stand = T, metric = "gower")
clus = pam(datos.dis, 2, diss = T)
clusplot(clus, lines = 1, col.p = "black", color = T, labels = 1)
clusplot(clus, color = T, labels = 2)

fviz_cluster(clus, data = clus$clustering, ellipse = T)

clus

