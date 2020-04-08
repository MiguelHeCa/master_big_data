setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Primera parte MBI -------------------------------------------------------

load("curas.rdata")

# 1
corr.curas = cor(curas)
corr.curas[2, 4]

# 2
sort(corr.curas[5, ])

# 3
corrplot::corrplot(corr.curas)

# 4
pca.curas = princomp(curas)
summary(pca.curas)

# 5
pca.curas$loadings[14, 1:3]

# 6
barplot(pca.curas$loadings[, 2],
        names.arg = colnames(curas),
        cex.names = 0.6,
        main = "Segunda componente principal")

# 7
print(pca.curas$loadings, cutoff = 0.2)

# 8 
print(pca.curas$loadings, cutoff = 0.4)

# 9
print(pca.curas$loadings, cutoff = 0.1)

# 10
pca.curas$loadings

# 11:16
library(elasticnet)

a_i = c(1:3, 6, 8, 13:14, 16, 20)
e_i = c(4, 7, 9, 12, 17:19, 21)
d_i = c(5, 10, 11, 15, 22)

agot = curas[, a_i]
auto = curas[, e_i]
desp = curas[, d_i]

dat = cbind(agot, auto, desp)
colnames(dat) = paste("MB", c(a_i, e_i, d_i), sep = "_")
head(dat)

spca_1 = spca(
  cor(dat),
  K = 3,
  type = "Gram",
  sparse = "varnum",
  trace = T,
  para = c(9, 8, 5)
)
spca_1

pca.curas$loadings

# 17

spca_2 = spca(
  cor(curas),
  K = 3,
  type = "Gram",
  sparse = "varnum",
  trace = T,
  para = c(9, 8, 5)
)

barplot(spca_2$loadings[, 3], names.arg = colnames(dat))

# 18:21
spca_1

# 22
library(rCUR)

cur1 = CUR(
  curas,
  k = 3,
  c = ncol(curas) - 1,
  method = "top.scores",
  weighted = T
)

plotLeverage(cur1)

## 23:25
lvg_score = cur1@C.leverage.score
names(lvg_score) = colnames(curas)
sort(lvg_score)

# 26
sum(lvg_score > 0.05)

# 27
cur2 = CUR(
  curas,
  k = 3,
  c = 17,
  method = "top.scores",
  error.return = T,
  weighted = T
)
cur2

# 28
cur3 = CUR(
  curas,
  k = 3,
  c = 15,
  r = 25,
  method = "top.scores",
  weighted = T
)
lvg_score2 = cur3@R.leverage.score
names(lvg_score2) = paste("Cura", 1:length(lvg_score2), sep = "_")
sort(lvg_score2)


# Técnicas de clasificación -----------------------------------------------

load("Datos_cluster.rdata")

profesion = c(rep("Curas", 40), rep("Farma", 40))
rownames(Datos_cluster) = profesion

# 29
set.seed(50)
cluster1 = kmeans(Datos_cluster, 2)

cluster1$cluster

# 30
table(cluster1$cluster)

# 31
table(profesion, cluster1$cluster)

# 33
table(profesion[c(5, 25)], cluster1$cluster[c(5, 25)])


# > PAM -------------------------------------------------------------------

# 34
library(cluster)

set.seed(50)
cluster2 = pam(Datos_cluster, 2)

cluster2$clusinfo

# 35:36
table(profesion, cluster2$clustering)

# 37
TRUE

# > Fuzzy c-means ---------------------------------------------------------

library(e1071)

set.seed(50)
cluster3 = cmeans(Datos_cluster, 2, method = "cmeans", m = 1.5)

# 38
cluster3$membership[19,]
cluster3$membership[16,]

# 39
plot(cluster3$membership[, 1],
     type = "l",
     ylab = "prop",
     main = "Prob de Cluster 1")

# > DBSCAN ----------------------------------------------------------------

library(dbscan)
