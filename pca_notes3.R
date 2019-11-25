# Carga de datos tmms con .RData
load("tmms.RData")

cor_tmms = cor(tmms)

cor_tmms

psych::scree(tmms)

res.pca = prcomp(tmms, center = T, scale. = T)

res.acp = prcomp(tmms, scale. = T)


summary(res.pca)

res.pca$sdev^2


mat_cov = cov(tmms)
val_propios = eigen(mat_cov)


d = 
d = svd(cor(tmms))$d

suma = sum(eigen_values)

d/suma

d2 = sqrt(d)

suma2 = sum(d2)


d2/suma2

summary(res.pca)

res.acp$rotation[,1:3]


factoextra::fviz_eig(res.pca)

factoextra::fviz_pca_ind(res.pca)

factoextra::get_eigenvalue(res.pca)

?factoextra::fviz_pca

res_pca = ade4::dudi.pca(tmms)

acp_2 = psych::principal(tmms, nfactors = 3)

acp_2$values
acp_2$rms
