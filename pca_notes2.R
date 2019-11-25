my.wine = data.frame(
  Wine = c(1, 2, 3, 4, 5),
  Hedonic = c(14, 10, 8, 2, 6),
  Meat = c(7, 7, 5, 4, 2),
  Dessert = c(8, 6, 5, 7, 4),
  Price = c(7, 4, 10, 16, 13),
  Sugar = c(7, 3, 5, 7, 3),
  Alcohol = c(13, 14, 12, 11, 10),
  Acidity = c(7, 7, 5, 3, 3)
)


pca = prcomp(my.wine[, 2:8], center = T, scale = T)
pca

# Kaiser criteria

summary(pca)

eigen_vector = pca$sdev^2
eigen_vector

# Retain principal componentes with eigen values above 1.

pca$rotation

pca.varimax = varimax(pca$rotation)





pca$rotation

ogn_loadings = data.frame(Var = rownames(pca$rotation),
                          PC1 = pca$rotation[,1],
                          PC2 = pca$rotation[,2])

ggplot(ogn_loadings, aes(PC1, PC2, label = Var)) +
  geom_point() +
  geom_text(vjust = 0, hjust = 0) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0))

pca_vmx = psych::principal(my.wine[, 2:8])

vmx_loadings = data.frame(Var = rownames(pca$rotation),
                          PC1 = pca$rotation[,1],
                          PC2 = pca$rotation[,2])
