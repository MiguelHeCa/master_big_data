
# Clustering jerárquico ---------------------------------------------------

paises = foreign::read.spss("data/PaisesProteinas.sav", to.data.frame = TRUE)

paises_estandarizados = scale(paises[, 2:ncol(paises)])
rownames(paises_estandarizados) = paises[,1]

# Distancia euclídea
de = dist(paises_estandarizados)

aj_ward = hclust(de, method = "ward.D")

plot(aj_ward)
grupos = cutree(aj_ward, k = 3)
# Dibujar dendrograma con bordes rojos de clusters
rect.hclust(aj_ward, k = 3, border = "red")

?cluster::agnes

ajuste_aglo = cluster::agnes(paises_estandarizados, method = "ward")

cluster::pltree(ajuste_aglo)

sub_grupo = cutree(aj_ward, k = 3)

ajuste_disi = cluster::diana(paises_estandarizados)

# Dendrogramas bonitos ----------------------------------------------------

factoextra::fviz_dend(
  ajuste_aglo,
  k = 3,
  horiz = T,
  rect = T,
  rect_fill = T,
  rect_border = "jco",
  k_colors = "jco"
)

factoextra::fviz_dend(
  ajuste_aglo,
  k = 3,
  horiz = T,
  rect = T,
  rect_fill = T,
  rect_border = "jco",
  k_colors = "jco",
  # cex = 0.1,
  type = "circular"
)

# Determinar número óptimos de clusters -----------------------------------

library(ggplot2)

p1 = factoextra::fviz_nbclust(paises_estandarizados, FUN = factoextra::hcut, method = "wss", k.max = 10) +
  ggtitle("A. Método codo")
p2 = factoextra::fviz_nbclust(paises_estandarizados, FUN = factoextra::hcut, method = "silhouette", k.max = 10) +
  ggtitle("B. Método silueta")
p3 = factoextra::fviz_nbclust(paises_estandarizados, FUN = factoextra::hcut, method = "gap_stat", k.max = 10) +
  ggtitle("C. Método brecha estadística")
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
