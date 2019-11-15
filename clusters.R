
# Clustering jerárquico ---------------------------------------------------

paises = foreign::read.spss("data/PaisesProteinas.sav", to.data.frame = TRUE)

# > Selección de variables ----
var.paises = paises[, 2:ncol(paises)]

# > Estandarización ----
p.esc = scale(var.paises)
rownames(p.esc) = paises[, 1]

# > Detección de atípicos ----

summary(p.esc)

boxplot(p.esc)

detec_atip = function(x) {
  resultado = list()
  
  # Rangos inter
  ran.int = function(x) quantile(x, c(0.25, 0.75))
  
  # Detección
  inferior = function(x) ran.int(x)[1] - (1.5 * IQR(x))
  superior = function(x) ran.int(x)[2] + (1.5 * IQR(x))
  
  # Escribir resultados
  resultado$ext.inferior = subset(x, x < inferior(x))
  resultado$ext.superior = subset(x, x > superior(x))
  
  return(resultado)
}

unlist(apply(p.esc, 2, detec_atip))

# > Colinealidad ----

corrplot::corrplot(cor(p.esc))

# Cereales esta inversamente correlacionada con todas.
# Frutos Secos, Huevos, Leche y cereales son las más correlacionaddas


# Distancias --------------------------------------------------------------

# Distancia euclídea
d.euc = dist(p.esc, method = "euclidean")


# Algoritmos de cluster ---------------------------------------------------

# > Aglomeración ----------------------------------------------------------

metodos = c("ward.D2",
            "single",
            "complete",
            "average",
            "mcquitty",
            "median",
            "centroid")
names(metodos) = metodos

met.agl = lapply(metodos, function(x) hclust(d.euc, method = x))

# Función de coeficiente de aglomeración
coe_agl = sapply(met.agl[1:5], cluster::coef.hclust)
coe_agl = round(coe_agl, digits = 2)

library(ggplot2)
library(factoextra)

ttl.met = c(
  "Método de Ward",
  "Vecino más próximo",
  "Vecino más Lejano",
  "Grupo promedio",
  "Método de McQuitty",
  "Grupo Mediano",
  "Método del Centroide"
)

coe.met = c(paste("Coef. aglo.:", coe_agl), "", "")

dendros = function(x, ttl, stl) {
  graf = fviz_dend(
    x,
    horiz = T,
    main = ttl,
    sub = stl,
    xlab = "Cluster",
    ylab = "Altura"
  )
  return(graf)
}


for(i in 1:7) {
  plot(met.agl[[i]],
       main = ttl.met[i],
       sub = coe.met[i],
       xlab = "Clusters",
       ylab = "Altura"
  )
}

lapply(1:7, function(x) {
  dendros(met.agl[[x]], ttl = ttl.met[x], stl = coe.met[x])
})



plot(met.agl[[1]], main = "Método de Ward", sub = paste("Coef. aglo.:", coe_agl[1]), xlab = "Clusters", ylab = "Altura")
rect.hclust(met.agl[[1]], k = 3, border = "red")

plot(met.agl[[2]], main = "Vecino más próximo", sub = paste("Coef. aglo.:", coe_agl[2]), xlab = "Clusters", ylab = "Altura")
rect.hclust(met.agl[[2]], k = 3, border = "red")

plot(met.agl[[3]], main = "Vecino más Lejano", sub = paste("Coef. aglo.:", coe_agl[3]), xlab = "Clusters", ylab = "Altura")
rect.hclust(met.agl[[3]], k = 3, border = "red")

plot(met.agl[[4]], main = "Grupo promedio", sub = paste("Coef. aglo.:", coe_agl[4]), xlab = "Clusters", ylab = "Altura")
rect.hclust(met.agl[[4]], k = 3, border = "red")

plot(met.agl[[5]], main = "Método de McQuitty", sub = paste("Coef. aglo.:", coe_agl[5]), xlab = "Clusters", ylab = "Altura")
rect.hclust(met.agl[[5]], k = 3, border = "red")

plot(met.agl[[6]], main = "Grupo Mediano", sub = paste("", coe_agl[6]), xlab = "Clusters", ylab = "Altura")
rect.hclust(met.agl[[6]], k = 3, border = "red")

plot(met.agl[[7]], main = "Método del Centroide", sub = paste("", coe_agl[7]), xlab = "Clusters", ylab = "Altura")
rect.hclust(met.agl[[7]], k = 3, border = "red")


# > Disimilar -------------------------------------------------------------

library(cluster)

met.dis = diana(d.euc)

met.dis$dc

# Con el paquete cluster

# > > Con el paquete cluster ----------------------------------------------


library(cluster)


library(cluster)

ajuste.aglo = cluster::agnes(p.esc, method = "ward")

cluster::pltree(ajuste.aglo)

sub.grupo = cutree(aj.ward, k = 3)

ajuste.disi = cluster::diana(p.esc)

# ANOVA

# Sedimentos

# Dendrogramas bonitos ----------------------------------------------------

factoextra::fviz.dend(
  ajuste.aglo,
  k = 3,
  horiz = T,
  rect = T,
  rect.fill = T,
  rect.border = "jco",
  k.colors = "jco"
)

factoextra::fviz.dend(
  ajuste.aglo,
  k = 3,
  horiz = T,
  rect = T,
  rect.fill = T,
  rect.border = "jco",
  k.colors = "jco",
  # cex = 0.1,
  type = "circular"
)

# Determinar número óptimos de clusters -----------------------------------

library(ggplot2)
library(factoextra)

p1 = factoextra::fviz.nbclust(paises.estandarizados, FUN = factoextra::hcut, method = "wss", k.max = 10) +
  ggtitle("A. Método codo")
p2 = factoextra::fviz.nbclust(paises.estandarizados, FUN = factoextra::hcut, method = "silhouette", k.max = 10) +
  ggtitle("B. Método silueta")
p3 = factoextra::fviz.nbclust(paises.estandarizados, FUN = factoextra::hcut, method = "gap.stat", k.max = 10) +
  ggtitle("C. Método brecha estadística")
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)

install.packages("ibmdbR")
library(ibmdbR)


paises.ida = as.ida.data.frame(paises, "PAISES", clear.existing = T)


install.packages("prcr")
devtools::install.github("jrosen48/prcr")
library(prcr)

df = paises[, 2:ncol(paises)]
rownames(df) = paises[, 1]

m3 = prcr::create.profiles(
  df,
  CarneRoja,
  CarneBlanca,
  Huevos,
  Leche,
  Pescado,
  Cereales,
  Feculas,
  FrutosSecos,
  FrutosyVegetales,
  n.profiles = 3,
  to.center = T,
  to.scale = T,
  linkage = "ward.D",
  plot.centered.data = T
)

m3

twostep = prcr::create.profiles(
  df,
  CarneRoja,
  CarneBlanca,
  Huevos,
  Leche,
  Pescado,
  Cereales,
  Feculas,
  FrutosSecos,
  FrutosyVegetales,
  n.profiles = 3,
  linkage = "ward.D",
  plot.raw.data = T
)
twostep
