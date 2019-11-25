
# Clustering jerárquico ---------------------------------------------------

paises = foreign::read.spss("data/PaisesProteinas.sav", to.data.frame = TRUE)

# > Selección de variables ------------------------------------------------

var.paises = paises[, 2:ncol(paises)]

# > Estandarización -------------------------------------------------------

p.esc = scale(var.paises)
rownames(p.esc) = paises[, 1]

# > Detección de atípicos -------------------------------------------------

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
            "mcquitty")
names(metodos) = metodos

met.agl = lapply(metodos, function(x) hclust(d.euc, method = x))

# Función de coeficiente de aglomeración
coe_agl = sapply(met.agl, cluster::coef.hclust)
coe_agl = round(coe_agl, digits = 2)

library(ggplot2)
library(factoextra)

ttl.met = c(
  "Método de Ward",
  "Vecino más próximo",
  "Vecino más Lejano",
  "Grupo promedio",
  "Método de McQuitty"
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


# for(i in 1:5) {
#   plot(met.agl[[i]],
#        main = ttl.met[i],
#        sub = coe.met[i],
#        xlab = "Clusters",
#        ylab = "Altura"
#   )
# }

lapply(1:5, function(x) {
  dendros(met.agl[[x]], ttl = ttl.met[x], stl = coe.met[x])
})
# 
# plot(met.agl[[1]], main = "Método de Ward", sub = paste("Coef. aglo.:", coe_agl[1]), xlab = "Clusters", ylab = "Altura")
# rect.hclust(met.agl[[1]], k = 3, border = "red")
# 
# plot(met.agl[[2]], main = "Vecino más próximo", sub = paste("Coef. aglo.:", coe_agl[2]), xlab = "Clusters", ylab = "Altura")
# rect.hclust(met.agl[[2]], k = 3, border = "red")
# 
# plot(met.agl[[3]], main = "Vecino más Lejano", sub = paste("Coef. aglo.:", coe_agl[3]), xlab = "Clusters", ylab = "Altura")
# rect.hclust(met.agl[[3]], k = 3, border = "red")
# 
# plot(met.agl[[4]], main = "Grupo promedio", sub = paste("Coef. aglo.:", coe_agl[4]), xlab = "Clusters", ylab = "Altura")
# rect.hclust(met.agl[[4]], k = 3, border = "red")
# 
# plot(met.agl[[5]], main = "Método de McQuitty", sub = paste("Coef. aglo.:", coe_agl[5]), xlab = "Clusters", ylab = "Altura")
# rect.hclust(met.agl[[5]], k = 3, border = "red")
# 
# plot(met.agl[[6]], main = "Grupo Mediano", sub = paste("", coe_agl[6]), xlab = "Clusters", ylab = "Altura")
# rect.hclust(met.agl[[6]], k = 3, border = "red")
# 
# plot(met.agl[[7]], main = "Método del Centroide", sub = paste("", coe_agl[7]), xlab = "Clusters", ylab = "Altura")
# rect.hclust(met.agl[[7]], k = 3, border = "red")

# > Disimilar -------------------------------------------------------------

library(cluster)

met.dis = diana(d.euc)

met.dis$dc

# > > Número óptimo de clusters -------------------------------------------

g1 = fviz_nbclust(p.esc, FUN = hcut, method = "wss", k.max = 10) +
  ggtitle("A. Método codo")
g2 = fviz_nbclust(p.esc, FUN = hcut, method = "silhouette", k.max = 10) +
  ggtitle("B. Método silueta")
g3 = fviz_nbclust(p.esc, FUN = hcut, method = "gap_stat", k.max = 10) +
  ggtitle("C. Método estadístico gap")
gridExtra::grid.arrange(g1, g2, g3, nrow = 1)

# Coeficientes
g1$data


# Visión de cluster -------------------------------------------------------


grupos = cutree(met.agl$ward.D2, k = 3)
fviz_cluster(list(data = p.esc, cluster = grupos),
             show.clust.cent = F,
             ggtheme = theme_light())

fviz_dend(
  met.agl$ward.D2,
  horiz = TRUE,
  k = 3,
  rect = TRUE,
  rect_fill = TRUE,
  k_colors = "lancet",
  cex = 0.6,
  ylab = "Altura"
) +
  theme(title = element_blank())


# > Variables que más influyen --------------------------------------------

grupos = cutree(met.agl$ward.D2, k = 3)

aggregate(p.esc, by = list(Cluster = grupos), mean)

# K-means -----------------------------------------------------------------

k.medias = kmeans(p.esc, centers = 3, nstart = 25)

k.medias$centers

k.medias$size


pmd.p.km = aggregate(p.esc, by = list(Cluster = k.medias$cluster), mean)

pmd.p.km

paises.km = data.frame(p.esc, Cluster = k.medias$cluster)

sapply(colnames(paises.km)[1:9], function(x) {
  summary(
    aov(formula(paste0("Cluster~",x)), data = paises.km)
  )
})

fviz_cluster(k.medias,
             data = p.esc,
             palette = "lancet",
             ellipse.type = "euclid",
             star.plot = T,
             repel = T,
             ggtheme = theme_light()) +
  theme(legend.position = "none",
        plot.title = element_blank())


# Método PAM --------------------------------------------------------------

# Partición Alrededor de Medioides

# cluster::pam

met.pam = pam(p.esc, k = 3)

met.pam$medoids

fviz_cluster(met.pam,
             palette = "lancet",
             ellipse.type = "t",
             repel = T,
             ggtheme = theme_light()) +
  theme(legend.position = "none",
        plot.title = element_blank())

