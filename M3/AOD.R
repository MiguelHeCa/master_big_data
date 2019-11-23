
# Carga de datos ----------------------------------------------------------

datos = foreign::read.spss("data/M3/Datos Mundo sin tipificar.sav", to.data.frame = TRUE)

nombres = c(
  "Paises",
  "GDPpcUSD03",
  "DecPNBpc90",
  "Infla90_03",
  "Pobre99_02",
  "Salud99-02",
  "Educa99-02",
  "Defen99-02",
  "Flujos_AOD"
)

colnames(datos) = nombres

str(datos)

apply(datos, 2, function(x) sum(is.na(x)))

dat_esc = scale(datos[, 2:ncol(datos)])
rownames(dat_esc) = datos[, 1]

boxplot(dat_esc)

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

atipicos = unlist(apply(dat_esc, 2, detec_atip))

cor = cor(dat_esc)

corrplot::corrplot(cor)

distancia = dist(dat_esc)

metodos = c("ward.D2",
            "single",
            "complete",
            "average",
            "mcquitty")
names(metodos) = metodos

met.agl = lapply(metodos, function(x) hclust(distancia, method = x))
coe_agl = sapply(met.agl, cluster::coef.hclust)
coe_agl = round(coe_agl, digits = 2)

ward = hclust(distancia, method = "ward.D2")


library(ggplot2)
library(factoextra)

graf = fviz_dend(
  ward,
  horiz = T,
  main = "Método de Ward",
  xlab = "Cluster",
  ylab = "Altura",
  cex = 0.3
)
graf

g1 = fviz_nbclust(dat_esc, FUN = hcut, method = "wss", k.max = 10) +
  ggtitle("A. Método codo")
g2 = fviz_nbclust(dat_esc, FUN = hcut, method = "silhouette", k.max = 10) +
  ggtitle("B. Método silueta")
g3 = fviz_nbclust(dat_esc, FUN = hcut, method = "gap_stat", k.max = 10) +
  ggtitle("C. Método estadístico gap")
gridExtra::grid.arrange(g1, g2, g3, nrow = 1)

# Coeficientes
g1$data

fviz_dend(
  ward,
  horiz = TRUE,
  k = 5,
  rect = TRUE,
  rect_fill = TRUE,
  k_colors = "lancet",
  cex = 0.3,
  ylab = "Altura"
) +
  theme(title = element_blank())

grupos = cutree(ward, k = 5)

influencia = aggregate(dat_esc, by = list(Cluster = grupos), mean)
influencia

sort(influencia[1, 2:ncol(influencia)], decreasing = T)
sort(influencia[2, 2:ncol(influencia)], decreasing = T)
sort(influencia[3, 2:ncol(influencia)], decreasing = T)
sort(influencia[4, 2:ncol(influencia)], decreasing = T)
sort(influencia[5, 2:ncol(influencia)], decreasing = T)

pai_grps = lapply(1:5, function(x) grupos[grupos == x])

paises_cl = data.frame(dat_esc, grupos)

anova = aov(grupos ~ ., data = paises_cl)
summary(anova)



# Segunda vuelta ----------------------------------------------------------

seleccion = nombres[nombres %in% c("Paises", "GDPpcUSD03", "Salud99-02", "Educa99-02", "Defen99-02")]
datos_2 = datos[, seleccion]

dat_esc = scale(datos_2[, 2:ncol(datos_2)])
rownames(dat_esc) = datos_2[, 1]

boxplot(dat_esc)

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

atipicos = unlist(apply(dat_esc, 2, detec_atip))

cor = cor(dat_esc)

corrplot::corrplot(cor)

distancia = dist(dat_esc)

metodos = c("ward.D2",
            "single",
            "complete",
            "average",
            "mcquitty")
names(metodos) = metodos

met.agl = lapply(metodos, function(x) hclust(distancia, method = x))
coe_agl = sapply(met.agl, cluster::coef.hclust)
coe_agl = round(coe_agl, digits = 2)
coe_agl

ward = hclust(distancia, method = "ward.D2")

graf = fviz_dend(
  ward,
  horiz = T,
  main = "Método de Ward",
  xlab = "Cluster",
  ylab = "Altura",
  cex = 0.3
)
graf

g1 = fviz_nbclust(dat_esc, FUN = hcut, method = "wss", k.max = 10) +
  ggtitle("A. Método codo")
g2 = fviz_nbclust(dat_esc, FUN = hcut, method = "silhouette", k.max = 10) +
  ggtitle("B. Método silueta")
g3 = fviz_nbclust(dat_esc, FUN = hcut, method = "gap_stat", k.max = 10) +
  ggtitle("C. Método estadístico gap")
gridExtra::grid.arrange(g1, g2, g3, nrow = 1)

# Coeficientes
g1$data

fviz_dend(
  ward,
  horiz = TRUE,
  k = 4,
  rect = TRUE,
  rect_fill = TRUE,
  k_colors = "lancet",
  cex = 0.3,
  ylab = "Altura"
) +
  theme(title = element_blank())

grupos = cutree(ward, k = 4)

influencia = aggregate(dat_esc, by = list(Cluster = grupos), mean)
influencia

sort(influencia[1, 2:ncol(influencia)], decreasing = T)
sort(influencia[2, 2:ncol(influencia)], decreasing = T)
sort(influencia[3, 2:ncol(influencia)], decreasing = T)
sort(influencia[4, 2:ncol(influencia)], decreasing = T)


pai_grps = lapply(1:5, function(x) grupos[grupos == x])

paises_cl = data.frame(dat_esc, grupos)

anova = aov(grupos ~ ., data = paises_cl)
summary(anova)


