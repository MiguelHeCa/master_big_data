

library(ggplot2)

calif_clases = readRDS("data/clases.rds")

ggplot(calif_clases, aes(x = Matematicas, y = Naturales)) +
  geom_point(color = "blue") +
  ggtitle("Calficaciones de matemáticas vs calificaciones de ciencias naturales")

calif_escala = as.data.frame(apply(calif_clases, 2, function(x) {x - mean(x)}))

ggplot(calif_escala, aes(x = Matematicas, y = Naturales)) +
  geom_point(color = "blue") +
  labs(
    title = "Calificaciones de matemáticas vs calificaciones de ciencias naturales",
    subtitle = "Media escalada"
  )
  
# Encontrar los valores propios

calif_corr = cor(calif_clases)

calif_cov = cov(calif_escala)
calif_eigen = eigen(calif_cov)

# Ponemos los nombres adecuados
rownames(calif_eigen$vectors) = c("Matematicas", "Naturales")
colnames(calif_eigen$vectors) = c("CP1", "CP2")

# La suma de los valorers propios es igual a la varianza total de los datos
sum(calif_eigen$values)

var(calif_escala[,"Matematicas"]) + var(calif_escala[,"Naturales"])

# Los vectores propios son los componentes principales.

cargas = calif_eigen$vectors

# Calculamos las pendientes

cp1_pendiente = calif_eigen$vectors["Matematicas","CP1"] / calif_eigen$vectors["Naturales","CP1"]
cp2_pendiente = calif_eigen$vectors["Matematicas","CP2"] / calif_eigen$vectors["Naturales","CP2"]

cp1_coord = paste0(
  "(",
  round(calif_eigen$vectors[1, 1], digits = 3),
  ", ",
  round(calif_eigen$vectors[2, 1], digits = 3),
  ")"
)

cp2_coord = paste0(
  "(",
  round(calif_eigen$vectors[1, 2], digits = 3),
  ", ",
  round(calif_eigen$vectors[2, 2], digits = 3),
  ")"
)

ggplot(calif_escala, aes(x = Matematicas, y = Naturales)) +
  geom_point(color = "blue") +
  geom_abline(slope = cp1_pendiente, intercept = 0, color = "red") +
  geom_abline(slope = cp2_pendiente, intercept = 0, color = "green") +
  annotate(
    geom = "text",
    x = 10,
    y = 7.5,
    label = cp1_coord,
    color = "red"
  ) +
  annotate(
    geom = "text",
    x = -7.5,
    y = 10,
    label = cp2_coord,
    color = "darkgreen"
  ) +
  labs(title = "Calificaciones de matemáticas vs calificaciones de ciencias naturales",
       subtitle = "Media escalada")

# Variación de cada vector propio

cp1_var = round(calif_eigen$values[1] / sum(calif_eigen$values), digits = 2)
cp2_var = round(calif_eigen$values[2] / sum(calif_eigen$values), digits = 2)

# Multiplicamos los datos escalados por los vectores propios (componentes principales)

puntuaciones = as.data.frame(as.matrix(calif_escala) %*% cargas)

d.est = sqrt(calif_eigen$values)

ggplot(puntuaciones, aes(CP1, CP2)) +
  geom_point(color = "blue") +
  geom_abline(slope = 0, intercept = 0, color = "red") +
  geom_abline(slope = 90, intercept = 0, color = "green") +
  labs(
    title = "Datos en términos de vectores propios / CP",
    x = paste0("CP1 - ", cp1_var, "% de variación"),
    y = paste0("CP2 - ", cp2_var, "% de variación")
  ) +
  coord_cartesian(ylim = c(-20, 20))
