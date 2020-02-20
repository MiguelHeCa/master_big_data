## importar datos

colores_ogn = readxl::read_xls("M5/data/COLORES.xls")

geomMean = function(x) prod(x) ^ (1 / length(x))

media_geom = apply(colores_ogn[, 2:7], 1, geomMean)

colores = cbind(
  media_geom,
  as.data.frame(
    lapply(colores_ogn[2:7], function(x) log(x / media_geom))
    )
  )

data = colores

# #To visualize the raw data
# rawdata
# #or for large datasets
# head(rawdata)
# 
# colores = colores_2

#data <- colores[, -1]
rownames(colores)


a <- prcomp(data, center = TRUE, scale = FALSE)
b <- summary(a)
b
plot(
  a$x[, 1:2],
  main = "PCA",
  xlab = paste(" Dim 1  (", (round(
    100 * b$importance[2, 1], digits = 1
  )), " % )"),
  ylab = paste(" Dim 2  (", (round(
    100 * b$importance[2, 2], digits = 1
  )), " % )"),
  type = "n",
  pch = 19
)
text(a$x[, 1:2], lab = colores_ogn$Pain)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

#Construction of a biplot using the function "biplot"
#The form biplot
biplot(
  a,
  choices = 1:2,
  pch = 15,
  cex = 0.8,
  cex.axis = 0.7,
  arrow.len = 0.05,
  xlab = paste(" Dim 1  (", (round(
    100 * b$importance[2, 1], digits = 1
  )), " % )"),
  ylab = paste(" Dim 2  (", (round(
    100 * b$importance[2, 2], digits = 1
  )), " % )"),
  var.axes = TRUE,
  scale = 0
)
#The covariance biplot
biplot(
  a,
  choices = 1:2,
  pch = 15,
  cex = 0.8,
  cex.axis = 0.7,
  arrow.len = 0.05,
  xlab = paste(" Dim 1  (", (round(
    100 * b$importance[2, 1], digits = 1
  )), " % )"),
  ylab = paste(" Dim 2  (", (round(
    100 * b$importance[2, 2], digits = 1
  )), " % )"),
  var.axes = TRUE,
  scale = 1
)



ghq_raw = readxl::read_excel("M5/data/GHQ.xlsx")

ghq = ghq_raw[, 31:34]


geomMean = function(x) { prod(x) ^ (1 / length(x)) }

media_geom = apply(ghq, 1, geomMean)

data2 = cbind(media_geom,
              as.data.frame(lapply(ghq, function(x)
                log(x / media_geom))))

a <- prcomp(data2, center = TRUE, scale = FALSE)
b <- summary(a)
b
plot(
  a$x[, 1:2],
  main = "PCA",
  xlab = paste(" Dim 1  (", (round(
    100 * b$importance[2, 1], digits = 1
  )), " % )"),
  ylab = paste(" Dim 2  (", (round(
    100 * b$importance[2, 2], digits = 1
  )), " % )"),
  type = "n",
  pch = 19
)
text(a$x[, 1:2], lab = paste0("P", seq(1:nrow(data2))))
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

#Construction of a biplot using the function "biplot"
#The form biplot
biplot(
  a,
  choices = 1:2,
  pch = 15,
  cex = 0.8,
  cex.axis = 0.7,
  arrow.len = 0.05,
  xlab = paste(" Dim 1  (", (round(
    100 * b$importance[2, 1], digits = 1
  )), " % )"),
  ylab = paste(" Dim 2  (", (round(
    100 * b$importance[2, 2], digits = 1
  )), " % )"),
  var.axes = TRUE,
  scale = 0
)
#The covariance biplot
biplot(
  a,
  choices = 1:2,
  pch = 15,
  cex = 0.8,
  cex.axis = 0.7,
  arrow.len = 0.05,
  xlab = paste(" Dim 1  (", (round(
    100 * b$importance[2, 1], digits = 1
  )), " % )"),
  ylab = paste(" Dim 2  (", (round(
    100 * b$importance[2, 2], digits = 1
  )), " % )"),
  var.axes = TRUE,
  scale = 1
)

GGEBiplotGUI::