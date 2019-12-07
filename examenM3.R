

# Paquetes ----------------------------------------------------------------

library(Hmisc)
library(xtable)
library(knitr)
library(kableExtra)
library(magrittr)
library(psych)
library(factoextra)
library(ggrepel)
library(cluster)
library(smacof)
library(ggthemes)

# Primera parte -----------------------------------------------------------

alim = foreign::read.spss("data/M3/PaisesProteinasExamen.sav", to.data.frame = T)

prot = Filter(is.numeric, alim)
rownames(prot) = trimws(alim$Pais)

# Correlaciones -----------------------------------------------------------


# > Tabla correlaciones LaTeX ---------------------------------------------
corstars <-
  function(x,
           method = c("pearson", "spearman"),
           removeTriangle = c("upper", "lower")) {
    #Compute correlation matrix
    x <- as.matrix(x)
    correlation_matrix <- rcorr(x, type = method[1])
    R <- correlation_matrix$r # Matrix of correlation coeficients
    p <- correlation_matrix$P # Matrix of p-value
    
    ## Define notions for significance levels; spacing is important.
    mystars <-
      ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   ")))
    
    ## trunctuate the correlation matrix to two decimal
    R <- format(round(cbind(rep(-1.11, ncol(
      x
    )), R), 3))[, -1]
    
    ## build a new matrix that includes the correlations with their apropriate stars
    Rnew <- matrix(paste(R, mystars, sep = ""), ncol = ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep = "")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep = "")
    
    ## remove upper triangle of correlation matrix
    if (removeTriangle[1] == "upper") {
      Rnew <- as.matrix(Rnew)
      Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove lower triangle of correlation matrix
    else if (removeTriangle[1] == "lower") {
      Rnew <- as.matrix(Rnew)
      Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove last column and return the correlation matrix
    Rnew <- cbind(Rnew[1:length(Rnew) - 1])
    
    return(Rnew)
  }

t_corstar = corstars(prot)
nombres_prot = c(
  "Carne roja",
  "Carne blanca",
  "Huevos",
  "Leche",
  "Pescado",
  "Cereales",
  "Féculas",
  "Frutos secos",
  "Futos y vegetales"
)
colnames(t_corstar) = nombres_prot[1:8]
rownames(t_corstar) = nombres_prot

correl = rcorr(as.matrix(prot))

r.mat = correl$r

kable(t_corstar,
      format = "latex",
      # align = c("r", "r", "r", "r", "r", "r", "r", "r" , "r"),
      align = "r",
      booktabs = T) %>%
  kable_styling(latex_options = "scale_down") %>%
  add_footnote(c(
    "p < .001 ‘***’, p < .01 ‘**’, p < .05 ‘*’",
    paste0("Determinante ", round(det(r.mat), 4))
  ))

## TODO
## library(pander)

# Pruebas de fiabilidad ---------------------------------------------------

kmo = KMO(r.mat)

kmo$Image
kmo$MSA

kmo$ImCov

a = kmo$ImCov
b = kmo$Image
c = kmo$MSAi

nuevo = matrix(NA, nrow = nrow(a), ncol = ncol(a))
nuevo[upper.tri(nuevo)] = a[upper.tri(a)]
nuevo[lower.tri(nuevo)] = b[lower.tri(b)]
diag(nuevo) = c
nuevo

bartlett = cortest.bartlett(r.mat, nrow(alim))

# AF ----------------------------------------------------------------------

af_srt = fa(prot, nfactors = ncol(r.mat), rotate = "none")
unclass(af_srt$Structure)
af_srt$Vaccounted
af_srt$communalities
af_srt$e.values

ggplot(data.frame(
  factores = 1:length(af_srt$e.values),
  eig = af_srt$e.values
  ),
  aes(x = factores, y = eig)) +
  geom_point(shape = 1) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_light() +
  labs(x = "factores",
       y = "Autovalor") +
  theme_base()

af_varimax = fa(prot,
                nfactors = 3,
                rotate = "varimax",
                scores = "Anderson")
af_varimax$scores
af_varimax$Structure
af_varimax$Vaccounted
af_varimax$communalities
af_varimax$e.values
af_varimax$uniquenesses

af_dim = data.frame(
  x = af_varimax$scores[, 1],
  y = af_varimax$scores[, 2],
  pais = trimws(alim$Pais),
  Comunista = alim$Comunista,
  Loc = alim$Localizacion
)

ggplot(af_dim, aes(x, y)) +
  geom_point(aes(col = Loc, shape = Comunista)) +
  geom_text_repel(aes(label = pais, color = Loc), show.legend = F)

X = unclass(af_varimax$Structure)
?sort
X1 = sort(X[, 1], decreasing = T)

library(data.table)

x.dt = as.data.table(unclass(af_varimax$Structure), keep.rownames = T)

x.dt[, (c("MR1", "MR2", "MR3")) := lapply(.SD, abs), .SDcols = c("MR1", "MR2", "MR3")]

x.dt[order(-MR1, -MR2, -MR3)]

Xt = t(X)

Xt[Xt[,1] == max(Xt[,1]), 1]

melt(x.dt, id.vars = "rn")

x.dt.t = dcast(melt(x.dt, id.vars = "rn"), variable ~ rn)

# MR1
x.dt.t[CarneBlanca == max(CarneBlanca), .(factor = variable, contr = CarneBlanca, var = "CarneBlanca")]
x.dt.t[Huevos == max(Huevos), .(factor = variable, contr = Huevos, var = "Huevos")]

mr1 = rbindlist(list(
  x.dt.t[CarneBlanca == max(CarneBlanca), .(factor = variable, contr = CarneBlanca, var = "CarneBlanca")],
  x.dt.t[Huevos == max(Huevos), .(factor = variable, contr = Huevos, var = "Huevos")]
))

# MR3
mr2 = rbindlist(list(
  x.dt.t[CarneRoja == max(CarneRoja), .(factor = variable, contr =  CarneRoja, var = "CarneRoja")],
  x.dt.t[Frutossecos == max(Frutossecos), .(factor = variable, contr =  Frutossecos, var = "Frutossecos")],
  x.dt.t[Frutosyvegetales == max(Frutosyvegetales), .(factor = variable, contr =  Frutosyvegetales, var = "Frutosyvegetales")],
  x.dt.t[Leche == max(Leche), .(factor = variable, contr =  Leche, var = "Leche")]
))

# MR2
mr3 = rbindlist(
  list(
    x.dt.t[Cereales == max(Cereales), .(variable, contr =  Cereales, var = "Cereales")],
    x.dt.t[`Féculas` == max(`Féculas`), .(variable, contr =  `Féculas`, var = "Féculas")],
    x.dt.t[Pescado == max(Pescado), .(variable, contr =  Pescado, var = "Pescado")]
  )
)

mr = rbindlist(list(mr1, mr3, mr2))

ggplot(af_dim, aes(x, y)) +
  geom_point(aes(col = Loc, shape = Comunista)) +
  geom_text_repel(aes(label = pais, color = Loc), show.legend = F) +
  labs(
    x = "F1: Carne blanca y huevos",
    y = "F2: Carne roja, frutos secos, frutos, vegetales y leche"
  ) +
  ggthemes::theme_base()


library(plotly)

af_dim2 = data.frame(
  nom = rownames(af_varimax$scores),
  x = af_varimax$scores[, 1],
  y = af_varimax$scores[, 2],
  z = af_varimax$scores[, 3],
  Localizacion = alim$Localizacion,
  Commies = alim$Comunista
)

p_2 = plot_ly(
  af_dim2,
  x = ~ x,
  y = ~ y,
  z = ~ z,
  color = ~ Commies
)
p_2 = add_markers(p_2)
p_2 = layout(
  p_2,
  title = 'Espacio de factores rotados con Varimax',
  scene = list(
    xaxis = list(title = "Carne blanca y huevos"),
    yaxis = list(title = "Carne roja, leche, frutos secos, frutos y vegetales"),
    zaxis = list(title = "Pescado, cereales y féculas")
  )
)

p_2


# Cluster -----------------------------------------------------------------

# Variables

d.pear = get_dist(t(prot), method = "pearson")

met.prom = hclust(d.pear, method = "average")
coe.prom = coef.hclust(met.prom)

plot(met.prom)
fviz_dend(met.prom, k = 3, horiz = T)

d2 = get_dist(t(prot), method = "euclidean")

met2 = hclust(d2, method = "ward.D2")
coe2 = coef.hclust(met2)

plot(met2)
fviz_dend(met2, k = 3, horiz = T)

# Países

prot_scl = scale(prot)
d.euclid = dist(prot_scl)
d.euclid2 = dist(prot) ^ 2

met.ward = hclust(d.euclid2, method = "ward.D2")
coe.ward = coef.hclust(met.ward)
coe.ward
met.ward_reduc = met.ward
met.ward_reduc$height = met.ward$height^(1/2)
fviz_dend(met.ward_reduc, k = 3, horiz = T)

met.ward2 = hclust(d.euclid, method = "ward.D2")
coe.ward2 = coef.hclust(met.ward2)
coe.ward2
fviz_dend(met.ward2, k = 3, horiz = T)


met.ward3 = hclust(d.euclid^2, method = "ward.D2")
coe.ward3 = coef.hclust(met.ward3)
coe.ward3
fviz_dend(met.ward3, k = 3, horiz = T)

fviz_nbclust(prot, FUNcluster = hcut, method = "wss")
fviz_nbclust(prot, FUNcluster = hcut, method = "silhouette")
fviz_nbclust(prot, FUNcluster = hcut, method = "gap_stat")

ward3.reduc = met.ward3
ward3.reduc$height = ward3.reduc$height^(1/2)
fviz_dend(ward3.reduc, k = 2, horiz = T)


# MDS ---------------------------------------------------------------------

prot_dis2_r = dist(prot) ^ 2

mds.r = mds(prot_dis2_r, type = "ordinal")

# plot(mds.r, plot.type = "confplot")
# plot(mds.r, plot.type = "stressplot")
plot(mds.r, plot.type = "Shepard")
plot(mds.r, plot.type = "resplot")
# plot(mds.r, plot.type = "bubbleplot")
# plot(mds.r, plot.type = "histogram")

dat.r = data.frame(etiq = rownames(mds.r$conf),
                   x = mds.r$conf[, 1],
                   y = mds.r$conf[, 2])

ggplot(dat.r, aes(x,-y)) +
  geom_point() +
  geom_text_repel(aes(label = etiq)) +
  theme_base()

prot_dis2_c = dist(t(prot)) ^ 2

mds.c = mds(prot_dis2_c, type = "ordinal")

plot(mds.c, plot.type = "confplot")
# plot(mds.c, plot.type = "stressplot")
plot(mds.c, plot.type = "Shepard")
plot(mds.c, plot.type = "resplot")
# plot(mds.c, plot.type = "bubbleplot")
# plot(mds.c, plot.type = "histogram")

plot_conf = plot(mds.c, plot.type = "Shepard")

dat.c = data.frame(etiq = rownames(mds.c$conf),
                   x = mds.c$conf[, 1],
                   y = mds.c$conf[, 2])

ggplot(dat.c, aes(x, y)) +
  geom_point() +
  geom_text_repel(aes(label = etiq)) +
  theme_base()

shep.df = data.frame(x = as.vector(mds.c$delta),
                     y = as.vector(mds.c$confdist))

ggplot(shep.df, aes(x, y)) +
  geom_point() +
  geom_line() +
  theme_base()

resp.df = data.frame(x = as.vector(mds.c$dhat),
                     y = as.vector(mds.c$confdist))

ggplot(resp.df, aes(x, y)) +
  geom_point(shape = 1, alpha = 0.5) +
  geom_smooth(
    method = 'loess',
    formula = y ~ x,
    se = F,
    size = 0.2
  ) +
  theme_base()

# AC ----------------------------------------------------------------------

library("FactoMineR")
library("factoextra")

t.prot = t(prot)

ac_prot = CA(t.prot, graph = FALSE)

summary(ac_prot)

autoval = get_eigenvalue(ac_prot)
autoval

1 / (nrow(t.prot) - 1)

1 / (ncol(t.prot) - 1)

fviz_screeplot(ac_prot) +
  geom_hline(yintercept = (1 / (ncol(t.prot) - 1) * 100),
             linetype = 2,
             color = "red")

# Filas

filas = get_ca_row(ac_prot)
filas$coord

fviz_ca_row(ac_prot,
            col.row = "darkgreen",
            shape.row = 15,
            repel = T) + ggthemes::theme_base()

filas$cos2

fviz_ca_row(
  ac_prot,
  col.row = "cos2",
  gradient.cols = c("red", "gold", "blue"),
  repel = TRUE
)

filas$contrib

fviz_contrib(ac_prot, choice = "row", axes = 1)
fviz_contrib(ac_prot, choice = "row", axes = 2)
fviz_contrib(ac_prot, choice = "row", axes = 3)
fviz_contrib(ac_prot, choice = "row", axes = 4)

fviz_ca_row(
  ac_prot,
  col.row = "contrib",
  gradient.cols = c("red", "gold", "blue"),
  repel = TRUE
)

# Columnas

columnas = get_ca_col(ac_prot)
columnas$coord

fviz_ca_col(ac_prot,
            col.col = "darkgreen",
            shape.col = 15,
            repel = T) + ggthemes::theme_base()

columnas$cos2

fviz_ca_col(
  ac_prot,
  col.col = "cos2",
  gradient.cols = c("red", "gold", "blue"),
  repel = TRUE
)

columnas$contrib

fviz_contrib(ac_prot, choice = "col", axes = 1)
fviz_contrib(ac_prot, choice = "col", axes = 2)
fviz_contrib(ac_prot, choice = "col", axes = 3)
fviz_contrib(ac_prot, choice = "col", axes = 4)

fviz_ca_col(
  ac_prot,
  col.col = "contrib",
  gradient.cols = c("red", "gold", "blue"),
  repel = TRUE
)

fviz_ca_biplot(ac_prot,
               map = "rowprincipal",
               repel = TRUE)

CA(t.prot)


# > Alt sin rot -----------------------------------------------------------

datos3 = foreign::read.spss("data/M3/PaisesProteinasExamenCorrespondencias.sav",
                            to.data.frame = T)

tabla = xtabs(consumo ~ PaisCat + AlimentosCat, data = datos3)

tabla = as.data.frame.matrix(tabla)

t.prot = t(tabla)

ac_prot = CA(t.prot, graph = FALSE)

summary(ac_prot)

autoval = get_eigenvalue(ac_prot)
autoval

1 / (nrow(t.prot) - 1)

1 / (ncol(t.prot) - 1)

fviz_screeplot(ac_prot) +
  geom_hline(yintercept = (1 / (ncol(t.prot) - 1) * 100),
             linetype = 2,
             color = "red")

# Filas

filas = get_ca_row(ac_prot)
filas$coord

coord.df.r = data.frame(x = filas$coord[, 1],
                        y = filas$coord[, 2],
                        etiq = rownames(filas$coord))

ggplot(coord.df.r, aes(-x, y)) +
  geom_point() +
  geom_text_repel(aes(label = etiq)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

fviz_ca_row(ac_prot,
            col.row = "darkgreen",
            shape.row = 15,
            repel = T) + ggthemes::theme_base()

filas$cos2

fviz_ca_row(
  ac_prot,
  col.row = "cos2",
  gradient.cols = c("red", "gold", "blue"),
  repel = TRUE
)

filas$contrib

fviz_contrib(ac_prot, choice = "row", axes = 1)
fviz_contrib(ac_prot, choice = "row", axes = 2)
fviz_contrib(ac_prot, choice = "row", axes = 3)
fviz_contrib(ac_prot, choice = "row", axes = 4)

fviz_ca_row(
  ac_prot,
  col.row = "contrib",
  gradient.cols = c("red", "gold", "blue"),
  repel = TRUE
)

# Columnas

columnas = get_ca_col(ac_prot)
columnas$coord

coord.df.c = data.frame(
  x = columnas$coord[, 1],
  y = columnas$coord[, 2],
  etiq = rownames(columnas$coord)
)

ggplot(coord.df.c, aes(-x, y)) +
  geom_point() +
  geom_text_repel(aes(label = etiq)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

fviz_ca_col(ac_prot,
            col.col = "darkgreen",
            shape.col = 15,
            repel = T) + ggthemes::theme_base()

columnas$cos2

fviz_ca_col(
  ac_prot,
  col.col = "cos2",
  gradient.cols = c("red", "gold", "blue"),
  repel = TRUE
)

columnas$contrib

fviz_contrib(ac_prot, choice = "col", axes = 1)
fviz_contrib(ac_prot, choice = "col", axes = 2)
fviz_contrib(ac_prot, choice = "col", axes = 3)
fviz_contrib(ac_prot, choice = "col", axes = 4)

fviz_ca_col(
  ac_prot,
  col.col = "contrib",
  gradient.cols = c("red", "gold", "blue"),
  repel = TRUE
)

fviz_ca_biplot(ac_prot,
               map = "rowprincipal",
               repel = TRUE)

CA(t.prot)






ac_prot = CA(prot, graph = FALSE)

summary(ac_prot)

autoval = get_eigenvalue(ac_prot)
autoval

1 / (nrow(prot) - 1)

1 / (ncol(prot) - 1)

fviz_screeplot(ac_prot) +
  geom_hline(yintercept = (1 / (ncol(prot) - 1) * 100),
             linetype = 2,
             color = "red")

# Filas

filas = get_ca_row(ac_prot)
filas$coord

fviz_ca_row(ac_prot,
            col.row = "darkgreen",
            shape.row = 15,
            repel = T) + ggthemes::theme_base()

filas$cos2

fviz_ca_row(
  ac_prot,
  col.row = "cos2",
  gradient.cols = c("red", "gold", "blue"),
  repel = TRUE
)

filas$contrib

fviz_contrib(ac_prot, choice = "row", axes = 1)
fviz_contrib(ac_prot, choice = "row", axes = 2)
fviz_contrib(ac_prot, choice = "row", axes = 3)
fviz_contrib(ac_prot, choice = "row", axes = 4)

fviz_ca_row(
  ac_prot,
  col.row = "contrib",
  gradient.cols = c("red", "gold", "blue"),
  repel = TRUE
)

# Columnas

columnas = get_ca_col(ac_prot)
columnas$coord

fviz_ca_col(ac_prot,
            col.col = "darkgreen",
            shape.col = 15,
            repel = T) + ggthemes::theme_base()

columnas$cos2

fviz_ca_col(
  ac_prot,
  col.col = "cos2",
  gradient.cols = c("red", "gold", "blue"),
  repel = TRUE
)

columnas$contrib

fviz_contrib(ac_prot, choice = "col", axes = 1)
fviz_contrib(ac_prot, choice = "col", axes = 2)
fviz_contrib(ac_prot, choice = "col", axes = 3)
fviz_contrib(ac_prot, choice = "col", axes = 4)

fviz_ca_col(
  ac_prot,
  col.col = "contrib",
  gradient.cols = c("red", "gold", "blue"),
  repel = TRUE
)

fviz_ca_biplot(ac_prot, repel = TRUE)

CA(prot)

plot(ac_prot)

# LDA ---------------------------------------------------------------------

library(tidyverse)
library(caret)
library(MASS)


# Load the data
data("iris")
# Split the data into training (80%) and test set (20%)
set.seed(123)

training.samples <- iris$Species %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data <- iris[training.samples,]
test.data <- iris[-training.samples,]

# Estimate preprocessing parameters
preproc.param <- train.data %>%
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)

# Fit the model
model <- lda(Species ~ ., data = train.transformed)
model

# Make predictions
predictions <- model %>% predict(test.transformed)
names(predictions)

# Predicted classes
head(predictions$class, 6)
# Predicted probabilities of class memebership.
head(predictions$posterior, 6)




# Linear discriminants
head(predictions$x, 3)

lda.data <- cbind(train.transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Species))

# Model accuracy
mean(predictions$class == test.transformed$Species)

sum(predictions$posterior[, 1] >= .5)

iris.lda = lda(Species ~ . , data = iris)
table(predict(iris.lda, type = "class")$class, iris$Species)

table(predictions$class, iris$Species)

hist(prot$CarneRoja)

lapply(prot, nortest::lillie.test)

par(mfrow = c(1,1))
for(i in 1:9) {
  hist(prot[[i]])
}
