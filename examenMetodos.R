## ----setup, include=FALSE-----------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_knit$set(kable.force.latex=FALSE)
options(knitr.kable.NA = '')

library(Hmisc)
library(magrittr)
library(psych)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(cluster)
library(smacof)
library(ggthemes)
library(skimr)


## ---- include=FALSE-----------------------------------------------------------------------------------------
alim = foreign::read.spss("data/M3/PaisesProteinasExamen.sav", to.data.frame = T)
alim$Pais = trimws(alim$Pais)

prot = Filter(is.numeric, alim)
rownames(prot) = alim$Pais

proteinas = alim
colnames(proteinas) = c("País", "Carne roja", "Carne blanca", "Huevos",
                        "Leche", "Pescado", "Cereales", "Féculas", 
                        "Frutos secos", "Frutas y vegetales", "Comunista",
                        "Localización")

mi_skim =
  skim_with(
    factor = sfl(
      ordered = NULL
    ),
    numeric = sfl(hist = NULL)
  )

eskim = mi_skim(proteinas)

eskim$n_missing = NULL
eskim$complete_rate = NULL

res_eskim = summary(eskim)
rownames(res_eskim) = c(
  "Nombre",
  "Número de filas",
  "Número de columnas",
  "_________________________ ",
  "Frecuencia de cada tipo: ",
  "character",
  "factor", 
  "numeric",
  "_________________________  ",
  "Variables de agrupación"
)
res_eskim[10, 1] = "Ninguna"


## ----gen----------------------------------------------------------------------------------------------------
knitr::kable(
  res_eskim,
  caption = "Resumen general",
  col.names = "Valores"
)


## ----num----------------------------------------------------------------------------------------------------
knitr::kable(
  partition(eskim)[[3]],
  caption = "Resumen de variables de composición alimenticia",
  col.names = c("Variable", "Media", "D.E.", "Q1", "Q2", "Q3", "Q4", "Q5"),
  digits = 3
)


## ----fac----------------------------------------------------------------------------------------------------
knitr::kable(
  partition(eskim)[[2]],
  caption = "Resumen de variables categóricas",
  col.names = c("Variable", "Único", "Conteo")
)


## ----corr---------------------------------------------------------------------------------------------------
corstars =
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
      ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    ")))
    
    ## trunctuate the correlation matrix to two decimal
    R <- format(round(cbind(rep(-1.11, ncol(
      x
    )), R), 3))[, -1]
    
    ## build a new matrix that includes the correlations with their apropriate stars
    Rnew <- matrix(paste(mystars, R, sep = ""), ncol = ncol(x))
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
  "Frutas y vegetales"
)
colnames(t_corstar) = nombres_prot[1:8]
rownames(t_corstar) = nombres_prot

correl = rcorr(as.matrix(prot))

r.mat = correl$r

knitr::kable(
  t_corstar[2:9, ],
  format = "latex",
  align = rep("r", times = 8),
  caption = "Correlaciones entre variables de composición alimenticia",
  booktabs = T
) %>%
  kableExtra::kable_styling(latex_options = "scale_down") %>%
  kableExtra::add_footnote(c(
    "p < .001 ‘***’, p < .01 ‘**’, p < .05 ‘*’",
    paste0("Determinante ", round(det(r.mat), 4))
  ))


## ----bartlett-----------------------------------------------------------------------------------------------
bartlett = cortest.bartlett(r.mat, nrow(prot))
bartlett = as.data.frame.list(bartlett)
bartlett[1, 2] = as.character(signif(bartlett[1,2], 2))

knitr::kable(as.data.frame.list(bartlett),
             caption = "Prueba de esfericidad de Bartlett",
             col.names = c("$\\chi^2$", "p-valor", "g.l."),
             align = c("r", "r", "r"),
             booktabs = TRUE,
             escape = FALSE)


## ----kmo----------------------------------------------------------------------------------------------------
kmo = psych::KMO(prot)

mat.kmo = matrix(format(round(kmo$MSAi, 3), 3), nrow = 1, ncol = 9)
mat.kmo = rbind(mat.kmo, c(round(kmo$MSA, 3), rep(NA, times = 8)))
rownames(mat.kmo) = c("MSA ind.", "MSA global")
colnames(mat.kmo) = c("C.r.", "C.b.", "H.", "L.", "P.", "C.", "F.", "F.s.", "F.v.")

knitr::kable(mat.kmo,
             format = "pandoc",
             caption = "Prueba de adecuación Kaiser-Meyer-Olkin",
             align = rep("r", times = 9))


## ----af-srt-str---------------------------------------------------------------------------------------------
af_srt = fa(prot, nfactors = ncol(r.mat), rotate = "none")

eig.ini = af_srt$e.values
eig.prp = eig.ini / sum(eig.ini)
eig.cum = cumsum(eig.prp)
af.ini.var = rbind(eig.ini, eig.prp, eig.cum)
af.ini.var = round(af.ini.var, 3)

af.srt.var = round(af_srt$Vaccounted[1:3, ], 3)
af.srt.vartab = rbind(rep(NA, times = 9),
                      format(af.ini.var, 3),
                      rep(NA, times = 9),
                      format(af.srt.var, 3))
rownames(af.srt.vartab) = NULL

af.srt.vt = data.frame(
  X = c(
    "**Inicial**",
    "Valores propios",
    "% varianza",
    "% var. acumulada",
    "**Sin rotar**",
    "Cargas",
    "% varianza",
    "% var. acumulada"
  )
)

af.srt.vt = cbind(af.srt.vt, af.srt.vartab)

knitr::kable(af.srt.vt,
             format = "pandoc",
             caption = "Varianza explicada de análisis de factores sin rotación",
             col.names = c("", paste0("M", 1:9)),
             align = c("l", rep("r", times = 8)))



## ----codo, fig.cap="Gráfico de sedimentos de valores propios", fig.width=4, fig.asp=0.7, fig.pos='b'--------
ggplot(data.frame(
  factores = 1:length(af_srt$e.values),
  eig = af_srt$e.values
  ),
  aes(x = factores, y = eig)) +
  geom_point(color = "#505050") +
  geom_line(color = "#505050") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = 1:9) +
  labs(x = "Factores",
       y = "Valores propios") +
  theme_tufte(base_family = "sans") +
  theme(panel.background = element_rect(colour = "black"),
        text = element_text(size = 10))


## ----matvmx, warning=FALSE----------------------------------------------------------------------------------
af_varimax = fa(prot,
                nfactors = 3,
                rotate = "varimax",
                scores = "Anderson")

nom_var_c = c("C.r.", "C.b.", "H.", "L.", "P.", "C.", "F.", "F.s.", "F.v.")
nom_var_r = colnames(proteinas)[2:10]

matvmx = t(round(unclass(af_varimax$Structure), 3))
matvmx = rbind(matvmx,
      round(af_varimax$communalities, 3),
      round(af_varimax$uniquenesses, 3))
rownames(matvmx) = c("MR1", "MR2", "MR3", "Comunalidades", "Unicidades")
colnames(matvmx) = nom_var_c

knitr::kable(matvmx, format = "pandoc", caption = "Cargas factoriales con rotación VARIMAX")


## ----afvrmx-------------------------------------------------------------------------------------------------
af.vmx.var = round(af_varimax$Vaccounted[1:3, ], 3)
af.vmx.vartab = rbind(rep(NA, times = 3),
                      format(af.ini.var[, 1:3], 3),
                      rep(NA, times = 3),
                      format(af.vmx.var, 3))
rownames(af.vmx.vartab) = NULL

af.vmx.vt = data.frame(
  Y = c(
    "**Inicial**",
    "Valores propios",
    "% varianza",
    "% var. acumulada",
    "**Rotación VARIMAX**",
    "Cargas",
    "% varianza",
    "% var. acumulada"
  )
)

af.vmx.vt = cbind(af.vmx.vt, af.vmx.vartab)

knitr::kable(af.vmx.vt,
             format = "pandoc",
             caption = "Varianza explicada de análisis de factores con rotación VARIMAX",
             col.names = c("", paste0("M", 1:3)),
             align = c("l", rep("r", times = 3)))



## ----grafact, fig.cap="Gráfico factorial con puntaciones Anderson-Rubin", fig.width=6, fig.asp=0.7----------
af_dim = data.frame(
  x = af_varimax$scores[, 1],
  y = af_varimax$scores[, 2],
  pais = trimws(alim$Pais),
  Comunista = alim$Comunista,
  Loc = alim$Localizacion
)

ggplot(af_dim, aes(x, y)) +
  geom_point(aes(col = Loc, shape = Comunista)) +
  geom_text_repel(aes(label = pais, color = Loc), show.legend = F, size = 2.5) +
  labs(
    x = "F1: Carne blanca y huevos",
    y = "F2: Carne roja, leche, frutos secos,\nfrutas y vegetales"
  ) +
  theme_tufte(base_family = "sans") +
  theme(panel.background = element_rect(colour = "black"),
        text = element_text(size = 10))


## ----cluavg, fig.cap="Dendrograma por método entre grupos y distancia de Pearson", fig.width=4, fig.asp=0.9----
prot.clust = prot
colnames(prot.clust) = nom_var_r

d.pear = get_dist(t(prot.clust), method = "pearson")
met.prom = hclust(d.pear, method = "average")
coe.prom = coef.hclust(met.prom)

fviz_dend(
  met.prom,
  k = 3,
  horiz = T,
  repel = T,
  lwd = 0.5,
  cex = 0.5,
  type = "circular"
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )


## ----clueuc, fig.cap="Dendrograma por método entre grupos y distancia de Pearson", fig.width=5, fig.asp=1----
met.ward2 = hclust(dist(prot.clust)^2, method = "ward.D2")
ward2.reduc = met.ward2
ward2.reduc$height = ward2.reduc$height^(1/3)
fviz_dend(
  ward2.reduc,
  k = 3,
  horiz = T,
  repel = T,
  lwd = 0.4,
  cex = 0.5,
  type = "circular"
) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )


## -----------------------------------------------------------------------------------------------------------
prot_dis2_c = dist(t(prot)) ^ 2
mds.c = mds(prot_dis2_c, type = "ordinal")
dat.c = data.frame(etiq = nom_var_r,
                   x = mds.c$conf[, 1],
                   y = mds.c$conf[, 2])

prot_dis2_r = dist(prot) ^ 2

mds.r = mds(prot_dis2_r, type = "ordinal")

dat.r = data.frame(etiq = rownames(mds.r$conf),
                   x = mds.r$conf[, 1],
                   y = mds.r$conf[, 2])


## ----mdsvar, fig.cap="Plano para variables (A) y países (B) con solución inicial de Torgerson y distancia euclídea al cuadrado", fig.asp=0.6----
fig.mds.c = ggplot(dat.c, aes(x, y)) +
  geom_point() +
  geom_text_repel(aes(label = etiq), size = 2.5) +
  labs(
    x = "Dimensión 1",
    y = "Dimensión 2"
  ) +
  theme_tufte(base_family = "sans") +
  theme(panel.background = element_rect(colour = "black"),
        text = element_text(size = 10))

fig.mds.r = ggplot(dat.r, aes(x,-y)) +
  geom_point() +
  geom_text_repel(aes(label = etiq), size = 2) +
  labs(
    x = "Dimensión 1",
    y = "Dimensión 2"
  ) +
  theme_tufte(base_family = "sans") +
  theme(panel.background = element_rect(colour = "black"),
        text = element_text(size = 10))

library(patchwork)

fig.mds.c + fig.mds.r + plot_annotation(tag_levels = 'A')


## ----shep, fig.cap="Diagrama de Shepard para variables y países"--------------------------------------------
par(mfrow = c(1, 2))
plot(mds.c, plot.type = "Shepard", main = "Variables", xlab = "Disimilaridades", ylab = "Distancias configuradas",
  cex.main = 1,
  cex.lab = 0.8,
  cex.axis = 0.8)
plot(mds.r, plot.type = "Shepard", main = "Países", xlab = "Disimilaridades", ylab = "Distancias configuradas",
  cex.main = 1,
  cex.lab = 0.8,
  cex.axis = 0.8)


## ----resi, fig.cap="Gráfico de residuos para variables y países"--------------------------------------------
par(mfrow = c(1, 2))

plot(mds.c, plot.type = "resplot", main = "Variables", xlab = "Proximidades transformadas", ylab = "Distancias configuradas",
  cex.main = 1,
  cex.lab = 0.8,
  cex.axis = 0.8)
plot(mds.r, plot.type = "resplot", main = "Países", xlab = "Proximidades transformadas", ylab = "Distancias configuradas",
  cex.main = 1,
  cex.lab = 0.8,
  cex.axis = 0.8)



## -----------------------------------------------------------------------------------------------------------
t.prot = t(prot)
rownames(t.prot) = nom_var_r

ac_prot = CA(t.prot, graph = FALSE)


## ----chi2ca, warning=FALSE----------------------------------------------------------------------------------
chi2 = chisq.test(prot)

chi2tab = matrix(c(format(round(chi2$statistic, 3), 3), chi2$parameter, as.character(signif(chi2$p.value, 3))), nrow = 1)
colnames(chi2tab) =  c("$\\chi^2$", "g.l.", "p-valor")

knitr::kable(chi2tab,
             caption = "Prueba de Chi cuadrado de correspondencias",
             format = "pandoc",
             align = rep("r", times = 3)
             )


## ----eig-ca-------------------------------------------------------------------------------------------------
eig.ca = round(t(ac_prot$eig), 3)
rownames(eig.ca) = c("Valores propios", "% varianza", "% var. acumulada")
knitr::kable(eig.ca,
             format = "pandoc",
             caption = "Valores propios de las correspondencias")


## ----cos-f, fig.cap="Calidad de representación de variables"------------------------------------------------
cos2.f = fviz_ca_row(
  ac_prot,
  col.row = "cos2",
  gradient.cols = c("#f46d43", "#fee08b", "#1a9850"),
  repel = TRUE,
  labelsize = 3
) + 
  theme_tufte(base_family = "sans") +
  theme(panel.background = element_rect(colour = "black"),
        text = element_text(size = 10),
        plot.title = element_blank())
cos2.f


## ----cont-f, fig.cap="Contribuciones de variables a las dimensiones"----------------------------------------
cont.f = fviz_ca_row(
  ac_prot,
  col.row = "contrib",
  gradient.cols = c("#d6604d", "#dedede", "#4393c3"),
  repel = TRUE,
  labelsize = 3
) + 
  theme_tufte(base_family = "sans") +
  theme(panel.background = element_rect(colour = "black"),
        text = element_text(size = 10),
        plot.title = element_blank())

cont.f


## ----cos-c, fig.cap="Calidad de representación de países"---------------------------------------------------
cos2.c = fviz_ca_col(
  ac_prot,
  col.col = "cos2",
  gradient.cols = c("#f46d43", "#fee08b", "#1a9850"),
  repel = TRUE,
  labelsize = 3
) + 
  theme_tufte(base_family = "sans") +
  theme(panel.background = element_rect(colour = "black"),
        text = element_text(size = 10),
        plot.title = element_blank())

cos2.c


## ----cont-c, fig.cap="Contribuciones de países a las dimensiones"-------------------------------------------
cont.c = fviz_ca_col(
  ac_prot,
  col.col = "contrib",
  gradient.cols = c("#d6604d", "#dedede", "#4393c3"),
  repel = TRUE,
  labelsize = 3
) + 
  theme_tufte(base_family = "sans") +
  theme(panel.background = element_rect(colour = "black"),
        text = element_text(size = 10),
        plot.title = element_blank())

cont.c


## ----biplot, fig.cap="Similitudes entre variables y países"-------------------------------------------------
biplot = fviz_ca_biplot(ac_prot, repel = TRUE, labelsize = 3) + 
  theme_tufte(base_family = "sans") +
  theme(panel.background = element_rect(colour = "black"),
        text = element_text(size = 10),
        plot.title = element_blank())
biplot

