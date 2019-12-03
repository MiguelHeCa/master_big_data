
# Paquetes ----------------------------------------------------------------

library(Hmisc)
library(xtable)
library(knitr)
library(kableExtra)
library(magrittr)
library(psych)

# Primera parte -----------------------------------------------------------

datos1 = foreign::read.spss("data/M3/PaisesProteinasExamen.sav", to.data.frame = T)

prot = Filter(is.numeric, datos1)
rownames(prot) = trimws(datos1$Pais)

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
    )), R), 3))[,-1]
    
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

kable(
  t_corstar,
  format = "latex",
  # align = c("r", "r", "r", "r", "r", "r", "r", "r" , "r"),
  align = "r",
  booktabs = T
) %>%
  kable_styling(latex_options = "scale_down") %>% 
  add_footnote(c("p < .001 ‘***’, p < .01 ‘**’, p < .05 ‘*’",
                 paste0("Determinante ", round(det(r.mat), 4)))
  )

## TODO
## library(pander)

# Pruebas de fiabilidad ---------------------------------------------------

kmo = KMO(r.mat)

kmo$Image
kmo$MSA

kmo$ImCov

mat.anti = kmo$Image

mat.anti2 = diag(kmo$MSAi)

mat.anti[lower.tri(mat.anti2)] = lower.tri(mat.anti)

mat.anti[diag(mat.anti)] = kmo$MSAi

a = kmo$ImCov
b = kmo$Image
c = kmo$MSAi

nuevo = matrix(NA, nrow = nrow(a), ncol = ncol(a))
nuevo[upper.tri(nuevo)] = a[upper.tri(a)]
nuevo[lower.tri(nuevo)] = b[lower.tri(b)]
diag(nuevo) = c
nuevo

bartlett = cortest.bartlett(r.mat, nrow(datos1))

# AF ----------------------------------------------------------------------

af_srt = fa(prot, nfactors = ncol(r.mat), rotate = "none")
unclass(af_srt$Structure)
af_srt$Vaccounted
af_srt$communalities
af_srt$e.values

ggplot(data.frame(factores = 1:length(af_srt$e.values), eig = af_srt$e.values),
       aes(x = factores, y = eig)) +
  geom_point(shape = 1) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_light() +
  labs(
    x = "factores",
    y = "Autovalor"
  )

af_varimax = fa(prot, nfactors = 3, rotate = "varimax", scores = "Anderson")
af_varimax$scores
af_varimax$Structure
af_varimax$Vaccounted
af_varimax$communalities
af_varimax$e.values
af_varimax$uniquenesses

af_dim = data.frame(
  x = af_varimax$scores[,1],
  y = af_varimax$scores[,2],
  pais = trimws(datos1$Pais),
  Comunista = datos1$Comunista,
  Loc = datos1$Localizacion)

ggplot(af_dim, aes(x, y)) +
  geom_text(aes(label = pais), vjust = 0, hjust = 1, nudge_x = -0.05) +
  geom_point(aes(col = Loc, shape = Comunista))

# Cluster -----------------------------------------------------------------

library(FactoMineR)
library(factoextra)
library(cluster)



prot_scl = scale(prot)

d.pearson = get_dist(prot, method = "pearson")
d.pear = get_dist(prot_scl, method = "pearson")

d.euclid = dist(prot_scl)
d.euclid2 = d.euclid^2

met.prom = hclust(d.pear, method = "average")
coe.prom = coef.hclust(met.prom)

plot(met.prom)
fviz_dend(met.prom, k = 3, horiz = T)

met.ward = hclust(d.euclid2, method = "ward.D2")
coe.ward = coef.hclust(met.ward)
coe.ward
fviz_dend(met.ward, k = 3, horiz = T)

plot(met.ward)

d.pearson = get_dist(prot, method = "pearson")
met.prom2 = hclust(d.pearson, method = "average")
coe.prom2 = coef.hclust(met.prom2)
fviz_dend(met.prom2, k = 3, horiz = T)

d.euc = dist(prot)
d.euc2 = d.euc^2
met.ward2 = hclust(d.euc2, method = "ward")
coe.ward2 = coef.hclust(met.ward2)
coe.ward2
fviz_dend(met.ward2, k = 3, horiz = T)

fviz_nbclust(prot, FUNcluster = hcut, method = "wss")
fviz_nbclust(prot, FUNcluster = hcut, method = "silhouette")
fviz_nbclust(prot, FUNcluster = hcut, method = "gap_stat")

fviz_dend(met.ward2, k = 2, horiz = T)
fviz_dend(met.prom2, k = 2, horiz = T)

# MDS ---------------------------------------------------------------------



# AC ----------------------------------------------------------------------


