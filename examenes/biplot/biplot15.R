## paquetes

library(MultBiplotR)
load("examenes/biplot/Datos15.rda")

Vinos = Datos
names(Vinos)[1] = "año"

# Expt --------------------------------------------------------------------

library(skimr)

skimr(Vinos)

# HJ-Biplot ---------------------------------------------------------------

vinnum = Vinos[, 4:21]

HJ_biplot = HJ.Biplot(vinnum)

HJ_clust = AddCluster2Biplot(
  HJ_biplot,
  NGroups = 4,
  ClusterType = "hi",
  method = "ward.D",
  Original = TRUE
)

plot(HJ_clust, PlotClus = TRUE, margin = 0.2)

clusteres = lapply(1:4, function(x) {
  clu = HJ_clust$Clusters[HJ_clust$Clusters == x]
  vinos_id = data.frame(id = rownames(Vinos), Vinos[, 1:3])
  dat = vinos_id[vinos_id$id %in% names(clu),]
  return(dat)
})
names(clusteres) = c("uno", "dos", "tres", "cuatro")

clusteres

# Canónico ----------------------------------------------------------------

ald = MASS::lda(vinnum, Vinos$denomina)

plot(ald, dimen=1, type="both", col = "steelblue")

canonico = Canonical.Variate.Analysis(vinnum, group = Vinos$grupo)
plot(canonico)
