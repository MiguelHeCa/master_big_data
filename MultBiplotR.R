
# install.packages(
#   "http://biplot.usal.es/classicalbiplot/multbiplot-in-r/multbiplotr_0332tar.gz",
#   repos = NULL,
#   type = "source"
# )
# 
# install.packages(
#   "http://biplot.usal.es/classicalbiplot/multbiplot-in-r/multbiplotrgui_0332tar.gz",
#   repos = NULL,
#   type = "source"
# )

# paquetes = c(
#   "scales",
#   "geometry",
#   "deldir",
#   "rgl",
#   "mirt",
#   "GPArotation",
#   "MASS",
#   "lattice",
#   "dae",
#   "gWidgets",
#   "gWidgetsRGtk2",
#   "RGtk2",
#   "foreign"
# )
# por_instal = paquetes[!paquetes %in% installed.packages()[, 1]]
# 
# if (length(por_instal) > 0)
#   install.packages(por_instal)

# local({
#   r <- getOption("repos")
#   r["CRAN"] <- "http://cran.cnr.berkeley.edu/"
#   options(repos = r)
# })
# install.packages(c("scales", "geometry", "deldir", "rgl", "mirt", "GPARotation",
# "MASS", "kde2d", "lattice", "splom", "dae"))
# install.packages("http://biplot.usal.es/multbiplot/multbiplot-in-r/multbiplotr_19
# 1119tar.gz", repos = NULL, type="source")

install.packages(c("scales", "geometry", "deldir", "rgl", "mirt", "GPARotation", "MASS", "kde2d", "lattice", "splom", "dae"))
install.packages("http://biplot.usal.es/multbiplot/multbiplot-in-r/multbiplotr_191119tar.gz", repos = NULL, type="source")

install.packages("ape")
install.packages("vegan")


if(!"MultBiplotR" %in% installed.packages()[,1]) {
  devtools::install_github("villardon/MultBiplotR", dependencies = TRUE)
  }

library(MultBiplotR)

data("RAPD")
help(RAPD)

Origin=c("B", "B",  "B", "B", "B",  "B", "B", "B",  "B", "B", "B",  "B", "B",
         "B", "SP", "C", "C", "Co",  "Co",  "CP", "CP", "CP", "CP", "CL", "MEX",
         "POJ", "POJ", "Ragnar", "PR", "PR", "PR", "PR", "PR",  "V",  "V", "V",
         "V", "V",  "V",  "V", "V", "V", "V", "V", "V", "V", "V", "V", "V", "V")

Origin = as.factor(Origin)
# No usar otra que no sea Jaccard a menos que uno sepa lo que está haciendo.

# Simple Analysis of Principal Coordinates
help(BinaryProximities)
Dis = BinaryProximities(RAPD, coefficient = 4)
pco = PrincipalCoordinates(Dis, dimension = 4)
plot(pco)

# Adding the Origin as clusters to plot
pco = AddCluster2Biplot(pco, ClusterType = "us", Groups = Origin)
plot(pco, PlotClus = T)
plot(pco, PlotClus = T, TypeClus = "st")
plot(pco, PlotClus = T, TypeClus = "el")

# Adding hierarchical clusters (using the coordinates) to plot
pco = AddCluster2Biplot(pco, ClusterType = "hi", NGroups = 4)
plot(pco, PlotClus = T)
plot(pco$Dendrogram)
help(hclust)

# Adding hierarchical clusters (using the coordinates) to plot
pco = AddCluster2Biplot(pco,
                        ClusterType = "hi",
                        NGroups = 4,
                        method = "complete")
plot(pco, PlotClus = T)

op <- par(mfrow = 1:2)
plot(pco$Dendrogram)
plot(pco, PlotClus = T)
par(op)

# Adding k-means clusters (using the coordinates) to plot
pco = AddCluster2Biplot(pco, ClusterType = "km", NGroups = 4)
plot(pco, PlotClus = T)


# Adding gaussian mixture clusters (using the coordinates) to plot
pco = AddCluster2Biplot(pco, ClusterType = "gm", NGroups = 4)
plot(pco, PlotClus = T) # error

# Adding Bootstrap
pco = PrincipalCoordinates(Dis, dimension = 4, Bootstrap = T)
plot(pco, Bootstrap = T)

pco = AddCluster2Biplot(pco, ClusterType = "us", Groups = Origin)
plot(pco, PlotClus = T, Bootstrap = T)

# MDS using SMACOF algorithm
sm = MDS(Dis, Model = "Ordinal")
sm = AddCluster2Biplot(sm, ClusterType = "us", Groups = Origin)
plot(sm, PlotClus = T)

sm = MDS(Dis, Model = "Identity")
plot(sm)



data(Protein)

bip = PCA.Biplot(Protein[, 3:11])

plot(bip, mode = "s", margin = 0.2)

