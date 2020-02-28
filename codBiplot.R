## ----setup, include=FALSE-----------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message =FALSE-----------------------------------------------------------------------------------------
library(MultBiplotR)
# load("M5/data/Vinos.rda")
load("examenes/biplot/Datos15.rda")
Vinos = Datos
head(Vinos)


## ----message =FALSE-----------------------------------------------------------------------------------------
names(Vinos)


## ----message =FALSE-----------------------------------------------------------------------------------------
library(car)
BoxPlotPanel(Vinos[,4:9], nrows=2, groups=Vinos$denomina)


## ----message =FALSE-----------------------------------------------------------------------------------------
BoxPlotPanel(Vinos[,4:9], nrows=2, groups=Vinos$grupo)


## ----message =FALSE-----------------------------------------------------------------------------------------
X=Vinos[,4:21]
plot(X[,1:9])
plot(X[,10:18])

## ----message =FALSE-----------------------------------------------------------------------------------------
acpvino=PCA.Analysis(X, Scaling = 5)
summary(acpvino)


## ----message =FALSE-----------------------------------------------------------------------------------------
names(acpvino)


## ----message =FALSE-----------------------------------------------------------------------------------------
plot(acpvino)


## ----message =FALSE-----------------------------------------------------------------------------------------
plot(acpvino, CorrelationCircle=TRUE, ShowAxis=TRUE)


## ----message =FALSE-----------------------------------------------------------------------------------------
acpvino=AddCluster2Biplot(acpvino, ClusterType="us", Groups = Vinos$grupo)
plot(acpvino, PlotClus=TRUE, ClustCenters=TRUE, margin=0.05, CexInd=0.7)


## ----message =FALSE-----------------------------------------------------------------------------------------
plot(acpvino, PlotClus=TRUE, ClustCenters=TRUE, margin=0.05, CexInd=0.7, TypeClus="el")


## ----message =FALSE-----------------------------------------------------------------------------------------
plot(acpvino, PlotClus=TRUE, ClustCenters=TRUE, margin=0.05, CexInd=0.7, TypeClus="st")


## ----message =FALSE-----------------------------------------------------------------------------------------
bipvino=PCA.Biplot(X, Scaling = 5, alpha = 2) # HJ-Biplot
summary(bipvino)


## ----message =FALSE-----------------------------------------------------------------------------------------
plot(bipvino)


## ----message =FALSE-----------------------------------------------------------------------------------------
plot(bipvino, mode="s", margin=0.2)


## ----message =FALSE-----------------------------------------------------------------------------------------
plot(bipvino, mode="ah", margin=0.2)


## ----message =FALSE-----------------------------------------------------------------------------------------
CorrelationCircle(bipvino)


## ----message =FALSE-----------------------------------------------------------------------------------------
ColContributionPlot(bipvino)


## ----message =FALSE-----------------------------------------------------------------------------------------
plot(bipvino, dp=1, mode="s", ColorVar=c("black", rep("grey",17)))


## ----message =FALSE-----------------------------------------------------------------------------------------
plot(bipvino, PredPoints=1, mode="s", ColorVar=1:18)


## ----message =FALSE-----------------------------------------------------------------------------------------
plot(bipvino, PlotVar=FALSE)


## ----message =FALSE-----------------------------------------------------------------------------------------
bipvino=AddCluster2Biplot(bipvino, NGroups=4, ClusterType="hi", method="ward.D", Original=TRUE)
plot(bipvino, PlotClus=TRUE)


## ----message =FALSE-----------------------------------------------------------------------------------------
plot(bipvino$Dendrogram)


## ----message =FALSE-----------------------------------------------------------------------------------------
plot(bipvino, MinQualityVars=0.7)


## ----message =FALSE-----------------------------------------------------------------------------------------
library(MASS)
help(lda)
LDA=lda(X, Vinos$denomina)
Prediccion=predict(LDA, X)$class
ct <- table(Vinos$denomina, Prediccion)


## ----message =FALSE-----------------------------------------------------------------------------------------
diag(prop.table(ct, 1))


## ----message =FALSE-----------------------------------------------------------------------------------------
plot(LDA, dimen=1, type="both")


## ----message =FALSE-----------------------------------------------------------------------------------------
sum(diag(prop.table(ct)))


## ----message =FALSE-----------------------------------------------------------------------------------------
help(manova)
MANOVA = manova(as.matrix(X) ~ Vinos$grupo)
summary(MANOVA)
summary(MANOVA, test = "Wilks")
summary(MANOVA, test = "Hotelling")
summary(MANOVA, test = "Roy")
summary.aov(MANOVA)


## ----message =FALSE-----------------------------------------------------------------------------------------
help(lda)
LDA2=lda(X, Vinos$grupo)
Prediccion2=predict(LDA2, X)$class
ct2 <- table(Vinos$grupo, Prediccion2)
ct2


## ----message =FALSE-----------------------------------------------------------------------------------------
diag(prop.table(ct2, 1))*100


## ----message =FALSE-----------------------------------------------------------------------------------------
plot(LDA2)


## ----message =FALSE-----------------------------------------------------------------------------------------
sum(diag(prop.table(ct2)))*100


## ----message =FALSE-----------------------------------------------------------------------------------------
help(Canonical.Variate.Analysis)
CVA=Canonical.Variate.Analysis(X, group= Vinos$grupo, InitialTransform = 5)
summary(CVA)
plot(CVA)


## ----message =FALSE-----------------------------------------------------------------------------------------
help(Canonical.Variate.Analysis)
CVAb=CanonicalBiplot(X, group= Vinos$grupo, InitialTransform = 5)
summary(CVAb)
plot(CVAb)


## ----message =FALSE-----------------------------------------------------------------------------------------
X=as.matrix(X)
X=TransformIni(X,transform=5)
rownames(X)=paste("V",1:45,sep="")
dis=ContinuousProximities(X, coef=1)
pco=PrincipalCoordinates(dis)
plot(pco, RowColors=as.integer(Vinos$grupo), RowCex=0.8)


## ----message =FALSE-----------------------------------------------------------------------------------------
data(RAPD)
dist2=BinaryProximities(RAPD, coefficient = "Jaccard")
pco2=PrincipalCoordinates(dist2)
plot(pco2, RowCex=0.8)


## ----message =FALSE-----------------------------------------------------------------------------------------
MDSSol=MDS(dis, Model="Ratio")
plot(MDSSol, RowColors=as.integer(Vinos$grupo), RowCex=0.8)
MDSSol=AddContVars2Biplot(MDSSol,  X, Scaling = 5)
plot(MDSSol, RowColors=as.integer(Vinos$grupo), margin=0.15, RowCex=0.8, PlotSupVars=TRUE, ColorSupContVars="black")


## ----message =FALSE-----------------------------------------------------------------------------------------
MDSSol2=MDS(dist2, Model="Ordinal")
plot(MDSSol2, RowCex=0.8)

