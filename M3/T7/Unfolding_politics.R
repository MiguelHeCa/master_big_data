if(!"smacof" %in% installed.packages()[,1]) install.packages("smacof")
library(smacof)

misdatos <-
  matrix(
    c(1, 2, 6, 5, 4, 3, 3, 4, 1, 5, 6, 2, 3, 1, 6, 2, 4, 5, 5, 3, 1, 2, 6, 4),
    nrow = 4,
    byrow = T
  )
dimnames(misdatos) <- list(
  c("PSOE", "PP", "PODEMOS", "CIUDADANOS"),
  c(
    "EDUCACION/SANIDAD",
    "LUCHA CORRUPCION",
    "UNIDAD DE ESPANA",
    "REG DEMOCRATICA",
    "MEDIO AMBIENTE",
    "ECONOMIA"
  )
)


res <- unfolding(misdatos)
res
summary(res)

## various configuration plots
plot(res)
plot(res, type = "p", pch = 25)
plot(
  res,
  type = "p",
  pch = 25,
  col.columns = 3,
  label.conf.columns = list(label = TRUE, pos = 3, col = 3),
  col.rows = 8,
  label.conf.rows = list(label = TRUE, pos = 3, col = 10)
)


library(MultBiplotR)
unf = Unfolding(misdatos, TransAbund = "None")
plot(unf, PlotTol = TRUE)

gensol = Genefold(misdatos)
class(gensol) = "Unfolding"
plot(gensol)

bip = PCA.Biplot(7 - misdatos, Scaling = 3)
plot(bip, margin = 0.2, mode = "s")
