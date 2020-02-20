
CHAEA = foreign::read.spss("M4/CHAEA.sav", to.data.frame = T, use.value.labels = F)

act_num = c(3, 5, 7, 9, 13, 20, 26, 27, 35, 37, 41, 43, 46, 48, 51, 61, 67, 74, 75, 77)
ref_num = c(10, 16, 18, 19, 28, 31, 32, 34, 36, 39, 42, 44, 49, 55, 58, 63, 65, 69, 70, 79)
teo_num = c(2, 4, 6, 11, 15, 17, 21, 23, 25, 29, 33, 45, 50, 54, 60, 64, 66, 71, 78, 80)
pra_num = c(1, 8, 12, 14, 22, 24, 30, 38, 40, 47, 52, 53, 56, 57, 59, 62, 68, 72, 73, 76)

act_col = paste0("CHAEA", act_num)
ref_col = paste0("CHAEA", ref_num)
teo_col = paste0("CHAEA", teo_num)
pra_col = paste0("CHAEA", pra_num)

estilos = data.frame(
  ACTIVO = rowSums(CHAEA[act_col]),
  REFLEXIVO = rowSums(CHAEA[ref_col]),
  TEORICO = rowSums(CHAEA[teo_col]),
  PRAGMATICO = rowSums(CHAEA[pra_col])
)

rango = c(0, 0.1, 0.3, 0.7, .9, 1)
etiqueta = c("Muy bajo", "Bajo", "Moderado", "Alto", "Muy alto")

lapply(estilos, quantile, rango)

estilos_rec = as.data.frame(lapply(estilos, function(x) {
  cut(x, breaks = quantile(x, rango), labels = etiqueta, include.lowest = T)
}))

estilos_num = as.data.frame(lapply(estilos_rec, as.numeric))

library(data.table)

estiloAprendizaje = as.data.table(estilos_num)
estiloAprendizaje[ACTIVO > REFLEXIVO & ACTIVO > TEORICO & ACTIVO > PRAGMATICO, ESTILO := 1]
estiloAprendizaje[REFLEXIVO > ACTIVO & REFLEXIVO > TEORICO & REFLEXIVO > PRAGMATICO, ESTILO := 2]
estiloAprendizaje[TEORICO > REFLEXIVO & TEORICO > ACTIVO & TEORICO > PRAGMATICO, ESTILO := 3]
estiloAprendizaje[PRAGMATICO > REFLEXIVO & PRAGMATICO > TEORICO & PRAGMATICO > ACTIVO, ESTILO := 4]
estiloAprendizaje[is.na(ESTILO), ESTILO := 5]

table(estiloAprendizaje$ESTILO)
