#IMPORTAMOS LOS ARCHIVOS FAMILIAS Y VARIABLES_STD de spss.

FAMILIAS = foreign::read.spss("data/M3/MDS/FAMILIAS.sav", to.data.frame = T)
VARIABLES_STD = foreign::read.spss("data/M3/MDS/VARIABLES_STD.sav", to.data.frame = T)
#le decimos en qué columnas tenemos los datos
VARIABLES_STD=VARIABLES_STD[,3:12]
VARIABLES_STD

#le decimos en qué columnas tenemos los datos
FAMILIAS_1=FAMILIAS[,4:16]
FAMILIAS_1

#convertimos los datos en matrices
FAMILIAS_1=as.matrix(FAMILIAS_1)
VARIABLES_1=as.matrix(VARIABLES_STD)
FAMILIAS_1
VARIABLES_1=VARIABLES_1[1:70,]
VARIABLES_1

#realizamos un MDS no métrico (el metaMDS directamente convierte la matriz en datos de 
#distancias utilizando la distancia ecológica de Bray-Curtis) 
#por defecto la solución la da en dos dimensiones.
library(vegan)

ord <- metaMDS(FAMILIAS_1)
ord
str(ord)
#realizamos el gráfico (directamente mete los lugares y las especies, es decir
#ya está realizando un "vectorial" para las especies

plot(ord, type="t")


#ahora realizamos el ajuste del modelo vectorial:

fit <- envfit(ord, VARIABLES_1, perm = 999)
summary(fit)
str(fit)
#aquí tenemos los coeficientes de regresión estandarizados para cada variable
#y la bondad de ajuste y la significación en el plano retenido 
fit$vectors


scores(fit, "vectors")

#representamos las variables sobre el anterior gráfico
plot(fit)
#representamos con color azul las variables que no han resultado significativas
plot(fit, p.max = 0.05, col = "red")



