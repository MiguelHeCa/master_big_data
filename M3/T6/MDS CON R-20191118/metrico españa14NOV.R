#IMPORTAMOS DE EXCEL EL ARCHIVO ESPANA_1



ESPANA_1 = readxl::read_excel("data/M3/MDS/ESPANA_1.xlsx")

ESPANA = as.data.frame(ESPANA_1)
ESPANA
str(ESPANA)
#Vemos  que las etiquetas de los países están en el archivo: ESPANA$...1
ESPANA$...1
#quitamos la priera columna de datos
D = ESPANA[, -1]
#decimos que se trata de una matriz
D = as.matrix(D)

#comenzamos a construir la matriz B con el algoritmo de TORGERSON:
A = -0.5 * D * D
I = diag(9)
I
J = matrix(1, 9, 9)
J
H = I - (1 / 9) * J
H
B = H %*% A %*% H
B

# Diagonalizamos la matriz B
sol = eigen(B)
sol
str(sol)

# vemos cuál es la bondad de ajuste. Representamos el diagrama de los valores propios
plot(sol$values)
lines(sol$values)

#vemos que dos dimensiones son adecuadas, calculamos la bondad de ajuste en 2 dimensiones:
GOF = (sol$values[1] + sol$values[2]) / sum(sol$values[1:6])
GOF

#ahora vamos a calcular la matriz de coordenadas en dos dimensiones: X=L*V^0.5

#L es la matríz diagonal (2x2), cuyos valores son los dos primeros valores propios:
L = diag(c(sol$values[1], sol$values[2]))
L
#V es la matríz (9x2), cuyas columnas son los dos primeros vectores propios:
V = matrix(c(sol$vectors[, 1], sol$vectors[, 2]), 9)
V

#calculamos X, la matríz de coordenadas (es una matriz, 9x2)
X = V %*% L ^ 0.5
X

#para representar el mapa en 2 dimensiones,
#elegimos como  coordenadas en el eje X la primera columna de X

x = X[, 1]
x

#elegimos como  coordenadas en el eje Y la segunda columna de X
y = X[, 2]
y

#representamos primero los puntos correspondientes a cada ciudad, sin las etiquetas
plot(x, y, text = NULL)
#ahora añadimos las etiquetas (que estaban en EXPANA$...1)
text(x, y, labels = ESPANA$...1, .008)


#HACERLO CON LA FUNCIÓN cmdscale (del paquete stats)

sol2 = cmdscale(D, k = 2, eig = TRUE)
sol2
summary(sol2)

str(sol2)

#representamos el diagrama de los valores propios para elegir la dimensionalidad
plot(sol2$eig)
lines(sol2$eig)

#calculamos la bondad de ajuste
sol2$GOF

#esta es la matriz de coordenadas:
sol2$points

#elegimos ahora las coordenadas de cada punto en la primera dimensión
x = sol2$points[, 1]
x
#elegimos ahora las coordenadas de cada punto en la segunda dimensión
y = sol2$points[, 2]
y
#repesentamos primero el mapa sin etiquetas, y luego con las etiquetas
plot(x, y, text = NULL)
text(x, y, labels = c(ESPANA$...1))







