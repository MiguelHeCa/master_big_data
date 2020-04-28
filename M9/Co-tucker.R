
# Establecer directorio ---------------------------------------------------

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(KTensorGraphs)

# Tucker ------------------------------------------------------------------

X <- read.table("especies.txt")
Y <- read.table("variables.txt")

namesF <- dimnames(X)[[1]]
namesc1 <- dimnames(X)[[2]][1:13]
namesc2 <- dimnames(Y)[[2]][1:10]
namesr <- c("spring", "summer", "autumn", "winter")

X <- array(as.matrix(X), dim = c(6, 13, 4))
Y <- array(as.matrix(Y), dim = c(6, 10, 4))

dimnames(Y)[[1]] <- dimnames(X)[[1]] <- namesF
dimnames(X)[[2]] <- namesc1
dimnames(Y)[[2]] <- namesc2
dimnames(Y)[[3]] <- dimnames(X)[[3]] <- namesr

colf <- rep("blue", 6)
colc1 <- rep("red", 13)
colc2 <- rep("green4", 10)

# Centra por tubo
TUCKER3(X, norm = TRUE)

TUCKER3(X,
        norm = TRUE,
        maximo = 5 # Primera dimensión
        )

# Línea rosa hace referencia entre modelos inestables y estables.
# El modelo arroja que la mejor combinación tiene forma 2X3X2.

TUCKER3(
  X,
  norm = TRUE,
  contr = TRUE,
  p = 2,
  q = 3,
  r = 2,
  coloresf = colf,
  coloresc = colc1
)
TUCKER3(
  X,
  norm = TRUE,
  p = 2,
  q = 3,
  r = 2,
  P1 = 1,
  P2 = 2,
  Q1 = 1,
  Q2 = 2,
  R1 = 1,
  R2 = 2,
  coloresf = colf,
  coloresc = colc1
)
TUCKER3(
  X,
  norm = TRUE,
  p = 2,
  q = 3,
  r = 2,
  P1 = 1,
  P2 = 2,
  Q1 = 1,
  Q2 = 3,
  R1 = 1,
  R2 = 2,
  coloresf = colf,
  coloresc = colc1
)


# Co-Tucker ---------------------------------------------------------------

COTUCKER3(
  X,
  Y,
  norm = TRUE
)

COTUCKER3(
  X,
  Y,
  norm = TRUE,
  p = 3,
  q = 4,
  r = 2,
  P1 = 1,
  P2 = 2,
  Q1 = 1,
  Q2 = 2,
  R1 = 1, 
  R2 = 2,
  coloresf = colf,
  coloresc1 = colc1,
  coloresc2 = colc2
)

# Coinercia
COTUCKER3(
  X,
  Y,
  norm = T,
  p = 3,
  q = 4,
  r = 2,
  dimAX = 1,
  dimAY = 2,
  coloresf = colf,
  coloresc1 = colc1,
  coloresc2 = colc2
)


# Coinercia
COTUCKER3(
  X,
  Y,
  norm = T,
  p = 3,
  q = 4,
  r = 2,
  dimBX = 1,
  dimBY = 2,
  coloresf = colf,
  coloresc1 = colc1,
  coloresc2 = colc2
)

# Coinercia
COTUCKER3(
  X,
  Y,
  norm = T,
  p = 3,
  q = 4,
  r = 2,
  dimCX = 1,
  dimCY = 2,
  coloresf = colf,
  coloresc1 = colc1,
  coloresc2 = colc2
)
