library(KTensorGraphs)
X <- read.table("M7/years.txt")
namesf <- dimnames(X)[[1]]
namesc <- dimnames(X)[[2]][1:21]
namesr <- c("2006", "2008", "2010", "2012")
X <- array(as.matrix(X), dim = c(151, 21, 4))
Y <- read.table("M7/2012.txt")
dimnames(X)[[1]] <- namesf
dimnames(X)[[2]] <- namesc
dimnames(X)[[3]] <- namesr
gruposf <-
  c(rep("low", 31),
    rep("lower-middle", 35),
    rep("upper-middle", 40),
    rep("high", 45))
coloresf <-
  c(rep("#000000", 31),
    rep("#0000AA", 35),
    rep("#5500FF", 40),
    rep("#FF00FF", 45))
coloresc1 <- rep("blue", 10)
coloresc2 <- rep("green4", 6)
coloresc3 <- rep("red", 5)
coloresc <- c(coloresc1, coloresc2, coloresc3)
A <- X[, 1:10, ]
B <- X[, 11:16, ]
C <- X[, 17:21, ]
P <- Y[, 1:10]
Q <- Y[, 11:16]
R <- Y[, 17:21]
PCA(Y, norm = FALSE, contr = FALSE)
PCA(
  Y,
  dimX = 1,
  dimY = 2,
  coloresf = coloresf,
  coloresc = coloresc
)
BGA(Y, gruposf)
BGA(
  Y,
  gruposf,
  dimX = 1,
  dimY = 2,
  coloresf = coloresf,
  coloresc = coloresc
)
COIA(P, Q)
COIA(
  P,
  Q,
  dimX = 1,
  dimY = 2,
  coloresf = coloresf,
  coloresc1 = coloresc1,
  coloresc2 = coloresc2
)
PTA(X)
PTA(
  X,
  dimX = 1,
  dimY = 2,
  coloresf = coloresf,
  coloresc = coloresc
)
BGCOIA(A, B)
BGCOIA(
  A,
  B,
  dimX = 1,
  dimY = 2,
  coloresf = coloresf,
  coloresc1 = coloresc1,
  coloresc2 = coloresc2
)
STATICO(A, B)
STATICO(
  A,
  B,
  dimX = 1,
  dimY = 2,
  coloresf = coloresf,
  coloresc1 = coloresc1,
  coloresc2 = coloresc2
)
COSTATIS(A, B)
COSTATIS(
  A,
  B,
  dimX = 1,
  dimY = 2,
  coloresf = coloresf,
  coloresc1 = coloresc1,
  coloresc2 = coloresc2
)
TUCKER3(X,
        maximo = 5,
        iter = 100,
        tol = 10 ^ -8)
TUCKER3(
  X,
  p = 4,
  q = 4,
  r = 2,
  coloresf = coloresf,
  coloresc = coloresc
)
TUCKER3(
  X,
  p = 4,
  q = 4,
  r = 2,
  P1 = 1,
  P2 = 2,
  Q1 = 1,
  Q2 = 2,
  R1 = 1,
  R2 = 2,
  coloresf = coloresf,
  coloresc = coloresc
)
COTUCKER3(A, B)
COTUCKER3(
  A,
  B,
  p = 3,
  q = 3,
  r = 2,
  coloresf = coloresf,
  coloresc1 = coloresc1,
  coloresc2 = coloresc2
)
COTUCKER3(
  A,
  B,
  p = 3,
  q = 3,
  r = 2,
  P1 = 1,
  P2 = 2,
  Q1 = 1,
  Q2 = 2,
  R1 = 1,
  R2 = 2,
  coloresf = coloresf,
  coloresc1 = coloresc1,
  coloresc2 = coloresc2
)
COTUCKER3(
  A,
  B,
  p = 3,
  q = 3,
  r = 2,
  dimAX = 1,
  dimAY = 2,
  coloresf = coloresf,
  coloresc1 = coloresc1,
  coloresc2 = coloresc2
)
COTUCKER3(
  A,
  B,
  p = 3,
  q = 3,
  r = 2,
  dimBX = 1,
  dimBY = 2,
  coloresf = coloresf,
  coloresc1 = coloresc1,
  coloresc2 = coloresc2
)
COTUCKER3(
  A,
  B,
  p = 3,
  q = 3,
  r = 2,
  dimCX = 1,
  dimCY = 2,
  coloresf = coloresf,
  coloresc1 = coloresc1,
  coloresc2 = coloresc2
)
