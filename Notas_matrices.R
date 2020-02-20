?matrix
A = matrix(c(1,4,7, 2,5,8, 3,6,9), nrow = 3, ncol = 3)
eigen(A)
det(A)
sum(diag(A))
rank(A)



# SVDD --------------------------------------------------------------------

A = matrix(c(1.1547, -1.0774, -0.0774,  -1.1547, 0.0774, 1.0774), nrow = 3, ncol = 2)

AAt = A %*% t(A)
AtA = t(A) %*% A

# P: vectores propios normalizados de AAt. Left singular vectors of A
P = eigen(AAt)$vectors[,1:2]

# Q: vectores propios normalizados de AtA. Right singular vectors of A
Q = eigen(AtA)$vectors

# D: Matriz diagonal de valores singulares de Lambda (su raíz cuadrada).
# Lambda es la matriz diagonal de AAt y AtA (son iguales).
AAtLambda = eigen(AAt)$values
AtALambda = eigen(AtA)$values

D = diag(round(sqrt(AtALambda)))

# Se puede mostrar que A = PDQt

A == round(-(P %*% D %*% t(Q)), 4)
P %*% (D %*% -Q

