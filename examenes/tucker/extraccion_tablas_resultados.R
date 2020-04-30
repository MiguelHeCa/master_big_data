# function (X,
#           p = NULL,
#           q = NULL,
#           r = NULL,
#           P1 = NULL,
#           P2 = NULL,
#           Q1 = NULL,
#           Q2 = NULL,
#           R1 = NULL,
#           R2 = NULL,
#           coloresf = NULL,
#           coloresc = NULL,
#           maximo = 5,
#           iter = 100,
#           tol = 10 ^ -8,
#           norm = FALSE,
#           contr = FALSE)
# {


# Inicio ------------------------------------------------------------------

X = var_cubo
# p = 3
# q = 4
# r = 3
# P1 = 1
# P2 = 2
# Q1 = 1
# Q2 = 2
# R1 = 1
# R2 = 2
coloresf = col_filas
coloresc = col_col_y
maximo = 5
iter = 100
tol = 10 ^ -8
norm = TRUE
contr = TRUE

l <- read(X)
X <- l[[1]]
filas <- l[[2]]
columnas <- l[[3]]
repeticiones <- l[[4]]
namesf <- l[[5]]
namesc <- l[[6]]
namesr <- l[[7]]
conf <- l[[11]]

c1 <- min(maximo, filas)
c2 <- min(maximo, columnas)
c3 <- min(maximo, repeticiones)

# Sin p q r ---------------------------------------------------------------

p = NULL
q = NULL
r = NULL
P1 = NULL
P2 = NULL
Q1 = NULL
Q2 = NULL
R1 = NULL
R2 = NULL

l <- as.matrix(expand.grid(1:c3, 1:c2, 1:c1))
l <- l[, 3:1]
names <- apply(l, 1, function(v) {
  return(paste(v[1], "x", v[2], "x", v[3], sep = ""))
})
a <- function(cont, l, T, iter, tol, c1, c2,
              c3) {
  if (((l[cont, 1]) <= ((l[cont, 2]) * (l[cont,
                                          3]))) &&
      ((l[cont, 2]) <= ((l[cont, 1]) *
                        (l[cont, 3]))) &&
      ((l[cont, 3]) <= ((l[cont,
                           1]) * (l[cont, 2])))) {
    return(c(cont, every(l[cont, 1], l[cont,
                                       2], l[cont, 3], T, iter, tol)))
  }
  else {
    return(rep(0, 5))
  }
}
table1 <- matrix(unlist(lapply(
  1:(dim(l)[1]),
  a,
  l = l,
  T = X,
  iter = iter,
  tol = tol,
  c1 = c1,
  c2 = c2,
  c3 = c3
)), ncol = 5, byrow = TRUE)
cual <- apply(table1, 1, function(v) {
  return(any(v != 0))
})
table1 <- table1[cual,]
names <- names[cual]
filas1 <- dim(table1)[1]
ini <- rbind(
  c(
    "Number",
    "Model_Size",
    "Sum",
    "Best_given_Sum",
    "SS(Res)",
    "Prop._SS(Fit)",
    "Number_of_iterations"
  ),
  cbind(table1[, 1],
        names, table1[, 2], rep("", filas1), table1[,
                                                    3:5])
)

cual <- unlist(lapply(3:(c1 + c2 + c3), function(i,
                                                 m) {
  if (any(m[, 2] == i)) {
    min(which(m[, 4] == max(m[m[, 2] == i, 4])))
  }
}, m = table1))
table2 <- table1[cual,]
names2 <- names[cual]
ini[cual + 1, 4] <- "*"


INI <- as.data.frame(ini[2:nrow(ini), ])

INI[, c(1, 3, 5:7)] <-
  apply(INI[, c(1, 3, 5:7)], 2, function(x)
    as.numeric(as.character(x)))
INI[, c(2, 4)] <- apply(INI[, c(2, 4)], 2, as.character)

knitr::kable(
  INI,
  format = "pandoc",
  col.names = gsub("_", " ", ini[1,]),
  caption = "Todas las combinaciones"
)

filas2 <- dim(table2)[1]
results(ini, "todas las combinaciones", first = TRUE)
ini2 <- rbind(
  c(
    "Number",
    "Model_Size",
    "S",
    "SS(Res)",
    "DifFit",
    "Prop._SS(Fit)",
    "Number_of_iterations"
  ),
  cbind(table2[, 1],
        names2,
        table2[, 2:3],
        table2[, 4] - c(0, table2[-filas2, 4]),
        table2[, 4:5])
)

INI2 <- as.data.frame(ini2[2:nrow(ini2), ])
#colnames(INI2) <- ini2[1,]

INI2[, c(1, 3:7)] <-
  apply(INI2[, c(1, 3:7)], 2, function(x)
    as.numeric(as.character(x)))
INI2[, 2] <- as.character(INI2[, 2])

knitr::kable(
  INI2,
  format = "pandoc",
  col.names = gsub("_", " ", ini2[1,]),
  caption = "Combinaciones con mejor ajuste"
)

#results(ini2, "combinaciones con mejor ajuste")
table3 <-
  table2[table2[, 3] <= (table2[, 2] - 3) * (table2[filas2, 3] - table2[1, 3]) /
           (c1 + c2 + c3 - 3) + table2[1, 3],]
cual <- chull(table3[, 2:3])
table3 <- table3[cual[order(cual)],]
filas3 <- dim(table3)[1]
cual <-
  ifelse(any((table3[, 3] - c(table3[-1, 3], 0)) < table3[, 3] /
               100), min(which((
                 table3[,
                        3] - c(table3[-1, 3], 0)
               ) < table3[, 3] / 100)),
         filas3)
table4 <- table3[1:cual,]
filas4 <- dim(table4)[1]
if (filas4 >= 3) {
  st <- c(0,
          ((table4[-c(filas4 - 1, filas4), 3] - table4[-c(1, filas4), 3]) / (table4[-c(1,
                                                                                       filas4), 2] - table4[-c(filas4 - 1, filas4),
                                                                                                            2])) /
            ((table4[-c(1, filas4), 3] - table4[-c(1,
                                                   2), 3]) /
               (table4[-c(1, 2), 2] - table4[-c(1,
                                                filas4), 2])), 0)
} else {
  st <- 0
}


# Con p q r ---------------------------------------------------------------

p = 3
q = 4
r = 3
# P1 = 1
# P2 = 2
# Q1 = 1
# Q2 = 2
# R1 = 1
# R2 = 2

comp <- c(p, q, r)
l <- every(p, q, r, X, iter, tol, mas = TRUE)
A <- l[[1]]
B <- l[[2]]
C <- l[[3]]
G <- l[[4]]
Aprox <- tensorial(tensorial(tensorial(G,
                                       1, t(A)), 2, t(B)), 3, t(C))
sos <- 100 * (matrix(unlist(lapply(1:3, function(i,
                                                 T, comp) {
  return(c(apply(T ^ 2, i, sum), rep(0, max(comp) -
                                       comp[i])))
}, T = G, comp = comp)), ncol = 3)) / (sum(X ^ 2))
sos <- rbind(sos, apply(sos, 2, sum))
ini3 <- rbind(c("Component", "Dimension_1",
                "Dimension_2", "Dimension_3"),
              cbind(c(1:(max(
                comp
              )),
              "Total_explained_variation"), sos))

SOS <- as.data.frame(round(sos, 4))
SOS <- cbind(x = c("1" ,"2", "3", "4", "Total de varianza explicada"),
             SOS)

knitr::kable(
  SOS,
  format = "pandoc",
  col.names = c("Componentes", "Dimensión 1", "Dimensión 2", "Dimensión 3"),
  caption = "Porcentajes de ajuste"
)

# results(ini3, "porcentajes de ajuste", first = TRUE)
core <- cbind(
  matrix(
    aperm(G, c(2, 1, 3)),
    nrow = p * r,
    ncol = q,
    byrow = TRUE
  ),
  matrix(
    aperm(100 *
            (G ^
               2) / (sum(X ^ 2)), c(2, 1, 3)),
    nrow = p *
      r,
    ncol = q,
    byrow = TRUE
  )
)

# rbind(
#   c(rep("", 3), rep(c("Mode_2_components", rep("", q-1)), 2)),
#   c(rep("", 3), "Residual_Sums_of_Squares", rep("", q - 1), "Explained_Variation",
#     rep("", q - 1)
#   )
# )
# 
# core_array <- data.frame(nom = c("Mode 1", rep("", p - 1), "Mode 2", rep("", p - 1)), core)
# col_core <- c("Mode 2\Residual sums of squares",
#               rep("", 3),
#               "Mode 2\Explained variance",
#               rep("", 3))
# row_core <- c("Mode 1", rep("", p - 1))

ini4 <- rbind(
  c(rep("", 3), rep(c(
    "Mode_2_components",
    rep("", q - 1)
  ), 2)),
  c(
    rep("", 3),
    "Residual_Sums_of_Squares",
    rep("", q - 1),
    "Explained_Variation",
    rep("",
        q - 1)
  ),
  c(rep("", 3), rep(1:q, 2)),
  cbind(unlist(lapply(1:r,
                      function(i, p) {
                        return(c(paste("Mode_3,_Component_", i,
                                       sep = ""), rep("", p - 1)))
                      }, p = p)), rep(c(
                        "Mode_1_components", rep("", p - 1)), r), rep(1:p, r), core)
)
# results(ini4, "core array")
fit1 <- apply(Aprox ^ 2, 1, sum)
fit2 <- apply(Aprox ^ 2, 2, sum)
fit3 <- apply(Aprox ^ 2, 3, sum)
fit1 <- cbind(fit1, apply(X ^ 2, 1, sum) - fit1)
fit2 <- cbind(fit2, apply(X ^ 2, 2, sum) - fit2)
fit3 <- cbind(fit3, apply(X ^ 2, 3, sum) - fit3)
# results(A, "coordenadas de las filas", names = namesf,
#         axis = TRUE)
# results(B, "coordenadas de las columnas",
#         names = namesc, axis = TRUE)
# results(C, "coordenadas de las repeticiones",
#         names = namesr, axis = TRUE)
if (contr) {
  contributions(A, namesf, "las filas")
  contributions(B, namesc, "las columnas")
  contributions(C, namesr, "las repeticiones")
}


# Con P, Q, R -------------------------------------------------------------


