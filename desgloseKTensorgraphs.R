library(KTensorGraphs)

nom_fil = ssi$i16$BM
nom_alt = as.character(seq(2006, 2016, 2))

ssi.var = lapply(ssi, function(x) {
  d = select(x, SF:PD)
})
names(ssi.var) = paste0("i", sprintf("%02d", seq(6, 16, 2)))

SSI = array(as.matrix(do.call(cbind, ssi.var)),
            dim = c(nrow(ssi$i16), ncol(ssi.var$i16), length(nom_alt)),
            dimnames = list( nom_fil, nom_ssi, nom_alt))

clr_Hu = rep("#ffc300", 9) # Human Wellbeing
clr_En = rep("#bcff00", 7) # Environmental Wellbeing
clr_Ec = rep("#ff4400", 5) # Economical Wellbeing
clr_col = c(clr_Hu, clr_En, clr_Ec)

clr_r = case_when(
  ssi$i16$Ingreso == "A"  ~ "#0084ff",
  ssi$i16$Ingreso == "MA" ~ "#44bec7",
  ssi$i16$Ingreso == "MB" ~ "#d696bb",
  ssi$i16$Ingreso == "B"  ~ "#fa3c4c"
)

Hu = SSI[, 1:9, ]
En = SSI[, 10:16, ]
Ec = SSI[, 17:21, ]


# Función costatis --------------------------------------------------------

X = Hu
Y = En
dimX = 1
dimY = 2
coloresf = clr_r
coloresc1 = clr_Hu
coloresc2 = clr_En
norm = FALSE
contr = FALSE

# function (X, Y, dimX = NULL, dimY = NULL, coloresf = NULL, coloresc1 = NULL, 
#           coloresc2 = NULL, norm = FALSE, contr = FALSE) 

l <- read(X, Y)
X <- l[[1]]
filas <- l[[2]]
columnas1 <- l[[3]]
repeticiones <- l[[4]]
namesf <- l[[5]]
namesc1 <- l[[6]]
namesr <- l[[7]]
Y <- l[[8]]
columnas2 <- l[[9]]
namesc2 <- l[[10]]
conf <- l[[11]]
if (!conf) {
  l <- colores(filas,
               columnas1,
               coloresf,
               coloresc1,
               conf,
               columnas2,
               coloresc2)
  coloresf <- l[[1]]
  coloresc1 <- l[[2]]
  coloresc2 <- l[[3]]
  conf <- l[[4]]
}
if (!conf) {
  X <- preproc(X, norm, TRUE)
  Y <- preproc(Y, norm, TRUE)
  l <- pesos(rep(1, filas))
  Dn <- l[[1]]
  l <- pesos(rep(1, repeticiones))
  Dk <- l[[1]]
  Dk2 <- l[[2]]
  l <- as.matrix(expand.grid(1:repeticiones, 1:repeticiones))
  CovvX <- matrix(apply(l, 1, function(v, X1, D) {
    return(sum(diag(t(X1[, , v[1]]) %*% D %*% X1[, ,
                                                 v[2]])))
  }, X1 = X, D = Dn),
  nrow = repeticiones,
  ncol = repeticiones)
  CovvY <- matrix(apply(l, 1, function(v, X1, D) {
    return(sum(diag(t(X1[, , v[1]]) %*% D %*% X1[, ,
                                                 v[2]])))
  }, X1 = Y, D = Dn),
  nrow = repeticiones,
  ncol = repeticiones)
  aX <- (eigen(CovvX %*% Dk, symmetric = TRUE))$vectors
  VIX <- solve(Dk2) %*% (aX[, 1:2]) %*% diag(1 / sqrt(diag(t(aX[,
                                                                1:2]) %*% (aX[, 1:2]))))
  VIX[, 1] <- abs(VIX[, 1])
  IX <- CovvX %*% Dk %*% VIX
  aY <- (eigen(CovvY %*% Dk, symmetric = TRUE))$vectors
  VIY <- solve(Dk2) %*% (aY[, 1:2]) %*% diag(1 / sqrt(diag(t(aY[,1:2]) %*% (aY[, 1:2]))))
  VIY[, 1] <- abs(VIY[, 1])
  IY <- CovvY %*% Dk %*% VIY
  Xc <- apply(X, 1:2, function(v, v2) {
    return(sum(v * v2))
  }, v2 = VIX[, 1] / sum(VIX[, 1]))
  Yc <- apply(Y, 1:2, function(v, v2) {
    return(sum(v * v2))
  }, v2 = VIY[, 1] / sum(VIY[, 1]))
  Xc <- preproc(Xc, norm)
  Yc <- preproc(Yc, norm)
  c <- svd(t(Yc) %*% Dn %*% Xc)
  d <- c$d
  u <- c$u
  v <- c$v
  Ur <- u %*% diag(1 / sqrt(diag(t(u) %*% u)))
  Vr <- v %*% diag(1 / sqrt(diag(t(v) %*% v)))
  FXc <- Xc %*% Vr
  CXc <- t(Xc) %*% Dn %*% Yc %*% Ur
  FYc <- Yc %*% Ur
  CYc <- t(Yc) %*% Dn %*% Xc %*% Vr
  FXt <- array(apply(X, 3, function(m, m2) {
    return(m %*% m2)
  }, m2 = Vr),
  dim = c(filas, columnas2, repeticiones))
  CXt <- array(apply(X, 3, function(m, m2) {
    return(t(m) %*% m2)
  }, m2 = Dn %*% Yc %*% Ur),
  dim = c(columnas1, columnas2,
          repeticiones))
  FYt <- array(apply(Y, 3, function(m, m2) {
    return(m %*% m2)
  }, m2 = Ur),
  dim = c(filas, columnas2, repeticiones))
  CYt <- array(apply(Y, 3, function(m, m2) {
    return(t(m) %*% m2)
  }, m2 = Dn %*% Xc %*% Vr),
  dim = c(columnas2, columnas2,
          repeticiones))
  FX <- matrix(
    aperm(FXt, c(2, 1, 3)),
    nrow = filas *
      repeticiones,
    ncol = columnas2,
    byrow = TRUE
  )
  CX <- matrix(
    aperm(CXt, c(2, 1, 3)),
    nrow = columnas1 *
      repeticiones,
    ncol = columnas2,
    byrow = TRUE
  )
  FY <- matrix(
    aperm(FYt, c(2, 1, 3)),
    nrow = filas *
      repeticiones,
    ncol = columnas2,
    byrow = TRUE
  )
  CY <- matrix(
    aperm(CYt, c(2, 1, 3)),
    nrow = columnas2 *
      repeticiones,
    ncol = columnas2,
    byrow = TRUE
  )
  CX2 <- rbind(CXc, CX)
  CY2 <- rbind(CYc, CY)
  # if (contr) {
  #   contributions(FXc,
  #                 namesf,
  #                 "las filas segun el primer cubo en el compromiso",
  #                 TRUE)
  #   contributions(CXc, namesc1, "las columnas del primer cubo en el compromiso")
  #   contributions(FYc, namesf, "las filas segun el segundo cubo en el compromiso")
  #   contributions(CYc, namesc2, "las columnas del segundo cubo en el compromiso")
  #   contributions(
  #     FX,
  #     rep(namesf, repeticiones),
  #     "las filas segun el primer cubo en todas las repeticiones"
  #   )
  #   contributions(
  #     CX,
  #     rep(namesc1, repeticiones),
  #     "las columnas del primer cubo en todas las repeticiones"
  #   )
  #   contributions(
  #     FY,
  #     rep(namesf, repeticiones),
  #     "las filas segun el segundo cubo en todas las repeticiones"
  #   )
  #   contributions(
  #     CY,
  #     rep(namesc2, repeticiones),
  #     "las columnas del segundo cubo en todas las repeticiones"
  #   )
  # }
  # if ((is.null(dimX)) || (is.null(dimY))) {
  #   dev.new()
  #   layout(
  #     matrix(c(1, 1, 2, 1), ncol = 2),
  #     widths = c(310.5 / 99,
  #                382.5 / 99),
  #     heights = c(5.5, 1.5)
  #   )
  #   layout.show(2)
  #   screeplot(d)
  #   plot(
  #     0,
  #     0,
  #     type = "n",
  #     xlab = "",
  #     ylab = "",
  #     xlim = c(-1.5,
  #              4.25),
  #     ylim = c(-7.25, 2.25),
  #     bty = "n",
  #     xaxt = "n",
  #     yaxt = "n"
  #   )
  #   rect(-1,-1, 1, 1)
  #   rect(1,-1, 3, 1)
  #   rect(-1,-4, 1,-2)
  #   rect(1,-4, 3,-2)
  #   rect(0,-7, 2,-5)
  #   segments(-1, 1, 0, 2)
  #   segments(0, 2, 4, 2)
  #   segments(4, 2, 4, 0)
  #   segments(4, 0, 3,-1)
  #   segments(1, 1, 2, 2)
  #   segments(3, 1, 4, 2)
  #   text(0, 0, "X")
  #   text(2, 0, "Y")
  #   text(0,-3, "Xc")
  #   text(2,-3, "Yc")
  #   text(1,-6, "Yc' Dn Xc")
  #   text(-1.25, 0, filas)
  #   text(0, 1.25, columnas1)
  #   text(2, 1.25, columnas2)
  #   text(-0.75, 1.75, repeticiones, srt = 45)
  #   text(-1.25,-3, filas)
  #   text(0,-1.75, columnas1)
  #   text(2,-1.75, columnas2)
  #   text(-0.25,-6, columnas2)
  #   text(1,-4.75, columnas1)
  # }
  # else {
  #   if (compr(dimX, dimY, d)) {
  #     dimension <- c(dimX, dimY)
  #     FXcd <- FXc[, dimension]
  #     CXcd <- CXc[, dimension]
  #     FYcd <- FYc[, dimension]
  #     CYcd <- CYc[, dimension]
  #     FXtd <- FXt[, dimension,]
  #     CXtd <- CXt[, dimension,]
  #     FYtd <- FYt[, dimension,]
  #     CYtd <- CYt[, dimension,]
  #     FXd <- FX[, dimension]
  #     FYd <- FY[, dimension]
  #     CX2d <- CX2[, dimension]
  #     CY2d <- CY2[, dimension]
  #     Fc <- rbind(FXcd, FYcd)
  #     dev.new(width = 14,
  #             height = 7,
  #             noRStudioGD = TRUE)
  #     layout(matrix(1:2, nrow = 1))
  #     layout.show(2)
  #     plot(
  #       IX[, 1],
  #       IX[, 2],
  #       type = "n",
  #       xlim = c(min(0,
  #                    IX[, 1]), max(0, IX[, 1])),
  #       ylim = c(min(0,
  #                    IX[, 2]), max(0, IX[, 2])),
  #       xlab = "",
  #       ylab = ""
  #     )
  #     text(IX[, 1], IX[, 2], namesr, cex = 0.8, pos = 2)
  #     box(lwd = 2)
  #     abline(h = 0, lwd = 2)
  #     abline(v = 0, lwd = 2)
  #     arrows(0, 0, IX[, 1], IX[, 2], angle = 10, length = 0.1)
  #     plot(
  #       IY[, 1],
  #       IY[, 2],
  #       type = "n",
  #       xlim = c(min(0,
  #                    IY[, 1]), max(0, IY[, 1])),
  #       ylim = c(min(0,
  #                    IY[, 2]), max(0, IY[, 2])),
  #       xlab = "",
  #       ylab = ""
  #     )
  #     text(IY[, 1], IY[, 2], namesr, cex = 0.8, pos = 2)
  #     box(lwd = 2)
  #     abline(h = 0, lwd = 2)
  #     abline(v = 0, lwd = 2)
  #     arrows(0, 0, IY[, 1], IY[, 2], angle = 10, length = 0.1)
  #     plotm(
  #       dim = dimension,
  #       d = d,
  #       M1 = FXcd,
  #       M2 = CXcd,
  #       M3 = FYcd,
  #       lim1 = Fc,
  #       lim2 = CX2d,
  #       names1 = namesf,
  #       names2 = namesc1,
  #       colores1 = coloresf,
  #       colores2 = coloresc1,
  #       contf = contributions2(FXc, dimension),
  #       contc = contributions2(CXc,
  #                              dimension)
  #     )
  #     plotm(
  #       dim = dimension,
  #       d = d,
  #       M1 = FYcd,
  #       M2 = CYcd,
  #       M3 = FXcd,
  #       lim1 = Fc,
  #       lim2 = CY2d,
  #       names1 = namesf,
  #       names2 = namesc2,
  #       colores1 = coloresf,
  #       colores2 = coloresc2,
  #       contf = contributions2(FYc, dimension),
  #       contc = contributions2(CYc,
  #                              dimension)
  #     )
  #     plotm(
  #       dim = dimension,
  #       d = d,
  #       M1 = FXtd[, ,
  #                 1],
  #       M2 = CXtd[, , 1],
  #       M4 = FXtd,
  #       M5 = CXtd,
  #       lim1 = FXd,
  #       lim2 = CX2d,
  #       names1 = namesf,
  #       names2 = namesc1,
  #       colores1 = coloresf,
  #       colores2 = coloresc1,
  #       contf = contributions2(FXt[, , 1], dimension),
  #       contc = contributions2(CXt[, , 1], dimension)
  #     )
  #     plotm(
  #       dim = dimension,
  #       d = d,
  #       M1 = FYtd[, ,
  #                 1],
  #       M2 = CYtd[, , 1],
  #       M4 = FYtd,
  #       M5 = CYtd,
  #       lim1 = FYd,
  #       lim2 = CY2d,
  #       names1 = namesf,
  #       names2 = namesc2,
  #       colores1 = coloresf,
  #       colores2 = coloresc2,
  #       contf = contributions2(FYt[, , 1], dimension),
  #       contc = contributions2(CYt[, , 1], dimension)
  #     )
  #   }
  # }
}

    dimension <- c(dimX, dimY)
    FXcd <- FXc[, dimension]
    CXcd <- CXc[, dimension]
    FYcd <- FYc[, dimension]
    CYcd <- CYc[, dimension]
    FXtd <- FXt[, dimension,]
    CXtd <- CXt[, dimension,]
    FYtd <- FYt[, dimension,]
    CYtd <- CYt[, dimension,]
    FXd <- FX[, dimension]
    FYd <- FY[, dimension]
    CX2d <- CX2[, dimension]
    CY2d <- CY2[, dimension]
    Fc <- rbind(FXcd, FYcd)

    

# Desglose plotm ----------------------------------------------------------

    
    dim = dimension
    d = d
    M1 = FXcd
    M2 = CXcd
    M3 = FYcd
    lim1 = Fc
    lim2 = CX2d
    names1 = namesf
    names2 = namesc1
    colores1 = coloresf
    colores2 = coloresc1
    contf = contributions2(FXc, dimension)
    contc = contributions2(CXc, dimension)
    
    # function (dim, d, M1, M2, M3 = NULL, M4 = NULL, M5 = NULL, M6 = NULL, 
    #           lim1, lim2, names1, names2, colores1, colores2, contf, contc, 
    #           cotucker = FALSE) 
    # function (dim, d, M1, M2, M3 = NULL, M4 = NULL, M5 = NULL, M6 = NULL, 
    #           lim1, lim2, names1, names2, colores1, colores2, contf, contc, 
    #           cotucker = FALSE) 
    
    contf[is.nan(contf)] <- 0
    contc[is.nan(contc)] <- 0
    mcontf <- max(contf)/2
    mcontc <- max(contc)/2
    s <- function(i, M, colores) {
      segments(M[, 1, i], M[, 2, i], M[, 1, i + 1], M[, 2, 
                                                      i + 1], col = colores)
    }
    #dev.new(width = 14, height = 7, noRStudioGD = TRUE)
    layout(matrix(1:2, nrow = 1))
    layout.show(2)
    e <- round((100 * d^2/sum(d^2))[dim], 3)
    plot(M1[, 1], M1[, 2], type = "n", xlim = c(min(0, lim1[, 
                                                            1]), max(0, lim1[, 1])), ylim = c(min(0, lim1[, 2]), 
                                                                                              max(0, lim1[, 2])), xlab = paste("Axis ", dim[1], " (", 
                                                                                                                               e[1], "%)", sep = ""), ylab = paste("Axis ", dim[2], 
                                                                                                                                                                   " (", e[2], "%)", sep = ""))
    text(M1[, 1], M1[, 2], names1, cex = 0.8, col = colores1)
    box(lwd = 2)
    abline(h = 0, lwd = 2)
    abline(v = 0, lwd = 2)
    if (!cotucker) {
      if (!is.null(M3)) {
        arrows(M1[, 1], M1[, 2], M3[, 1], M3[, 2], col = colores1, 
               angle = 10, length = 0.1)
      }
    }
    else {
      if (!is.null(M3)) {
        cual1 <- cbind(M1[, 1], M3[, 1])
        cual2 <- cbind(M1[, 2], M3[, 2])
        cual <- (apply(cual1, 1, function(v) {
          return(abs(v[1] - v[2]) > 1e-15)
        })) | (apply(cual2, 1, function(v) {
          return(abs(v[1] - v[2]) > 1e-15)
        }))
        if (sum(cual) != 0) {
          arrows(M1[cual, 1], M1[cual, 2], M3[cual, 1], 
                 M3[cual, 2], col = colores1[cual], angle = 10, 
                 length = 0.1)
        }
      }
    }
    if (!is.null(M4)) {
      lapply(1:(dim(M4)[3] - 1), s, M = M4, colores = colores1)
    }
    if (!is.null(M6)) {
      lcolores1 <- colores1[!apply(M6 == 0, 1, all)]
      lM6 <- M6[!apply(M6 == 0, 1, all), ]
      arrows(0, 0, lM6[, 1], lM6[, 2], col = lcolores1, angle = 10, 
             length = 0.1)
    }
    plot(M2[, 1], M2[, 2], type = "n", xlim = c(min(0, lim2[, 
                                                            1]), max(0, lim2[, 1])), ylim = c(min(0, lim2[, 2]), 
                                                                                              max(0, lim2[, 2])), xlab = paste("Axis ", dim[1], " (", 
                                                                                                                               e[1], "%)", sep = ""), ylab = paste("Axis ", dim[2], 
                                                                                                                                                                   " (", e[2], "%)", sep = ""))
    text(M2[, 1], M2[, 2], names2, cex = 0.8, col = colores2)
    box(lwd = 2)
    abline(h = 0, lwd = 2)
    abline(v = 0, lwd = 2)
    lcolores2 <- colores2[!apply(M2 == 0, 1, all)]
    if (!cotucker) {
      lM2 <- M2[!apply(M2 == 0, 1, all), ]
    }
    else {
      lM2 <- M2[!apply(abs(M2) < 1e-15, 1, all), ]
    }
    arrows(0, 0, lM2[, 1], lM2[, 2], col = lcolores2, angle = 10, 
           length = 0.1)
    if (!is.null(M5)) {
      lapply(1:(dim(M5)[3] - 1), s, M = M5, colores = colores2)
    }
    h <- function(m) {
      return(abs(c(min(0, m[, 1]), max(0, m[, 1]), min(0, 
                                                       m[, 2]), max(0, m[, 2]))))
    }
    k <- h(lim1)/h(lim2)
    k <- min(k[k != 0])
    # dev.new()
    plot(M1[, 1], M1[, 2], type = "n", xlim = c(min(0, lim1[, 
                                                            1], k * lim2[, 1]), max(0, lim1[, 1], k * lim2[, 1])), 
         ylim = c(min(0, lim1[, 2], k * lim2[, 2]), max(0, lim1[, 
                                                                2], k * lim2[, 2])), xlab = paste("Axis ", dim[1], 
                                                                                                  " (", e[1], "%)", sep = ""), ylab = paste("Axis ", 
                                                                                                                                            dim[2], " (", e[2], "%)", sep = ""))
    lcolores1 <- colores1[contf >= mcontf]
    lnames1 <- names1[contf >= mcontf]
    lM1 <- matrix(M1[contf >= mcontf, ], ncol = 2)
    box(lwd = 2)
    abline(h = 0, lwd = 2)
    abline(v = 0, lwd = 2)
    title(main = paste("Elementos con mayor contribucion.\nVariables multiplicadas por una constante (", 
                       round(k, 3), ")\npara que sean representables conjuntamente.", 
                       sep = ""), cex.main = 0.75, font.main = 1)
    if (!cotucker) {
      if (!is.null(M3)) {
        lM3 <- matrix(M3[contf >= mcontf, ], ncol = 2)
        arrows(lM1[, 1], lM1[, 2], lM3[, 1], lM3[, 2], col = lcolores1, 
               angle = 10, length = 0.1)
      }
    }
    else {
      if (!is.null(M3)) {
        lM3 <- matrix(M3[contf >= mcontf, ], ncol = 2)
        cual1 <- cbind(lM1[, 1], lM3[, 1])
        cual2 <- cbind(lM1[, 2], lM3[, 2])
        cual <- (apply(cual1, 1, function(v) {
          return(abs(v[1] - v[2]) > 1e-15)
        })) | (apply(cual2, 1, function(v) {
          return(abs(v[1] - v[2]) > 1e-15)
        }))
        if (sum(cual) != 0) {
          arrows(lM1[cual, 1], lM1[cual, 2], lM3[cual, 
                                                 1], lM3[cual, 2], col = lcolores1[cual], angle = 10, 
                 length = 0.1)
        }
      }
    }
    if (!is.null(M4)) {
      lM4 <- array(M4[contf >= mcontf, , ], dim = c(length(lcolores1), 
                                                    2, dim(M4)[3]))
      lapply(1:(dim(lM4)[3] - 1), s, M = lM4, colores = lcolores1)
    }
    if (!is.null(M6)) {
      lM6 <- matrix(lM6[contf >= mcontf, ], ncol = 2)
      lcolores1 <- lcolores1[!apply(lM6 == 0, 1, all)]
      lM6 <- matrix(lM6[!apply(lM6 == 0, 1, all), ], ncol = 2)
      arrows(0, 0, lM6[, 1], lM6[, 2], col = lcolores1, angle = 10, 
             length = 0.1)
    }
    lcolores2 <- colores2[contc >= mcontc]
    lnames2 <- names2[contc >= mcontc]
    lM2 <- matrix(M2[contc >= mcontc, ], ncol = 2)
    lcolores2 <- lcolores2[!apply(lM2 == 0, 1, all)]
    lnames2 <- lnames2[!apply(lM2 == 0, 1, all)]
    lM2 <- matrix(lM2[!apply(lM2 == 0, 1, all), ], ncol = 2)
    text(k * lM2[, 1], k * lM2[, 2], lnames2, cex = 0.8, col = lcolores2)
    arrows(0, 0, k * lM2[, 1], k * lM2[, 2], col = lcolores2, 
           angle = 10, length = 0.1)
    if (!is.null(M5)) {
      lM5 <- array(M5[contc >= mcontc, , ], dim = c(length(lcolores2), 
                                                    2, dim(M5)[3]))
      lapply(1:(dim(lM5)[3] - 1), s, M = k * lM5, colores = lcolores2)
    }
    if (is.null(M6)) {
      points(lM1[, 1], lM1[, 2], pch = 19, col = lcolores1, 
             cex = 0.75)
      identify(lM1[, 1], lM1[, 2], lnames1, cex = 0.8, tolerance = 3)
    }
    else {
      text(lM1[, 1], lM1[, 2], lnames1, cex = 0.8, col = lcolores1)
    }
    
    
    