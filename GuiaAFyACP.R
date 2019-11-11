## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.pos = "H", message = FALSE, warning = FALSE)


## ---- include=FALSE------------------------------------------------------
CL = foreign::read.spss("data/Ciencias-Letras TOY EJEMPLO.sav", to.data.frame = T)
CL_mat = as.data.frame(apply(CL, 2, as.integer))
nom_CL = c("Matemáticas", "Naturales", "Francés", "Latín")
names(CL_mat) = nom_CL


## ---- include=FALSE------------------------------------------------------
# CL_tab = knitr::kable(CL_mat, format = "latex", caption = "Calificaciones de materias")
CL_tab = knitr::kable(CL_mat, format = "pandoc", caption = "Calificaciones de materias\\label{tab:cl_tab}")


## ---- echo=FALSE---------------------------------------------------------
CL_tab


## ------------------------------------------------------------------------
CL = foreign::read.spss("data/Ciencias-Letras TOY EJEMPLO.sav",
                        to.data.frame = T)
names(CL) = c("Matemáticas", names(CL)[2:4])


## ------------------------------------------------------------------------
# Correlaciones
CL_cor = Hmisc::rcorr(as.matrix(CL))
CL_cor

# Determinante de la matriz de correlaciones
det(cor(CL))


## ------------------------------------------------------------------------
acp = psych::principal(CL, nfactors = ncol(CL), rotate = "none")

# Cargas
unclass(acp$loadings)

# Proporción de la varianza
acp$Vaccounted


## ----screeplot0, fig.cap="\\label{fig:screeplot0}Gráfico de sedimentos"----
# Gráfico de sedimentos o de codos
psych::scree(CL)

## ------------------------------------------------------------------------
acp = psych::principal(CL, nfactors = 2, rotate = "none")

# Cargas
unclass(acp$loadings)

# Proporción de la varianza
acp$Vaccounted

# Autovalores
acp$values

# Comunalidades
acp$communality

# Unicidades
acp$uniquenesses

# Matriz de correlaciones reproducida
psych::factor.model(acp$loadings)

# Residuales de la matriz de correlaciones reproducida
acp$residual


## ------------------------------------------------------------------------
psych::plot.psych(acp, xlim = c(-1, 1), ylim = c(-1, 1))


## ------------------------------------------------------------------------
acp_var = psych::principal(CL, nfactors = 2, rotate = "varimax", method = "Anderson")

# Cargas
unclass(acp_var$loadings)

# Proporción de la varianza
acp_var$Vaccounted

# Autovalores
acp_var$values

# Comunalidades
acp_var$communality

# Unicidades
acp_var$uniquenesses

# Matriz de correlaciones reproducida
psych::factor.model(acp_var$loadings)

# Residuales de la matriz de correlaciones reproducida
acp_var$residual


## ------------------------------------------------------------------------
psych::plot.psych(acp_var, xlim = c(-1, 1), ylim = c(-1, 1))


## ------------------------------------------------------------------------
AF = psych::fa(CL, 2, rotate = "none", scores = "Anderson")

# Cargas
AF$loadings

# Comunalidades
AF$communality

# Unicidades
AF$uniquenesses

# Valores propios de la matriz original
AF$e.values

# Proporcion de la varianza
AF$e.values / 4

# Suma acumulada de la proporción de la varianza
cumsum(AF$e.values / 4)

# Valores propios de la extracción
AF$values

# Proporcion de la varianza después de la extracción
AF$Vaccounted

# Matriz de correlaciones reproducida
psych::factor.model(AF$loadings)

# residuales de correlación
residuals(AF)


## ------------------------------------------------------------------------
psych::plot.psych(AF, xlim = c(-1, 1), ylim = c(-1, 1))


## ------------------------------------------------------------------------
AF_rot = psych::fa(CL, 2, rotate = "varimax", scores = "Anderson")

AF_rot$loadings

# Comunaliddades
AF_rot$communality

# Varianza
AF_rot$Vaccounted

# Matriz de correlaciones reproducida
psych::factor.model(AF_rot$loadings)

# residuales de correlación rotada
residuals(AF_rot)


## ------------------------------------------------------------------------
psych::plot.psych(AF_rot, xlim = c(-1, 1), ylim = c(-1, 1))


## ------------------------------------------------------------------------
# Nombre de los paquetes que se utilizarán
paquetes = c("psych", "ggplot2", "ggcorrplot")

# Se crea un objeto con los nombres de los paquetes que no están instalados
nuevos_paquetes = paquetes[!paquetes %in% installed.packages()[,1]]

# Instala aquellos que no están instalados.
# Si no hay ninguno por instalar, no hace nada.
if(length(nuevos_paquetes)) install.packages(nuevos_paquetes)


## ------------------------------------------------------------------------
CL = foreign::read.spss("data/Ciencias-Letras TOY EJEMPLO.sav", to.data.frame = T)
names(CL) = c("Matemáticas", names(CL)[2:4])
str(CL)


## ------------------------------------------------------------------------
# Media
summary(CL)

# Desviación estándar
apply(CL, 2, sd)


## ------------------------------------------------------------------------
CL_cor = Hmisc::rcorr(as.matrix(CL))
cor_mat = CL_cor$r # Matriz de correlaciones
sig_mat = CL_cor$P # Matriz de p-valores de la correlación


## ----corplot, message=FALSE, warning=FALSE, fig.cap="\\label{fig:corplot}Matriz de correlaciones de materias"----
library(ggcorrplot)
p.mat = cor_pmat(CL) # Otra forma de calcular la matriz de p-valores
ggcorrplot(
  cor_mat,
  hc.order = TRUE,
  type = "lower",
  lab = TRUE,
  colors = c("#ee5200", "white", "#009cee"),
  p.mat = p.mat, # Habilitar ver las correlaciones no significativas
  pch = 0, # Los p-valores no significativos se muestran como cuadros
  pch.cex = 12
)


## ------------------------------------------------------------------------
det(cor_mat)


## ------------------------------------------------------------------------
psych::cortest.bartlett(cor_mat, nrow(CL))


## ------------------------------------------------------------------------
kmo = psych::KMO(CL)
kmo


## ------------------------------------------------------------------------
# Anti-imagen de correlaciones
kmo$Image

# Anti-imagen de covarianzas
kmo$ImCov


## ------------------------------------------------------------------------
diag(kmo$ImCov)


## ------------------------------------------------------------------------
# Matriz de correlaciones
mat_cor = cor(CL, use = "pairwise")

# En caso de covarianzas, estandarizamos
mat_cov = cov(CL, use = "pairwise")
dvest = sqrt(diag(mat_cov))
mat_cov = mat_cov / (dvest %o% dvest)

# Comprobamos que ya son idénticas
identical(round(mat_cor, 3), round(mat_cov, 3))


## ------------------------------------------------------------------------
autov = eigen(mat_cor)

# Valores propios o autovectores
autovalores = autov$values
autovalores

# Vectores propios
autovectores = autov$vectors
autovectores


## ----screeplot, fig.cap="\\label{fig:screeplot}Gráfico de sedimentación"----
ggplot(data.frame(factores = 1:length(autovalores), eig = autovalores),
       aes(x = factores, y = eig)) +
  geom_point(shape = 1) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_light() +
  labs(
    x = "factores",
    y = "Autovalor"
  )


## ------------------------------------------------------------------------
# Proporción de la varianza
prop_varianza = autovalores / length(autovalores)
scales::percent(prop_varianza)

# Varianza acumulada
varianza_acum = cumsum(prop_varianza)
scales::percent(varianza_acum)


## ------------------------------------------------------------------------
autoval = autovalores[1:2]
autovec = autovectores[, 1:2]


## ------------------------------------------------------------------------
cargas = autovec %*% sqrt(diag(autoval, nrow = length(autoval)))
colnames(cargas) = c("CP1", "CP2")
rownames(cargas) = colnames(CL)
cargas


## ------------------------------------------------------------------------
# comunidades
comunalidades = rowSums(cargas^2)
comunalidades


## ------------------------------------------------------------------------
# unicidades
unicidades = diag(mat_cor) - comunalidades
unicidades


## ------------------------------------------------------------------------
colSums(cargas^2)


## ------------------------------------------------------------------------
# Matriz de identidad
I = diag(1, nrow = ncol(cargas))
# Matriz de correlaciones reproducida
mat_cor_rep = cargas %*% I %*% t(cargas)
mat_cor_rep


## ------------------------------------------------------------------------
# Residuales
residuales = mat_cor - mat_cor_rep
residuales


## ----acp_plot, fig.cap="\\label{fig:acp_plot}Gráfico de componentes sin rotar"----
ggplot(
  data.frame(
    Materias = rownames(cargas),
    CP1 = cargas[, 1],
    CP2 = cargas[, 2]
  ),
  aes(CP1, CP2, label = Materias)
) +
  geom_point(color = "blue", size = 3) +
  geom_text(hjust = 0,
            nudge_x = 0.03,
            nudge_y = 0.03) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(
    x = paste0(
      "CP1 - ",
      scales::percent(prop_varianza, accuracy = .1)[1],
      " de variación"
    ),
    y = paste0(
      "CP2 - ",
      scales::percent(prop_varianza, accuracy = .1)[2],
      " de variación"
    )
  ) +
  theme_bw() +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1))


## ------------------------------------------------------------------------
rot_varimax = varimax(cargas)
cargas_rot = rot_varimax$loadings
cargas_rot


## ------------------------------------------------------------------------
# Convertir la clase "loadings" a clase "matrix"
cargas_2 = unclass(cargas_rot)

# Obtener la contribución de las variables a cada uno de los factores
colSums(cargas_2^2)

# Proporción de la varianza
prop_var_rot = colSums(cargas_2^2) / 4

# Porcentaje acumulado
cumsum(prop_var_rot)


## ------------------------------------------------------------------------
comunalid_rot = rowSums(cargas_2^2)
identical(round(comunalidades, 3), round(comunalid_rot, 3))


## ----acp_rot_plot, fig.cap="\\label{fig:acp_rot_plot}Gráfico de componentes con rotación Varimax"----
ggplot(
  data.frame(
    Materias = rownames(cargas_2),
    CP1 = cargas_2[, 1],
    CP2 = cargas_2[, 2]
  ),
  aes(CP1, CP2, label = Materias)
) +
  geom_point(color = "green", size = 3) +
  geom_text(vjust = 0,
            nudge_x = 0.05,
            nudge_y = 0.05) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(
    x = paste0(
      "CP1 - ",
      scales::percent(prop_var_rot, accuracy = .1)[1],
      " de variación"
    ),
    y = paste0(
      "CP2 - ",
      scales::percent(prop_var_rot, accuracy = .1)[2],
      " de variación"
    )
  ) +
  theme_bw() +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1))


## ------------------------------------------------------------------------
# Obtención de signos
signos_totales = sign(colSums(cargas))

# Aplicación sobre matriz factorial sin rotar
cargas_srt = cargas %*% diag(signos_totales)
colnames(cargas_srt) = c("CP1", "CP2")
cargas_srt


## ------------------------------------------------------------------------
# Varianza explicada o autovalores
colSums(cargas_srt^2)

# Prueba de que son idénticas
identical(round(colSums(cargas^2), 3), round(colSums(cargas_srt^2), 3))


## ---- include=FALSE------------------------------------------------------
pro_var_spss = scales::percent(colSums(cargas_srt^2) / 4, accuracy = 0.1)


## ----acp_spss, echo=FALSE, fig.cap="\\label{fig:acp_spss}Gráfico de componentes como se muestra en SPSS"----
ggplot(
  data.frame(
    Materias = rownames(cargas_srt),
    CP1 = cargas_srt[, 1],
    CP2 = cargas_srt[, 2]
  ),
  aes(CP1, CP2, label = Materias)
) +
  geom_point(color = "blue", size = 3) +
  geom_text(hjust = 1,
            nudge_x = 0.1,
            nudge_y = 0.1) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(
    x = paste0(
      "CP1 - ", pro_var_spss[1], " de variación"
    ),
    y = paste0(
      "CP2 - ", pro_var_spss[2], " de variación"
    )
  ) +
  theme_bw() +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1))


## ------------------------------------------------------------------------
# matriz de componente rotado
rot_varimax_2 = varimax(cargas_srt)
cargas_rot_2 = rot_varimax_2$loadings
unclass(cargas_rot_2)

# Varianza explicada
colSums(cargas_rot_2^2)


## ---- include=FALSE------------------------------------------------------
pro_var_spss_rot = scales::percent(colSums(cargas_rot_2^2) / 4, accuracy = 0.1)


## ----acp_rot_spss, echo=FALSE, fig.cap="\\label{fig:acp_rot_spss}Gráfico de componentes rotados como se muestra en SPSS"----
ggplot(
  data.frame(
    Materias = rownames(cargas_rot_2),
    CP1 = cargas_rot_2[, 1],
    CP2 = cargas_rot_2[, 2]
  ),
  aes(CP1, CP2, label = Materias)
) +
  geom_point(color = "green", size = 3) +
  geom_text(hjust = 1,
            vjust = 1,
            nudge_x = -0.02,
            nudge_y = -0.02) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(
    x = paste0(
      "CP1 - ", pro_var_spss_rot[1], " de variación"
    ),
    y = paste0(
      "CP2 - ", pro_var_spss_rot[2], " de variación"
    )
  ) +
  theme_bw() +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1))


## ------------------------------------------------------------------------
valores_acp = prcomp(CL, scale. = TRUE)

# Vectores propios
vec_prop = valores_acp$rotation
vec_prop

# Valores propios o varianza explicada
val_prop = valores_acp$sdev^2
val_prop

# Aplicando el mismo criterio de los factores
cargas_prcomp = vec_prop[, 1:2] %*% diag(valores_acp$sdev[1:2])
cargas_prcomp

# Signo como en SPSS
cargas_prcomp %*% diag(c(-1, -1))

# Porcentaje de varianza explicada
summary(valores_acp)

# Comunalidades
rowSums(cargas_prcomp^2)

# Matriz de correlaciones reproducida
cargas_prcomp %*% I %*% t(cargas_prcomp)

# Residuales
cor(CL) - cargas_prcomp %*% I %*% t(cargas_prcomp)


## ------------------------------------------------------------------------
varimax(cargas_prcomp)


## ------------------------------------------------------------------------
acp_psych = psych::principal(CL, nfactors = 2, rotate = "none")

# Cargas y varianza explicada
acp_psych$loadings

# Comunalidades
acp_psych$communality

# Unicidades 
acp_psych$uniquenesses


## ------------------------------------------------------------------------
acp_psych_rot = psych::principal(CL, nfactors = 2, rotate = "varimax")

acp_psych_rot$loadings

# residuales de correlación
residuals(acp_psych_rot)


## ------------------------------------------------------------------------
diag(kmo$ImCov)


## ------------------------------------------------------------------------
AF = psych::fa(CL, 2, rotate = "none", scores = "Anderson")

# Cargas
AF$loadings

# Comunalidades
AF$communality

# Unicidades
AF$uniquenesses


## ------------------------------------------------------------------------
# Valores propios de la matriz original
AF$e.values

# Proporcion de la varianza
AF$e.values / 4

# Suma acumulada de la proporción de la varianza
cumsum(AF$e.values / 4)

# Valores propios de la extracción
AF$values

# Proporcion de la varianza después de la extracción
AF$Vaccounted

# Matriz de correlaciones reproducida
psych::factor.model(AF$loadings)

# residuales de correlación
residuals(AF)


## ------------------------------------------------------------------------
AF_rot = psych::fa(CL, 2, rotate = "varimax", scores = "Anderson")

AF_rot$loadings

# Comunaliddades
AF_rot$communality

# Varianza
AF_rot$Vaccounted

# Matriz de correlaciones reproducida
psych::factor.model(AF_rot$loadings)

# residuales de correlación rotada
residuals(AF_rot)


## ---- include=FALSE------------------------------------------------------
prop_var_af = scales::percent(AF_rot$Vaccounted[2,], accuracy = 0.1)


## ----af_plot, echo=FALSE, fig.cap="\\label{fig:af_plot}Gráfico de factor con rotación varimax"----
ggplot(
  data.frame(
    Materias = rownames(AF_rot$loadings),
    F1 = AF_rot$loadings[, 1],
    F2 = AF_rot$loadings[, 2]
  ),
  aes(F1, F2, label = Materias)
) +
  geom_point(color = "orange", size = 3) +
  geom_text(hjust = 1,
            nudge_x = -0.03,
            nudge_y = -0.03) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(
    x = paste0(
      "Factor 1 - ", prop_var_af[1], " de variación"
    ),
    y = paste0(
      "Factor 2 - ", prop_var_af[2], " de variación"
    )
  ) +
  theme_bw() +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1))

