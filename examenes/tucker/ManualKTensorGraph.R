## ----setup, include=FALSE------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- eval=FALSE, include=FALSE------------------------------------------------------------------------------
## defin = data.frame(
##     esp = c("Eda", "Bsp", "Brh", "Bni", "Bpu", "Cen", "Ecd", "Rhi", "Hla", "Hab", "Par", "Cae", "Eig"),
##     Des = c(
##       "Ephemera danica",
##       "Baetis sp.",
##       "Baetis rhodani",
##       "Baetis niger",
##       "Baetis pumilus",
##       "Centroptilum",
##       "Ecdyonurus",
##       "Rhithrogena",
##       "Habrophlebia lauta",
##       "Habroletoides modesta",
##       "Paraletophlebia",
##       "Caenis",
##       "Ephemerella ignita"
##       ),
##     var = c("Temp.", "Flow", "pH", "Conduct.", "Oxygen", "BDO5", "Oxydab.", "Ammo.", "Nitrates", "Phosps.", "", "", ""),
##     Des = c(
##       "Temperatura",
##       "Flujo",
##       "Concentración de iones de hidrógeno",
##       "Conductividad",
##       "Oxígeno",
##       "Demanda biológica por oxígeno",
##       "Oxidabilidad",
##       "Amonio",
##       "Nitratos",
##       "Fosfato",
##       "", "", ""
##       )
##     )
## 
## knitr::kable(defin, format = "latex", col.names = c("Especies", "Descripción", "Variables", "Descripción"))


## ---- eval=FALSE---------------------------------------------------------------------------------------------
## install.packages("KTensorGraphs")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## ---- message=FALSE------------------------------------------------------------------------------------------
library(KTensorGraphs)


## ---- eval=FALSE---------------------------------------------------------------------------------------------
## setwd(dirname(rstudioapi::getSourceEditorContext()$path))
## # Descripción de funciones
## # rstudioapi::getSourceEditorContext()$path # muestra el directorio del script
## # dirname() # Extrae el directorio padre del archivo donde se trabajará todo
## # setwd()   # Establece el directorio de trabajo


## ---- eval=FALSE---------------------------------------------------------------------------------------------
## setwd("C:/Users/usuario/carpeta/personalizada")


## ------------------------------------------------------------------------------------------------------------
variables <- read.table("variables.txt")
class(variables)
dim(variables) # Filas y columnas


## ------------------------------------------------------------------------------------------------------------
colnames(variables)


## ------------------------------------------------------------------------------------------------------------
especies <- read.table("especies.txt")
class(especies)
dim(especies)
colnames(especies)


## ------------------------------------------------------------------------------------------------------------
nom_esp <- dimnames(especies)
nom_var <- dimnames(variables)
class(nom_var)


## ------------------------------------------------------------------------------------------------------------
names(nom_var) <- names(nom_esp) <- c("filas", "columnas")
nom_var


## ------------------------------------------------------------------------------------------------------------
nom_filas <- nom_esp$filas
nom_col_x <- nom_esp$columnas[1:13]
nom_col_y <- nom_var$columnas[1:10]

# Como no existen previamente los nombres de las estaciones,
# los establecemos manualmente
nom_repet <- c("Primavera", "Verano", "Otoño", "Invierno")


## ------------------------------------------------------------------------------------------------------------
n_repet <- length(nom_repet)
n_filas <- nrow(especies)
n_col_x <- ncol(especies)  / n_repet
n_col_y <- ncol(variables) / n_repet


## ------------------------------------------------------------------------------------------------------------
esp_cubo <- array(
  data = as.matrix(especies),
  dim = c(n_filas, n_col_x, n_repet),
  dimnames = list(nom_filas, nom_col_x, nom_repet)
)
round(esp_cubo, 2) # Se redondean los números para impresión sin cambiar el tensor


## ------------------------------------------------------------------------------------------------------------
var_cubo <- array(
  data = as.matrix(variables),
  dim = c(n_filas, n_col_y, n_repet),
  dimnames = list(nom_filas, nom_col_y, nom_repet)
)
round(var_cubo, 2)


## ------------------------------------------------------------------------------------------------------------
col_filas <- rep("#428bca", times = n_filas)
col_col_x <- rep("#d9534f", times = n_col_x)
col_col_y <- rep("#5cb85c", times = n_col_y)


## ---- eval=FALSE---------------------------------------------------------------------------------------------
TUCKER3(X = var_cubo, norm = TRUE)


## ------------------------------------------------------------------------------------------------------------
TUCKER3(X = var_cubo, norm = TRUE, contr = TRUE)


## ---- eval=FALSE---------------------------------------------------------------------------------------------
TUCKER3(X = var_cubo,
        norm = TRUE,
        contr = TRUE,
        p = 3,
        q = 4,
        r = 3,
        coloresf = col_filas,
        coloresc = col_col_y)

## ---- eval=FALSE---------------------------------------------------------------------------------------------
TUCKER3(X = var_cubo,
        norm = TRUE,
        p = 3,
        q = 4,
        r = 3,
        P1 = 1,
        P2 = 2,
        Q1 = 1,
        Q2 = 2,
        R1 = 1,
        R2 = 2,
        coloresf = col_filas,
        coloresc = col_col_y)


## ---- eval=FALSE---------------------------------------------------------------------------------------------
TUCKER3(X = var_cubo,
        norm = TRUE,
        p = 3,
        q = 4,
        r = 3,
        P1 = 2,
        P2 = 3,
        Q1 = 3,
        Q2 = 4,
        R1 = 1,
        R2 = 2,
        coloresf = col_filas,
        coloresc = col_col_y)

