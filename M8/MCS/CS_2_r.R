#-------------------------------------------------------#
# 2.3. Análisis estadístico individual de cada variable.#
#-------------------------------------------------------#

#-------------------------------------------------------#
# 2.3.1 Inspección de los datos y filtrado de variables.#
#-------------------------------------------------------#

#Indicación del Directori de Trabajo.
#------------------------------------

setwd("C:/RW")

#Carga de Librerias nesarias para el script.
#-------------------------------------------

library(foreign)
library(dplyr)
library(scorecard)
library(reshape)
library(ClustOfVar)
library(perturb)

#Lectura y visualización de los datos de inicio desde SPSS.
#----------------------------------------------------------

df_csi64<-read.spss("C:/RW/FICH_73210_64_JOB.sav", use.value.labels=TRUE, max.value.labels=TRUE,
          to.data.frame=TRUE)
names(df_csi64)
fix(df_csi64)
glimpse(df_csi64)
summary(df_csi64)

#------------------------------------------------------------------------------------#
# Filtro de variables var_filter() package scorecard                                 #
# Esta función filtra variables sobre la base de condiciones especificas, tales como:#
# Valor de la Información, tasa de datos faltantes, tasa de valores indénticos.      #
#------------------------------------------------------------------------------------#

#Var_filter 64 variables
#-----------------------

dt<-df_csi64[,c(2:66)] # x variables desde df_csi64 data
numacredit<-df_csi64[,c(1:1)] # variable numero de acreditado desde df_csi64 data

x<-dt[,c(2:65)]  # X variables desde dt data
names(dt) 
names(x)

dt_sel=var_filter(dt,"CODDEFAULT", x = NULL, iv_limit = 0.02, missing_limit = 0.95, 
                  identical_limit = 0.95, var_rm = NULL,
                  var_kp = NULL, return_rm_reason = TRUE, positive = "bad|1")
dt_sel

lapply(dt_sel, dim)
str(dt_sel$dt) 
str(dt_sel$rm)

# Remover las columnas manualmente.
#----------------------------------

dt$rowid = row.names(dt)
dt_sel3 = var_filter(dt, y = "CODDEFAULT", var_rm = 'rowid')

names(dt_sel3)
View(dt_sel3)

#reconstruir el fichero con numero de aceditado, coddefault y
# 59 variables conservadas.
#------------------------------------------------------------

dtx<-dt_sel3[,c(1:59)] # x variables desde dt_sel3 data
dtcoddefault<-dt_sel3[,c(60:60)] # y desde dt_sel3 data
numacredit<-df_csi64[,c(1:1)] # variable numero de acreditado desde df_csi64 data
df_csi59<-cbind(numacredit,dtcoddefault,dtx)

names(df_csi59)
View(df_csi59)

#Guardamos nuestro data frame df_csi59 en el fichero "C:/RW/FICH_73210_59"
#-------------------------------------------------------------------------
saveRDS(object=df_csi59, file= "C:/RW/FICH_73210_59")

#-------------------------------------#
#2.3.2 Tratamiento de datos faltantes.#
#-------------------------------------#

# Las dos sentencias siguientes devuelven los nombre las variables 
# con datos faltantes.
#-----------------------------------------------------------------

list_na<-colnames(df_csi59)[apply(df_csi59,2,anyNA)]
list_na

# Exclusion a efectos informativos de las observaciones faltantes.
#-----------------------------------------------------------------

df_csi59_drop <- df_csi59 %>% na.omit()
dim(df_csi59_drop)

# Vamos a obtener con woebin los valores que sustituiran a los datos faltantes en las variables:
# X12, X46, X47, X49, X54, X55 y X56.
#-----------------------------------------------------------------------------------------------

df_csi59_wk1<-readRDS( file= "C:/RW/FICH_73210_59")
names(df_csi59_wk1)
summary(df_csi59_wk1)

woebin_wk = woebin(df_csi59_wk1,y="CODDEFAULT",x=c("X12", "X46", "X47", "X49", "X54", "X55", "X56"),
            var_skip = NULL,
            breaks_list = NULL,special_values = NULL, stop_limit = 0.1, count_distr_limit = 0.05,
            bin_num_limit = 8, positive = "bad|1", no_cores = NULL, print_step = 0L, method = "tree",
            save_breaks_list = NULL, ignore_const_cols = TRUE, ignore_datetime_cols = TRUE,
            check_cate_num = TRUE, replace_blank_inf = TRUE)

woebin_plot(woebin_wk, x=c("X12", "X46","X47", "X49", "X54", "X55", "X56"), title = NULL)

# Convertir woebin_wk en un datafrane
#------------------------------------

woebin_wk2=data.table::rbindlist(woebin_wk)

woebin_wk2

# Con woebin_ply convertimos los datos originales de entrada en "df_csi59_wk1" a los
# valores woe basados sobre la "binning information" generada por woebin ("woebin_wk2").
#---------------------------------------------------------------------------------------

woebin_ply_wk2=woebin_ply(df_csi59_wk1, woebin_wk2, print_step = 1L)
woebin_ply_wk2
summary(woebin_ply_wk2)

woebin_ply_wk2=rename(woebin_ply_wk2, c(X12_woe="X12_W", X46_woe="X46_W",X47_woe="X47_W", X49_woe="X49_W",
                                                X54_woe="X54_W", X55_woe="X55_W", X56_woe="X56_W"))

names(woebin_ply_wk2)

# Guardamos nuestro objeto woebin_ply_wk2 en un nuevo fichero df_csi59_woe consistente en el original
# mas 7 variables transformadas:
#                                X12_W, X46_W, X47_W,X49_W ,X54_W, X55_W y X56_W
#---------------------------------------------------------------------------------------------------

saveRDS(object=woebin_ply_wk2, file= "C:/RW/df_csi59_woe")
saveRDS(object=woebin_ply_wk2, file= "C:/RW/df_csi59_IC")

#Comprobacion de ficheros:
#-------------------------

df_csi59_woe<-readRDS( file= "C:/RW/df_csi59_woe")
names(df_csi59_woe)

df_csi59_IC<-readRDS( file= "C:/RW/df_csi59_IC")
names(df_csi59_IC)

#--------------------------------------------------------------------------------------#
# 2.4  Correlación, Multicolinealidad, Valor Informativo de las variables explicativas.#
#--------------------------------------------------------------------------------------#

#----------------------------------------------------#
# 2.4.1 Correlación entre las variables explicativas.#
#----------------------------------------------------#

df_csi59_IC <-readRDS( file= "C:/RW/df_csi59_IC")
names(df_csi59_IC)

# Cluster de variables #
#----------------------#

Xquant <- df_csi59_IC[,c(3:61)]
tree <- hclustvar(Xquant, init=NULL)

#Valores de tree: height, clusmat, merge

#Plot del Dendograma asociado a hclustvar
#----------------------------------------

plot(tree, type = "tree", sub = "hclustvar")

#Elección del número de cluster a través de la gráfica de agregación de niveles.
#-------------------------------------------------------------------------------

#Aggregation levels plot
plot(tree, type = "index")

saveRDS(object=tree, file= "C:/RW/df_csi59_tree_hclustvar")

# cutreev: corta un arbol jerarquico de variables obtenido a partir hclustvar en varios 
# cluster si se solicita para un número especificado de cluster.
#--------------------------------------------------------------------------------------

tree <-readRDS( file= "C:/RW/df_csi59_tree_hclustvar")

cutreevar<-cutreevar(tree,15)

#print(cutreevar)
#value:vae, sim, cluster, wss, E, size, scores, coef, 
#Call:

summary(cutreevar)

#----------------------------------------------------------#
# 2.4.2 Multicolinialidad entre las variables explicativas.#
#----------------------------------------------------------#


# 2.4.2.2  Análisis de la Multicolinealidad entre lasvariables explicativas con R.#
#----------------------------------------------------------------------------------#

# Regresion lineal lm()
#----------------------

df_csi59_IC<-readRDS( file= "C:/RW/df_csi59_IC")
names(df_csi59_IC)

# 1 - MCO con las 59 variables
#-----------------------------
dt<-df_csi59_IC[,c(2:61)] # x variables desde df_csi59_IC data
names(dt)
dt$CODDEFAULT=as.factor(dt$CODDEFAULT)

fit <- lm(y~.,data=dt)

summary(fit)

# 2 - MCO sin considerar las variables X13, X18 y X41.
#-----------------------------------------------------

dt<- df_csi59_IC[,c(2:12, 14:17, 19:37, 39:61)]   #No consideradas las variables X13, X18 y X41
dt$CODDEFAULT=as.numeric(dt$CODDEFAULT)

fit<- lm(CODDEFAULT~.,data=dt)
summary(fit)

# Cálculo de los VIF. Función vif() del paquete scorecard de R. 
#--------------------------------------------------------------
viffit <- vif(fit)
viffits <- viffit[order(-viffit$gvif)]
viffits

#-----------------------------------------------------------------#
# 2.4.2.3 Indices de Condición (CI) y proporcion de Varianza (VP).#
#-----------------------------------------------------------------#

df_csi59_IC<-readRDS( file= "C:/RW/df_csi59_IC")
names(df_csi59_IC)

dt<- df_csi59_IC[,c(2:12, 14:17, 19:37, 39:61)]   #No consideradas las variables X13, X18 y X41
dt$CODDEFAULT=as.numeric(dt$CODDEFAULT)

fit<- lm(CODDEFAULT~.,data=dt)
summary(fit)

cd<-colldiag(fit,center=TRUE)
print(cd,dec.plsces=5)

#-------------------------------------------------#
# 2.4.2.4 Autovalores de X'X (Belsey (1991)cmtest.#
#-------------------------------------------------#

library(mctest)
df_csi59_IC<-readRDS( file= "C:/RW/df_csi59_IC")
names(df_csi59_IC)

#eigprop sin considerar las variables X13, X18 y X41.
#----------------------------------------------------

dt<- df_csi59_IC[,c(2:12, 14:17, 19:37, 39:61)]   #No consideradas las variables X13, X18 y X41
dt$CODDEFAULT=as.numeric(dt$CODDEFAULT)
x<-dt[,c(2:57)]  # X variables desde dt data

eigprop(x, na.rm = TRUE, Inter = FALSE, prop = 0.5)

# Individual and Overall collinearity diagnostic Sin considerar las variables X13, X18 y X41. mctest
#---------------------------------------------------------------------------------------------------
 
library(mctest)
df_csi59_IC<-readRDS( file= "C:/RW/df_csi59_IC")
names(df_csi59_IC)

dt<- df_csi59_IC[,c(2:12, 14:17, 19:37, 39:61)]   #No consideradas las variables X13, X18 y X41
dt$CODDEFAULT=as.numeric(dt$CODDEFAULT)
y<-dt[,1]  # y variable desde dt data (CODDEFAULT, numeric)
x<-dt[,c(2:57)]  # X variables desde dt data

# 1 - Overall collinearity diagnostic sin considerar las variables X13, X18 y X41. mctest
# Determinant of correlation matrix, Farrar test of Chi-square, Red indicator, 
# sum of lambda inverse values, Theils’ indicator and CN.
#----------------------------------------------------------------------------------------

omcdiag(x, y, na.rm = TRUE, Inter = TRUE, detr = 0.01, red = 0.5, conf = 0.95, theil = 0.5, cn = 30)

# 2 - Individual collinearity diagnostic sin considerar las variables X13, X18 y X41. mctest
# Function imcdiag() detects the existence of multicollinearity due to certain X-variable. 
# This includes VIF, TOL, Klein’s rule, CVIF, F&G test of Chi-square and F-test.
#--------------------------------------------------------------------------------------------

imcdiag(x = x, y, vif =10, leamer = 0.05)   # with threshold of VIF and leamer method

imcdiag(x = x, y = y, all = TRUE)# Ceros y unos.

#Representación gráfica de VIF y autovalores.
#--------------------------------------------

mc.plot(x, y, Inter = FALSE, vif = 10, ev = 0.01)

#---------------------------------------------#
# 2.4.3 Poder explicativo de las 59 variables #
# Valor de la informacion.                    #
#---------------------------------------------#

df_csi59_IC<-readRDS( file= "C:/RW/df_csi59_IC")
names(df_csi59_IC)

dt<-df_csi59_IC[,c(2:61)] # x variables desde df_csi59_IC data
dt$CODDEFAULT=as.numeric(dt$CODDEFAULT)

x<-dt[,c(2:60)]  # X variables desde dt data

iv(dt,"CODDEFAULT", x=NULL, positive="bad|1", order = TRUE)

#---------------------------------------------------------------------------#
# 2.5.2.1  Construcción de las muestras de entrenamiento, validación y test.#
#---------------------------------------------------------------------------#

# Lectura del fichero de datos df_csi59_IC.
#------------------------------------------
dt<-readRDS( file= "C:/RW/df_csi59_IC")
names(dt)
dim(dt)

# 1. Partición de los datos en dos grupos: D (DEFAULT), ND (NO DEFAULT).
#-----------------------------------------------------------------------

X<- split(dt,dt$CODDEFAULT)
str(X)
SX<-lapply(seq_along(X),function(x) as.data.frame(X[[x]]))
ND<-SX[[1]]
str(ND)
D<-SX[[2]]

# 2. Obtención de los acreditados no default replicados 6 veces.
#---------------------------------------------------------------
library(dplyr)
 
# Extrae una muestra aleatoria de 11283 acreditados No Default.
#--------------------------------------------------------------
SND<-sample_n(ND,11283) #Extrae una muestra aleatoria de 11283 acreditados no default

# Replica cada elemento de ND 6 veces con la función rep()
#--------------------------------------------------------
SND6<- SND[rep(seq_len(nrow(SND)),6),]

# 3. Partición de los acreditados no default en tres muestras: trainND, cvrND
#    y testND (proporción 50%, 25% y 25% respectivamente.
#------------------------------------------------------------------------
SSND<-sample(rep(1:3, diff(floor(nrow(SND6)* c(0,0.5,0.75,1)))))
trainND<-SND6[SSND==1,]
testND<-SND6[SSND==2,]
cvrND<-SND6[SSND==3,]

# 4. Partición de los acreditados default en tres muestras: trainD, cvrD y
#    testD (proporción 50%, 25% y 25% respectivamente.
#-------------------------------------------------------------------------
SD<-sample(rep(1:3, diff(floor(nrow(D)* c(0,0.50,0.75,1)))))
trainD<-D[SD==1,]
testD<-D[SD==2,]
cvrD<-D[SD==3,]

# 5. Obtención de las muestras train59, cvr59 y test59 por rebind() de las
#    correspondientes muestras de default y no default.
#-------------------------------------------------------------------------
train59<-rbind(trainND,trainD)
cvr59<-rbind(cvrND,cvrD)
test59<-rbind(testND,testD)

totalr59<-rbind(train59,cvr59,test59)

# Guarda en el área de trabajo las muestras de entrenamiento (train59,
# validación (cvr59) y test (test59) y el total de los datos replicados 
# (totalr59).

saveRDS(object=train59, file= "C:/RW/df_csi59_train")
saveRDS(object=cvr59, file= "C:/RW/df_csi59_cvr")
saveRDS(object=test59, file= "C:/RW/df_csi59_test")

saveRDS(object=totalr59, file= "C:/RW/df_csi59_totalr")

