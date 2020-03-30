setwd("C:/RW")

# Leer fichero definitivo con las variables seleccionadas.
#---------------------------------------------------------

dtotalr<-readRDS(file="C:/RW/df_csi59_totalr")
df_totalr15 <- dplyr::select(dtotalr,CODDEFAULT,X5,X7,X8,X19,X25,X45,X46_W,X47_W,X48, 
                                         X49_W,X53,X55_W,X57,X58,X63)
df_totalr11 <- dplyr::select(dtotalr,CODDEFAULT,X5,X7,X8,X19,X47_W,X48,
                                         X53,X55_W,X57,X58,X63)

# Carga algunos paquetes r para la manipulación de datos:
#--------------------------------------------------------
 
library(magrittr)
library(scorecard)
library(dplyr)
library(tidyr)
library(ggplot2)

#------------------------------------------------------#
# 5.2.1.- Reducción de la Complejidad del Modelo mod15.#
#------------------------------------------------------#

 # comprobando los datos. Filtrado de variables  via tasa de datos faltantes,iv,
# y tasa de valores idénticos.
#------------------------------------------------------------------------------

dt_f = var_filter(df_totalr15, "CODDEFAULT")

# Partiendo los datos dt_f en tres muestras, entrenamiento, validación y test
#----------------------------------------------------------------------------

dt_list = split_df(dt_f, y = "CODDEFAULT", ratio = c(0.5, 0.25, 0.25), seed = 618, no_dfs = 3,
                name_dfs = c("train","cvr", "test"))

label_list = lapply(dt_list, function(x) x$CODDEFAULT)

saveRDS(object=dt_list, file= "C:/RW/dt_list15")
saveRDS(object=label_list, file= "C:/RW/label_list15")

#-----------------------------------------------------------------#
# 5.1.2 Discretización WOE de las 15 variables pre-seleccionadas  #
#       woe binning: convirtiendo train, cvr y test a valores WOE #
#-----------------------------------------------------------------#

bins = woebin(dt_list$train, "CODDEFAULT")

saveRDS(object=bins, file= "C:/RW/bins15")

dt_woe_list = lapply(dt_list, function(x) woebin_ply(x, bins))

saveRDS(object=dt_woe_list, file= "C:/RW/dt_woe_list15")

names(dt_woe_list$train)

# Guardando los data set dt_woe_list$train, dt_woe_list$cvr and dt_woe_list$test en "C:/WR".
#-------------------------------------------------------------------------------------------

saveRDS(object= dt_woe_list$train, file= "C:/RW/df_csi15_woe_train")
saveRDS(object=dt_woe_list$cvr, file= "C:/RW/df_csi15_woe_cvr")
saveRDS(object=dt_woe_list$test, file= "C:/RW/df_csi15_woe_test")

dt_woe_totalr = rbind(dt_woe_list$train,dt_woe_list$cvr,dt_woe_list$test)
saveRDS(object= dt_woe_totalr, file= "C:/RW/df_csi15_woe_totalr")

# Ajuste por Regresión Logistica Lineal para las 15 variables preseleccionadas #
#------------------------------------------------------------------------------#
m1 = glm(CODDEFAULT ~ ., family = binomial(), data = dt_woe_list$train)
summary(m1)

saveRDS(object=m1, file= "C:/RW/mod15woe")

scorecard::vif(m1, merge_coef = TRUE)

#---------------------------------------------------------------------#
# 5.2.2 Reducción de la complejidad del modelo de 15 variables.       #          #
#       Selección de la estructura reducida en base a AIC, stepAIC(). #
#---------------------------------------------------------------------#
library(MASS)
m_step <- stepAIC(m1, direction = "both", trace = FALSE)
m2 = eval(m_step$call)
summary(m2)

scorecard::vif(m2, merge_coef = TRUE)

saveRDS(object=m2, file= "C:/RW/mod15AICwoe")

#--------------------------------------------------------------#
#  5.3.1.1.- Modelo de 11 variables con valores woe (mod11woe) #
#--------------------------------------------------------------#
dt_f = var_filter(df_totalr11, "CODDEFAULT")

# Partiendo los datos dt_f en tres muestras,  entrenamiento (train), validacion (cvr)
# y test (test).
#---------------------------------------------------------------------------------------------------
dt_list = split_df(dt_f, y = "CODDEFAULT", ratio = c(0.5, 0.25, 0.25), seed = 618, no_dfs = 3,
                name_dfs = c("train","cvr", "test"))
label_list = lapply(dt_list, function(x) x$CODDEFAULT)

saveRDS(object=dt_list, file= "C:/RW/dt_list11")
saveRDS(object=label_list, file= "C:/RW/label_list11")

# Tramado por pesos de la evidencia woe, woebin().
#-------------------------------------------------
# woebin () devuelve una lista con un elemento para cada variable. También hay una gráfica 
# que podemos usar para hacer trazados significativos y verificar el tramado, woebin_plot ().

bins <- woebin(dt_list$train,"CODDEFAULT")
bins

saveRDS(object=bins, file= "C:/RW/bins11")

# Convirtiendo los data set train, cvr and test en valores woe, woebin_ply().
#----------------------------------------------------------------------------
# Tomamos la lista con toda la información de tramado y a través de woebin_ply transformamos
# nuestro conjunto de datos en un conjunto de datos de valor WOE

dt_woe_list = lapply(dt_list, function(x) woebin_ply(x, bins))  
dt_woe_list

saveRDS(object=dt_woe_list, file= "C:/RW/dt_woe_list11")

#woebinplot para cada variable
#-----------------------------

plotlist <-woebin_plot(bins, x = NULL, title = "")
plotlist

# save binning plot
 for (i in 1:length(plotlist)) {
   ggplot2::ggsave(
      paste0("c:/RW/woebinplot11/", names(plotlist[i]), ".png"), plotlist[[i]],
      width = 15, height = 9, units="cm" ) }


# Guardando los data set dt_woe_list$train, dt_woe_list$cvr and dt_woe_list$test en "C:/WR".
#-------------------------------------------------------------------------------------------

saveRDS(object= dt_woe_list$train, file= "C:/RW/df_csi11_woe_train")
saveRDS(object=dt_woe_list$cvr, file= "C:/RW/df_csi11_woe_cvr")
saveRDS(object=dt_woe_list$test, file= "C:/RW/df_csi11_woe_test")

dt_woe_totalr = rbind(dt_woe_list$train,dt_woe_list$cvr,dt_woe_list$test)
saveRDS(object= dt_woe_totalr, file= "C:/RW/df_csi11_woe_totalr")

#-------------------------------------------------------------------------------#
# 5.3.1.2 Ajuste de la muestra de entrenamiento woe, dt_woe_list$train, por RLL.#
#-------------------------------------------------------------------------------#
library(MASS)

m1 = glm(CODDEFAULT ~ ., family = binomial(), data = dt_woe_list$train)

summary(m1)

scorecard::vif(m1, merge_coef = TRUE)

saveRDS(object=m1, file= "C:/RW/mod11woe")

#-------------------------------------------------------------------------#
# 5.3.1.3 Comprobación de la estructura reducida basandonos en AIC, step()# 
#-------------------------------------------------------------------------#

m_step <- stepAIC(m1, direction = "both", trace = FALSE)
m2 = eval(m_step$call)
summary(m2)

scorecard::vif(m2, merge_coef = TRUE)

saveRDS(object=m2, file= "C:/RW/mod11AICwoe")

#---------------------------------------------------------------------------------#
# 5.4. Cálculo de la Bondad de Ajuste de los modelos mod15woe y mod11woe sobre la #
#      Muestra de Entrenamiento, (dtrain).                                        #
#---------------------------------------------------------------------------------#

# 5.4.1 Función BondadAjuste
#---------------------------

BondadAjuste=function(model,nombremodel,muestra,numacr,numvar)
{
 modnulo <- glm(CODDEFAULT ~ 1,family=binomial(link="logit"),data=muestra) 
 
#Desvianza del modelo nulo 
devianceM0 <- modnulo$deviance

#A)Medidas del ajuste del modelo basadas en la log-verosimilitud negativa.

# Logverosimilitud del modelo nulo

logverM0=-devianceM0/2

# Desvianza de model
devianceM <- model$deviance

# N: número de acreditados, nunvar: número de variables y p: numvar+1
N=numacr
p=numvar+1

# Logverosimilitud negativa del model 
logverM=-devianceM/2

# Diferencia de las Logverosimilitudes negativas (modnulo – model)
logverDif=logverM0 - logverM

# -2*logverosimilitud negativa del modelo completo
logvernegM=-2*logverM

# Error Empírico del modelo completo en la muestra de entrenamiento
ErEmp= -2*logverM/N

#B)PSEUDO COEFICIENTES DE DETERMINACIÓN GENERALIZADA.

#Pseudo Coeficiente de McFADDEN R2U.
McF = 1-logverM/logverM0

#pseudo-R2 de Cox y Snell
CS=1-exp(2*logverDif/N)

#Pseudo Coeficiente de NAGELKERKE.
NG = (1-exp(2/N*(logverM0 - logverM)))/(1-exp(2/N*logverM0))

#C)Bondad de Ajuste basada en Citerios de Información, AIC y BIC.

# Criterio de Informacion de Akaike,AIC.
AIC=-2*logverM+2*p

# Criterio de Informacion Bayesiana,BIC.
BIC=-2*logverM+log(N)*p

#D)Medidas de ajuste del modelo basadas en el test de Hosmer-Lemeshow.

#Test Hosmer-Lemeshow sobre las muestra.
# Para este test se requiere el paquete ResourceSelection

library(ResourceSelection)

hl.muestra <- hoslem.test(muestra$CODDEFAULT, fitted(model), g = 10)

return(list("Bondad de Ajuste:"=nombremodel,
"Número de acreditados, N" = N,
"Número de variables del modelo, p"=numvar,
"Logverosimilitud negativa del modelo nulo"=logverM0,
"Logverosimilitud negativa del modelo ajustado"=logverM,
"logverM0 - logverM"=logverDif,
"-2*logverosimilitud negativa del modelo"=logvernegM,
"Error Empírico de modelo"=ErEmp,
"Pseudo Coeficiente de McFadden R2U"=McF,
"Pseudo-R2 de Cox y Snell"=CS,
"Pseudo Coeficiente de Nagelkerke"=NG,
"Criterio de Informacion de Akaike,AIC"=AIC,
"Criterio de Información Bayesiana,BIC"=BIC,
"Test Hosmer-Lemeshow sobre las muestra"=hl.muestra))
}

# 5.4.2. Bondad de Ajuste del modelo mod15woe sobre la Muestra de Entrenamiento woe. 
#-----------------------------------------------------------------------------------

dt <- readRDS( file= "C:/RW/df_csi15_woe_train")
mod15woe <- readRDS(file = "C:/RW/mod15woe")

model=mod15woe
nombremodel="mod15woe"
muestra=dt
numacr=dim(dt)[1]
numvar=15
BondadAjuste(model,nombremodel,muestra,numacr,numvar)

# 5.4.3. Bondad de Ajuste del modelo mod11woe sobre la Muestra de Entrenamiento woe. 
#-----------------------------------------------------------------------------------

dt <- readRDS( file= "C:/RW/df_csi11_woe_train")

mod11woe <- readRDS(file = "C:/RW/mod11woe")

model=mod11woe
nombremodel="mod11woe"
muestra=dt
numacr=dim(dt)[1]
numvar=11
BondadAjuste(model,nombremodel,muestra,numacr,numvar)

#-------------------------------------------------------------------------#
# 5.5 Cálculo del Poder Discriminante del Modelo mod12woe en el sistema R # 
#      sobre la Muestra de Entrenamiento, (train).                        #              #
#-------------------------------------------------------------------------#

# 5.5.1 Función PoderDiscriminante
#---------------------------------

PoderDiscriminante=function(model,nombremodel,muestra,numacr,numvar)
{
library(pROC)
library(scorecard)

# N: número de acreditados, nunvar: número de variables y p: numvar+1
N=numacr
p=numvar+1

#Tabla de clasificación.

muestra.prob<-predict(model,newdata=muestra,type="response")
muestra.pred<-ifelse(muestra.prob>.5,"1","0")
tabla=table(muestra.pred, muestra$CODDEFAULT)
media=mean(muestra.pred ==muestra$CODDEFAULT)
errtip1=tabla[2:2]/(tabla[2:2]+tabla[1:1])
errtip2=tabla[3:3]/(tabla[3:3]+tabla[4:4])

#Curva ROC (se requiere el paquete ROCR).

library(ROCR)
muestra.roc <- prediction(muestra.prob, muestra$CODDEFAULT)
plot(performance(muestra.roc, "tpr", "fpr"), col = "red", main = "Curva ROC")
abline(0, 1, lty = 8, col = "blue")

#AUC del modelo.

muestra.auc <- performance(muestra.roc,"auc")
AUC=slot(muestra.auc, "y.values")

# Test de Kolmogorov-Smirnov para la muestra de entrenamiento.

ks.muestra <- performance(muestra.roc, "tpr", "fpr")
muestra.ks<-max(attr(ks.muestra,"y.values")[[1]]-(attr(ks.muestra,"x.values")[[1]]))
# muestra.ks

return(list("Poder Discriminante"=nombremodel,
"Número de acreditados, N" = N,
"Número de variables del modelo, p"=numvar,
"Tabla de Clasificación del modelo sobre la muestra"=tabla,
"Riesgo de Crédito (Error Tipo I)"=errtip1,
"Coste de Oportunidad de Negocio (Error Tipo II)"=errtip2,
"Media Predicción"=media,
"Area Bajo la Curva"=AUC,
"Test Kolmogorov-Smirnov"=muestra.ks))
}

#---------------------------------------------------------------------------------#
# 5.5.2 Poder discriminante del modelo mod15woe sobre la Muestra de Entrenamiento.#
#---------------------------------------------------------------------------------#

dt <- readRDS( file= "C:/RW/df_csi15_woe_train")
mod15woe <- readRDS(file = "C:/RW/mod15woe")

model=mod15woe
nombremodel="mod15woe"
muestra=dt
numacr=dim(dt)[1]
numvar=15
PoderDiscriminante(model,nombremodel,muestra,numacr,numvar)

#---------------------------------------------------------------------------------#
# 5.5.3 Poder discriminante del modelo mod11woe sobre la Muestra de Entrenamiento.#
#---------------------------------------------------------------------------------#

dt <- readRDS( file= "C:/RW/df_csi11_woe_train")

mod11woe <- readRDS(file= "C:/RW/mod11woe")

model=mod11woe
nombremodel="mod11woe"
muestra=dt
numacr=dim(dt)[1]
numvar=11
PoderDiscriminante(model,nombremodel,muestra,numacr,numvar)

#-----------------------------------------------------------------------------------#
#  5.6 Criterio de Información de Akaike (AIC) y Criterios de Información Bayesiano # 
#      (BIC) sobre la muestra de validación.                                        #
#-----------------------------------------------------------------------------------#

# 5.6.1 Función ValidaciónAICyBIC.
#---------------------------------

ValidaciónAICyBIC=function(model,nombremodel,muestra,numacr,numvar)
{
# Desvianza de model
devianceM <- model$deviance

# N: número de acreditados, nunvar: número de variables y p: numvar+1
N=numacr
p=numvar+1

# Logverosimilitud negativa del model 
logverM=-devianceM/2

# Citerios de Información, AIC y BIC.

# Criterio de Informacion de Akaike,AIC.
AIC=-2*logverM+2*p

# Criterio de Informacion Bayesiana,BIC.
BIC=-2*logverM+log(N)*p

return(list("Validación de los Criterios de Información AIC y BIC:"=nombremodel,
"Número de acreditados, N:" = N,
"Número de variables del modelo, p:"=numvar,
"Logverosimilitud negativa del modelo ajustado"=logverM,
"Criterio de Informacion de Akaike,AIC:"=AIC,
"Criterio de Información Bayesiana,BIC:"=BIC))
}

#--------------------------------------------------------------------------#
# 5.6.2 Validación Criterios de Información AIC y BIC del modelo mod15woe, #
#       sobre la Muestra de Validación.                                    #
#--------------------------------------------------------------------------#

dt <- readRDS( file= "C:/RW/df_csi15_woe_cvr")
mod15woe <- readRDS(file = "C:/RW/mod15woe")
model=mod15woe
nombremodel="mod15woe"
muestra=dt
numacr=dim(dt)[1]
numvar=15
ValidaciónAICyBIC(model,nombremodel,muestra,numacr,numvar)

#--------------------------------------------------------------------------#
# 5.6.3 Validación Criterios de Información AIC y BIC del modelo mod11woe, #
#       sobre la Muestra de Validación.                                    #
#--------------------------------------------------------------------------#

dt <- readRDS( file= "C:/RW/df_csi11_woe_cvr")
mod11woe <- readRDS(file= "C:/RW/mod11woe")
model=mod11woe
nombremodel="mod11woe"
muestra=dt
numacr=dim(dt)[1]
numvar=11
ValidaciónAICyBIC(model,nombremodel,muestra,numacr,numvar)

#---------------------------------------------------------------------------------#
# 5.7 Poder Discriminante de mod15woe y mod11woe  sobre la muestra de Validación. #
#---------------------------------------------------------------------------------#

# Para la selección de modelos es habitual, por su demostrada eficacia, utilizar como medida de 
# poder discriminante el Área bajo la Curva ROC, AUC calculados sobre la muestra de validación. 

# 5.7.1 Poder Discriminante de mod15woe sobre la muestra de Validación. 
#----------------------------------------------------------------------

dt <- readRDS( file= "C:/RW/df_csi15_woe_cvr")

mod15woe <- readRDS(file= "C:/RW/mod15woe")

model=mod15woe
nombremodel="mod15woe"
muestra=dt
numacr=dim(dt)[1]
numvar=15
PoderDiscriminante(model,nombremodel,muestra,numacr,numvar)

# 5.7.2 Poder Discriminante de mod11woe sobre la muestra de Validación.
#----------------------------------------------------------------------

dt <- readRDS( file= "C:/RW/df_csi11_woe_cvr")

mod11woe <- readRDS(file= "C:/RW/mod11woe")

model=mod11woe
nombremodel="mod11woe"
muestra=dt
numacr=dim(dt)[1]
numvar=11
PoderDiscriminante(model,nombremodel,muestra,numacr,numvar)

#------------------------------------------------------------------------------------#
# 5.8 Error de Predicción sobre la muestra de validación. Validacion Cruzada K_folds.# 
#------------------------------------------------------------------------------------#

dtrain15woe <- readRDS( file= "C:/RW/df_csi15_woe_train")
dcvr15woe <- readRDS( file= "C:/RW/df_csi15_woe_cvr")
dtest15woe <- readRDS( file= "C:/RW/df_csi15_woe_test")

dtotal15woe <- rbind(dtrain15woe,dcvr15woe,dtest15woe)

dtrain11woe <- readRDS( file= "C:/RW/df_csi11_woe_train")
dcvr11woe <- readRDS( file= "C:/RW/df_csi11_woe_cvr")
dtest11woe <- readRDS( file= "C:/RW/df_csi11_woe_test")

dtotal11woe <- rbind(dtrain11woe,dcvr11woe,dtest11woe)

#---------------------------------------------------------------------------#
# 5.8.1 Función Error de Predicción por Validacion Cruzada k_Folds, VC_K_F, #
#       en R usando el paquete boot.                                        #
#---------------------------------------------------------------------------#

library(boot)

ValidacionCruzada_CV_K_F_errpred=function(modelo,muestra,Nfolds)
{
# Puesto que la respuesta es una variable binaria, una función de coste apropiada es la siguiente:
cost <- function(r, pi) mean(abs(r-pi)> 0.5)

cv.err=cv.glm(muestra,modelo,K=Nfolds, cost = cost)
cv.err$delta[1]
}

#------------------------------------------------------------------------------#
# 5.8.2 Error de predicción de mod15woe por VC sobre la muestra de validación. # 
#------------------------------------------------------------------------------#

dt=dtotal15woe

mod15woeVC=glm(CODDEFAULT~.,family=binomial(link="logit"),data=dt)

modelo=mod15woeVC
muestra=dt
Nfolds=10

ValidacionCruzada_CV_K_F_errpred(modelo,muestra,Nfolds)

#------------------------------------------------------------------------------#
# 5.8.3 Error de predicción de mod11woe por VC sobre la muestra de validación. # 
#------------------------------------------------------------------------------#

dt=dtotal11woe

mod11woeVC=glm(CODDEFAULT~.,family=binomial(link="logit"),data=dt)

modelo=mod11woeVC
muestra=dt
Nfolds=10

ValidacionCruzada_CV_K_F_errpred(modelo,muestra,Nfolds)

#----------------------------------------------------------------------------------#
# 5.9. Cálculo de la precisión del modelo (Accuracy models) por Validacion Cruzada #
# k_Folds)R usando la función cv.binary, paquete {DAAG}.                           #
#----------------------------------------------------------------------------------#

library(DAAG)

# 5.9.1 Precisión mod15woe sobre la muestra total woe.
#-----------------------------------------------------

dt=dtotal15woe

mod15woeVC=glm(CODDEFAULT~.,family=binomial(link="logit"),data=dt)

modelo=mod15woeVC
K=10

ermod15woeVC <- cv.binary(modelo,rand=NULL,nfolds=K,print.details=TRUE)
ermod15woeVC$acc.cv

# 5.9.2 Precisión mod11woe sobre la muestra total woe.
#-----------------------------------------------------

dt=dtotal11woe

mod11woeVC=glm(CODDEFAULT~.,family=binomial(link="logit"),data=dt)

modelo=mod11woeVC
K=10

ermod11woeVC <- cv.binary(modelo,rand=NULL, nfolds=K,print.details=TRUE)
ermod11woeVC$acc.cv

#-----------------------------------------#
# 5.10 Error de Generalición. Err. CV_K_F #
#-----------------------------------------#

#install.packages("regclass")
library(regclass)

# 5.10.1 Función ErrorGeneralizacion_CV_K_F.
#-------------------------------------------

ErrorGeneralizacion_CV_K_F=function(modelo,muestra,Nfolds,Repetic)
{
generalization_error(modelo,muestra,Kfold=TRUE,K=Nfolds,R=Repetic,seed=1050)
}
  
# 5.10.2 mod15woe sobre la muestra train. Error Generalización sobre la muesta test.
#-----------------------------------------------------------------------------------

modelo=mod15woe
muestra=dtest15woe
Nfolds=10
Repetic=10

ErrG<-ErrorGeneralizacion_CV_K_F(modelo,muestra,Nfolds,Repetic)
ErrG

# 5.10.3 mod11woe sobre la muestra train. Error Generalización sobre la muesta test.
#-----------------------------------------------------------------------------------

modelo=mod11woe
muestra=dtest11woe
Nfolds=10
Repetic=10

ErrG<-ErrorGeneralizacion_CV_K_F(modelo,muestra,Nfolds,Repetic)
ErrG



