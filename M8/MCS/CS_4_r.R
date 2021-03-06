#--------------------------------------#
# 4.2. Bondad de Ajuste de los Modelos.#
#--------------------------------------#

#--------------------------------------------------------------------------------#
#4.2.2 C�lculo de la Bondad de Ajuste de los Modelos CMM en el sistema R sobre la#
#      Muestra de Entrenamiento, (dtrain).                                       #
#--------------------------------------------------------------------------------#

dt <- readRDS( file= "C:/RW/df_csi59_train")

# 4.2.2.1 Funci�n BondadAjuste()
#-------------------------------

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

# N: n�mero de acreditados, nunvar: n�mero de variables y p: numvar+1
N=numacr
p=numvar+1

# Logverosimilitud negativa del model 
logverM=-devianceM/2

# Diferencia de las Logverosimilitudes negativas (modnulo � model)
logverDif=logverM0 - logverM

# -2*logverosimilitud negativa del modelo completo
logvernegM=-2*logverM

# Error Emp�rico del modelo completo en la muestra de entrenamiento
ErEmp= -2*logverM/N

#B)PSEUDO COEFICIENTES DE DETERMINACI�N GENERALIZADA.

#Pseudo Coeficiente de McFADDEN R2U.
McF = 1-logverM/logverM0

#pseudo-R2 de Cox y Snell
CS=1-exp(2*logverDif/N)

#Pseudo Coeficiente de NAGELKERKE.
NG = (1-exp(2/N*(logverM0 - logverM)))/(1-exp(2/N*logverM0))

#C)Bondad de Ajuste basada en Citerios de Informaci�n, AIC y BIC.

# Criterio de Informacion de Akaike,AIC.
AIC=-2*logverM+2*p

# Criterio de Informacion Bayesiana,BIC.
BIC=-2*logverM+log(N)*p

#D)Medida de ajuste del modelo basada en el test de Hosmer-Lemeshow.

#Test Hosmer-Lemeshow sobre las muestra.
# Para este test se requiere el paquete ResourceSelection

library(ResourceSelection)

hl.muestra <- hoslem.test(muestra$CODDEFAULT, fitted(model), g = 10)

return(list("Bondad de Ajuste:"=nombremodel,
"N�mero de acreditados, N" = N,
"N�mero de variables del modelo, p"=numvar,
"Logverosimilitud negativa del modelo nulo"=logverM0,
"Logverosimilitud negativa del modelo ajustado"=logverM,
"logverM0 - logverM"=logverDif,
"-2*logverosimilitud negativa del modelo"=logvernegM,
"Error Emp�rico de modelo"=ErEmp,
"Pseudo Coeficiente de McFadden R2U"=McF,
"Pseudo-R2 de Cox y Snell"=CS,
"Pseudo Coeficiente de Nagelkerke"=NG,
"Criterio de Informacion de Akaike,AIC"=AIC,
"Criterio de Informaci�n Bayesiana,BIC"=BIC,
"Test Hosmer-Lemeshow sobre las muestra"=hl.muestra))
}

#--------------------------------------------------------------------------------#
# 4.2.2.2 Bondad de Ajuste del modelo completo sobre la Muestra de Entrenamiento.#
#--------------------------------------------------------------------------------#

modcompleto <- readRDS(file = "C:/RW/modcompleto")
model=modcompleto
nombremodel="modcompleto"
muestra=dt
numacr=dim(dt)[1]
numvar=25
BondadAjuste(model,nombremodel,muestra,numacr,numvar)

#--------------------------------------------------------------------------------#
# 4.2.2.3 Bondad de Ajuste del modelo d�bil sobre la Muestra de Entrenamiento.   #
#--------------------------------------------------------------------------------#

moddebil <- readRDS(file= "C:/RW/moddebil")
model=moddebil
nombremodel="moddebil"
muestra=dt
numacr=dim(dt)[1]
numvar=13
BondadAjuste(model,nombremodel,muestra,numacr,numvar)

#--------------------------------------------------------------------------------#
# 4.2.2.4 Bondad de Ajuste del modelo paso a paso,(Backwards-Forwards), sobre la #
#         la Muestra de Entrenamiento.                                           #
#--------------------------------------------------------------------------------#

modpasos <- readRDS(file= "C:/RW/modpasos")
model=modpasos
nombremodel="modpasos"
muestra=dt
numacr=dim(dt)[1]
numvar=17
BondadAjuste(model,nombremodel,muestra,numacr,numvar)

#---------------------------------------------------------------------------------------#
# 4.2.2.5 Bondad de Ajuste del modelo de 15 variables sobre la Muestra de Entrenamiento.#
#---------------------------------------------------------------------------------------#

mod15 <- readRDS(file= "C:/RW/mod15")
model=mod15
nombremodel="mod15"
muestra=dt
numacr=dim(dt)[1]
numvar=15
BondadAjuste(model,nombremodel,muestra,numacr,numvar)

#--------------------------------------------------------------------#
# 4.3.2 C�lculo del Poder Discriminante de los Modelos CMM  sobre la #
#       Muestra de Entrenamiento, (dtrain).                          #
#--------------------------------------------------------------------#

#4.3.2.1 Funci�n PoderDiscriminante.
#-----------------------------------
 
PoderDiscriminante=function(model,nombremodel,muestra,numacr,numvar)
{
library(pROC)
library(scorecard)

# N: n�mero de acreditados, nunvar: n�mero de variables y p: numvar+1
N=numacr
p=numvar+1

#Tabla de clasificaci�n.

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
"N�mero de acreditados, N" = N,
"N�mero de variables del modelo, p"=numvar,
"Tabla de Clasificaci�n del modelo sobre la muestra"=tabla,
"Riesgo de Cr�dito (Error Tipo I)"=errtip1,
"Coste de Oportunidad de Negocio (Error Tipo II)"=errtip2,
"Media Predicci�n"=media,
"Area Bajo la Curva"=AUC,
"Test Kolmogorov-Smirnov"=muestra.ks))
}

#4.3.2.2 Poder discriminante del modelo completo sobre la Muestra de Entrenamiento.
#----------------------------------------------------------------------------------

model=modcompleto
nombremodel="modcompleto"
muestra=dt
numacr=dim(dt)[1]
numvar=25
PoderDiscriminante(model,nombremodel,muestra,numacr,numvar)

#4.3.2.3 Poder discriminante del modelo debil sobre la Muestra de Entrenamiento.
#-------------------------------------------------------------------------------

model=moddebil
nombremodel="moddebil"
muestra=dt
numacr=dim(dt)[1]
numvar=13
PoderDiscriminante(model,nombremodel,muestra,numacr,numvar)

#4.3.2.4 Poder discriminante del modelo Paso a paso sobre la Muestra de Entrenamiento.
#-------------------------------------------------------------------------------

model=modpasos   #Modelo Paso a Paso (Backwards-Forwards)
nombremodel="modpasos"
muestra=dt
numacr=dim(dt)[1]
numvar=17
PoderDiscriminante(model,nombremodel,muestra,numacr,numvar)

#4.3.2.5 Poder discriminante del Modelo de 15 Variables sobre la Muestra de Entrenamiento.
#-----------------------------------------------------------------------------------------

model=mod15
nombremodel="mod15"
muestra=dt
numacr=dim(dt)[1]
numvar=15
PoderDiscriminante(model,nombremodel,muestra,numacr,numvar)

#-----------------------------------------------------------------------------------#
# 4.4.2 Criterio de Informaci�n de Akaike (AIC) y Criterios de Informaci�n Bayesiano# 
#      (BIC) sobre la muestra de validaci�n.                                        #
#-----------------------------------------------------------------------------------#

# 4.4.2.1 Funci�n Validaci�nAICyBIC
#----------------------------------

Validaci�nAICyBIC=function(model,nombremodel,muestra,numacr,numvar)
{
# Desvianza de model
devianceM <- model$deviance

# N: n�mero de acreditados, nunvar: n�mero de variables y p: numvar+1
N=numacr
p=numvar+1

# Logverosimilitud negativa del model 
logverM=-devianceM/2

# Citerios de Informaci�n, AIC y BIC.

# Criterio de Informacion de Akaike,AIC.
AIC=-2*logverM+2*p

# Criterio de Informacion Bayesiana,BIC.
BIC=-2*logverM+log(N)*p

return(list("Validaci�n de los Criterios de Informaci�n AIC y BIC:"=nombremodel,
"N�mero de acreditados, N:" = N,
"N�mero de variables del modelo, p:"=numvar,
"Logverosimilitud negativa del modelo ajustado"=logverM,
"Criterio de Informacion de Akaike,AIC:"=AIC,
"Criterio de Informaci�n Bayesiana,BIC:"=BIC))
}

dcvr <- readRDS( file= "C:/RW/df_csi59_cvr")

# 4.4.2.2 Validaci�n Criterios de Informaci�n AIC y BIC del modelo completo,
#          modcompleto, sobre la Muestra de Validaci�n. 
#---------------------------------------------------------------------------

cvrmodcompleto=glm(CODDEFAULT~X3+X5+X6+X7+X8+X15+X19+X20+X25+X26+X35+X37+X42+X45+
                        X46_W+X47_W+X48+X49_W+X53+X55_W+X56_W+X57+X58+X63+X64,
                        family=binomial(link="logit"),data=dcvr)
model=cvrmodcompleto
nombremodel="modcompleto"
muestra=dcvr
numacr=dim(dcvr)[1]
numvar=25
Validaci�nAICyBIC(model,nombremodel,muestra,numacr,numvar)

# 4.4.2.3 Validaci�n Criterios de Informaci�n AIC y BIC del modelo debil,
#         moddebil, sobre la Muestra de Validaci�n. 
#------------------------------------------------------------------------

cvrmoddebil=glm(CODDEFAULT~X7+X8+X19+X25+X46_W+X47_W+X48+X49_W+X53+X55_W+X57+
                           X58+X63, family=binomial(link="logit"),data=dcvr)
model=cvrmoddebil
nombremodel="moddebil"
muestra=dcvr
numacr=dim(dcvr)[1]
numvar=13
Validaci�nAICyBIC(model,nombremodel,muestra,numacr,numvar)

# 4.4.2.4 Validaci�n Criterios de Informaci�n AIC y BIC del modelo paso a paso,
#         modpasos, sobre la Muestra de Validaci�n. 
#------------------------------------------------------------------------------

cvrmodpasos=glm(CODDEFAULT~X5+X7+X8+X19+X25+X37+X42+X45+X46_W+X47_W+X48+X49_W+X53+X55_W+
                           X57+X58+X63,family=binomial(link="logit"),data=dcvr)
model=cvrmodpasos
nombremodel="modpasos"
muestra=dcvr
numacr=dim(dcvr)[1]
numvar=17
Validaci�nAICyBIC(model,nombremodel,muestra,numacr,numvar)

# 4.4.2.5 Validaci�n Criterios de Informaci�n AIC y BIC del modelo de 
#         15 variables, mod15, sobre la Muestra de Validaci�n. 
#--------------------------------------------------------------------

cvrmod15=glm(CODDEFAULT~X5+X7+X8+X19+X25+X45+X46_W+X47_W+X48+X49_W+X53+X55_W+X57+ 
                     X58+X63,family=binomial(link="logit"),data=dcvr)
model=cvrmod15
nombremodel="mod15"
muestra=dcvr
numacr=dim(dcvr)[1]
numvar=15
Validaci�nAICyBIC(model,nombremodel,muestra,numacr,numvar)

#-----------------------------------------------------------------------------#
#4.4.3. Poder Discriminante de los Modelos CMM sobre la Muestra de Validaci�n.#
#-----------------------------------------------------------------------------#

# Para la selecci�n de modelos es habitual, por su demostrada eficacia, utilizar
# como medida de poder discriminante el �rea bajo la Curva ROC, AUC calculados 
# sobre la muestra de validaci�n. 

# 4.4.3.1. Poder Discriminante del modelo completo sobre la Muestra de Validaci�n.
#---------------------------------------------------------------------------------

model=cvrmodcompleto
nombremodel="modcompleto"
muestra=dcvr
numacr=dim(dcvr)[1]
numvar=25
PoderDiscriminante(model,nombremodel,muestra,numacr,numvar)


# 4.4.3.2. Poder Discriminante del modelo debil sobre la Muestra de Validaci�n.
#------------------------------------------------------------------------------

model=cvrmoddebil
nombremodel="moddebil"
muestra=dcvr
numacr=dim(dcvr)[1]
numvar=13
PoderDiscriminante(model,nombremodel,muestra,numacr,numvar)

#4.4.3.3. Poder Discriminante del modelo paso a paso (Backwards-Forwards) sobre la 
#         Muestra de Validaci�n.
#---------------------------------------------------------------------------------

model=cvrmodpasos #Modelo modelo paso a paso (Backwards-Forwards)
nombremodel="modpasos"
muestra=dcvr
numacr=dim(dcvr)[1]
numvar=17
PoderDiscriminante(model,nombremodel,muestra,numacr,numvar)

#4.4.3.4. Poder Discriminante del modelo mod15 sobre la Muestra de Validaci�n. #    
#-----------------------------------------------------------------------------

model=cvrmod15
nombremodel="mod15"
muestra=dcvr
numacr=dim(dcvr)[1]
numvar=15
PoderDiscriminante(model,nombremodel,muestra,numacr,numvar)

#-------------------------------------------------------------------------------------#
#4.5.4 Error de Predicci�n sobre la muestra de validaci�n. Validacion Cruzada K_folds.# 
#-------------------------------------------------------------------------------------#

# En primer lugar cargamos en el sistema el fichero que contiene la muestra total replicada
# df_csi59_totalr:
#------------------------------------------------------------------------------------------

dtotalr<-readRDS(file="C:/RW/df_csi59_totalr")

# A continuaci�n ajustamos los modelos CMM a dtotalr
#---------------------------------------------------

trmodcompleto=glm(CODDEFAULT~ X3+X5+X6+X7+X8+X15+X19+X20+X25+X26+X35+X37+X42+X45+
                        X46_W+X47_W+X48+X49_W+X53+X55_W+X56_W+X57+X58+X63+X64,
                        family=binomial(link="logit"),data=dtotalr)

trmoddebil=glm(CODDEFAULT~ X7+X8+X19+X25+X46_W+X47_W+X48+X49_W+X53+X55_W+X57+
                           X58+ X63, family=binomial(link="logit"),data=dtotalr)

trmodpasos=glm(CODDEFAULT~ X5+X7+X8+X19+X25+X37+X42+X45+X46_W+X47_W+X48+X49_W+X53+
                           X55_W+X57+X58+X63,
                           family=binomial(link="logit"),data=dtotalr)

trmod15=glm(CODDEFAULT~ X5+X7+X8+X19+X25+X45+X46_W+X47_W+X48+X49_W+X53+X55_W+X57+ 
                        X58+X63,family=binomial(link="logit"),data=dtotalr)

# 4.5.4.1. funci�n para obtener el Error de Predicci�n por Validacion Cruzada k_Folds, VC_K_F,
# usando el paquete boot.                                                                     
#---------------------------------------------------------------------------------------------

library(boot)

# Puesto que la respuesta es una variable binaria, una funci�n de coste apropiada es la siguiente:
# cost <- function(r, pi) mean(abs(r-pi)> 0.5)

ValidacionCruzada_CV_K_F_errpred=function(modelo,muestra,Nfolds)
{
cost <- function(r, pi) mean(abs(r-pi)> 0.5)
cv.err=cv.glm(muestra,modelo,K=Nfolds, cost = cost)
cv.err$delta[1]
}

# 4.5.4.2. Error de Predicci�n por Validacion Cruzada k_Folds de modcompleto sobre la muestra total replicada. 
#------------------------------------------------------------------------------------------------------------

modelo=trmodcompleto
muestra=dtotalr
Nfolds=10

ValidacionCruzada_CV_K_F_errpred(modelo,muestra,Nfolds)

# 4.5.4.3. Error de Predicci�n por Validacion Cruzada k_Folds de moddebil sobre la muestra total replicada. 
#------------------------------------------------------------------------------------------------------------

modelo=trmoddebil
muestra=dtotalr
Nfolds=10

ValidacionCruzada_CV_K_F_errpred(modelo,muestra,Nfolds)

# 4.5.4.4. Error de Predicci�n por Validacion Cruzada k_Folds de modpasos sobre la muestra total replicada. 
#------------------------------------------------------------------------------------------------------------

modelo=trmodpasos
muestra=dtotalr
Nfolds=10

ValidacionCruzada_CV_K_F_errpred(modelo,muestra,Nfolds)

# 4.5.4.5. Error de Predicci�n por Validacion Cruzada k_Folds de mod15 sobre la muestra total replicada. 
#------------------------------------------------------------------------------------------------------------

modelo=trmod15
muestra=dtotalr
Nfolds=10

ValidacionCruzada_CV_K_F_errpred(modelo,muestra,Nfolds)

#----------------------------------------------------------------------------------#
# 4.5.5 C�lculo de la precisi�n del modelo (Accuracy model) por Validacion Cruzada#
#       k_Folds)R usando la funci�n cv.binary, paquete {DAAG}.                     #
#----------------------------------------------------------------------------------#

library(DAAG)

# 4.5.5.1. C�lculo de la Precisi�n (accuracy) de modcompleto por Validaci�n Cruzada
#         k_Folds, VC_K_F, sobre la muestra total replicada.
#----------------------------------------------------------------------------------

modelo=trmodcompleto
K=10

ermodcompleto <- cv.binary(modelo,nfolds=K,print.details=TRUE)
ermodcompleto$acc.cv

# 4.5.5.2. C�lculo de la Precisi�n (accuracy) de moddebil por Validaci�n Cruzada
#         k_Folds, VC_K_F, sobre la muestra total replicada.
#-------------------------------------------------------------------------------

modelo=trmoddebil
K=10
ermoddebil <- cv.binary(modelo, nfolds=K,print.details=TRUE)
ermoddebil$acc.cv

# 4.5.5.3. C�lculo de la Precisi�n (accuracy) de modpasos por Validaci�n Cruzada
#         k_Folds, VC_K_F, sobre la muestra total replicada.
#-------------------------------------------------------------------------------

modelo=trmodpasos
K=10
ermodpasos <- cv.binary(modelo, nfolds=K,print.details=TRUE)
ermodpasos$acc.cv

# 4.5.5.4. C�lculo de la Precisi�n (accuracy) de mod15 por Validaci�n Cruzada
#         k_Folds, VC_K_F, sobre la muestra total replicada.
#----------------------------------------------------------------------------

modelo=trmod15
K=10
ermod15 <- cv.binary(modelo, nfolds=K,print.details=TRUE)
ermod15$acc.cv

#----------------------------------------#
# 4.6 Error de Generalici�n. Err. CV_K_F #
#----------------------------------------#

# Comenzamos cargando desde nuestra �rea de trabajo, C:/RW, las muestras 
# de entrenamiento (dtrain) y test (dtest), as� como los objetos resultado
# del ajuste de los 4 modelos CMM a la muestra de entrenamiento:

dtest <- readRDS( file= "C:/RW/df_csi59_test")

modcompleto <- readRDS(file = "C:/RW/modcompleto")
moddebil <- readRDS(file = "C:/RW/moddebil")
modpasos <- readRDS(file = "C:/RW/modpasos")
mod15 <- readRDS(file = "C:/RW/mod15")

library(regclass)

# 4.6.1 Funci�n  Error de Generalici�n ErrorGeneralizacion_CV_K_F
#----------------------------------------------------------------

# Activaremos el par�metro Kfold a TRUE, para que la funci�n estime el error de
# generalizaci�n del modelo utilizando la validaci�n cruzada CV_K_Fold repetida. 

ErrorGeneralizacion_CV_K_F=function(modelo,muestra,Nfolds,Repetic)
{
generalization_error(modelo,muestra,Kfold=TRUE,K=Nfolds,R=Repetic,seed=1050)
}
  
# 4.6.2. modcompleto sobre la muestra train. Error Generalizaci�n sobre la muesta test.
#--------------------------------------------------------------------------------------

modelo=modcompleto
muestra=dtest
Nfolds=10
Repetic=10

ErrG<-ErrorGeneralizacion_CV_K_F(modelo,muestra,Nfolds,Repetic)
ErrG

# 4.6.3. moddebil sobre la muestra train. Error Generalizaci�n sobre la muesta test.
#-----------------------------------------------------------------------------------

modelo=moddebil
muestra=dtest
Nfolds=10
Repetic=10

ErrG<-ErrorGeneralizacion_CV_K_F(modelo,muestra,Nfolds,Repetic)
ErrG

# 4.6.4. modpasos sobre la muestra train. Error Generalizaci�n sobre la muesta test.
#-----------------------------------------------------------------------------------

modelo=modpasos
muestra=dtest
Nfolds=10
Repetic=10

ErrG<-ErrorGeneralizacion_CV_K_F(modelo,muestra,Nfolds,Repetic)
ErrG

# 4.6.5. mod15 sobre la muestra train. Error Generalizaci�n sobre la muesta test.
#-----------------------------------------------------------------------------------

modelo=mod15
muestra=dtest
Nfolds=10
Repetic=10

ErrG<-ErrorGeneralizacion_CV_K_F(modelo,muestra,Nfolds,Repetic)
ErrG




