
# Asignaci�n del directorio de trabajo:
#--------------------------------------

setwd("C:/RW")

# Carga algunos paquetes r para la manipulaci�n de datos:
#--------------------------------------------------------
 
library(magrittr)
library(scorecard)
library(dplyr)
library(tidyr)
library(ggplot2)

#-------------------------------------------------------#
#  6. Modelo de 11 variables con valores woe (mod11woe) #
#-------------------------------------------------------#

# Carga de ficheros necesarios que han sido obtenidos y guardados en 
# "C:/WR" en el Cap�tulo 5.
#-------------------------------------------------------------------

dt_list <- readRDS(file= "C:/RW/dt_list11")
label_list <-readRDS(file= "C:/RW/label_list11")
bins <- readRDS(file= "C:/RW/bins11")
dt_woe_list <- readRDS(file= "C:/RW/dt_woe_list11")
mod11woe <- readRDS( file= "C:/RW/mod11woe")
mod11AICwoe <- readRDS( file= "C:/RW/mod11AICwoe")
m2=mod11AICwoe

#-------------------------------------------------------------------#
# 6.2 Relaci�n entre las Probabilidades de Default  Pronosticadas y #
#     las Puntuaciones asignadas por el modelo mod11woe.            #  
#     Sobre la muestra de entrenamiento.                            #
#-------------------------------------------------------------------# 

# Probabilidades de Default  Pronosticadas y Puntuaciones Asignadas.
#-------------------------------------------------------------------

logit_list = lapply(dt_woe_list, function(x) predict(m2, x)) # logit(X) 
pred_list  = lapply(dt_woe_list, function(x) predict(m2, type = 'response', x)) # PD(X)

# logit, odds, prob y prob_ctrl
#------------------------------

logit     = logit_list$train                  
prob_ctrl = pred_list$train                  
odds      = exp(logit)
prob      = odds / (odds + 1)
prob_default=prob_ctrl
res = tibble( logit, odds, prob_default)
res

range(round(logit,6))
range(round(prob_ctrl,6))

# filter control values

res = res %>%
  dplyr::select( - ends_with('_ctrl') )

score_train=log(pred_list$train/(1-pred_list$train)) # S(X)
score=score_train
range(score)

# Histogramas de logit, odds, probabilidad de default
#----------------------------------------------------

res %>%
  gather( key = 'key', value = 'value' ) %>%
  ggplot( aes(value) ) +
    geom_histogram( bins = 50
                    , fill = 'aquamarine3'
                    , color = 'black' ) +
    geom_rug()+
    facet_wrap(~key, scales = 'free')

# Histograma conjunto de probabilidad de default y logit
#-------------------------------------------------------

res %>%
  dplyr::select( prob_default, logit ) %>%
  mutate_all( scale, center = T ) %>%
  mutate_all( as.vector ) %>%
  gather( key = 'key', value = 'value' ) %>%
  ggplot( )+
    geom_histogram( aes( x = value, fill = key )
                    , bins = 50
                    , position="identity"
                    , alpha = 0.5 )

#Logit vs. Odds, Probabilities 
#-----------------------------

res %>%
  gather( key = 'key', value = 'value', - logit ) %>%
  ggplot( aes( logit, value, color = key) ) +
  geom_point() +
  geom_line() +
  facet_wrap(~key, scales = 'free_y')

# Puntuaci�n (mon�tona creciente) con respecto a la probabilidad de default.
#---------------------------------------------------------------------------

res %>%
  gather( key = 'key', value = "value", - logit, - odds ) %>%
  ggplot( aes( score, prob, color = key) ) +
  geom_point() +
  geom_line()

# Puntuaci�n Escalada (mon�tona decreciente) (Siddiqi 2006)
#----------------------------------------------------------

points0 = 600 # Target Score Value.

odds0 = 50   # Target Odds (odds0). En una  puntuaci�n objetivo de 600 
             # los odds deber�an ser 1:50.
 
pdo = 20     # Puntos para duplicar las probabilidades (pdo). Las probabilidades
             # deber�an duplicarse cada 20 puntos. 

factor=pdo/log(2)
offset=points0-log(odds0)*factor
nscore =offset+factor*(-score)
puntuacion=round(nscore,6)
range(puntuacion)

# Puntuaci�n Escalada (mon�tona decreciente) con respecto a la probabilidad de 
# default.
#--------------------------------------------------------------------------------

res %>%
  gather( key = 'key', value = "value", - logit, - odds ) %>%
  ggplot( aes( nscore, prob, color = key) ) +
  geom_point() +
  geom_line()


# En algunas ocasiones a pesar del reescalado propuesto por Sidiqi(2006)
# pueden resultar puntuaciones negativas para los acreditados con mayor 
# probabilidad de default. Por tanto, procedemos a corregir este hecho:

pm=min(nscore)
pmi=100
nscore=nscore-pm+pmi

puntuacion=round(nscore,6)
range(puntuacion)

res %>%
  gather( key = 'key', value = "value", - logit, - odds ) %>%
  ggplot( aes( nscore, prob, color = key) ) +
  geom_point() +
  geom_line()

#-----------------------------------------#
# 6.3.2 Tarjeta de puntuaci�n (Score Card)#
#-----------------------------------------#

# En el apartado anterior 6.1.2, se obtuvo un m�nimo para la puntuaci�n de
# cr�dito de  261 puntos. Como, por otro lado, fijamos una puntuaci�n m�nima 
# para los clientes de 100 puntos, para conseguir que la funci�n scorecard_ply
# fije un m�nimo de 100 puntos para los clientes sobre la muestra de entrenamiento
# tanto establecemos una puntuaci�n objetivo points0 = 600-259+100 = 441.
 
points0 = 600-259+100
odds0 = 50
pdo = 20

card = scorecard( bins , m2
                  , points0 = points0 
                  , odds0 = 1/odds0 # scorecard wants the inverse
                  , pdo = pdo)
card
saveRDS(object= card, file= "C:/RW/card_mod11woe")

# 6.3.2.3 Resumen de la Tarjeta de Puntuaci�n: 
#---------------------------------------------

library(stringr)
do.call("bind_rows", card) %>% 
  slice(-1) %>% 
  dplyr::select(-breaks, -is_special_values, -count, -count_distr, -good, -bad, -badprob) %>% 
  mutate_if(is.numeric, function(x) {round(x, 3)}) %>% 
  mutate(bin = bin %>% 
           str_replace_all("\\[", "From ") %>% 
           str_replace_all("\\,", " to ") %>% 
           str_replace_all("\\)", "")) -> iv_for_predictors_point

iv_for_predictors_point %>% 
  knitr::kable(col.names = c("Predictor", "Group", "WOE", "Scorecard", "Bin IV", "Total IV"))

#--------------------------------------------------------------------------------#   
# 6.3.2.4. Tarjetas de Puntuaci�n para cada una de las 11 variables consideradas.#
#--------------------------------------------------------------------------------#

# Puntos base que sumados a las puntuaciones globales de las variables nos da
# la puntuaci�n total del cliente.
#----------------------------------------------------------------------------

card[1]

# Tarjetas de Puntuaci�n para cada una de las 11 variables consideradas.
#-----------------------------------------------------------------------

# Tarjeta de Puntuaci�n de X5.
card[[2]]

# Tarjeta de Puntuaci�n de X7.
card[[3]]

# Tarjeta de Puntuaci�n de X8.
card[[4]]

# Tarjeta de Puntuaci�n de X19.
card[[5]]

# Tarjeta de Puntuaci�n de X47_W.
card[[6]]

# Tarjeta de Puntuaci�n de X48.
card[[7]]

# Tarjeta de Puntuaci�n de X53.
card[[8]]

# Tarjeta de Puntuaci�n de X55_W.
card[[9]]

# Tarjeta de Puntuaci�n de X57.
card[[10]]

# Tarjeta de Puntuaci�n de X58.
card[[11]]

# Tarjeta de Puntuaci�n de X63.
card[[12]]

# 6.3.2.5 Listas de Puntuaciones de Cr�dito Totales y por Variables.
#-------------------------------------------------------------------

#Solo Puntuaci�n de Cr�dito Total
#--------------------------------
score_list = lapply(dt_list,function(x) scorecard_ply(x,card))

#Puntuaci�n de Cr�dito total y por Variables
#-------------------------------------------
score_list2 = lapply(dt_list,function(x) scorecard_ply(x,card,only_total_score=FALSE))
 
df_train_aic <- dt_list$train
sc = scorecard_ply(df_train_aic, card )

res$score = sc
range(res$score)

saveRDS(object=score_list, file= "C:/RW/score_list_mod11woe")
saveRDS(object=score_list2, file= "C:/RW/score_list2_mod11woe")


# 6.3.2.6 Algunos estad�sticos b�sicos scorecard por grupos default - no default. 
#--------------------------------------------------------------------------------

my_points_train <- scorecard_ply(df_train_aic, card, only_total_score = TRUE, print_step = 0) %>% as.data.frame()
df_train_aic <- dt_list$train
my_points_train <- my_points_train %>% mutate(score = score)

df_scored_train <- df_train_aic %>% 
     mutate(SCORE = my_points_train$score) %>% 
     mutate(CODDEFAULT = case_when(CODDEFAULT == 1 ~ "Default", TRUE ~ "NonDefault")) 

#Tabla 1: Puntos Scorecard por Grupos para los datos de Entrenamiento
#--------------------------------------------------------------------
df_scored_train %>% 
  group_by(CODDEFAULT) %>% 
  summarise_each(funs(min, max, median, mean, n()), SCORE) %>% 
  mutate_if(is.numeric, function(x) {round(x, 0)}) %>% 
  knitr::kable(caption = "Puntos Scorecard por Grupos para Datos de Entrenamiento")

# 6.3.2.7 Gr�fico de la Distribuci�n de la Tarjeta de Puntuaci�n para Default y No Default 
#         para los datos de Entrenamiento.
#-----------------------------------------------------------------------------------------

df_scored_train %>% 
  group_by(CODDEFAULT) %>% 
  summarise(tb = mean(SCORE)) %>% 
  ungroup() -> mean_score_train

df_scored_train %>% 
  ggplot(aes(SCORE, color = CODDEFAULT, fill = CODDEFAULT)) + 
  geom_density(alpha = 0.3, kernel="epanechnikov") + 
  geom_vline(aes(xintercept = mean_score_train$tb[1]), linetype = "dashed", color = "red") + 
  geom_vline(aes(xintercept = mean_score_train$tb[2]), linetype = "dashed", color = "blue") + 
  geom_text(aes(x = 400 - 15, y = 0.0042, label = mean_score_train$tb[1] %>% round(0)), color = "red", size = 4) + 
  geom_text(aes(x = 565, y = 0.0042, label = mean_score_train$tb[2] %>% round(0)), color = "blue", size = 4) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.2, 0.8)) + 
  labs(x = NULL, y = NULL, title = "Distribuci�n de la Tarjeta de Puntuaci�n para Default y No Default.", 
       subtitle = "Datos de Entrenamiento.")

# 6.3.3 Importancia de cada variable en la puntuaci�n de cada cliente.  
#-----------------------------------------------------------------------

iv_for_predictors_point %>% 
  group_by(variable) %>% 
  summarise(iv_var = mean(total_iv)) %>% 
  ungroup() %>% 
  arrange(iv_var) %>% 
  mutate(variable = factor(variable, levels = variable)) -> iv_values

theme_set(theme_minimal())
iv_values %>% 
  ggplot(aes(variable, iv_var)) + 
  geom_col(fill = "#377eb8") + 
  coord_flip() + 
  geom_col(data = iv_values %>% filter(iv_var < 0.1), aes(variable, iv_var), fill = "grey60") + 
  geom_text(data = iv_values %>% filter(iv_var < 0.1), aes(label = round(iv_var, 3)), 
            hjust = -0.1, size = 5, color = "grey40") + 
  geom_text(data = iv_values %>% filter(iv_var >= 0.0), aes(label = round(iv_var, 3)), 
            hjust = -.1, size = 5, color = "#377eb8") + 
  labs(title = "Valor de la Informaci�n (IV) para las Variables", 
       x = NULL, y = "Valor de la Informaci�n (IV)") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) + 
  theme(panel.grid.major.y = element_blank()) + 
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))

#-----------------------------------------------------------------------#
# 6.4 Validacion de la Tarjeta de puntuaci�n del modelo mod11woe sobre  #
# la muestra de validaci�n.                                             #
#-----------------------------------------------------------------------#

df_cvr_aic <- dt_list$cvr
my_points_cvr <- scorecard_ply(df_cvr_aic, card, only_total_score = TRUE, print_step = 0) %>% as.data.frame()
range(my_points_cvr)

my_points_cvr <- my_points_cvr %>% mutate(score = score)
range(round(my_points_cvr$score,6))

# 6.4.2 Estad�sticos b�sicos de la Tarjeta de Puntuaci�n sobre la muestra de validaci�n por 
#       grupos : 
#------------------------------------------------------------------------------------------
                               
df_scored_cvr <- df_cvr_aic %>% 
  mutate(SCORE = my_points_cvr$score) %>% 
  mutate(CODDEFAULT = case_when(CODDEFAULT == 1 ~ "Default", TRUE ~ "NonDefault")) 

df_scored_cvr %>% 
  group_by(CODDEFAULT) %>% 
  summarise_each(funs(min, max, median, mean, n()), SCORE) %>% 
  mutate_if(is.numeric, function(x) {round(x, 0)}) %>% 
  knitr::kable(caption = "Puntos Scorecad por Grupos para los Datos de Validaci�n")

# 6.4.3 Representaci�n Gr�fica de la Distribuci�n de la Tarjeta de Puntuaci�n para Default y No Default
#       sobre la Muestra de Validaci�n.
#------------------------------------------------------------------------------------------------------

df_scored_cvr %>% 
  group_by(CODDEFAULT) %>% 
  summarise(tb = mean(SCORE)) %>% 
  ungroup() -> mean_score_cvr

df_scored_cvr %>% 
  ggplot(aes(SCORE, color = CODDEFAULT, fill = CODDEFAULT)) + 
  geom_density(alpha = 0.3, kernel="epanechnikov") + 
  geom_vline(aes(xintercept = mean_score_cvr$tb[1]), linetype = "dashed", color = "red") + 
  geom_vline(aes(xintercept = mean_score_cvr$tb[2]), linetype = "dashed", color = "blue") + 
  geom_text(aes(x = 412, y = 0.0042, label = mean_score_cvr$tb[1] %>% round(0)), color = "red", size = 4) + 
  geom_text(aes(x = 570, y = 0.0042, label = mean_score_cvr$tb[2] %>% round(0)), color = "blue", size = 4) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.2, 0.8)) + 
  labs(x = NULL, y = NULL, title = "Distribuci�n de la Tarjeta de Puntuaci�n para Default y No Default.", 
       subtitle = "Datos de Validaci�n.")

# 6.4.4 Evaluaci�n del rendimiento del modelo mod11woe basada en las Probabilidades y en las Puntuaciones
#       Pronosticadas.
#--------------------------------------------------------------------------------------------------------

# 6.4.4.2 Evaluaci�n del rendimiento del modelo mod11woe basada en las Probabilidades Pronosticadas.
#---------------------------------------------------------------------------------------------------

pred_list  = lapply(dt_woe_list, function(x) predict(m2, type = 'response', x)) # PD(X)

#  6.4.4.2.1 Funci�n MatrizdeConfusion(). 
#----------------------------------------

MatrizdeConfusion <- function(my_data, nombremodelo, nombremuestra)
{
VN=my_data[1,2]  # N�mero de Verdaderos Negativos
FP=my_data[1,3]  # N�mero de Falsos Positivos
FN=my_data[2,2]  # N�mero de Falsos Negativos
VP=my_data[2,3]  # N�mero de Verdaderos Psitivos
TOTAL=(VN+FP+FN+VP)  # Total
TAP=FN+VP          # Total Positivos Actuales
TAN=VN+FP          # Total Negativos Actuales
TPP=FP+VP          # Total Positivos Pronosticados
TNP=VN+FN          # Total negativos pronosticados
Tasadeerror=(FP+FN)/TOTAL
Accuracy=(VP+VN)/TOTAL
Precision=VP/TPP            # Tasa de Verdaderos Positivos Pronosticados
Recall=VP/TAP               # Tasa de Verdaderos Positivos Actuales. Sensitivity.
Especificidad=VN/TAN        # Tasa de Verdaderos Negativos Pronosticados
TVNP=VN/TNP                 # Tasa de Verdaderos Negativos Actuales
ErrorTipoI =FN/(FN+VN)      # Error Tipo I. Coste de P�rdida de Negocio.
ErrorTipoII=FP/(FP+VP)      # Error Tipo II. Riesgo de Cr�dito.
return(list("Medidas de rendimiento del modelo" =nombremodelo, 
"Muestra "=nombremuestra,
"Precisi�n = VP/TPP:"=Precision,
"Recuperaci�n = VP/TAP. Sensibilidad:"=Recall))
}

# 6.4.4.2.2 Evaluaci�n de la Probabilidad Pronosticada sobre la muestra de validaci�n, pred_list$cvr.
#-----------------------------------------------------------------------------------------------------

pev <- perf_eva(pred = pred_list$cvr, label=dt_woe_list$cvr$CODDEFAULT,type = c("ks","roc","pr"),
                show_plot = TRUE,confusion_matrix = TRUE, positive = "bad|1", seed = 186, title = 'cvr')

pev$binomial_metric$cvr
pev$confusion_matrix$cvr

my_data=pev$confusion_matrix$cvr

# Renombrar columnas

names(my_data)[names(my_data) == "pred_0"] <- " "
names(my_data)[names(my_data) == "pred_1"] <- " "

nombremodelo="mod11woe"
nombremuestra="cvr"
MatrizdeConfusion(my_data, nombremodelo, nombremuestra)

# 6.4.4.3 Evaluaci�n del rendimiento del modelo mod11woe basada en las Puntuaciones Pronosticadas.
#-------------------------------------------------------------------------------------------------

# Puntuaci�n de Cr�dito Total
#----------------------------
score_list = lapply(dt_list,function(x) scorecard_ply(x,card))

# Puntuaci�n de Cr�dito Total y por Variables
#--------------------------------------------
score_list2 = lapply(dt_list,function(x) scorecard_ply(x,card,only_total_score=FALSE))
 
# 6.4.4.3.1 Evaluaci�n de la Puntuaci�n Pronosticada sobre la muestra de validaci�n, score_list$cvr.
#---------------------------------------------------------------------------------------------------

pev <- perf_eva(pred = score_list$cvr, label=dt_woe_list$cvr$CODDEFAULT,type = c("ks","roc","pr"),
                              show_plot = TRUE, confusion_matrix = TRUE,
                              positive = "bad|1", seed = 186, title = 'cvr')

pev$binomial_metric$cvr
pev$confusion_matrix$cvr

my_data=pev$confusion_matrix$cvr

# Renombrar columnas

names(my_data)[names(my_data) == "pred_0"] <- " "
names(my_data)[names(my_data) == "pred_1"] <- " "

nombremodelo="mod11woe"
nombremuestra="cvr"
MatrizdeConfusion(my_data, nombremodelo, nombremuestra)

# 6.4.5. Evaluaci�n de la Estabilidad de la Poblaci�n perf_psi. 
#--------------------------------------------------------------

# Puntuaci�n de Cr�dito Total
#-----------------------------
score_list = lapply(dt_list,function(x) scorecard_ply(x,card))

# Puntuaci�n de Cr�dito Total y por variables
#--------------------------------------------
score_list2 = lapply(dt_list,function(x) scorecard_ply(x,card,only_total_score=FALSE))

# 6.4.5.2 �ndice de Estabilidad Poblacional (s�lo total, OPSI).
#--------------------------------------------------------------

opsi = perf_psi(score = score_list, label = label_list)
opsi$psi  # psi data frame
opsi$pic  # pic of score distribution
plotlist=list(opsi$pic$score$train_cvr,opsi$pic$score$train_test)
plotlist[1]
plotlist[2]

# save opsi plot
#namelist=list("train_cvr","train_test")
# for (i in 1:length(plotlist)) {
#   ggplot2::ggsave(
#      paste0("c:/RW/opsiplot11/", names(namelist[i]), ".png"), plotlist[[i]],
#      width = 15, height = 9, units="cm" ) }

# 6.4.5.3 �ndice de Estabilidad Poblacional (ambas, APSI).
#--------------------------------------------------------------

bpsi = perf_psi(score = score_list2, label = label_list)
bpsi$psi           # psi data frame
bpsi$pic           # pic of score distribution
bpsi$pic

# 6.5. Tablas de Ganancias (Gains Table) para las muestras train, cvr y test. 
#----------------------------------------------------------------------------

# Las entradas score y label deben ser una lista o un vector.

# Tabla de ganancias para la muestra train. 
#------------------------------------------

g1 = gains_table(score = score_list$train, label = label_list$train)
g1

# Tabla de ganancias para la muestra cvr. 
#----------------------------------------

g2 = gains_table(score = score_list$cvr, label = label_list$cvr)
g2

# Tabla de ganancias para la muestra test. 
#-----------------------------------------

g3 = gains_table(score = score_list$test, label = label_list$test)
g3

#----------------------------------------------------#
# 6.6 Puntuaciones de Cr�dito de nuevos acreditados. #  
#----------------------------------------------------#

# Carga desde C:/RW del fichero de datos de los 20 nuevos solictantes
# de cr�dito.
#--------------------------------------------------------------------
df_solcredit <- readRDS(file= "C:/RW/df_solcredit_mod11woe")
df_solcredit[,c(1:7)]
df_solcredit[,c(1,8:12)]

# Tramado de las variables por woebin_ply()
#------------------------------------------ 
bins <- readRDS(file= "C:/RW/bins11")
df_solcredit_woe = woebin_ply(df_solcredit, bins)
df_solcredit_woe[,c(1:7)]
df_solcredit_woe[,c(1,8:12)]

# Probabilidad de default Pron�sticada.
#--------------------------------------
m2 <- readRDS( file= "C:/RW/mod11AICwoe")
pred_solcredit=predict(m2,type = 'response',df_solcredit_woe)
round(pred_solcredit,6)

# Puntuacion de los acreditados (mon�tona decreciente) 
#---------------------------------------------------
card <- readRDS(file= "C:/RW/card_mod11woe")
scorecard_solcredit = scorecard_ply(df_solcredit, card )

# Convertir dataframe a data.matrix 
#---------------------------------------------------
punt_card=data.matrix(scorecard_solcredit)
prob_default=data.matrix(round(pred_solcredit,6))

# Dataframe con N�mero de Acreditado, Probabilidad de Default y 
# Puntuaci�n de Cr�dito asignado a cada nuevo solicitante de cr�dito. 
#--------------------------------------------------------------------
numacredit=df_solcredit$numacredit
res = tibble( numacredit, prob_default, punt_card)
res

# Representaci�n gr�fica del comportamiento de la probabilidad de default frente a la puntuaci�n
#  de los nuevos solicitantes de cr�dito.
#------------------------------------------------------------------------------------------------
plot(punt_card, prob_default,pch = 16, xlab = "score", ylab = "pd")
text(punt_card, prob_default,labels=numacredit, cex= 0.8, pos=4)


