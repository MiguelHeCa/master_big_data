#3 Selección de las variables de entrenamiento.

dt <- readRDS( file= "C:/RW/df_csi59_train")

library(scorecard)
library(MASS)


#-------------------------------------------------------------------------------------------------------#
#3.2.2 Contraste de los coeficientes de las 26 variables iniciales, a través del modelo logístico lineal# 
#    (glm, family = binomial(link="logit")sobre los datos de entrenamiento.                             # 
#-------------------------------------------------------------------------------------------------------#

modcompleto=glm(CODDEFAULT~X3+X5+X6+X7+X8+X15+X19+X20+X25+X26+X35+X37+X42+X45+X46_W+X47_W+X48+X49_W+
                           X53+X55_W+X56_W+X57+X58+X63+X64,
family=binomial(link="logit"),data=dt)
summary(modcompleto)
scorecard::vif(modcompleto, merge_coef = TRUE)
saveRDS(object=modcompleto, file= "C:/RW/modcompleto")

#----------------------------------------------------------------------------------------------------------#
#3.2.3 Contraste de los coeficientes de las 13 variables que resultaron con significación lineal           #
# en ajuste del modelo completo, a través del modelo logístico lineal (glm, family = binomial(link="logit")#
# sobre los datos de entrenamiento.                                                                        #
#----------------------------------------------------------------------------------------------------------#

moddebil=glm(CODDEFAULT~X7+X8+X19+X25+X46_W+X47_W+X48+X49_W+X53+X55_W+X57+
                        X58+ X63, family=binomial(link="logit"),data=dt)
summary(moddebil)
scorecard::vif(moddebil, merge_coef = TRUE)
saveRDS(object=moddebil, file= "C:/RW/moddebil")

#----------------------------------------------------------------------------------------#
#3.2.4 Selección de variables pasos a paso (Regressión Paso a Paso Backwards y Forwards).#
#----------------------------------------------------------------------------------------#

modpasos <- stepAIC(modcompleto, direction = "both", trace = FALSE)
summary(modpasos)
scorecard::vif(modpasos, merge_coef = TRUE)
saveRDS(object=modpasos, file= "C:/RW/modpasos")

#--------------------------------------------------------------------------------#
#3.2.5 Contraste de los coeficientes de las 15 variables que resultaron con mayor#
#      significación lineal  en el ajuste del modelo completo Paso a paso.       #
#--------------------------------------------------------------------------------#

mod15=glm(CODDEFAULT~X5+X7+X8+X19+X25+X45+X46_W+X47_W+X48+X49_W+X53+X55_W+X57+ 
                     X58+X63,family=binomial(link="logit"),data=dt)
summary(mod15)
scorecard::vif(mod15, merge_coef = TRUE)
saveRDS(object=mod15, file= "C:/RW/mod15")

