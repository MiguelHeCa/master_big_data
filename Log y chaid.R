###############################################
########  Encuesta Argentina 2014 #############
####### Logista y arbol de decisiones #########
###############################################


#### Cargamos paquetes #########

paquetes <- c("rpart", "rpart.plot", "caret", "tidyverse", "dplyr", "haven",
              "MultBiplotR", "foreign" )
lapply(paquetes, require, character.only = TRUE)

######

######## Abrimos Encuesta Limpia #########

EncuestaLimpia <- read_sav("EncuestaLimpia.sav")
View(EncuestaLimpia)

glm(Genero~NES+Matrimonio, family = "binomial", data= EncuestaLimpia)


###### Aleatorizar datos #######
Aleatorizarencuesta <- sample(1:nrow(EncuestaLimpia))
Aleatorizarencuesta
EncuestaLimpia <- Aleatorizarencuesta
str(EncuestaLimpia)
head(EncuestaLimpia)


####### Transformar en dos tablas para entrenar ####### #######

Voto_Entrenamiento <- sample_frac(EncuestaLimpia, .7)
Voto_Entrenamiento
Voto_Prueba <- setdiff(EncuestaLimpia, Voto_Entrenamiento)
Voto_Prueba



#############################################
######## Arbol ############################## #######


arbol_Voto <- rpart(Voto1~., data = Voto_Entrenamiento,
                    method ='class', control = rpart.control(Cp=o))
arbol_Voto
plot(arbol_Voto)

rpart.plot(arbol_Voto)
rpart.plot(arbol_Voto, extra = 100)

EncuestaLimpia$D3005_PR_1

###### Fiabilidad del Arbol #######

modelo.predictivo<-predict(arbol_Voto, Voto_Prueba, type = 'class')

modelo.predictivo
length(modelo.predictivo)
length(arbol_Voto)

tablacomparada <- table(Voto_Prueba$Voto1, modelo.predictivo)
tablacomparada

Fiabilidad<- sum(diag(tablacomparada)) / sum(tablacomparada)
print(paste('Fiabilidad :', Fiabilidad))







#############################################
########### Regresión logistica #############
############################################

###### Prepraración de datos #######
length(EncuestaLimpia)
Votos2014 <- EncuestaLimpia$Voto1
EncuestaLimpia
EncuestaLimpia=  cbind(EncuestaLimpia, scioli= rep(0, 1408))
EncuestaLimpia=  cbind(EncuestaLimpia, macri= rep(0, 1408))
EncuestaLimpia=  cbind(EncuestaLimpia, massa= rep(0, 1408))
EncuestaLimpia=  cbind(EncuestaLimpia, carrio= rep(0, 1408))
EncuestaLimpia=  cbind(EncuestaLimpia, sanz= rep(0, 1408))
EncuestaLimpia=  cbind(EncuestaLimpia, stolbitzer= rep(0, 1408))
EncuestaLimpia=  cbind(EncuestaLimpia, delasota= rep(0, 1408))
EncuestaLimpia=  cbind(EncuestaLimpia, altamira= rep(0, 1408))
EncuestaLimpia=  cbind(EncuestaLimpia, saa= rep(0, 1408))
EncuestaLimpia=  cbind(EncuestaLimpia, indeciso= rep(0, 1408))
EncuestaLimpia=  cbind(EncuestaLimpia, blanco= rep(0, 1408))
View(EncuestaLimpia)


####### Tablas ############
tv <- table(EncuestaLimpia$Voto1)
tv

barplot(tv)

tv2 <- table(EncuestaLimpia$Eleccionesprevias, EncuestaLimpia$Voto1)
tv2

tv3 <- table(EncuestaLimpia$Identi_Partidaria, EncuestaLimpia$Voto1)
tv3

###### Regresión ##########
# 
# for (i in 1:1408) { ### adentro switch ####
#   if (EncuestaLimpia[i, Voto1] = 1)
#   {
#     EncuestaLimpia[i,"scioli"] = 1
# 
#   }
#   else if (EncuestaLimpia[i, "Voto1"] = 2)
#   {
#     EncuestaLimpia[i,"macri"] = 1
# 
#   }
#   else if (EncuestaLimpia[i, "Voto1"] = 3)
#   {
#     EncuestaLimpia[i,"massa"] = 1
# 
#   }
#   else if (EncuestaLimpia[i, "Voto1"] = 4)
#   {
#     EncuestaLimpia[i,"sanz"] = 1
# 
#   }
#   else if (EncuestaLimpia[i, "Voto1"] = 5)
#   {
#     EncuestaLimpia[i,"sanz"] = 1
# 
#   }
#   else if (EncuestaLimpia[i, "Voto1"] = 5)
#   {
#     EncuestaLimpia[i,"sanz"] = 1
# 
#   }
#   else if (EncuestaLimpia[i, "Voto1"] = 6)
#   {
#     EncuestaLimpia[i,"stolbitzer"] = 1
# 
#   }
#   else if (EncuestaLimpia[i, "Voto1"] = 7)
#   {
#     EncuestaLimpia[i,"delasota"] = 1
# 
#   }
#   else if (EncuestaLimpia[i, "Voto1"] = 8)
#   {
#     EncuestaLimpia[i,"altamira"] = 1
# 
#   }
#   else if (EncuestaLimpia[i, "Voto1"] = 9)
#   {
#     EncuestaLimpia[i,"saa"] = 1
# 
#   }
#   else if (EncuestaLimpia[i, "Voto1"] = 10)
#   {
#     EncuestaLimpia[i,"indeciso"] = 1
# 
#   }
#   else if (EncuestaLimpia[i, "Voto1"] = 11)
#   {
#     EncuestaLimpia[i,"blanco"] = 1
# 
#   }
# }


# Generar columnas
votos = EncuestaLimpia %>%
  mutate(
    scioli = if_else(Voto1 == 1, 1, 0),
    macri = if_else(Voto1 == 2, 1, 0),
    massa = if_else(Voto1 == 3, 1, 0),
    carrio = if_else(Voto1 == 4, 1, 0),
    sanz = if_else(Voto1 == 5, 1, 0),
    stolbizer = if_else(Voto1 == 6, 1, 0),
    delasota = if_else(Voto1 == 7, 1, 0),
    altamira = if_else(Voto1 == 8, 1, 0),
    saa = if_else(Voto1 == 9, 1, 0),
    indeciso = if_else(Voto1 == 10, 1, 0),
    blanco = if_else(Voto1 == 11, 1, 0),
    ninguno = if_else(!Voto1 %in% 1:11, 1, 0)
  )

# Ver el conteo de cada valor de Voto1
votos %>% 
  count(factor(Voto1))

# Comprobar que el conteo de cada nueva columna coincida. 
votos %>% 
  select(scioli:ninguno) %>%
  map( ~ count(data.frame(x = .x), x))


glm(Voto1~Genero, data = EncuestaLimpia, family = binomial)
Regre.voto=plot(lm(Voto1~Eleccionesprevias , data=EncuestaLimpia, family= "binomial"))
summary(Regre.voto)
Regre.voto

###### Biplot Canónico #######
MenosVoto <- EncuestaLimpia[-14]
View(MenosVoto)
PCA.Encuesta <- PCA.Biplot(MenosVoto, Scaling = 5)
PCA.Encuesta


MANOVA.BiplotVoto <-  manova(as.matrix(MenosVoto) ~ EncuestaLimpia$Voto1)
MANOVA.BiplotVoto$model
plot(MANOVA.BiplotVoto)
