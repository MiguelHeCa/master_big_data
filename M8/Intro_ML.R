setwd(dirname(rstudioapi::getSourceEditorContext()$path))

heart = read.csv("heart.csv")

oldnames = colnames(heart)

colnames(heart) = c(
  'age',
  'sex',
  'chest_pain_type',
  'resting_blood_pressure',
  'cholesterol',
  'fasting_blood_sugar',
  'rest_ecg',
  'max_heart_rate_achieved',
  'exercise_induced_angina',
  'st_depression',
  'st_slope',
  'num_major_vessels',
  'thalassemia',
  'target'
)

classes = sapply(heart, class)

heart$sex = factor(heart$sex)
levels(heart$sex) = c("mujer", "hombre")

heart$chest_pain_type=factor(heart$chest_pain_type)
levels(heart$chest_pain_type)=c("typical", "atypical", "non-ang", "asympt")

heart$fasting_blood_sugar=factor(heart$fasting_blood_sugar)
levels(heart$fasting_blood_sugar)=c("<120", ">120")

heart$rest_ecg=factor(heart$rest_ecg)
levels(heart$rest_ecg)=c("normal", "ST-T", "lv hypertrophy")

heart$exercise_induced_angina=factor(heart$exercise_induced_angina)
levels(heart$exercise_induced_angina)=c("no", "yes")

heart$st_slope=factor(heart$st_slope)
levels(heart$st_slope)=c("upsloping", "flat", "downsloping")

heart$thalassemia=factor(heart$thalassemia)
levels(heart$thalassemia)=c("normal", "normal", "fixed", "reversable")

heart$target=factor(heart$target)
levels(heart$target)=c("No evento", "Evento")

Classes=sapply(heart,class)
for (i in 1:ncol(heart))
  if (Classes[i]=='integer') heart[[i]]=as.numeric(heart[[i]])
Classes=sapply(heart,class)
Classes


## ---------------------------------------------------------------------------------------------------------------------------------------------------
which(is.na(heart))


## ---------------------------------------------------------------------------------------------------------------------------------------------------
summary(heart[,Classes=="numeric"])
apply(heart[,Classes=="numeric"],2,sd)



## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)
# install.packages("gridExtra")
library(patchwork)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
g1 = ggplot(heart, aes(x=target))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()
g2 = ggplot(heart, aes(x=thalassemia))+
  geom_bar(stat="count", width=0.7, fill="red")+
  theme_minimal()
g3 = ggplot(heart, aes(x=sex))+
  geom_bar(stat="count", width=0.7, fill="green")+
  theme_minimal()
g4 = ggplot(heart, aes(x=st_slope))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()
# grid.arrange(g1, g2, g3, g4, nrow = 2, ncol=2)
(g1 + g2 )/ (g3 + g4)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
qplot(resting_blood_pressure, cholesterol, data = heart, colour = target)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
qplot(cholesterol, max_heart_rate_achieved, data = heart, colour = target)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
p1 <- ggplot(heart, aes(x=target, y=resting_blood_pressure, color=target)) + 
  geom_boxplot()
p2 <- ggplot(heart, aes(x=target, y=cholesterol, color=target)) + 
  geom_boxplot()
p3 <- ggplot(heart, aes(x=target, y=max_heart_rate_achieved, color=target)) + 
  geom_boxplot()
p4 <- ggplot(heart, aes(x=target, y=st_depression, color=target)) + 
  geom_boxplot()
# grid.arrange(p1, p2, p3, p4, nrow = 2, ncol=2)
(p1 + p2) / (p3 + p4) +  plot_layout(guides = 'collect')

## ---------------------------------------------------------------------------------------------------------------------------------------------------
if(!"ggmosaic" %in% installed.packages()[,1]) install.packages("ggmosaic")
library(ggmosaic)
q1=ggplot(data = heart) +
  geom_mosaic(aes(x = product(target, sex), fill=target))+  labs(x = "sex ", title='Heart Data')
q2=ggplot(data = heart) +
  geom_mosaic(aes(x = product(target, chest_pain_type), fill=target))+  labs(x = "chest_pain_type ", title='Heart Data')
q3=ggplot(data = heart) +
  geom_mosaic(aes(x = product(target, fasting_blood_sugar), fill=target))+  labs(x = "fasting_blood_sugar ", title='Heart Data')
q4=ggplot(data = heart) +
  geom_mosaic(aes(x = product(target, rest_ecg), fill=target))+  labs(x = "rest_ecg ", title='Heart Data')
q5=ggplot(data = heart) +
  geom_mosaic(aes(x = product(target, exercise_induced_angina), fill=target)) +  labs(x = "exercise_induced_angina ", title='Heart Data')
q6=ggplot(data = heart) +
  geom_mosaic(aes(x = product(target, thalassemia), fill=target)) +  labs(x = "thalassemia ", title='Heart Data')
# grid.arrange(q1, q2, q3, q4, q5, q6, nrow = 3, ncol=2)
(q1 + q2) / (q3 + q4) / (q5 + q6) + plot_layout(guides = "collect")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
if(!"GGally" %in% installed.packages()[,1]) install.packages("GGally")
library(GGally)
library(ggplot2)
ggpairs(heart[,Classes=="numeric"])+ theme_bw()



## ---------------------------------------------------------------------------------------------------------------------------------------------------
p <- ggpairs(heart[,c(which(Classes=="numeric"),14)], aes(color = target))+ theme_bw()
# Change color manually.
# Loop through each plot changing relevant scales
for(i in 1:p$nrow){
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=c("#00AFBB", "#E7B800")) +
      scale_color_manual(values=c("#00AFBB", "#E7B800"))  
  }
}
p


## ---------------------------------------------------------------------------------------------------------------------------------------------------
heart[,Classes=="numeric"]=scale(heart[,Classes=="numeric"])
head(heart)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
X=model.matrix(target~., data=heart)
head(X)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
tr=round(nrow(heart)*0.7)
set.seed(0)
muestra=sample.int(nrow(heart), tr)
Train.heart=heart[muestra,]
Val.heart=heart[-muestra,]


## ---------------------------------------------------------------------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------------------------------------------------------------------
gfit1=glm(target~., data=heart, family=binomial)
summary(gfit1)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
gfit0=glm(target~1, data=heart, family=binomial)
anova(gfit0, gfit1, test = "Chisq")


## ---------------------------------------------------------------------------------------------------------------------------------------------------
anova(gfit1, test="Chisq")


## ---------------------------------------------------------------------------------------------------------------------------------------------------
gfit2=glm(target~., data=Train.heart, family=binomial)
cbind(gfit1$coefficients, gfit2$coefficients)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
p=predict(gfit2, Val.heart, type="response")
PredTarget=as.factor(p>0.5)
levels(PredTarget)=c("No evento", "Evento")
install.packages("caret")
library(caret)
matrizLogis<-confusionMatrix(Val.heart$target, PredTarget)
matrizLogis


## ---------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("pROC")
library(pROC)
test_prob = predict(gfit2, newdata = Val.heart, type = "response")
test_roc = roc(Val.heart$target ~ test_prob, plot = TRUE, print.auc = TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("e1071")
library(e1071)
fitsvm1 <-svm(target ~., data = Train.heart)
summary(fitsvm1)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
#Predict Output 
predictedSVM = predict(fitsvm1,Val.heart)
matrizSVM1<-confusionMatrix(Val.heart$target, predictedSVM)
matrizSVM1


## ---------------------------------------------------------------------------------------------------------------------------------------------------
fitsvm2 <-svm(target ~., data = Train.heart, kernel="polynomial")
summary(fitsvm2)
predictedSVM = predict(fitsvm2,Val.heart)
matrizSVM2<-confusionMatrix(Val.heart$target, predictedSVM)
matrizSVM2


## ---------------------------------------------------------------------------------------------------------------------------------------------------
fitsvm3 <-svm(target ~., data = Train.heart, kernel="sigmoid")
predictedSVM = predict(fitsvm3,Val.heart)
matrizSVM3<-confusionMatrix(Val.heart$target, predictedSVM)
matrizSVM3


## ---------------------------------------------------------------------------------------------------------------------------------------------------
fitsvm4 <-svm(target ~., data = Train.heart, kernel="linear")
predictedSVM = predict(fitsvm4,Val.heart)
matrizSVM4<-confusionMatrix(Val.heart$target, predictedSVM)
matrizSVM4


## ---------------------------------------------------------------------------------------------------------------------------------------------------
Accuracy=c(matrizLogis$overall[1], matrizSVM1$overall[1], matrizSVM2$overall[1],matrizSVM3$overall[1],matrizSVM4$overall[1])
names(Accuracy)=c("Logística","SVM-radial", "SVM-polynomial", "SVM-sigmoid", "SVM-Linear")
Accuracy


## ---------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("neuralnet")
require(neuralnet)
Train=data.frame(Train.heart$target,model.matrix(target~., data=Train.heart)[,-1])
colnames(Train)[1]="target"
nn1=neuralnet(target ~., data=Train, hidden=3, act.fct = "logistic", linear.output = FALSE)
plot(nn1)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
Validate=data.frame(Val.heart$target,model.matrix(target~., data=Val.heart)[,-1])
colnames(Validate)[1]="target"
Predict=compute(nn1,Validate)
predictedNN1=factor(Predict$net.result[,1]>0.5, labels = c("No evento", "Evento"))
matrizNN1<-confusionMatrix(Val.heart$target, predictedNN1)
matrizNN1


## ---------------------------------------------------------------------------------------------------------------------------------------------------
nn1=neuralnet(target ~., data=Train, hidden=c(5,3), act.fct = "logistic", linear.output = FALSE)
plot(nn1)
Predict=compute(nn1,Validate)
predictedNN1=factor(Predict$net.result[,1]>0.5, labels = c("No evento", "Evento"))
matrizNN1<-confusionMatrix(Val.heart$target, predictedNN1)
matrizNN1


## ---------------------------------------------------------------------------------------------------------------------------------------------------
fitbayes <-naiveBayes(target ~., data = Train.heart)
summary(fitbayes)
#Predict Output 
predictedBayes= predict(fitbayes,Val.heart)
matrizNB<-confusionMatrix(Val.heart$target, predictedBayes)
matrizNB


## ---------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("tree")
require(tree)
tree1 = tree(target~., data = Train.heart)
summary(tree1)
plot(tree1)
text(tree1, pretty = 1)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
predicedtree1 = predict(tree1, Val.heart, type="class")
matriztree1<-confusionMatrix(Val.heart$target, predicedtree1)
matriztree1


## ---------------------------------------------------------------------------------------------------------------------------------------------------
cv.tree1 = cv.tree(tree1, FUN = prune.misclass)
cv.tree1
plot(cv.tree1)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
prune.tree1 = prune.misclass(tree1, best = 10)
plot(prune.tree1)
text(prune.tree1, pretty=0)
predicedtree2 = predict(prune.tree1, Val.heart, type="class")
matriztree2<-confusionMatrix(Val.heart$target, predicedtree2)
matriztree2


## ---------------------------------------------------------------------------------------------------------------------------------------------------
prune.tree2 = prune.misclass(tree1, best = 4)
plot(prune.tree2)
text(prune.tree2, pretty=0)
predicedtree3 = predict(prune.tree2, Val.heart, type="class")
matriztree3<-confusionMatrix(Val.heart$target, predicedtree2)
matriztree3


## ---------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("randomForest")
library(randomForest)
fitRF <- randomForest(target ~., data = Train.heart,ntree=500)
summary(fitRF)
#Predict Output 
predictedRF = predict(fitRF,Val.heart)
matrizRF1<-confusionMatrix(Val.heart$target, predictedRF)
matrizRF1


## ---------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("gbm")
require(gbm)
boost.HEART = gbm(target~., data = Train.heart, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(boost.HEART)
