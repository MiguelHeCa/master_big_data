library(car)
# install.packages('rattle')
data(wine, package='rattle')
attach(wine)
head(wine)

scatterplotMatrix(wine[2:6])

library(MASS)
wine.lda <- lda(Type ~ ., data=wine)

wine.lda

wine.lda.values <- predict(wine.lda)

table(wine.lda.values$class, wine$Type)

ldahist(data = wine.lda.values$x[,1], g=wine$Type)

ldahist(data = wine.lda.values$x[,2], g=wine$Type)

plot(wine.lda.values$x[,1],wine.lda.values$x[,2]) # make a scatterplot
text(wine.lda.values$x[,1],wine.lda.values$x[,2],wine$Type,cex=0.7,pos=4,col="red") # add labels


url <- 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/admission.csv'

admit <- read.csv(url)

head(admit)

adm=data.frame(admit)

plot(adm$GPA,adm$GMAT,col=adm$De)

m1=lda(De~.,adm)
m1

predict(m1,newdata=data.frame(GPA=3.21,GMAT=497))

m2=qda(De~.,adm)
m2

predict(m2,newdata=data.frame(GPA=3.21,GMAT=497))


n=85
nt=60
neval=n-nt
rep=100

### LDA
set.seed(123456789)
errlin=dim(rep)
for (k in 1:rep) {
  train=sample(1:n,nt)
  ## linear discriminant analysis
  m1=lda(De~.,adm[train,])
  predict(m1,adm[-train,])$class
  tablin=table(adm$De[-train],predict(m1,adm[-train,])$class)
  errlin[k]=(neval-sum(diag(tablin)))/neval
}
merrlin=mean(errlin)
merrlin


set.seed(123456789)
errqda=dim(rep)
for (k in 1:rep) {
  train=sample(1:n,nt)
  ## quadratic discriminant analysis
  m1=qda(De~.,adm[train,])
  predict(m1,adm[-train,])$class
  tablin=table(adm$De[-train],predict(m1,adm[-train,])$class)
  errqda[k]=(neval-sum(diag(tablin)))/neval
}
merrqda=mean(errlin)
merrqda

library(klaR)
partimat(De~.,data=adm,method="lda") 



credit <- read.csv("http://www.biz.uiowa.edu/faculty/jledolter/DataMining/germancredit.csv")
head(credit,2) # See details about codification in the attached documentation.

cred1=credit[, c("Default","duration","amount","installment","age")]
head(cred1)

summary(cred1)

hist(cred1$duration)
hist(cred1$amount)
hist(cred1$installment)
hist(cred1$age)

cred1=data.frame(cred1)

zlin=lda(Default~.,cred1)

# Confusion Matrix:
table(predict(zlin)$class, cred1$Default)

predict(zlin,newdata=data.frame(duration=6,amount=1100,installment=4,age=67))

predict(zlin,newdata=data.frame(duration=6,amount=1100, installment=4,age=67))$class 

## QDA: class proportions of the training set used as prior probabilities
zqua=qda(Default~.,cred1)

# Confusion Matrix:
table(predict(zqua)$class, cred1$Default)

predict(zqua,newdata=data.frame(duration=6,amount=1100,installment=4,age=67))

predict(zqua,newdata=data.frame(duration=6,amount=1100, installment=4,age=67))$class


# Checking assumptions ----------------------------------------------------

data(iris)

iris = subset(iris, Species %in% c("versicolor", "virginica"))

aggregate(Petal.Length~Species, data = iris, FUN = var)

var.test(x = iris[iris$Species == "versicolor", "Petal.Length"],
         y = iris[iris$Species == "virginica", "Petal.Length"] )


vinos = foreign::read.spss("data/M3/vinos.sav", to.data.frame = T)

nortest::lillie.test(vinos$grado)
nortest::lillie.test(vinos$avol)
nortest::lillie.test(vinos$atot)
nortest::lillie.test(vinos$acfi)
nortest::lillie.test(vinos$ph)
nortest::lillie.test(vinos$ic2)


var.test(x = vinos[vinos$a_o == "1986", "grado"],
         y = vinos[vinos$a_o == "1987", "grado"])

var.test(x = vinos[vinos$a_o == "1986", "grado"],
         y = vinos[vinos$a_o == "1987", "grado"])

var.test(x = vinos[vinos$a_o == "1986", "grado"],
         y = vinos[vinos$a_o == "1987", "grado"])

var.test(x = vinos[vinos$a_o == "1986", "grado"],
         y = vinos[vinos$a_o == "1987", "grado"])

library(rstatix)
mdebox = box_m(Filter(is.numeric, vinos), factor(vinos$a_o))
mdebox$parameter
mdebox$method

library(MVTests)

BoxM(Filter(is.numeric, vinos), factor(vinos$a_o))


with(Moore, leveneTest(conformity, fcategory))
with(Moore, leveneTest(conformity, interaction(fcategory, partner.status)))
leveneTest(conformity ~ fcategory*partner.status, data=Moore)
leveneTest(lm(conformity ~ fcategory*partner.status, data=Moore))
leveneTest(conformity ~ fcategory*partner.status, data=Moore, center=mean)
leveneTest(conformity ~ fcategory*partner.status, data=Moore, center=mean, trim=0.1)

library(car)
leveneTest(grado ~ a_o, data = vinos, center = median)
leveneTest(avol ~ a_o, data = vinos, center = median)
leveneTest(atot ~ a_o, data = vinos, center = median)

hist(vinos$avol)

plot(count ~ spray, data = InsectSprays)
bartlett.test(InsectSprays$count, InsectSprays$spray)
bartlett.test(count ~ spray, data = InsectSprays)
