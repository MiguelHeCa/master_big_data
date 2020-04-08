
data = readxl::read_excel("M8/Cluster/Reglas de Asociacion.xlsx")

dim(data)

library(data.table)

t<-data.table(Obesidad=data$Obesidad, Dislipidemia=data$Dislipidemia, 
              Diabetes=data$DIABETES, Hipertension=data$Hipertensión, 
              HDLBajo=data$HDL_Bajo, Microalbuminuria=data$Microalbuminuria, 
              id=data$Historia)

trx<-melt.data.table(data=t, id.vars="id", na.rm=T)[value==1,.(id,variable)]
trx

library(arules)
library(arulesViz)


trx<-split(trx$variable, trx$id)
trx<-as(trx, "transactions")


itemFrequency(trx, "relative")
itemFrequencyPlot(trx, topN=3, type="absolute")


reglas<-apriori(trx, parameter = list(support=0.106, confidence=0.9))
reglas
inspect(reglas)

reglas<-sort(reglas, by="confidence", decreasing=T)
inspect(reglas[1:5])


plot(reglas, method="paracoor")

plot(reglas, method="graph")
plot(reglas, method="graph", control=list(layout=igraph::in_circle()))
plot(reglas, method="graph",
     control=list(layout=igraph::as_star()))



