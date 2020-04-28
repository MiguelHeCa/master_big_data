# crear un array dos filas, tres columnas, dos dimensiones

arr <-
  array(1:12,
        dim = c(2, 3, 2),
        dimnames = list(
          A = c("a2", "a2"),
          B = c("b1", "b2", "b3"),
          C = c("c1", "c2")
        ))

library(rTensor)

tnsr <- rand_tensor(rep(20, 6))
tnsr@data

ria<-as.data.frame(ria)
row.names(ria)<-ria[,1]
ria<-ria[,-1]

X<-toArray(ria,6,11,18)
labA<-row.names(ria)
labB<-c("SAL","TEMP","PH","O2", "NO3","NO4","NH4","PO4",  "NP", "CHLa", "DIST")
labC<-c(paste( c("ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC") , "02",sep="" ),    paste(c("ENE","FEB","MAR","ABR","MAY","JUN") , "03",sep=""))

dimnames(X)<-list(labA,labB,labC)
dimnames(X)