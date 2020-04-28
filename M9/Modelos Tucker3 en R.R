library(rrcov3way)
library(ThreeWay)
library(rTensor)

arr<-array(1:12,dim=c(2,3,2),
           dimnames = list(A=c("a1","a2"),B=c("b1","b2","b3"),C=c("c1","c2")))

tnsr<-rand_tensor(rep(20,6))
tnsr@data
tnsr<-as.tensor(arr)

#Xa
#rTensor
k_unfold(as.tensor(arr),1)
#rrcov3way
unfold(arr,"A")

#ThreeWay: 
supermat(arr)
#ThreeWay
Y<-rarray(ria, 6,11,18)

X<-matrix(c(1,2,1,4,0,3,4,2,2),
          nrow=3,ncol=3)
Y<-matrix(c(-1,3,0,2,1,3,1,0,1),
          nrow=3,ncol=3)  

kronecker(X,Y)
kronecker_list(list(X,Y)) #rTensor
hadamard_list(list(X,Y)) #rTensor
khatri_rao(X,Y)

ttm(as.tensor(arr), Y, m=1)

fnorm(as.tensor(arr)) #rTensor

ria<-as.data.frame(ria)
row.names(ria)<-ria[,1]

ria<-ria[,-1]
dim(ria)


X<-toArray(ria,6,11,18)

labA<-row.names(ria)
labB<-c("SAL","TEMP","PH","O2", "NO3","NO4","NH4","PO4",  
        "NP", "CHLa", "DIST")
labC<-c(paste( c("ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO",
                 "SEP","OCT","NOV","DIC") , "02",sep="" ),
        paste(c("ENE","FEB","MAR","ABR","MAY","JUN") , "03",sep=""))

dimnames(X)<-list(labA,labB,labC)

ria.norm<-do3Scale(X,center=T,scale=T, 
                   center.mode = "A", scale.mode = "B")

#Tucker3
res<-Tucker3(ria.norm, P=2, Q=4, R=4, 
             center=F, scale=F)
  
res$fp

round(res$GA,3)

core2<-res$GA^2
var<-round(core2/11,3)
sum(var[1,])
sum(var[2,])


res$A 
res$B 
res$C  

plot(res, which="comp", mode="A", main="Modo A")
plot(res, which="comp", mode="B", main="Modo B")
plot(res, which="comp", mode="C", main="Modo C")
plot(res, which="jbplot",main="Joint Biplot")















  
  











