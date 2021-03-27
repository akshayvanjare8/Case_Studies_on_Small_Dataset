##### Data Combine #####

data<-
?datasets
library(help = "datasets")

data<-pressure
str(data)
data

str(iris)

data1<-iris[(iris[,1]>4),5]
str(data1)

data<-cbind.data.frame(data,sample(data1,19))
data

data2<-iris[(iris[,1]>4|iris[,2]>3),5]    #AND Function
str(data2)

data<-cbind.data.frame(data,sample(data2,19))
data

##### Orange & CO2 #####

str(Orange)
str(CO2[,1])

View(Orange)
View(CO2)

zope<-c(levels(CO2[,1])[1:6])
zope

data<-CO2[c(CO2[,1]=="Qn1"|CO2[,1]=="Qn2"|CO2[,1]=="Qn3"|CO2[,1]=="Qc1"|CO2[,1]=="Qc2"|CO2[,1]=="Qc3"),]  #AND Function
data
dim(Orange)  #Dimension
View(data)

data1<-cbind.data.frame(Orange,sample(data[,4],35))
data1


##### Sonspot.month & Volcano (For_loop Function) #####

d1<-sunspot.month
dim(d2)

d2<-volcano
d2

d2<-as.matrix(volcano)
View(d2)
max(d2)
min(d2)

#For_Loop Start

x=NULL
for(i in 94:195){     
  a=0
  if(!(i%%2==0)){
  for(k in 2:((i-1)/2)){
    if(!(i%%k==0))
      a=a+0
    else
      a=a+1
    }
    if(a==0){
      x=append(x,i,after = length(x))
    }
  }
}
 x 

######
 
prime=function(k,m){
 x=NULL
 if(k<m){
 for(i in k:m){     
   a=0
   if(!(i%%2==0)){
     for(k in 2:((i-1)/2)){
       if(!(i%%k==0))
         a=a+0
       else
         a=a+1
     }
     if(a==0){
       x=append(x,i,after = length(x))
     }
   }
 }
return(x)
}
else
 print("ERROR: k>m")
}
 prime(1050,1140)

 


































































