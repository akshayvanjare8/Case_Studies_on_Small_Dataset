
#################### BERNOULLIAN NAIVE BAYES ###########################
Outlook<-c('R',"R","O",'S',"S","S",'O',"R",'R','S','R','O',"O",'S')
Temperature<-c(2,2,2,1,0,0,0,1,0,1,1,1,2,1)
Temperature<-as.factor(Temperature)
levels(Temperature)<-c("Cold", "Mild", "Hot")
Outlook<-as.factor(Outlook)
levels(Outlook)<-c("Overcast", "Rainy", "Sunny")
Humidity<-c(0,0,0,0,1,1,1,0,1,1,1,0,1,0)
Humidity<- as.factor(Humidity)
levels(Humidity)<-c("Humid", "Not Humid")
Windy<-c(0,1,0,0,0,1,1,0,0,0,1,1,0,1)
Windy<-as.factor(Windy)
levels(Windy)<-c("No", "Yes")
Play<-c(0,0,1,1,1,0,1,0,1,1,1,1,1,0)
Play<-as.factor(Play)
levels(Play)<-c("No", "Yes")

######Compilation

data<-cbind.data.frame(Outlook,Temperature,Humidity,Windy,Play)


tab1<-xtabs(~Play+Outlook, data = data)
tab2<-xtabs(~Play+Temperature, data = data)
tab3<-xtabs(~Play+Humidity, data = data)
tab4<-xtabs(~Play+Windy, data = data)

dataY<-data[data$Play=="Yes",-5]
dataN<-data[data$Play=="No",-5]

dim(dataY)

pP1<-dim(dataY)[1]/dim(data)[1]
pP0<-1-pP1

#####Conditional Probabilities

#Outlook
pOs_P1<-sum(dataY$Outlook=="Sunny")/dim(dataY)[1]
pOs_P0<-sum(dataN$Outlook=="Sunny")/dim(dataN)[1]

#Humid
pHn_P1<-sum(dataY$Humidity=="Not Humid")/dim(dataY)[1]
pHn_P0<-sum(dataN$Humidity=="Not Humid")/dim(dataN)[1]

#Temperature
pTh_P1<-sum(dataY$Temperature=="Hot")/dim(dataY)[1]
pTh_P0<-sum(dataN$Temperature=="Hot")/dim(dataN)[1]

#Windy
pWn_P1<-sum(dataY$Windy=="No")/dim(dataY)[1]
pWn_P0<-sum(dataN$Windy=="No")/dim(dataN)[1]

P1<-pOs_P1*pHn_P1*pTh_P1*pWn_P1
P0<-pOs_P0*pHn_P0*pTh_P0*pWn_P0
Final_Prob<-P1/(P1+P0)


#################### GAUSSIAN NAIVE BAYES ######################

str(iris)
summary(iris)
colnames(iris)
mu1<-mean(iris[,1])
mu2<-mean(iris[,2])
mu3<-mean(iris[,3])
mu4<-mean(iris[,4])

v1<-var(iris[,1])
v2<-var(iris[,2])
v3<-var(iris[,3])
v4<-var(iris[,4])
levels(iris$Species)
d.setosa<-iris[iris$Species=="setosa",-5]
d.versicolor<-iris[iris$Species=="versicolor",-5]
d.virginica<-iris[iris$Species=="virginica",-5]

#setosa
mu.s<-c(mean(d.setosa$Sepal.Length),mean(d.setosa$Sepal.Width),
        mean(d.setosa$Petal.Length), mean(d.setosa$Petal.Width))
sig.s<-as.vector(diag(cov(d.setosa)))
pSL_S<-dnorm((5.1-mu.s[1])/sqrt(sig.s[1]))
pSW_S<-dnorm((3.8-mu.s[2])/sqrt(sig.s[2]))
pPL_S<-dnorm((1.9-mu.s[3])/sqrt(sig.s[3]))
pPW_S<-dnorm((1.05-mu.s[4])/sqrt(sig.s[4]))
PSet<-pSL_S*pSW_S*pPL_S*pPW_S

#Versicolor
mu.v<-c(mean(d.versicolor$Sepal.Length),mean(d.versicolor$Sepal.Width),
        mean(d.versicolor$Petal.Length), mean(d.versicolor$Petal.Width))
sig.v<-as.vector(diag(cov(d.versicolor)))
pSL_V<-dnorm((5.1-mu.v[1])/sqrt(sig.v[1]))
pSW_V<-dnorm((3.8-mu.v[2])/sqrt(sig.v[2]))
pPL_V<-dnorm((1.9-mu.v[3])/sqrt(sig.v[3]))
pPW_V<-dnorm((1.05-mu.v[4])/sqrt(sig.v[4]))
PVer<-pSL_V*pSW_V*pPL_V*pPW_V

#Virginica
mu.vi<-c(mean(d.virginica$Sepal.Length),mean(d.virginica$Sepal.Width),
        mean(d.virginica$Petal.Length), mean(d.virginica$Petal.Width))
sig.vi<-as.vector(diag(cov(d.virginica)))
pSL_Vi<-dnorm((5.1-mu.vi[1])/sqrt(sig.vi[1]))
pSW_Vi<-dnorm((3.8-mu.vi[2])/sqrt(sig.vi[2]))
pPL_Vi<-dnorm((1.9-mu.vi[3])/sqrt(sig.vi[3]))
pPW_Vi<-dnorm((1.05-mu.vi[4])/sqrt(sig.vi[4]))
PVir<-pSL_Vi*pSW_Vi*pPL_Vi*pPW_Vi

PSetosa<-PSet/(PSet+PVer+PVir)
PVersicolor<-PVer/(PSet+PVer+PVir)
PVirginica<-PVir/(PSet+PVer+PVir)



Orange
summary(Orange)
Orange$Tree<-as.factor(as.numeric(Orange$Tree))
