##### BERNOLIAN NAIVE BAYES #####

outlook<-c("R","R","O","S","S","S","O","R","R","S","R","O","O","S")
outlook
outlook<-as.factor(outlook)
outlook
levels(outlook)<-c("Overcast", "Rainy", "Sunny")

temperature<-c(2,2,2,1,0,0,0,1,0,1,1,1,2,1)
temperature
temperature<-as.factor(temperature)
temperature
levels(temperature)<-c("Cold", "Mild", "Hot")

humidity<-c(0,0,0,0,1,1,1,0,1,1,1,0,1,0)
humidity
humidity<-as.factor(humidity)
humidity
levels(humidity)<-c("Humid","Not Humid")

windy<-c(0,1,0,0,0,1,1,0,0,0,1,1,0,1)
windy
windy<-as.factor(windy)
windy
levels(windy)<-c("No","Yes")

play<-c(0,0,1,1,1,0,1,0,1,1,1,1,1,0)
play
play<-as.factor(play)
play
levels(play)<-c("No","Yes")

##### Compilation

data<-cbind.data.frame(outlook,temperature,humidity,windy,play)
data

tab1<-xtabs(~play+outlook, data = data)
tab1
tab2<-xtabs(~play+temperature, data = data)
tab2
tab3<-xtabs(~play+humidity, data = data)
tab3
tab4<-xtabs(~play+windy, data = data)
tab4

dataY<-data[data$play=="Yes",-5]
dataY
dataN<-data[data$play=="No",-5]
dataN

dim(dataY)
dataY$outlook
sum(dataY$outlook=="Sunny")

pP1<-dim(dataY)[1]/dim(data)[1]
pP0<-1-pP1

##### Conditional Probabilities 

#outlook
p0s_P1<-sum(dataY$outlook=="Sunny")/dim(dataY)[1]
p0s_P1
p0s_P0<-sum(dataN$outlook=="Sunny")/dim(dataN)[1]
p0s_P0

#humidity
pHn_P1<-sum(dataY$humidity=="Not Humid")/dim(dataY)[1]
pHn_P1
pHn_P0<-sum(dataN$humidity=="Not Humid")/dim(dataN)[1]
pHn_P0

#temperature
pTh_P1<-sum(dataY$temperature=="Hot")/dim(dataY)[1]
pTh_P1
pTh_P0<-sum(dataN$temperature=="Hot")/dim(dataN)[1]
pTh_P0

#windy
pWn_P1<-sum(dataY$windy=="No")/dim(dataY)[1]
pWn_P1
pWn_P0<-sum(dataN$windy=="No")/dim(dataN)[1]
pWn_P0

summary(dataN)
summary(dataY)

P1<-p0s_P1*pHn_P1*pTh_P1*pWn_P1
P1
P0<-p0s_P0*pHn_P0*pTh_P0*pWn_P0
P0
Final_Prob<-P1/(P1+P0)
Final_Prob

##### GAUSSIAN NAIVE BAYES #####

iris
str(iris)
summary(iris)
hist(iris[,1])   #change value of 1,2,3,4

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
d.setosa

d.versicolor<-iris[iris$Species=="versicolor",-5]
d.versicolor

d.virginica<-iris[iris$Species=="virginica",-5]
d.virginica

#setosa
mu.s<-c(mean(d.setosa$Sepal.Length),mean(d.setosa$Sepal.Width),mean(d.setosa$Petal.Length),mean(d.setosa$Petal.Width))
mu.s
sig.s<-as.vector(diag(cov(d.setosa)))
sig.s
pSL_S<-dnorm((5.1-mu.s[1])/sqrt(sig.s[1]))
pSW_S<-dnorm((3.8-mu.s[2])/sqrt(sig.s[2]))
pPL_S<-dnorm((1.9-mu.s[3])/sqrt(sig.s[3]))
pPW_S<-dnorm((1.05-mu.s[4])/sqrt(sig.s[4]))

PSet<-pSL_S*pSW_S*pPL_S*pPW_S
PSet

#versicolor
mu.v<-c(mean(d.versicolor$Sepal.Length),mean(d.versicolor$Sepal.Width),mean(d.versicolor$Petal.Length),mean(d.versicolor$Petal.Width))
mu.v
sig.v<-as.vector(diag(cov(d.versicolor)))
sig.v
pSL_v<-dnorm((5.1-mu.v[1])/sqrt(sig.v[1]))
pSW_v<-dnorm((3.8-mu.v[2])/sqrt(sig.v[2]))
pPL_v<-dnorm((1.9-mu.v[3])/sqrt(sig.v[3]))
pPW_v<-dnorm((1.05-mu.v[4])/sqrt(sig.v[4]))

PVer<-pSL_v*pSW_v*pPL_v*pPW_v
PVer

#virginica
mu.vi<-c(mean(d.virginica$Sepal.Length),mean(d.virginica$Sepal.Width),mean(d.virginica$Petal.Length),mean(d.virginica$Petal.Width))
mu.vi
sig.vi<-as.vector(diag(cov(d.virginica)))
sig.vi
pSL_vi<-dnorm((5.1-mu.vi[1])/sqrt(sig.vi[1]))
pSW_vi<-dnorm((3.8-mu.vi[2])/sqrt(sig.vi[2]))
pPL_vi<-dnorm((1.9-mu.vi[3])/sqrt(sig.vi[3]))
pPW_vi<-dnorm((1.05-mu.vi[4])/sqrt(sig.vi[4]))

PVir<-pSL_vi*pSW_vi*pPL_vi*pPW_vi
PVir

PSetosa<-PSet/(PSet+PVer+PVir)
PSetosa
PVersicolor<-PVer/(PSet+PVer+PVir)
PVersicolor
PVirginica<-PVir/(PSet+PVer+PVir)
PVirginica



##### PRACTICE (Orange) #####

Orange
summary(Orange)
colnames(Orange)

Orange$Tree<-as.factor(as.numeric(Orange$Tree))
Orange$age<-as.factor(as.numeric(Orange$age))
Orange$circumference<-as.factor(as.numeric(Orange$circumference))

levels(Orange$Tree)
T.1<-Orange[Orange$Tree==1,-3]
T.1

T.2<-Orange[Orange$Tree==2,-3]
T.2

T.3<-Orange[Orange$Tree==3,-3]
T.3

T.4<-Orange[Orange$Tree==3,-3]
T.4

T.5<-Orange[Orange$Tree==3,-3]
T.5

#Tree 1
mu.t1<-c(mean(T.1$age),mean(T.2$circumference))
mu.t1
sig.t1<-as.vector(diag(cov(T.1)))
sig.t1
pag_t1<-dnorm((1200-mu.t1[2])/sqrt(sig.t1[2]))
pci_t1<-dnorm((161-mu.t1[3])/sqrt(sig.t1[3]))

Pt1<-pag_t1*pci_t1
Pt1





