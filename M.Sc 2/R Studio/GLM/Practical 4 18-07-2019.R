##### Multinomial Logistic Regression #####

data<-read.csv("C:\\Users\\21031\\Desktop\\Akkiii\\GLM\\Ldata.csv", header = TRUE)
View(data)
require(nnet)
str(data)

data$admit<-as.factor(data$admit)
data$rank<-as.ordered(as.factor(data$rank))

multinom(admit~., data = data)

model1<-multinom(Species~., data = iris)
model1

s<-summary(model1)

coef<-s$coefficients
coef

head(iris,6)

x<-c(1,4.7,4.3,2.1,0.4)
x

bver<-coef[1,]
bver
ever<-exp(t(x)%*%bver)
ever
pi.ver<-ever/(1+ever)
pi.ver

bvir<-as.vector(coef[2,])
bvir
evir<-exp(t(x)%*%bvir)
evir
pi.vir<-evir/(1+evir)
pi.vir

pi.set<-1-pi.vir-pi.ver
pi.set

model2prob<-function(coef,x){
  e.mat<-exp(coef%*%x)
  pi.mat<-e.mat/(1+e.mat)
  pi.mat<-append(pi.mat, 1-sum(pi.mat), after = )
  mylist<-list("Versicolor"=pi.mat[1], "Verginica"=pi.mat[2])
  return(mylist)
}
model2prob(coef,c(1,6.1,2.3,3.8,1.1))
str(data)

library(MASS)
model2<-polr(rank~., data = data, method = "logistic")
s2<-summary(model2)
c2<-s2$coefficients













































