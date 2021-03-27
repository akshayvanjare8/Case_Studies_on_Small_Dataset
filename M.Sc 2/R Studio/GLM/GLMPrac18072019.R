################# MULTINOMIAL LOGISTIC REGRESSION ####################

data<-read.csv("D:AD/Ldata.csv", header = T)
View(data)
data=data[,-1]
require(nnet)
str(data)
data$admit<-as.factor(data$admit)
data$rank<- as.ordered(as.factor(data$rank))
model1<-multinom(Species~., data = iris)
s<-summary(model1)
k1<-sample(1:150, 0.8*150, replace = F)
train1<-iris[k1,]
test1<-iris[-k1,]
pred1<-predict(model1, test1)
conf.mtx1<-table("actual"=test1$Species, "Predicted"=pred1)
prob1<-predict(model1, test1, type = 'p')

#################Heuristic Code
coef<-s$coefficients
head(iris,6)
x<-c(1,4.7,4.3,2.1,0.4)
bver<-as.vector(coef[1,])
ever<-exp(t(x)%*%(bver))
pi.ver<-ever/(1+ever)
bvir<-as.vector(coef[2,])
evir<-exp(t(x)%*%bvir)
pi.vir<-evir/(1+evir)
pi.set<-1-pi.vir-pi.ver

model2prob<-function(coef,x){
  e.mat<-exp(coef%*%x)
  pi.mat<-e.mat/(1+e.mat)
  pi.mat<-append(pi.mat, 1-sum(pi.mat), after = length(pi.mat))
  mylist<- list("Versicolor"=pi.mat[1], "Virginica"=pi.mat[2], "Setosa"=pi.mat[3])
  return(mylist)
}
model2prob(coef,c(1,6.1, 2.3, 3.8, 1.1))
str(data)


################# OLR
library(MASS)
k<-sample(1:dim(data)[1], 0.8*dim(data)[1], replace = F)
train<-data[k,]
test<-data[-k,]
model2<-polr(rank~., data = train, method = "logistic")
s2<-summary(model2)
c2<-s2$coefficients
c2<-c2[,1]

pred<-predict(model2, test)
conf.mtx<-table('actual' = test$rank, 'predicted' = pred)
predict(model2, test, type = "p")
