str(iris)
d<-as.matrix(cov(iris[,-5]))
d
t<-chol(d)
X<-cbind(c(rnorm(150)),c(rnorm(150)), c(rnorm(150)), c(rnorm(150)))
mu<-NULL
for (i in 1:4)
mu<-append(mu,mean(iris[,i]), after = length(mu))
mu
Z=NULL
for (i in 1:dim(X)[1]) {
  Z<- cbind(Z, t%*%(X[i,])-mu)  
}
Z<-as.data.frame(t(Z))
###########################
Z
lm1<-lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width, data = Z)
summary(lm1)

glm1<-glm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width, data = Z,family = "gaussian"(link = "identity"))
summary(glm1)

############# BINARY LOGISTIC REGRESSION ######################

data<-iris[!iris$Species=='virginica',]
str(data)
data[,5]<-as.factor(data[,5])
glm2<-glm(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data = data, family = "binomial"(link = "logit"))
plot
coef<-glm2$coefficients
rt<-function(alpha){
  x<-exp(alpha)/(1+exp(alpha))
  return(x)
}
