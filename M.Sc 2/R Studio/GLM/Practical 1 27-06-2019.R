str(iris)
iris[,-5]
d<-cov(iris[,-5])
d
t<-chol(d)
X<-cbind(c(rnorm(150)),c(rnorm(150)),c(rnorm(150)),c(rnorm(150)))
#t%*%t
#t(t)%*%t
mu<-NULL
for (i in 1:4)
mu<-append(mu,mean(iris[,i]), after = length(mu))
mu
#X[1,]
#t(X[1,])
#t%*%(X[1,])-mu
Z=NULL
for (i in 1:dim(X)[1]) {
  Z<-cbind(Z, t%*%(X[i,])-mu)
}
z<-as.data.frame(t(Z))

View(z)

##########

z

lm1<-lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width, data = z)
lm1
summary(lm1)

glm1<-glm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width, data = z,family = "gaussian"(link = "identity"))
glm1
summary(glm1)

########## Binary Logistic Regression ##########

data<-iris[!iris$Species=='virginica',]
str(data)
View(data)
data[,5]<-as.factor(data[,5])
glm2<-glm(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data = data,family = "binomial"(link = "logit"))
glm2
#glm2<-glm(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data = data,family = "binomial"(link = "progit"))
#glm2
plot(data)

mu
coef<-glm2$coefficients
coef
t(c(1,mu))%*%coef
rt<-function(alpha){
  x<-exp(alpha)/(1+exp(alpha))
  return(x)
}
rt(t(c(1,mu))%*%coef)
exp(t(c(1,mu))%*%coef)

