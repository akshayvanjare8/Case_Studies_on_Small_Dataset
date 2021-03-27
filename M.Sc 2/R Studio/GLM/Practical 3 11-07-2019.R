##### 
Y<-c(10,15,21,12,19)
X<-c(21.5,13.2,22.5,21.1,3.2)
W<-c(1,1,0.2,1,1)

d<-cbind.data.frame(Y,X)
plot(Y~X)

fit1<-lm(Y~X, data=d, weights = W)
abline(coef = fit1$coefficients, col = "Red")



































