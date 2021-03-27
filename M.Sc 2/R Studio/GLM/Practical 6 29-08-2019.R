library(MASS)

data("Cars93")
View(Cars93)
attach(Cars93)
hist(MPG.city)
ks.test(MPG.city, "gamma")
model1<-glm(MPG.city~Price+Cylinders+
              EngineSize+Horsepower+RPM+Man.trans.avail+
              Wheelbase+Wei0ght, family = "Gamma"(link = "log"), 
            data = Cars93)
model1
summary(model1)
k<-sample(1:nrow(Cars93), 20, replace = T)
k
colnames(Cars93)
test<-Cars93[k,c(5,11,12,13,14,16,20,25)]
test
pred<-predict.glm(model1, test, type = "response")
pred
actual<-Cars93[k,7]
actual
se<-sqrt(mean((actual-pred)^2))
se
cbind(actual, pred)
cbind(Cars93[,7], pred)
plot(actual, pred)

#####

library(MASS)

data("Cars93")
View(Cars93)
attach(Cars93)
hist(Length)
ks.test(Length, "gamma")
model2<-glm(Length~Price+MPG.city+Cylinders+
              EngineSize+Horsepower+RPM+Man.trans.avail+
              Wheelbase+Width+Weight, family = "Gamma"(link = "log"), 
            data = Cars93)
model2
summary(model2)
k1<-sample(1:nrow(Cars93), 20, replace = T)
k1
colnames(Cars93)
test1<-Cars93[k1,c(5,7,11,12,13,14,16,20,21,25)]
test1
pred1<-predict.glm(model1, test1, type = "response")
pred1
actual1<-Cars93[k1,7]
actual1
se<-sqrt(mean((actual1-pred1)^2))
se
cbind(actual1, pred1)
cbind(Cars93[,7], pred1)
plot(actual1, pred1)


#####



k<-sample(1:nrow(Cars93), 20, replace = T)
k
colnames(Cars93)
test<-Cars93[k,c(5,11,12,13,14,16,20,25)]
test
pred<-predict.glm(model1, test, type = "response")
pred
actual<-Cars93[k,7]
actual
se<-sqrt(mean((actual-pred)^2))
se
cbind(actual, pred)
cbind(Cars93[,7], pred)
plot(actual, pred)
model12



aic3<-AIC(model3)
aic<-c(aic1,aic2,aic3)
aic<-(aic-min(aic))/sd(aic)

p2<-predict(model2, type = "response")
se2<-sqrt(mean((actual-p2)^2))
p3<-predict(model3, type = "response")
se3<-sqrt(mean((actual-p3)^2))
















