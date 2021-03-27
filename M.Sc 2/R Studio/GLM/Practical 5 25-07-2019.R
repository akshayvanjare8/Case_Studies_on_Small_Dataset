set.seed(555)
##### Poisson Regression #####

##### Award File 

k<-file.choose()
df<-read.csv(k, header = T)
str(df)

df$id<-as.factor(df$id)
df$prog<-as.factor(df$prog)
df$math<-as.numeric(df$math)

model<-glm(num_awards~prog+math, family = "poisson"(link = "log"), data = df)
model

k1<-sample(1:dim(df)[1], 0.8*dim(df)[1], replace = F)
k1
train<-df[k1,]
train
test<-df[-k1,]
test
str(train)
library(VGAM)
mod1<-glm(num_awards~prog+math, family = "poisson"(link = "log"), data = train)
mod1
summary(mod1)

pred<-predict.glm(mod1,test, type = "response")
pred

test$num_awards

rmse<-mean((pred-test$num_awards)^2)
rmse

##### Crab Example
#crab<-file.choose()
crab<-read.csv("C:\\Users\\210\\Desktop\\Akkiii\\GLM\\crab.txt", header = TRUE)
str(crab)
crab$C<-as.factor(crab$C)
crab$S<-as.factor(crab$S)

mod2<-glm(Sa~., family = "poisson"(link = "log"), data = crab)
mod2

k2<-sample(1:dim(crab)[1], 0.8*dim(crab)[1], replace = F)
k2

train.crab<-crab[k2,]
train.crab
test.crab<-crab[-k2,]
test.crab

mod3<-glm(Sa~., family = "poisson"(link = "log"), data = train.crab)
mod3

pred2<-predict.glm(mod3, test.crab, type = "response")
pred2

rmse<-mean((test.crab$Sa-pred2)^2)
rmse

sd(crab$Sa)/mean(crab$Sa)















































