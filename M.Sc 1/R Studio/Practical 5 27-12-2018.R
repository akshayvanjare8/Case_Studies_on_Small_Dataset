##### Data Stacking #####

g1<-c(31,65,82,52,31,29,45)  #imp for exam
g1

g2<-c(61,81,25,85,63,97,46)
g2

g3<-c(95,62,31,64,84,81,35)
g3

combined_g<-cbind.data.frame(g1,g2,g3)
combined_g

stacked_g<-stack(combined_g)
stacked_g

aov(values~ind, data = stacked_g)  #anova

#####

g1<-c(31,65,82,52,31,29,45, NA)   #not this
g1

g2<-c(61,81,25,85,63,97,46, 51)
g2

g3<-c(95,62,31,64,84,81,35, NA)
g3

combined_g<-cbind.data.frame(g1,g2,g3)
combined_g

stacked_g<-stack(combined_g)
stacked_g

aov(values~ind, data = stacked_g)  #anova

lm1<-lm(values~ind, data = stacked_g)  #anova significant difference
anova(lm1)

aov(values~ind, data = stacked_g)  #anova

unstack_g<-unstack(stacked_g)
unstack_g

##### IRIS ######

data<-iris
str(data)

lm.fit<-lm(data[,1]~data[,5], data=data)  #anova significant difference
anova(lm.fit)

aov(data[,1]~data[,5], data=data)    #anova

k<-cbind.data.frame(data[,1],as.factor(data[,5]))
View(unstack(k))

##### Airpassenger Data #####

data1<-AirPassengers

k<-length(as.vector(data1))
#k

data2<-NULL
data2$x<-as.data.frame(as.vector(data1[1:150]))
#View(data2)

data2$Group<-as.data.frame(as.factor(rep(c(1,2,3), each=50)))
#data2[,2]<-as.factor(data2[,2])
#str(data2)

data2<-as.data.frame(data2)
colnames(data2)<-c("x","group")
data2[,2]<-as.factor(data2[,2])
#str(data2)

data3<-unstack(data2)

lm3<-lm(x~group, data=data2)
anova(lm3)

aov(x~group, data=data2)


##### IRIS #####

data<-iris
View(data)

lm.fit<-lm(Sepal.Length~Species, data)
anova(lm.fit)

lm.fit1<-lm(Sepal.Length~Species+Sepal.Width, data)
anova(lm.fit1)

#t.test(data$Sepal.Length, data$Species)    #in this line ERRoR t_test
t.test(data$Sepal.Length, data$Sepal.Width)   #t_test

t.test(data$Sepal.Length, data$Sepal.Width, alternative = "greater",paired = F) #t_test


##### Re_cap all above cammands #####

x1<-seq(log(3), by = log(2), length.out = 35)
x1<-exp(x1)
x1

x2<-sample(1500:6503, 35)
x2

x3<-rep(c(1,2,3), 35)
x3

x4<-sample(c(1,2), 35, replace = T)
x4

x5<-sample(3500:10000, 35)
x5

data<-cbind.data.frame(x1,x2,x3,x4,x5)
data$x3<-as.factor(data$x3)
data$x4<-as.factor(data$x4)
str(data)

k<-sample(1:35, 15)
samp.data<-data[k,]
(samp.data)

str(samp.data)

