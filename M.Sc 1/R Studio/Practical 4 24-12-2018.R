############ Input ################

data<-iris

View(data)
str(data)

summary(data)


########### Regression ###########

######### Assumption Check

library(tseries)
chisq.test(data[,1])


lm.fit=lm(data[,1]~.,data=data)   #Console
lm.fit         #Console

lm.fit<-lm(Sepal.Length~Petal.Length+factor(Species),dada=data)        #Console
anova(lm.fit)   #Console


shapiro.test(data[,1])  #Check Data Normal

qqnorm(data[,1], abline(col="Red"))   #graph plot

qqnorm(data[,1], abline(a=NULL,col="Red"))    #graph plot

qqnorm(data[,2], abline(a=NULL,col="Red"))   #graph plot
 
bartlett.test(data[,1])











