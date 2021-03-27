##### NONPARAMETRIC BOOTSTRAP #####

data<-iris[,1]
str(data) #delete in final code
n<-length(data)
n

boot.stat<-NULL
n.boot<-50
for (i in 1:n.boot){
  boot.size<-sample(1:n, n, replace = T)
  boot.size
  View(boot.size) #delete in final code
  boot.stat<-append(boot.stat, mean(data[boot.size]), after = length(boot.stat))
}
boot.stat

boot.mean<-mean(boot.stat)
boot.mean

actual.stat<-mean(data)
actual.stat

boot.bias<-boot.mean-actual.stat
boot.bias

boot.var<-((sum(boot.stat-actual.stat)^2))/(n.boot-1)
#boot.var<-((1/(n.boot-1))*sum((boot.mean)^2))-((n.boot/(n.boot-1))*((actual.stat)^2))
boot.var

boot.ci<-cbind.data.frame("Lower CI" = boot.stat-1.96*sqrt(boot.var), "Upper CI" = boot.stat+1.96*sqrt(boot.var))
boot.ci

overall.boot.ci<-c(mean(boot.stat)-1.96*sqrt(boot.var), mean(boot.stat)+1.96*sqrt(boot.var))
overall.boot.ci


##### For data 2

data<-iris[,2]
str(data) #delete in final code
n<-length(data)
n

boot.stat<-NULL
n.boot<-50
for (i in 1:n.boot){
  boot.size<-sample(1:n, n, replace = T)
  boot.size
  View(boot.size) #delete in final code
  boot.stat<-append(boot.stat, var(data[boot.size]), after = length(boot.stat))
}
boot.stat

boot.mean<-mean(boot.stat)
boot.mean

actual.stat<-var(data)
actual.stat

boot.bias<-boot.mean-actual.stat
boot.bias

boot.var<-((sum(boot.stat-actual.stat)^2))/(n.boot-1)
#boot.var<-((1/(n.boot-1))*sum((boot.mean)^2))-((n.boot/(n.boot-1))*((actual.stat)^2))
boot.var

boot.ci<-cbind.data.frame("Lower CI" = boot.stat-1.96*sqrt(boot.var), "Upper CI" = boot.stat+1.96*sqrt(boot.var))
boot.ci

overall.boot.ci<-c(mean(boot.stat)-1.96*sqrt(boot.var), mean(boot.stat)+1.96*sqrt(boot.var))
overall.boot.ci


overall.boot.civar<-c(boot.mean-qchisq(0.025, 149)*(boot.var), boot.mean+qchisq(0.025,149)*(boot.var))
overall.boot.civar

#### Now For IRIS[,2]

data1<-iris[,2]
str(data1) #delete in final code
n<-length(data1)
n

boot.stat1<-NULL
n.boot1<-50
for (i in 1:n.boot1){
  boot.size1<-sample(1:n, n, replace = T)
  boot.size1
  View(boot.size1) #delete in final code
  boot.stat1<-append(boot.stat1, var(data[boot.size1]), after = length(boot.stat1))
}
boot.stat1

boot.mean1<-mean(boot.stat1)
boot.mean1

actual.stat1<-var(data1)
actual.stat1

boot.bias1<-boot.mean1-actual.stat1
boot.bias

boot.var1<-((sum(boot.stat1-actual.stat1)^2))/(n.boot1-1)
#boot.var1<-((1/(n.boot1-1))*sum((boot.mean1)^2))-((n.boot1/(n.boot1-1))*((actual.stat1)^2))
boot.var1

boot.ci1<-cbind.data.frame("Lower CI" = boot.stat1-1.96*sqrt(boot.var1),
                           "Upper CI" = boot.stat1+1.96*sqrt(boot.var1))
boot.ci1

overall.boot.ci1<-c(mean(boot.stat)-1.96*sqrt(boot.var), mean(boot.stat)+1.96*sqrt(boot.var))
overall.boot.ci1

pchisq(0.05, 149)


overall.boot.civar<-c(boot.mean1-qchisq(0.025, 149)*(boot.var1), boot.mean1+qchisq(0.025,149)*(boot.var1))
overall.boot.civar

##### NON PARAMETRIC BOOSTRAP #####




#####
#### Homework Making A function

# Practice
data1<-rnorm(100, 2500, 100)
data1

str(data1) #delete in final code
n<-length(data1)
n

boot.stat1<-NULL
n.boot1<-50
for (i in 1:n.boot1){
  boot.size1<-sample(1:n, n, replace = T)
  boot.size1
  View(boot.size1) #delete in final code
  boot.stat1<-append(boot.stat1, var(data[boot.size1]), after = length(boot.stat1))
  boot.stat2<-append(boot.stat2, mean(data[boot.size1]), after = length(boot.stat2))
}
boot.stat1
boot.stat2

boot.mean1<-mean(boot.stat1)
boot.mean1
boot.mean2<-mean(boot.stat2)
boot.mean2


actual.stat1<-var(data1)
actual.stat1
actual.stat2<-mean(data1)
actual.stat2

boot.bias1<-boot.mean1-actual.stat1
boot.bias1
boot.bias2<-boot.mean2-actual.stat2
boot.bias2

boot.var1<-((sum(boot.stat1-actual.stat1)^2))/(n.boot1-1)
#boot.var1<-((1/(n.boot1-1))*sum((boot.mean1)^2))-((n.boot1/(n.boot1-1))*((actual.stat1)^2))
boot.var1
boot.var2<-((sum(boot.stat2-actual.stat2)^2))/(n.boot1-1)
#boot.var2<-((1/(n.boot1-1))*sum((boot.mean2)^2))-((n.boot1/(n.boot1-1))*((actual.stat2)^2))
boot.var2

boot.ci1<-cbind.data.frame("Lower CI" = boot.stat1-1.96*sqrt(boot.var1),
                           "Upper CI" = boot.stat1+1.96*sqrt(boot.var1))
boot.ci1
boot.ci2<-cbind.data.frame("Lower CI" = boot.stat2-1.96*sqrt(boot.var2),
                           "Upper CI" = boot.stat2+1.96*sqrt(boot.var2))
boot.ci2

overall.boot.ci1<-c(mean(boot.stat1)-1.96*sqrt(boot.var1), mean(boot.stat1)+1.96*sqrt(boot.var1))
overall.boot.ci1
overall.boot.ci2<-c(mean(boot.stat2)-1.96*sqrt(boot.var2), mean(boot.stat2)+1.96*sqrt(boot.var2))
overall.boot.ci2

pchisq(0.05, 149)


overall.boot.civar1<-c(boot.mean1-qchisq(0.025, 149)*(boot.var1), boot.mean1+qchisq(0.025,149)*(boot.var1))
overall.boot.civar1
overall.boot.civar2<-c(boot.mean2-qchisq(0.025, 149)*(boot.var2), boot.mean2+qchisq(0.025,149)*(boot.var2))
overall.boot.civar2



































































