##### Booststrap #####

nrow(iris)
boot.data=NULL
iris.boot<-iris[sample(1:nrow(iris), nrow(iris)/2, replace = T)]
str(iris.boot)
summary(iris)
summary(iris.boot)
str(iris.boot)
diag(cov(iris[,-5]))
m=NULL
for(i in 1:4)
m<-append(m, mean(iris[i]), after = length(m))
m
m1=NULL
for (i in 1:4)
m1<-append(m1, mean(iris.boot[i]), after = length(m1))
m1
(m-m1)