##### K Fold Cross Varification #####

iris

k1<-sample(1:150, 0.8*dim(iris)[1])
k1

train<-iris[k1,]
train
test<-iris[-k1,]
test

k<-5
n<-dim(train)[1]/k
n

y<-rep(c(1,2,3,4,5), times = 24)
y
KNN<-function(train, test, k){
  k<-5
  yhat<-NULL
  i=1
  j=1
  for(i in 1:dim(test)[1]){
    d<-NULL
    for(j in 1:dim(train)[1]){
      d[j]<-sqrt(sum((train[j,-5]-test[i,-5])^2))
    }
    d1<-rank(d)
    tar1<-train[d1<=k,5]
    t1<-table(tar1)
    yhat<-append(yhat, ifelse(t1[1]>t1[2],ifelse(t1[1]>t1[3], "setosa", "virginica"),ifelse(t1[2]>t1[3],"versicolor", "virginica")), after = length(yhat))
  }
  t1<-table("actual"=test[,5], "pred"=yhat)
  return(t1)
}
KNN(train1, test1, 5)



k1<-sample(1:150, 0.8*dim(iris)[1])
k1

train<-iris[k1,]
train
test<-iris[-k1,]
test

k<-5
n<-dim(train)[1]/k
n

y<-rep(c(1,2,3,4,5), times = 24)
y
acc1<-NULL
for (i in 1:k){
  test1<-train[y==i,]
  train1<-train
  target<-train1[,5]
  m1<-KNN(train1, test1, 5)
  acc1<-append(acc1, sum(m1)-sum(diag(m1)), after = length(acc1))
}
s<-mean(acc1)
s

test1<-train[!(y==1),]
test1





















































