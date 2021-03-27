
set.seed(1234)

library(rpart)
iris
par<-sample(1:2, nrow(iris), replace = T, prob = NULL)
train<-iris[par==1,]
test<-iris[par==2,]
kNN<-function(train, test, k){
  yhat<-NULL
  for(i in 1:dim(test)[1]){
    d<-NULL
    for(j in 1:dim(train)[1]){
      d[j]<-sqrt(sum((train[j,-5]-test[i,-5])^2))
    }
    d1<-rank(d)
    tar1<-train[d1<=k,5]
    t1<-table(tar1)
    yhat<-append(yhat, ifelse(t1[1]>t1[2], 
                              ifelse(t1[1]>t1[3], "setosa", "virginica"),
                              ifelse(t1[2]>t1[3], "versicolor", "virginica")), 
                 after = length(yhat))
  }
  t1<-table("actual"=test[,5], "pred"=yhat)
  missclass.prob<-1-(sum(diag(t1)))/(sum(t1))
  mylist<-list('Confusion Matrix'=t1, 'Missclassification Probability'=missclass.prob)
  return(mylist)
}
v1.0<-kNN(train, test, 5)
v1.0

##### 

library(rpart)
tree<-rpart(Species~., data = train)
tree
plot(tree)
pred1<-predict(tree, test , type = "class")
pred1
tab<-table('actual'= test$Species,  'prdicted'=pred1)
tab
mis.prob2<-1-((sum(diag(tab)))/(sum(tab)))
mis.prob2

##### with Cross Validation


kNN<-function(train, test, k){
  yhat<-NULL
  for(i in 1:dim(test)[1]){
    d<-NULL
    for(j in 1:dim(train)[1]){
      d[j]<-sqrt(sum((train[j,-5]-test[i,-5])^2))
    }
    d1<-rank(d)
    tar1<-train[d1<=k,5]
    t1<-table(tar1)
    yhat<-append(yhat, ifelse(t1[1]>t1[2], 
                              ifelse(t1[1]>t1[3], "setosa", "virginica"),
                              ifelse(t1[2]>t1[3], "versicolor", "virginica")), 
                 after = length(yhat))
  }
  
   t1<-table("actual"=test[,5], "pred"=yhat)
   misclass.prob<-1-(sum(diag(t1)))/(sum(t1))
   
   mylist<-list('Confusion Matrix'=t1, 'Misclassification Probability'=misclass.prob)
   return(mylist)
 }

library(rpart)
tree<-function(train, test){
  tree<-rpart(Species~., data = train)
  tree
  plot(tree)
  pred1<-predict(tree, test , type = "class")
  pred1
  tab<-table('actual'= test$Species,  'prdicted'= pred1)
  tab
  mis.prob2<-1-((sum(diag(tab)))/(sum(tab)))
  mis.prob2
  mylist<-list('Confusion Matrix'=t1, 'Misclassification Probability'=mis.prob2)
  return(mylist)
}

iris
k<-10
par<-sample(1:k, nrow(iris), replace = T, prob = c(rep((1/k), times = k)))

knn.mp=NULL
dtree.mp=NULL

for (i in 1:k){
 test<-iris[par==i,]
 train<-iris[!(par==i),]
 v1.0<-kNN(train, test, 5)
 knn.mp<-append(knn.mp, v1.0$'Misclassification Probability', after = length(knn.mp))
 v2.0<-tree(train, test)
 dtree.mp<-append(dtree.mp, v2.0$'Misclassification Probability', after = length(dtree.mp))
}

misclass<-cbind.data.frame(knn.mp, dtree.mp)
misclass

vote<-NULL
for (i in 1:nrow(misclass)){
  vote=append(vote, ifelse(misclass[i,2]<misclass[i,1], 'Dtree',
                           ifelse(misclass[i,2]>misclass[i,1], 'kNN', 'Tie')), after = length(vote))
}

vote<-as.factor(vote)
vote
table(vote)

misclass[vote=='Tie',]
misclass[!(vote=='Tie'),]
misclass[vote=='kNN',]

mean(misclass$knn.mp)
mean(misclass$dtree.mp)

Verdict<-ifelse(mean(misclass$knn.mp)>mean(misclass$dtree.mp), "Decision Tree", "K Nearest Neighbours")
























