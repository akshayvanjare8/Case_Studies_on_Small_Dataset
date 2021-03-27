a<-file.choose()

data<-read.csv(a, header = FALSE)
str(data)

colnames(data)<-c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "hd")
str(data)

data[data=="?"]<-NA

data[data$sex==0,]$sex<-"F"
data[data$sex==1,]$sex<-"M"

k<-c(2,3,6,7,9,11)
for(i in k)
  data[,i]<-as.factor(data[,i])
#data$thalach<-as.numeric(data$thalach)
data$ca<-as.factor(as.numeric(data$ca))
data$thal<-as.factor(as.numeric(data$thal))
data$hd<-ifelse(data$hd==0, "Healthy", "Unhealthy")
data$hd<-as.factor(data$hd)
str(data)
data<-data[!(is.na(data$ca)|is.na(data$thal)),]

xtabs(~hd+sex, data = data)
xtabs(~hd+cp, data = data)
xtabs(~hd+fbs, data = data)
xtabs(~hd+restecg, data = data)
xtabs(~hd+exang, data = data)
xtabs(~hd+slope, data = data)
xtabs(~hd+ca, data = data)
xtabs(~hd+thal, data = data)

glm.fit<-glm(hd~., family = "binomial"(link = "logit"), data = data)
summary(glm.fit)

##### McFadden's Pseudo Rsq #####

ll.null<-glm.fit$null.deviance/-2
ll.null
ll.propoded<-glm.fit$deviance/-2
ll.propoded

rsq<-(ll.null-ll.proposed)/ll.null
rsq
pval<-1-pchisq(2*(ll.proposed-ll.null), df = (length(glm.fit$coefficients)-1))
pval

##### IRIS #####

View(data)
str(data)
k<-c(1,4,5,8,10,14)
data<-data[k]
Y<-data[,6]
X<-as.matrix(cbind.data.frame(ones=rep(1, times = dim(data)[1]),data[,-6]))
View(X)
n<-dim(data)[1]
py<-as.vector(table(Y)/sum(table(Y)))
y<-vector("numeric")
for(i in 1:n) {
  y[i]<-ifelse(Y[i]=="Healthy", py[1], py[2])
}
beta<-as.vector(rep(0, times = dim(X)[2]))
eps<-10^(-10)

for(i in 1:100){
eta<-X%*%beta
mu<-exp(eta)/(1+exp(eta))
nu<-mu*(1-mu)
w<-n*nu
z<-eta+(y-mu)/nu
bnew<-as.vector(na.omit(lm(z~X, weights = w)$coefficients))
if(sum(abs(bnew-beta))<eps)
  break
beta<-bnew
print(i)
}
beta

#####

d1<-iris[1:100,]

IRLS<-function(X,Y,eps,max.iter){
  n<-dim(data)[1]
  py<-as.vector(table(Y)/sum(table(Y)))
  y<-vector("numeric")
  for(i in 1:n) {
    y[i]<-ifelse(Y[i]=="Healthy", py[1], py[2])
  }
  n<-length(as.vector(Y))
  x<-cbind.data.frame(ones=rep(1, times = dim(X)[1]),X)
  b<-rep(0, times = dim(X)[2])
  for (i in 1:max.iter){
    eta<-X%*%b
    mu<-exp(eta)/(1+exp(eta))
    nu<-mu*(1-mu)
    w<-n*nu
    z<-eta+((y-mu)/nu)
    bnew<-as.vector(na.omit(lm(z~X, weights = w)$coefficients))
    if(sum(abs(bnew-b))<eps){
      k<-i
      break
    }
    else
      k<-"FULL"
    b<-bnew
  }
  return(list("coef"=b, "Break Iter"=k))
}
levels(Y)<-c(0,1)
IRLS(X,Y,10^(-10),1000)







