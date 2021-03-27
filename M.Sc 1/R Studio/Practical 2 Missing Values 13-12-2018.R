############ Practical 2 Missing Value (13/12/2018) ############ 

x=c(1,2,3,4,5,6,7)
x

x=append(x,NA,after=length(x))
x

is.na(x)

y=x[is.na(x)==FALSE]
y

z=x[!is.na(x)] #compliment
z

z=x[!(x<3)] #compliment
z

z=x[(x<3)]
z

data=iris
str(data)
data1=data[(data$Species=="setosa"),-5]
View(data1)
data2=data[(data$Sepal.Length<4.8),-5]
View(data2)
data3=data[(data$Sepal.Length<4.8),]
View(data3)

#r=data[c(data$Sepal.Length>6 | data$Sepal.Width>3),]  #OR function
View(r)

#data2=data[c(data$Sepal.Length>6 & data$Sepal.Width>3),]   #AND function
View(data)

k=sample(1:150,20,replace=T)
#m=(sample(1:5, replace = T))
for(i in 1:5)
  data[c(sample(k,2)),i]=NA

#data2=data[c(data$Sepal.Length>6 & data$Sepal.Width>3),]
View(data)

na.omit(data)   #use this code for remove all missing values in data like "NA"


############# Sequencing

seq(1, 20, by = 2)
seq(1, by=2, length.out =10)

seq(19, by = -2, length.out=10)
sort.int(seq(19, by = -2, length.out=10), decreasing = FALSE)
sort.int()

seq(19, by = -2, length.out=10)
sort.int(seq(19, by = -2, length.out=10), decreasing = FALSE)

log(6)
seq(log(6), by = log(2), length.out = 10)->k1
k1
exp(k1)

########################

data=iris
data
data[,1]%in%data[,2]
match(data[,1],data[,1])

y=as.factor(data[,1])
str(y)    #levels
summary(y)

y1=y[!frequency(y)==1]

summary.factor(y)
summ=summary(y)




data<-AirPassengers
str(data)
duplicated(data)
!duplicated(data)
data1<-data[!duplicated(data)]
(data1)
duplicated(data1)




data<-iris
str(data)
sum(duplicated(data))
data1<-data[!duplicated(data[,1]),]
(data1)
dim(data)
dim(data1)


data<-iris
str(data)
sum(duplicated(data))
data1<-data[!duplicated(data[,2]),]
(data1)
dim(data)
dim(data1)

###################################