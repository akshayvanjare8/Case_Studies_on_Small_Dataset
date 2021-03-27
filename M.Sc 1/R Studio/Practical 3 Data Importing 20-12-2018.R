#################### Data Importing ################



data2<-read.csv("D:/ships.csv", header = T)
data2


data3<-read.table("D:r2d2.txt", header = T)
data3

library("foreign")
read.spss("C:/Program Files/IBM/SPSS/Statistics/20/Samples/English/accidents.sav") use.value.labels = TRUE, to.data.frame = TRUE, max.value.labels =Inf, trim.factor.names =  trim_values= TRUE, reencode= NA, use.missing
str(data4)

############ Practice set


data<-AirPassengers
str(data)
data<-as.data.frame(data)
data$month<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
data$year<-rep(1949:1960, each = 12)
data$month<-as.factor(data$month)
data$year<-as.factor(data$year)
View(data)