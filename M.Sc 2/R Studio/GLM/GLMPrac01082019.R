k<-file.choose()
df<-read.csv(k, header = T)
str(df)
df$id<-as.factor(df$id)
df$prog<-as.factor(df$prog)
df$math<-as.numeric(df$math)
model<-glm(num_awards~prog+math, family = "poisson"(link = "log"), data = df)
k1<-sample(1:dim(df)[1], 0.8*dim(df)[1], replace = F)
train<-df[k1,]
test<-df[-k1,]
str(train)
library(VGAM)
mod1<-glm(num_awards~prog+math, family = "poisson"(link = "log"), data = train)
summary(mod1)
pred<-predict.glm(mod1,test, type = "response")
test$num_awards
rmse<-mean((pred-test$num_awards)^2)




#### R code for Crab example
####Poisson Regression Model for Count Data

k<-file.choose()
crab=read.table(k)
colnames(crab)=c("Obs","C","S","W","Wt","Sa")
#str(crab)

#### to remove the column labeled "Obs"

crab=crab[,-1]
crab$C<-as.factor(crab$C)
crab$S<-as.factor(crab$S)
#str(crab)


#### the following code corresponds to crab.SAS-crab1.SAS

#### Fitting the intercept only model 
#### This model implies the expected number of satellites 
#### per each crab is the same
#### in this case: E(Sa)=2.919=exp(1.0713)

model1=glm(crab$Sa~1, family=poisson(link=log))
summary(model1)
model1$fitted

#### Poisson Regression of Sa on W

model2=glm(crab$Sa~1+crab$W,family=poisson(link=log))
summary(model2)
summary(anova(model2))

#### to get the predicted count for each observation: 
#### e.g. for the first observation E(y1)=3.810

print=data.frame(crab,pred=model$fitted)
print

#### note the linear predictor values
#### e.g., for the first observation, exp(1.3378)=3.810

model2$linear.predictors
exp(model2$linear.predictors)

rstandard(model2)

#### Interpretation of the slope which is statistically significant here
#### e.g., exp(0.1640)=1.18
#### as the width increases by one unit, the number of satilies will 
#### increase (positive sign of the coef), 
#### it will be multiplied by 1.18
#### e.g., for W=26 and W=25, first for all values
#### then for specific rows

model2$fitted[crab$W==26]/model2$fitted[crab$W==25]

model2$fitted[2]/model2$fitted[6]

#### Based on the residual deviance the model does NOT fit well
#### e.g., 567.88/171 = 3.3209

1-pchisq(model2$deviance, model2$df.residual)

#### creating a scatter plot of Sa vs. W

plot(crab$W,crab$Sa)
identify(crab$W, crab$Sa) 

#### click on the plot to identify individual values
#### identified on the screen and the plot, \#48,101,165

#### Diagnostics measures (like in logistic regression)
#### But these work for ungrouped data too, 
#### as long as there is a variable with counts
#### You can do many more but here are a few indicating a lack of fit

influence(model2)
plot(influence(model2)$pear.res)
plot(model2$linear.predictors, residuals(model2, type="pearson"))

#### To predict a new value

newdt=data.frame(C=2, S=3, W=26.3, Wt=3, Sa=6)
str(newdt)
model2$coefficients[1]+26.3*model2$coefficients[2]
#### Let's assume for now that we do not have other covariates
#### and we will adjust for overdispersion 
#### first look at the sample mean and variances 
## e.g., tapply(crab$Sa, crab$W,function(x)c(mean=mean(x),variance=var(x)))

#### To estimate dispersion parameter
#### Do it on your own by X2/df & use summary.glm() (see log. reg notes)
#### Use DISMOD package (see log.reg notes)
#### Use quasipoisson family 

model.disp=glm(crab$Sa~crab$W, family=quasipoisson(link=log), data=crab)
summary.glm(model.disp)
summary.glm(model2)$dispersion


#### Fit a negative binomial model
#### the dispersion parameter is THETA
#### Here are two different ways, both must use library(MASS) 
#### In the first one, we fix theta to be 1.0
#### In the second one we let glm.nb() to estimate it

library(MASS)
nb.fit=glm(Sa~W, data=crab, family=negative.binomial(theta=1, link="identity"), start=model$coef)
summary(nb.fit)

nb.fit1=glm.nb(Sa~W, data=crab, init.theta=1, link=identity, start=model$coef)
summary(nb.fit1)

#### Adding a categorical predictor
#### This corresponds with crab2.SAS
#### make sure C is a factor

is.factor(crab$C)
crab$C=as.factor(crab$C)
model=glm(Sa~W+C,family=poisson(link=log), data=crab)
summary(model2)
anova(model)
print=data.frame(crab,pred=model$fitted)
print

#### to get the same order as you do in SAS

contrasts(crab$C)=contr.SAS(levels(crab$C))
model=glm(Sa~W+C,family=poisson(link=log),data=crab)
summary(model)
anova(model)

#### to get back to the default level

contrasts(crab$C)=contr.treatment(levels(crab$C),base=1)

#### Or you can explicilty code the levels to correspond to SAS
#### Notice that change from C1 to C4 in a number of SA is significant 
#### exp(0.447)=1.54, pvalue=0.0324
#### but if you adjust for overdispersion it's not significant

Sa=crab$Sa
W=crab$W
C1=1*(crab$C==1)
C2=1*(crab$C==2)
C3=1*(crab$C==3)
model=glm(Sa~W+C1+C2+C3,family=poisson(link=log))
summary(model)
anova(model)
print=data.frame(crab,pred=model$fitted)
print

plot(crab$W, model$fitted)

model.disp=glm(Sa~W+C1+C2+C3, family=quasipoisson(link=log))
summary.glm(model.disp)
summary.glm(model.disp)$dispersion
anova(model.disp)

#### Treat Color as a numeric predictor 
#### This corresponds to crab3.SAS

Sa=crab$Sa
W=crab$W
C=as.numeric(crab$C)
model=glm(Sa~W+C,family=poisson(link=log))
summary(model)
anova(model)
print=data.frame(crab,pred=model$fitted)
print

newdt=data.frame(W=0,C=1)
predict.glm(model, type="response", newdata=newdt)

#### This corresponds to to crab5.SAS

width=c(22.69,23.84,24.77, 25.84,26.79,27.74,28.67,30.41)
cases=c(14,14,28,39,22,24,18,14)
SaTotal=c(14,20,67,105,63,93,71,72)
lcases=log(cases)
CrabGrp=data.frame(width,cases,SaTotal,lcases)
model=glm(SaTotal~width,offset=lcases,family=poisson(link=log))
residuals(model)
summary(model)
anova(model)
print=data.frame(CrabGrp,pred=model$fitted)
print

#### create a plot of the results

plot(SaTotal, pch="o", col="blue", main="Plot of Observed and Predicted Sa vs. groups", 
     xlab="Width groups", ylab="Number of Satellites")
points(model$fitted, pch="p", col="red")
legend(6,30,c("obs","pred"), pch=c("o","p"), col=c("blue","red"))

model=glm(SaTotal~width,offset=lcases,family=poisson(link=identity))
residuals(model)

#### if you wanted to aggregate from the original data
#### create W as a factor variable with 8 levels

W.fac=cut(crab$W, breaks=c(0,seq(23.25, 29.25),Inf))
numcases=table(W.fac)

#### now compute sample means for Width variable by the cuts
#### and total number of Sa cases

width=aggregate(crab$W, by=list(W=W.fac),mean)$x
width
SaMean=aggregate(crab$Sa, by=list(W=W.fac),mean)$x
SaMean
plot(width,SaMean)
SaTotal=aggregate(crab$Sa, by=list(W=W.fac),sum)$x
SaTotal
lcases=log(numcases)
lcases




########Crab Model

str(crab)
crab$C<-as.factor(crab$C)
crab$S<-as.factor(crab$S)
mod2<-glm(Sa~., family = "poisson"(link = "log"), data = crab)
k2<-sample(1:dim(crab)[1], 0.8*dim(crab)[1], replace = F)
train.crab<-crab[k2,]
test.crab<-crab[-k2,]
mod3<-glm(Sa~., family = "poisson"(link = "log"), data = train.crab)
pred2<-predict.glm(mod3, test.crab, type = "response")
rmse<-mean((test.crab$Sa-pred2)^2)

sd(crab$Sa)/mean(crab$Sa)

