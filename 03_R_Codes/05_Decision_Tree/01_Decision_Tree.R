setwd('F:\\Library\\Analytics Path\\02-R\\02-R DataSets\\Decision Tree')

data1<-read.csv('dataMerged.csv')
str(data1)

#Seeing no of nas

sum(apply(data1,1,is.na))

#Removing column not required
data1$infoReq<-NULL

dim(data1)

dim(na.omit(data1))

hist(data1$age)
quantile(data1$age,na.rm = T)

#Filling nas in the data
sum(is.na(data1$age))
data1$age[is.na(data1$age)]<-mean(data1$age,na.rm = T)

str(data1)
sum(is.na(data1$exp))
hist(data1$exp)

data1$exp[is.na(data1$exp)]<-mean(data1$exp,na.rm = T)
str(data1)
sum(is.na(data1$inc))
hist(data1$inc)
#not normally distributed-use median

data1$inc[is.na(data1$inc)]<-median(data1$inc,na.rm = T)
str(data1)
dim(data1)

sum(is.na(data1$family))
mean(data1$family,na.rm = T)
data1$family[is.na(data1$family)]<-2

sum(is.na(data1$edu))
hist(data1$edu)
data1$edu[is.na(data1$edu)]<-1
str(data1)


sum(is.na(data1$mortgage))
hist(data1$mortgage)
data1$mortgage[is.na(data1$mortgage)]<-median(data1$mortgage,na.rm = T)

sum(is.na(data1$ccAvg))

sum(is.na(data1$securities))
hist(data1$securities)
data1$securities[is.na(data1$securities)]<-0

sum(is.na(data1$cd))
hist(data1$cd)
data1$cd[is.na(data1$cd)]<-0

sum(is.na(data1$online))
hist(data1$online)
data1$online[is.na(data1$online)]<-1

sum(is.na(data1$cc))
hist(data1$cc)
data1$cc[is.na(data1$cc)]<-0

sum(is.na(data1$online))

sum(apply(data1,1,is.na))

str(data1)
#converting categorical variables into factors
colsToFactor=c('family','edu','securities','cd','online','cc','loan')

for(i in colsToFactor){
  data1[,i]<-as.factor(data1[,i])
}

str(data1)

set.seed(0)
ids<-sample(nrow(data1),round(0.7*nrow(data1)))
train<-data1[ids,]
test<-data1[-ids,]

library(rpart)
?rpart
dtree1 = rpart(loan~.,train,control = c(cp = 0.01,maxdepth = 10))

library(rpart.plot)
rpart.plot(dtree1)

preds <- predict(dtree1,test,type = 'class')
table(test$loan,preds)

preds <- predict(dtree1,train,type = 'class')
table(train$loan,preds)


121/(121+28)
261/(261+70)

#recall on test and train data is almost same.So there is no 
#problem of overfitting
