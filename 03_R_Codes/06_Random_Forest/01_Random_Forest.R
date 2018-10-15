setwd('F:\\Library\\Analytics Path\\02-R\\02-R DataSets\\K means Clustering')

data1<-read.csv('Churn Data.csv')
sum(apply(data1,1,is.na))
str(data1)

#Doing necessary preprocessing
data1$state<-NULL
length(unique(data1$account.length))
data1$account.length<-NULL
length(unique(data1$area.code))
data1$area.code<-as.factor(data1$area.code)
str(data1)
data1$phone.number<-NULL
str(data1)
length(unique(data1$number.vmail.messages))
length(unique(data1$customer.service.calls))
hist(data1$customer.service.calls)
table(data1$customer.service.calls)

data1$customer.service.calls<-ifelse(data1$customer.service.calls>3,'>3',data1$customer.service.calls)
str(data1)
data1$customer.service.calls<-as.factor(data1$customer.service.calls)
data1$churn<-as.factor(data1$churn)
str(data1)

set.seed(0)

ids<-sample(nrow(data1),round(0.7*nrow(data1)))
train<-data1[ids,]
test<-data1[-ids,]
#Building a decision tree

library(rpart)
dtree1<-rpart(churn~.,train,control = c(cp = 0.01,maxdepth = 10))
library(rpart.plot)
rpart.plot(dtree1)

preds<-predict(dtree1,test,type = 'class')
table(test$churn,preds,dnn = c('Actual','Predicted'))

#model performance on train data
preds<-predict(dtree1,train,type = 'class')
table(train$churn,preds,dnn = c('Actual','Predicted'))

101/(101+39)
271/(271+72)

#recall is almost same in both test and train data.So, no problem of overfitting
#may be little overfitting
#Lets try with Bagging

library(adabag)
?bagging
bag1<-bagging(churn~.,train,mfinal = 50)
preds<-predict(bag1,test,type = 'response')

table(test$churn,preds$class)

preds<-predict(bag1,train,type = 'response')
table(train$churn,preds$class)

95/(95+45)
253/(253+90)
#Overfitting has reduced by little like 2%

# lets try random forest

library(randomForest)
?randomForest
rf1<-randomForest(churn~.,train,ntree = 100,mtry = 8,strata = train$churn)
preds<-predict(rf1,test,type = 'response')
table(test$churn,preds)

preds<-predict(rf1,train,type = 'response')
table(train$churn,preds)
105/(105+35)
343/(343)

#Since recall on test and train data are not close, Overfitting taking place
#control overfitting by maxdepth or node size

rf2<-randomForest(churn~.,train,ntree = 100,mtry = 8,strata = train$churn,nodesize = 10)
preds<-predict(rf2,test,type = 'response')
table(test$churn,preds)

preds<-predict(rf2,train,type = 'response')
table(train$churn,preds)
103/(103+37)
291/(291+52)
# Overfitting reduced by little

rf3<-randomForest(churn~.,train,ntree = 100,mtry = 8,strata = train$churn,nodesize = 20)
preds<-predict(rf3,test,type = 'response')
table(test$churn,preds)

preds<-predict(rf3,train,type = 'response')
table(train$churn,preds)
102/(102+38)
277/(277+66)

rf4<-randomForest(churn~.,train,ntree = 100,mtry = 8,strata = train$churn,nodesize = 40)
preds<-predict(rf4,test,type = 'response')
table(test$churn,preds)

preds<-predict(rf4,train,type = 'response')
table(train$churn,preds)
98/(98+42)
267/(267+76)

rf5<-randomForest(churn~.,train,ntree = 100,mtry = 8,strata = train$churn,nodesize = 60)
preds<-predict(rf5,test,type = 'response')
table(test$churn,preds)

preds<-predict(rf5,train,type = 'response')
table(train$churn,preds)
94/(94+46)
257/(257+86)

#though overfitting is reducing, recall is taking a hit
#increase no of trees and decrease node size

rf6<-randomForest(churn~.,train,ntree = 1000,mtry = 8,strata = train$churn,nodesize = 10)
preds<-predict(rf6,test,type = 'response')
table(test$churn,preds)

preds<-predict(rf6,train,type = 'response')
table(train$churn,preds)
105/(105+35)
290/(290+53)
#no much improvement in recall even for more trees,and overfitting taking place

rf6<-randomForest(churn~.,train,ntree = 300,mtry = 8,strata = train$churn,nodesize = 10,
                  classwt = c(30,1))
preds<-predict(rf6,test,type = 'response')
table(test$churn,preds)

preds<-predict(rf6,train,type = 'response')
table(train$churn,preds)
92/(92+48)
268/(268+75)
