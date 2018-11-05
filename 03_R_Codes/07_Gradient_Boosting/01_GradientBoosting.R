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
str(data1)

library(adabag)
?boosting
boosting1 = boosting(churn~.,train)
preds = predict(boosting1,test,type = 'response')
boosting1$importance
length(preds)
table(test$churn,preds$class)
102/(102+38)
head(train$churn)
train$churn<-as.numeric(as.logical(train$churn))

library(gbm)
?gbm
gbm1 = gbm(churn~.,train,distribution = 'bernoulli',
           n.trees = 2500,
           interaction.depth = 4,
           n.minobsinnode = 10,
           bag.fraction = 0.8,
           train.fraction = 1)
#interaction depth - depth of tree

preds = predict(gbm1,test,n.trees = 2500,type = 'response')
head(preds)
preds = ifelse(preds>0.4,1,0)
table(test$churn,preds)
