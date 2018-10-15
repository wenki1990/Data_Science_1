data1 = read.csv("C:/Users/phsivale/Documents/Trainings/universalBank.csv")
data1$ZIP.Code  = NULL
data1$ID = NULL

data1$Family = as.factor(data1$Family)
data1$Education = as.factor(data1$Education)

summary(data1)
data1$Experience[data1$Experience <0] = 0

View(cor(data1[,-c(4,6)]))

data1$Experience = NULL # High cor with Age
# data1$CCAvg = NULL # High cor with Income Will drop this later

### Convert Cat to numeric
library(dummies)
data1_dummies = dummy.data.frame(data1)
names(data1_dummies)
data1_dummies = data1_dummies[,-c(6,10)]


### Scaling
###Min max scaling
fnScaling = function(x){
  return((x-min(x))/(max(x)-min(x)))
}


for(i in 1:ncol(data1_dummies)){
  data1_dummies[,i] = fnScaling(data1_dummies[,i])
}
summary(data1_dummies)

### Kmeans clustering
##
betweenByTotal = c()
for(i in 2:15){
  clust = kmeans(x=data1_dummies,centers = i)
  betweenByTotal = c(betweenByTotal,clust$betweenss/clust$totss)
}
# kmeans(x=data1_dummies,centers = 5)
plot(2:15,betweenByTotal,type = 'l')


clust =  kmeans(x=data1_dummies,centers = 12)
clust  ##gives betweenss/totss ratio
clust$centers ##gives coordinates of each cluster
clust$cluster ##gives each data point belong to which cluster
