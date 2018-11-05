library(recommenderlab)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DT)
library(knitr)
library(grid)
library(gridExtra)
library(corrplot)
library(methods)
library(Matrix)

setwd('F:\\Library\\Analytics Path\\02-R\\01-R Scripts\\17-Recommender Systems\\goodbooks-10k')


ratings<-fread('ratings.csv')
head(ratings)
class(ratings)
data.frame(name = c("Akanksha",'Ritesh'))
#removing duplicates
ratings[,N:= .N,.(user_id,book_id)]
head(ratings)
nrow(ratings[ratings$N>1,])


ratings<-ratings[N==1]


ratings[,N:= .N,.(user_id)]

head(ratings)
ratings<-ratings[N>2]

users<-unique(ratings$user_id)
set.seed(1)
sample_users<-sample(users,round(0.2*length(users)))
ratings<-ratings[user_id %in% sample_users]

cat('Number of ratings :',nrow(ratings))

dimension_names<-list(user_id=sort(unique(ratings$user_id)),book_id = sort(unique(ratings$book_id)))

ratingmat<-spread(select(ratings,user_id,book_id,rating),book_id,rating) %>% select(-user_id)
rm(ratings)
class(ratingmat)
head(ratingmat)
View(ratingmat)

ratingmat<-as.matrix(ratingmat)
rm(ratingmat)
dimnames(ratingmat)<-dimension_names
ratingmat[1:5,1:5]


ratingmat0<-ratingmat
ratingmat0[is.na(ratingmat0)]<-0
ratingmat0[1:5,1:5]

sparse_ratings<-as(ratingmat0,'sparseMatrix')
rm(ratingmat0)
sparse_ratings[1:5,1:5]

real_ratings<-new('realRatingMatrix',data=sparse_ratings)
rm(sparse_ratings)
as(real_ratings,'list')[[1]]
rec<-Recommender(real_ratings,method = 'UBCF',para = list(method='pearson',nn=5))
predictions<-predict(rec,real_ratings[2],n=2)
as(predictions,'list')
