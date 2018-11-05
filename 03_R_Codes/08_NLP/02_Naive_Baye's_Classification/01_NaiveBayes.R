setwd('F:\\Library\\Analytics Path\\02-R\\02-R DataSets\\Sentiment Analysis and Navie Baye')

sms_raw<-read.csv('sms_spam.csv')
head(sms_raw)
View(sms_raw)

sms_raw$type<-as.factor(sms_raw$type)

str(sms_raw)

library(tm)
library(NLP)

sms_corpus<-Corpus(VectorSource(sms_raw$text))

clean_corpus<-tm_map(sms_corpus,tolower)
clean_corpus<-tm_map(clean_corpus,removePunctuation)
clean_corpus<-tm_map(clean_corpus,removeNumbers)
clean_corpus<-tm_map(clean_corpus,removeWords,stopwords('english'))
clean_corpus<-tm_map(clean_corpus,stripWhitespace)


inspect(clean_corpus[1:5])

sms_dtm<-DocumentTermMatrix(clean_corpus)


sms_raw_train<-sms_raw[1:4169,]
sms_raw_test<-sms_raw[4170:5559,]

sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5559,]

sms_corpus_train<-clean_corpus[1:4169]
sms_corpus_test<-clean_corpus[4170:5559]

#EDA

library(wordcloud)

wordcloud(clean_corpus,min.freq = 30,max.words = 50,colors = brewer.pal(8,'Dark2'))


#finding most differentiating terms in spam and ham

spam<-sms_raw[sms_raw$type=='spam',]
ham<-sms_raw[sms_raw$type=='ham',]

wordcloud(spam$text,max.words = 50,colors = brewer.pal(8,'Dark2'))
#most freq words in spam->free,prize,mobile,claim,text,reply,stop,
#text,txt,won,cash,win,send
wordcloud(ham$text,max.words = 50,colors = brewer.pal(8,'Dark2'))
#most freq words in ham->can,will,come,know,like,good,time,dont,ill,love,sorry,
#just,going,home,one,want,need


#Developing Document term matrix only with most differentiating words in
# spam and ham

myTerms<-c('free','prize','mobile','claim','text','can','will','come','know','like',
           'good','time','love')

sms_train<-DocumentTermMatrix(sms_corpus_train,list(dictionary = myTerms))
sms_test<-DocumentTermMatrix(sms_corpus_test,list(dictionary = myTerms))


class(sms_train)

convert_counts<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels = c(0,1),labels = c('NO','YES'))
}

sms_train<-apply(sms_train,2,convert_counts)
sms_test<-apply(sms_test,2,convert_counts)
class(sms_test)#matrix
library(e1071)
sms_classifier<-naiveBayes(sms_train,sms_raw_train$type)#matrix is coerced into 
#df
sms_test_pred<-predict(sms_classifier,sms_test)
table(sms_raw_test$type,sms_test_pred)

## Building DocumentTermMatrix with exhaustive terms
myTerms2 = c(myTerms,'reply','stop','txt','won','cash','win','send','sorry',
             'just','going','home','one','want','need')
myTerms2

sms_train2<-DocumentTermMatrix(sms_corpus_train,list(dictionary=myTerms2))
sms_test2<-DocumentTermMatrix(sms_corpus_test,list(dictionary = myTerms2))

convert_counts<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels = c(0,1),labels = c('NO','YES'))
}

sms_train2<-apply(sms_train2,2,convert_counts)
sms_test2<-apply(sms_test2,2,convert_counts)

sms_classifier2<-naiveBayes(sms_train2,sms_raw_train$type)
sms_test_pred2<-predict(sms_classifier2,sms_test2)
table(sms_raw_test$type,sms_test_pred2)

sms_train3<-DocumentTermMatrix(sms_corpus_train)
sms_test3<-DocumentTermMatrix(sms_corpus_test)

sms_train3<-apply(sms_dtm_train,2,convert_counts)
sms_test3<-apply(sms_dtm_test,2,convert_counts)
sms_test3[1:5,1:5]

sms_classifier3<-naiveBayes(sms_train2,sms_raw_train$type,laplace = 50)
sms_test_pred3<-predict(sms_classifier3,sms_test2)
table(sms_raw_test$type,sms_test_pred3)
