setwd('F:\\Library\\Analytics Path\\02-R\\02-R DataSets\\Sentiment Analysis and Navie Baye')

library(tm)
library(NLP)
library(plyr)#laply
library(dplyr)
library(ggplot2)
library(stringr)#str_split
# only tm and wordcloud is required in this entire prg
posText = read.delim(file='polarity_pos.txt',header = FALSE,stringsAsFactors = FALSE)

posText<-posText$V1

posText<-unlist(lapply(posText,function(x){str_split(x,'\n')}))


score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  #
{
  require(plyr)
  require(stringr)
  #laply will return a vector
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence) #gsub-> it replaces a character with a given chara
    #punct-> @,$, , ,# base pkg
    sentence <- gsub('[[:cntrl:]]', "", sentence) 
    #cntrl carriage returns-> \n , 
    sentence <- gsub('\\d+', "", sentence)  #0-9
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+') #splitting sentence into words based on space,but they 
    # are together as list data structure is a list
    words <- unlist(word.list)# bag of words, data structure is converted into vector
    pos.matches <- match(words, pos.words) # pos.words is list of +ve words or dictionary
    #match - base pkg
    neg.matches <- match(words, neg.words)#if no match, na is returned
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches)  - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

affin_list<-read.delim(file='AFINN-111.txt',header=F,stringsAsFactor = F)

head(affin_list)
names(affin_list)=c('word','score')
affin_list$word<-tolower(affin_list$word)

negterms<-affin_list$word[affin_list$score<0]
posterms<-affin_list$word[affin_list$score>0]

posResult = data.frame(score.sentiment(posText,posterms,negterms))

head(posResult)
hist(posResult$score)

### Deep Dive into negative reviews

table(posResult$score)
negativereviews<-posResult[posResult$score<0,]

pos_corpus<-Corpus(VectorSource(negativereviews$text))

list_stop = c('movie','movies','film','cinema','picture')
corpus_clean<-tm_map(pos_corpus,tolower)
inspect(head(corpus_clean))
corpus_clean<-tm_map(corpus_clean,removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords,stopwords('english'))
corpus_clean<-tm_map(corpus_clean,removeWords,list_stop)
corpus_clean<-tm_map(corpus_clean,removePunctuation)
inspect(head(corpus_clean))
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
inspect(head(corpus_clean))

#creating document term matrix

dtm_pos<-DocumentTermMatrix(corpus_clean)
tdm_pos<-TermDocumentMatrix(corpus_clean)
class(dtm_pos)
dtm_pos_mat<-as.matrix(dtm_pos)
dtm_pos_mat[1:5,1:5]


m<-as.matrix(tdm_pos)
m2<-rowSums(m)
head(m2)
v<-sort(m2,decreasing = T)
head(v)
class(v)
is.vector(v)
d<-data.frame(word = names(v),freq = v)


library(wordcloud)
wordcloud(words = d$word, freq = d$freq, min.freq = 20,
          max.words=30, random.order=TRUE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud(corpus_clean, min.freq = 50, colors=brewer.pal(8, "Dark2"))

findAssocs(dtm_pos, terms = "characters",corlimit = 0.1)
