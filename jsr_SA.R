library("tm")
library("rJava")
library("wordcloud")
library("textir")
library("RWeka")
#library("qdap")
library("maptpx")
library("readr")
library("ngram")
library("SnowballC")
library("NLP")

demonetization_tweets <- read_csv("D:/1 DATA BYTE/1_projects/demonetization proj/from shivani/demonetization-tweets.csv")
neg_words_list <- read_csv("~/R/negative-words.txt")
pos_words_list <- read_csv("~/R/positive-words.txt")
tweets = demonetization_tweets$text
tweets <- as.character(tweets)
senti_score <- function(liness,pos_words_list,neg_words_list,.progress = 'none')
{
  require(plyr)
  require(stringr)
scorefunc <- laply(liness,function(tweet,pos_words_list,neg_words_list)
{
  tweet = gsub("[[:cntrl:]]", " ", tweets)
  tweet <- gsub("(RT|via)((?:\\b\\W*@\\W+)+)", " ", tweet, ignore.case = T)
  tweet <- gsub('@\\w+', '', tweet)
  tweet <- gsub("[[:punct:]]"," ", tweet)
  tweet <- gsub("[[:digit:]]"," ", tweet)
  tweet <- gsub("http[s]?\\w+", " ", tweet)
  tweet <- gsub("[ \t]{2,}", " ", tweet)
  tweet <- gsub("^\\s+|\\s+$", " ", tweet)
  tweet <- tweet[!is.na(tweet)]
  tweet = gsub("^ ", "", tweet)
  tweet = gsub(" $", "", tweet)
  tweet = gsub("[^[:alnum:] ]", " ", tweet)
  tweet = tolower(tweet)
  
  word.list <- str_split(liness, '\\s+')
  wordss <- unlist(word.list)
  
  neg_matches <- match(wordss, neg_words_list)
  pos_matches <- match(wordss, pos_words_list)
  
  #myposwlist <- c(myposwlist,words[pos_matches])
  #mynegwlist <- c(mynegwlist,words[neg_matches])
  
  pos_matches <- !is.na(pos_matches)
  neg_matches <- !is.na(neg_matches)
  
  score <- sum(pos_matches) - sum(neg_matches)
  return (score)
},pos_words_list,neg_words_list, .progress = .progress) 

 scoredf  <- data.frame(score=scorefunc, text=liness)
 return(scoredf)
}

positive <- scan("~/R/positive-words.txt", what= "character", comment.char= ";")
negative <- scan("~/R/negative-words.txt", what= "character", comment.char= ";")

tweetsanalysis <- senti_score(tweets, positive, negative, .progress="none")

str(tweetsanalysis)