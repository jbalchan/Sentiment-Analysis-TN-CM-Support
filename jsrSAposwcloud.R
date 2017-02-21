library(twitteR)
library(tm)
library(SnowballC)
library(wordcloud)
library(RCurl)
library(ROAuth)
library(stringr)
library(ggplot2)


tweets.df <- read.csv("D:/1 DATA BYTE/1_projects/demonetization proj/from shivani/demonetization-tweets.csv")

str(tweets.df)
tweets <- as.character(tweets.df$text)
sentiment.score <- function(sentences, positive.words, negative.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores <- laply(sentences, function(sentence, positive.words, negative.words)
  {
    
    ## clean up sentences with R's regex-driven global substitute, gsub():
    
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    
    # remove retweets
    sentence <- gsub('(RT|via)((?:\\b\\W*@\\W+)+)', '', sentence)
    
    # remove at people
    sentence <- gsub('@\\w+', '', sentence)
    
    # remove punctuations
    sentence <- gsub('[[:punct:]]', '', sentence)
    
    # remove numbers
    sentence <- gsub('[[:digit:]]', '', sentence)
    
    # remove html links
    sentence <- gsub('http[s]?\\w+', '', sentence)
    
    # remove extra spaces
    sentence <- gsub('[ \t]{2,}', '', sentence)
    sentence <- gsub('^\\s+|\\s+$', '', sentence)
    
    # removing NA's
    sentence <- sentence[!is.na(sentence)]
    
    # convert to lower case:
    sentence <- tolower(sentence)
    
    # split into words. str_split is in the stringr package
    
    word.list <- str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    
    words <- unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    negative.matches <- match(words, negative.words)
    positive.matches <- match(words, positive.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    
    positive.matches <- !is.na(positive.matches)
    negative.matches <- !is.na(negative.matches)
    
    poswordl <- c("hi","hi")
    negwordl <- c("hi","hi")
    
    poswordl <- c(poswordl,words[positive.matches])
    negwordl <- c(negwordl,words[negative.matches])
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    
    score <- sum(positive.matches) - sum(negative.matches)
    #mylist <- list("scoress" = score, "poswlist" = poswordl, "negwlist" = negwordl)
    #return(mylist)
    #setClass(Class = "mylist", representation(scoresss="integer",poswlist = "character",negwlist="character") )
    #return(new("mylist",scoresss = score,poswlist=poswordl ,negwlist=negwordl )) 
    return(list(pwl = poswordl, nwl = negwordl))
  }, positive.words, negative.words, .progress=.progress )
  
  #scores.df <- data.frame(score=scores@scoresss, text=sentences)
  return(scores)
}

positive <- scan("~/R/positive-words.txt", what= "character", comment.char= ";")
negative <- scan("~/R/negative-words.txt", what= "character", comment.char= ";")

tweets.analysis <- sentiment.score(tweets, positive, negative, .progress="none")


bag <- tweets.analysis$pwl

#bag <- Corpus(VectorSource(bag))
bag <- Corpus(VectorSource(bag))
bag <- tm_map(bag, tolower)

bag <- tm_map(bag, PlainTextDocument)

bag <- tm_map(bag, removePunctuation)

bag <- tm_map(bag, removeWords, c("demonetization","demonetisation","hi", stopwords("english")))

bag <- tm_map(bag, stripWhitespace)

bag <- tm_map(bag, stemDocument)


wordcloud(bag, min.freq = 50,
          max.words=1500, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), scale = c(3,0.5))
