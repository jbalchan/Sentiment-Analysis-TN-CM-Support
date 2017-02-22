library(twitteR)
library(ROAuth)
library(httr)
library(plyr)
library(stringr)
library(ggplot2)
library(plotly)

# the following 4 are supplied by twitter
customer_key <- "udwLTM7gchrQYMQcW15wzj3wj"
customer_secret <- "cK5xlJhQ3rf2Vaix1uCOpg8LJcozmMIQM8g16JNvO2AtDqB0Ed"
access_token <- "466566632-1y63l7WVwsOTOcevkAFexWqqJ6lXHgpHz8GKbHTB"
access_token_secret <- "spMYZGwCk69iCEuO7LSLE5l4qSNFgFqThUuiU5thZScdk"
# This function wraps the OAuth authentication handshake functions from the httr package for a twitteR session
setup_twitter_oauth(customer_key, customer_secret, access_token, access_token_secret)

#tweets are searched from 5th feb 2017 - the day ops resigned from CM post.
tweets_ops <- searchTwitter('ops OR OPaneerSelvam', n=500,since="2017-02-05") #O Paneer Selvam is also called ops
tweets_sasikala <- searchTwitter('sasikala', n=500,since="2017-02-05")
tweets_palaniswami <- searchTwitter('Palaniswami OR Palaniswamy', n=500,since="2017-02-05") #searches for both spellings

#splitting into list, getting text and returning array
feed_ops <- laply(tweets_ops, function(t) t$getText())
feed_sasikala <- laply(tweets_sasikala, function(t) t$getText())
feed_Palaniswami <- laply(tweets_palaniswami, function(t) t$getText())

# reads data from files as strings
good <- scan('~/R/positive-words.txt', what='character', comment.char=';')
bad <- scan('~/R/negative-words.txt',what='character', comment.char=';')

#combines few more words as bad or good
bad_text <- c(bad, 'wtf', 'wth')
good_text <- c(good, 'voted')

score.sentiment <- function(sentences, good_text, bad_text, .progress='none')
{
  require(plyr)
  require(stringr)
  
  #converting into array of scores
  scores = laply(sentences, function(sentence, good_text, bad_text) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    #to remove emojis
    sentence <- iconv(sentence, 'UTF-8', 'ASCII')
    sentence = tolower(sentence)
    
    # split the sentence into words. 
    word.list = str_split(sentence, '\\s+')
    # Unlist a list of vectors into a single vector
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, good_text)
    neg.matches = match(words, bad_text)
    
    # match() returns the position of the matched term or NA
    # selecting the TRUE/FALSE values only :
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, good_text, bad_text, .progress=.progress )
  
  # data frame containing score for every sentence
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# Retreive scores and add person's name.
ops <- score.sentiment(feed_ops, good_text, bad_text, .progress='text')
ops$name <- 'o paneer selvam'
sasikala <- score.sentiment(feed_sasikala, good_text, bad_text, .progress='text')
sasikala$name <- 'sasikala'
Palaniswami <- score.sentiment(feed_Palaniswami, good_text, bad_text, .progress='text')
Palaniswami$name <- 'Edappadi k Palaniswami'

# Merge (by rows) into one dataframe for plotting
plotdataa <- rbind(ops, sasikala,Palaniswami)

# Cutting text and score
plotdataa <- plotdataa[c("name", "score")]

# Remove neutral values of 0
plotdataa <- plotdataa[!plotdataa$score == 0, ]

# Remove anything less than -3 or greater than 3
plotdataa <- plotdataa[!plotdataa$score > 3, ]
plotdataa <- plotdataa[!plotdataa$score < (-3), ]

# little quick plot
qplot(factor(score), data=plotdataa, geom="bar", 
      fill=factor(name),
      xlab = "Sentiment Score")

# using ggplot2 and Plotly
properplot <- plotdataa %>%   #dplyr provides the %>% operator. x %>% f(y) turns into f(x, y) so you can use it to rewrite multiple operations that you can read left-to-right, top-to-bottom
  ggplot(aes(x = score, fill = name)) +  #aes creates a list of unevaluated expressions
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values = c("#0067F7", "#F70000","#66CC99")) +
  theme_classic(base_size = 12) +
  scale_x_continuous(name = "Score") +
  scale_y_continuous(name = "count of tweets") +
  ggtitle("Sentiment Analysis")
theme(axis.title.y = element_text(face="bold", colour="#000000", size=10),
      axis.title.x = element_text(face="bold", colour="#000000", size=8),
      axis.text.x = element_text(angle=16, vjust=0, size=8))
ggplotly(properplot)

#creating a word cloud for palaniswami
# taking text data  
bag <- feed_Palaniswami
#converting text into corpus 
bag <- Corpus(VectorSource(bag))
# converting all characters into lowercase
bag <- tm_map(bag, tolower)

bag <- tm_map(bag, PlainTextDocument)
# removing all punctuations
bag <- tm_map(bag, removePunctuation)
# removing stopwords
bag <- tm_map(bag, removeWords, c("jayalalitha", stopwords("english")))
# removing whitespaces
bag <- tm_map(bag, stripWhitespace)

bag <- tm_map(bag, stemDocument)
# using wordcloud function
wordcloud(bag, min.freq = 10,max.words=1500, rot.per=0.15, colors=brewer.pal(7, "Dark2"), scale = c(4.8,0.4))

  
