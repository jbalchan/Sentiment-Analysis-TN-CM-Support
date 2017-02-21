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
#text  = readLines(tweets)
text <- tweets

Doc.id=seq(1:length(text)) 
calib=data.frame(Doc.id,text)

stpw1 = stopwords('english') 
stopwords = unique(c(gsub("'","",stpw1),stpw1))

test = text.clean(text)   
test  =  removeWords(test,stopwords) 

clean_text = test
x1 = Corpus(VectorSource(test))
x1 = n.gram(x1,"tri",5) 
dtm1 = custom.dtm(x1,"tf") 
dtm2 = custom.dtm(x1,"tfidf")  
freq1 = (sort(apply(dtm1,2,sum), decreasing =T))
freq1[1:80]
windows()
wordcloud(names(freq1), freq1, scale=c(4,0.5),1, max.words=200,colors=brewer.pal(8, "Dark2"))
title(sub = "Term Frequency - Wordcloud")
freq2 = (sort(apply(dtm2,2,sum), decreasing =T))
freq2[1:80]
windows()
wordcloud(names(freq2), freq2, scale=c(4,0.5),1, max.words=200,colors=brewer.pal(8, "Dark2"))
title(sub = "Term Frequency Inverse Document Frequency - Wordcloud")
clean_text0 = clean_text[clean_text != ""] 
pol = polarity(clean_text0)
wc = pol$all[,2]                  # Word Count in each doc
val = pol$all[,3]                 # average polarity score
p  = pol$all[,4]                  # Positive words info
n  = pol$all[,5]  
positive_words = unique(setdiff(unlist(p),"-"))  # Positive words list
negative_words = unique(setdiff(unlist(n),"-"))  # Negative words list

print(positive_words)       # Print results
tdm_temp = t(TermDocumentMatrix(Corpus(VectorSource(clean_text0))))
pos.tdm = tdm_temp[,setdiff(match(positive_words,colnames(tdm_temp)),NA)]
m = as.matrix(pos.tdm)
v = sort(colSums(m), decreasing = TRUE)
windows()
wordcloud(names(v), v, scale=c(4,0.5),1, max.words=100,colors=brewer.pal(8, "Dark2"))
title(sub = "Positive Words - Wordcloud")
neg.tdm = tdm_temp[,setdiff(match(negative_words,colnames(tdm_temp)),NA)]
m = as.matrix(neg.tdm)
v = sort(colSums(m), decreasing = TRUE)
windows()
wordcloud(names(v), v, scale=c(4,0.5),1, max.words=100,colors=brewer.pal(8, "Dark2"))         
title(sub = "Negative Words - Wordcloud")

len = function(x){
  if ( x == "-" && length(x) == 1)  {return (0)} 
  else {return(length(unlist(x)))}
}


pcount = unlist(lapply(p, len))
ncount = unlist(lapply(n, len))
doc_id = seq(1:length(wc))

windows()
plot(doc_id,pcount,type="l",col="green",xlab = "Document ID", ylab= "Word Count")
lines(doc_id,ncount,type= "l", col="red")
title(main = "Positive words vs Negative Words" )
legend("topright", inset=.05, c("Positive Words","Negative Words"), fill=c("green","red"), horiz=TRUE)

