library("twitteR")
library("wordcloud")
library("tm")

#necessary file for Windows
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'rTv9spgUO7mY6D45hmvfhld0a'
consumer_secret <- 'fwxw6B93PubgQMpsj0I2YbJWZVYMzjloXxb5Wt17FAMm7LvjE9'
access_token <- '970578129650462720-RKFtiPGKwaQKEpFiwQHwIGwlzZiaMqH'
access_secret <- 'hN5BkTw2T57BobRPrX4yQZuKyxrl8I0xEgAf1naNzxWfX'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

#the cainfo parameter is necessary only on Windows
oscar.tweets = searchTwitter("Oscars_2018live", n=3500)  

#converts to data frame
df <- do.call("rbind", lapply(oscar.tweets, as.data.frame))

#remove odd characters
df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub="")) #remove emoticon
df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text) #remove URL
sample <- df$text

score.sentiment = function(tweets, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  neut = 0
  list=lapply(tweets, function(tweet, pos.words, neg.words)
  {
    tweet = gsub('[[:punct:]]',' ',tweet)
    tweet = gsub('[[:cntrl:]]','',tweet)
    tweet = gsub('\\d+','',tweet)  #removes decimal number
    tweet = gsub('\n','',tweet)    #removes new lines
    tweet = tolower(tweet)
    word.list = str_split(tweet, '\\s+')
    words = unlist(word.list)  #changes a list to character vector
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp = sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    if (score==0) neut=neut+1 
      
    list1 = c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  score_new = lapply(list, `[[`, 1)
  pp1 = lapply(list, '[[', 2)
  nn1 = lapply(list, '[[', 3)
  neut1 = lapply(list, '[[', 3)
  scores.df = data.frame(score = score_new, text=tweets)
  positive.df = data.frame(Positive = pp1, text=tweets)
  negative.df = data.frame(Negative = nn1, text=tweets)
  neutral.df = data.frame(Neutral = neut1, text=tweets)
  
  list_df = list(scores.df, positive.df, negative.df, neutral.df)
  return(list_df)
}

pos.words = scan('C:/Users/purba/Desktop/Sentiment/positive-words.txt', what='character', comment.char=';') #Make sure you edit the location
neg.words = scan('C:/Users/purba/Desktop/Sentiment/negative-words.txt', what='character', comment.char=';')


#Adding words to positive and negative databases

pos.words=c(pos.words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader')
neg.words = c(neg.words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not')

# Clean the tweets and returns merged data frame
result = score.sentiment(sample, pos.words, neg.words)

library(reshape)
score=result[[1]]
positive=result[[2]]
negative=result[[3]]
neutral=result[[4]]  
#Creating three different data frames for Score, Positive and Negative
#Removing text column from data frame
score$text=NULL
positive$text=NULL
negative$text=NULL
neutral$text=NULL

#Storing the first row(Containing the sentiment scores) in variable q
sc=score[1,]
pt=positive[1,]
ng=negative[1,]
nu=neutral[1,]

sent1=melt(sc, ,var='Score')
sent2=melt(pt, ,var='Positive')
sent3=melt(ng, ,var='Negative') 
sent4=melt(nu, ,var='Neutral') 

sent1['Score'] = NULL
sent2['Positive'] = NULL
sent3['Negative'] = NULL
sent4['Neutral'] = NULL

#Creating data frame
table1 = data.frame(Text=result[[1]]$text, Score=sent1)
table2 = data.frame(Text=result[[2]]$text, Score=sent2)
table3 = data.frame(Text=result[[3]]$text, Score=sent3)
table4 = data.frame(Text=result[[4]]$text, Score=sent3)

#Merging three data frames into one
sentiment_table=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value, Neutral=table4$value )

hist(sentiment_table$Positive, col=blues9, xlab = "Positive Sentiments", main = "Histogram of Positive Sentiments")
hist(sentiment_table$Negative, col=blues9, xlab = "Negative Sentiments", main = "Histogram of Negative Sentiments")
hist(sentiment_table$Neutral, col=blues9, xlab = "Neutral Sentiments", main = "Histogram of Neutral Sentiments")
hist(sentiment_table$Score, col=blues9 ,xlab = "Scoring Sentiments", main = "Histogram of Sentiment Score")


library(corrgram)
corrgram(sentiment_table, main="Corrgram of Sentiment Variables", lower.panel=panel.shade,
         upper.panel=panel.pie,
         text.panel=panel.txt)

slices <- c(sum(sentiment_table$Positive), sum(sentiment_table$Negative), sum(sentiment_table$Neutral))
labels <- c("Positive", "Negative", "Neutral")
library(plotrix)
pie3D(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Oscar Twwets Sentiment Analysis")
