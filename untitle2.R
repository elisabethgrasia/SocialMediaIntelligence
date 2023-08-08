# Additional tweet text preprocessing

################################################# optional ###############################
strsplit(tweets[1], "[^A-Za-z]+")
tweet.words = strsplit(tweets, "[A-Za-z]+") # for all tweets, creating a list
?strsplit
word.table = table(unlist(tweet.words))
head(word.table)

tweettext <- Corpus(VectorSource(tweets))
cleaningtweet <- tm_map(tweettext, removePunctuation)
cleaningtweet <- tm_map(cleaningtweet, stripWhitespace)

cleaningtweet <- tm_map(cleaningtweet, removeWords, stopwords('english'))

# analyzing the frequencies of Words in Text
library(dplyr)
library(tidytext)
library(ggplot2)

head(cleaningtweet)

tweetdf <- data_frame(Text = cleaningtweet)

  
  
  # Data Lookup
  
library('twitteR')
library(dplyr)
library(igraph)  
write.csv(em, "elonmusktwitter.csv")
setup_twitter_oauth("VBvnBS50B3WsGhzeQgHXBpwni", 
                    "FVRzcDoppeuQa1AAOOWkXeb6YMqspWDkBUwBLuCHp2JWsodAB3",
                    access_token = "",
                    access_secret = "")

elonmusktweet <- twListToDF(searchTwitter("Elon buys Twitter", n = 5000, lang=NULL, since = NULL, until = NULL, locale = NULL, geocode = NULL, sinceID = NULL, maxID = NULL, resultType = NULL, retryOnRateLimit = 120))  
