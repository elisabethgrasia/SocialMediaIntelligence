# Social Media Intelligence
# week 8 - lab 7
# The Structure of the Web

install.packages('rtweet')
install.packages('tm')

library('rtweet')
library('tm')

install.packages("base64enc")
install.packages("httpuv")

library("base64enc")
library("httpuv")

key = "VBvnBS50B3WsGhzeQgHXBpwni"
secret = "FVRzcDoppeuQa1AAOOWkXeb6YMqspWDkBUwBLuCHp2JWsodAB3"

create_token(app = "SentiTweets_",
             consumer_key = key,
             consumer_secret = secret)

options(RCurlOptions = list(verbose = FALSE, 
                            capath = system.file("CurlSSL", "cacaer.pem")))

tweets = search_tweets(
  "kevin_bacon", n = 100, include_rts = FALSE)

names(tweets)
tweets$text

# examining word frequencies
strsplit(tweets$text[1], "[^A-Za-z]+")
tweet.words = strsplit(tweets$text, "[A-Za-z]+") # for all tweets, creating a list

word.table = table(unlist(tweet.words))


# To identify the top 20 occurring words, we must sort the table and 
# examine the top 20 items.


# Examine the help page for sort and work out how to obtain the top 20 
# occurring words from the table.

# Do these words tell us anything about Kevin Bacon. It is likely that they don't. 
# The list is likely to contain words such as is, of, a and so on. 
# We need to use a more sophisticated method to extract meaningful terms.


# TEXT MINING
# tm doesn't work with regular text, we should convert it to corpus
tweet.corpus = Corpus(VectorSource(tweets$text))

# converting the characters to ASCII
tweet.corpus = tm_map(tweet.corpus, function(x) iconv(x, to='ASCII'))

# PREPROCESSING THE DOCUMENT SET
## remove numbers
rwnb = removeNumbers(tweet.corpus[[24]])
rwnb$content
rwnb$meta

## remove punctuation
rwpunc = removePunctuation(tweet.corpus[[1]])
rwpunc$content

## remove whitespace
rwsp = stripWhitespace(tweet.corpus[[1]])

## case fold all characters to lower case
tmp = tolower("Hello Wordls")
tmp

## remove a set of stop words
rmwstopwords = removeWords(x = tweet.corpus[[1]], word = 'The')
rmwstopwords$content

#install.packages("SnowballC")

## reduce each word to its stem
reducetostem = stemDocument(x = tweet.corpus[[1]])
reducetostem$content

# EXPLORING RTWEET LIBRARY
# friendship between accounts
friendship = lookup_friendships(source = 'lafpark', target = 'oliverobst')
tmp = lookup_tweets('151585685398458367')
tw = lookup_users('lapark')
tw = get_trends()
tw


# TERM DOCUMENT MATRIX
tweet.corpus = tm_map(tweet.corpus, PlainTextDocument)
