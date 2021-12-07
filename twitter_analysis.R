#install.packages("twitteR")
library(twitteR)
install.packages("ROAuth")
library("ROAuth")
consumer_key <- "2MPjtXERAv7MJ6E4HXCwgzD5s"
consumer_secret_key <- "XBQ0MjIj4oWd9QyW6wGQuFtzCYw3WfFQzt0csWWIFTnPuLwciZ"
access_token <- "715743924393000960-24YtQvqWSx6lh8HFMsvG6qmLnlLgkgV"
access_secret_token <- "ABYbgvfUMTuUx6bNhRczxS7vXrMMBA0LvjW6tqn5iIdut"
setup_twitter_oauth(consumer_key,consumer_secret_key,access_token,access_secret_token)
namo_tweet <- userTimeline("narendramodi",n=200,includeRts = T)
View(namo_tweet)
length(namo_tweet)
namo_df <- twListToDF(namo_tweet)
View(namo_df)
namo_df[100,c("text","id","favoriteCount","retweetCount")]
install.packages("tm")
library(tm)
?Corpus
?removeWords
?stopwords
?VectorSource
docs <- c("This is a text.","This another one.")
(vs <- VectorSource(docs))
my_tweets <- Corpus(VectorSource(namo_df$text))
View(my_tweets)
?tm_map
my_tweets <- tm_map(my_tweets,content_transformer(tolower))
remove_url <- function(x)
{
  gsub("http[^[:space:]]*","",x)
}
my_tweets <- tm_map(my_tweets,content_transformer(remove_url))
View(my_tweets)
my_tweets <- tm_map(my_tweets,removePunctuation)
my_tweets <- tm_map(my_tweets,function(x)removeWords(x,stopwords()))
?TermDocumentMatrix

tweet_matrix <- TermDocumentMatrix(my_tweets , control = list(wordLengths = c(1,Inf)))
View(tweet_matrix)

id <- which(dimnames(tweet_matrix)$Terms %in% c("india" , "vote" ,"appeal"))                 
as.matrix(tweet_matrix[id,1:20])

namo_words <- rowSums((as.matrix(tweet_matrix)))
View(namo_words)

namo_freq_words <- subset(namo_words,namo_words>=10)
View(namo_freq_words)
namo_freq_df <- data.frame(words=names(namo_freq_words),count=namo_freq_words)
View(namo_freq_df)
install.packages("ggplot2")
library(ggplot2)
ggplot(namo_freq_df,aes(words,count,fill=words))+geom_bar(stat="identity")+coord_flip()
install.packages("wordcloud")
library(wordcloud)
wordcloud(namo_freq_df$words,namo_freq_df$count,colors = rainbow(28))

