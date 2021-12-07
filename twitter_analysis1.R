#25th march
#install.packages("twitteR")
library(twitteR)
#install.packages("ROAuth")
library("ROAuth")
consumer_key <- "2MPjtXERAv7MJ6E4HXCwgzD5s"
consumer_secret_key <- "XBQ0MjIj4oWd9QyW6wGQuFtzCYw3WfFQzt0csWWIFTnPuLwciZ"
access_token <- "715743924393000960-24YtQvqWSx6lh8HFMsvG6qmLnlLgkgV"
access_secret_token <- "ABYbgvfUMTuUx6bNhRczxS7vXrMMBA0LvjW6tqn5iIdut"
# install.packages("devtools")
setup_twitter_oauth(consumer_key,consumer_secret_key,access_token,access_secret_token)
trump_tweet <- userTimeline("realdonaldtrump",n=3200,includeRts = T)
View(trump_tweet)
length(trump_tweet)
trump_df <- twListToDF(trump_tweet) # convert  tho twitter list in data frame
View(trump_df)
trump_df[100,c("text","id","favoriteCount","retweetCount")]  # access perticular columns
# install.packages("tm")
library(tm) # tweet mining
?Corpus #  collections of documents containing (natural language) text
?removeWords
?stopwords
?VectorSource
docs <- c("This is a text.","This another one.")
(vs <- VectorSource(docs))
my_tweets <- Corpus(VectorSource(trump_df$text))
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

id <- which(dimnames(tweet_matrix)$Terms %in% c("america" , "wall" ,"great"))                 
as.matrix(tweet_matrix[id,1:20])

trump_words <- rowSums((as.matrix(tweet_matrix)))
View(trump_words)

trump_freq_words <- subset(trump_words,trump_words>=10)
View(trump_freq_words)
trump_freq_df <- data.frame(words=names(trump_freq_words),count=trump_freq_words)
View(trump_freq_df) 
# install.packages("ggplot2")
library(ggplot2)
ggplot(trump_freq_df,aes(words,count,fill=words))+geom_bar(stat="identity")+coord_flip()
# install.packages("wordcloud")
library(wordcloud)
wordcloud(trump_freq_df$words,trump_freq_df$count,colors = rainbow(28))

