library(twitteR)
library(ROAuth)
consumer_key <- "yi0sHT8y3CSTH9V3JtL1kIZ5L"
consumer_secret_key <- "TespO0QyA4GqAixy0DSdA6LZmok4BIjG1unk6o2LBEzuDHETTf"
access_token <- "1546205527-QJ1TS58dQCZWIZe9sw0Vluscc8L3a98o6KyaZ6p"
access_secret_token <- "e5TXQ9sGTz5z8haSxaGKxJJ93fEvWXSBR60UsTFQXaA0X"

#To establish a handshake connection with twitter API by authorizing consumer keys and access tokens
setup_twitter_oauth(consumer_key, consumer_secret_key, access_token, access_secret_token)

# 3200 is the maximum to retrieve, without retweets
donald_tweets <- userTimeline("RealDonaldTrump",n=3200)
length(donald_tweets)
donald_df <-twListToDF(donald_tweets)
head(donald_df)

#To fetch 50th tweet of Donald Trump
donald_df[50, c("id", "created", "screenName", "favoriteCount", "retweetCount","text")]

library(tm)
#Converts into corpus for tm with a vector source of trump's tweets
tweet_corpus <- Corpus(VectorSource(donald_df$text))
View(tweet_corpus)
#Transform tweets into lower alphabetical order
tweet_corpus <- tm_map(tweet_corpus, content_transformer(tolower))
#Function to remove urls
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
tweet_corpus <- tm_map(tweet_corpus, content_transformer(removeURL))
#Function to remove punctuations
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
tweet_corpus <- tm_map(tweet_corpus, content_transformer(removeNumPunct)) 
#We have to remove this stopwords of english language
stopwords()
tweet_corpus <- tm_map(tweet_corpus,function(x)removeWords(x,stopwords()))
#Plotting wordcloud
library(wordcloud)
wordcloud(tweet_corpus)
#To create a sparse matrix of words used by trump
tweet_matrix <- TermDocumentMatrix(tweet_corpus,control = list(wordLengths=c(1,Inf)))
id = which(dimnames(tweet_matrix)$Terms %in% c("america","wall","great"))
#To find the occurence of tweet words in first 20 tweets
as.matrix(tweet_matrix[id,1:20])
donald_words <- rowSums(as.matrix(tweet_matrix))
#To find Trump's frequently used words
donald_freq_words <- subset(donald_words,donald_words>=3)
donald_freq_words
#Converting into data frame for plotting
donald_freq_df <- data.frame(Words = names(donald_freq_words),Count = donald_freq_words)
head(donald_freq_df)

library(ggplot2)
ggplot(donald_freq_df,aes(Words,Count,fill=Words))+geom_bar(stat="identity")+coord_flip()
wordcloud(donald_freq_df$Words,donald_freq_df$Count,colors = rainbow(60))

#finding association of words
findAssocs(tweet_matrix,"president",0.4) #  where 0.4 shows min freq
findAssocs(tweet_matrix,"america",0.2)
findAssocs(tweet_matrix,"fake",0.5)
findAssocs(tweet_matrix,"wall",0.25)

# source("https://bioconductor.org/biocLite.R")
# biocLite("graph")
# biocLite("Rgraphviz")
library(graph)
library(Rgraphviz)
plot(tweet_matrix,names(donald_freq_words),corThreshold=0.1,weighting=T)
donald_freq_words <- subset(donald_words,donald_words>=6)
View(donald_freq_words)
plot(tweet_matrix,names(donald_freq_words),corThreshold=0.1,weighting=T)
plot(tweet_matrix,names(donald_freq_words),corThreshold=0.1,weighting=F)

# sentimental_analysis
install.packages("devtools")
library(devtools)
install_github("okugami79/sentiment140")
library(sentiment)
donald_sentiments <- sentiment(donald_df$text)
head(donald_sentiments)
donald_polarity <- table(donald_sentiments$polarity)
donald_polarity
library(MASS)
pie(donald_polarity)

#for IDate
install.packages("data.table")
library(data.table)
library(ggplot2)
donald_sentiments$score <- 0
donald_sentiments$score[donald_sentiments$polarity == "positive"] <- 1
donald_sentiments$score[donald_sentiments$polarity == "negative"] <- -1
donald_sentiments$date <- as.IDate(donald_df$created)
head(donald_sentiments)
result <- aggregate(score ~ date, data = donald_sentiments, sum)
result
ggplot(result,aes(score,date))+geom_line()



#Using nrc for sentimental analysis
install.packages("tidytext")
library(tidytext)
install.packages("syuzhet")
library(syuzhet)
my_example_text <- "I begin this story with a neutral statement.Basically this is a very silly test.You are testing the Syuzhet package using short, inane sentences. I am actually very happy today.I have finally finished writing this package.Tomorrow I will be very sad. I won't have anything left to do.I might get angry and decide to do something horrible. I might destroy the entire package and start from scratch.Then again, I might find it satisfying to have completed my first R package.Honestly this use of the Fourier transformation is really quite elegant. You might even say it's beautiful!"
sentiment_vector <- get_sentences(my_example_text)
get_sentiment(sentiment_vector, method="syuzhet")
get_sentiment(sentiment_vector, method="afinn")
get_sentiment(sentiment_vector, method="bing")
get_sentiment(sentiment_vector, method="nrc")

