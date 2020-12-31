#package used for search for tags
#install.packages("twitteR") #install package
library(twitteR) #load package
#package used to download user's tweets
#install.packages("rtweet")
library (rtweet)
#credentials to connect
consumer_key <- 'LTrUPNCzPseoPqJuiz1by8P1C' #apikey
consumer_secret <- 'axCPl3jKj7nZL2nv17YoyJWR3v4uFSQ8wX752kLFT4mdSmymFC' #apikeysecret
access_token <- '2751208720-ZhfcRSljFyJ5boUY8zRKRERL5Y5nL1FQzRSWeKF'
access_secret <- '3anl2d1AGpqleey1dlTOfQq16pOwSVzw0t0wFWjc4rrPc'
appname <- "politics_r_sgh"
#connecting to twitter API by twitteR and rtweet package respectively
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
twitter_token <- create_token(
  app = 'politics_r_sgh',
  consumer_key = 'LTrUPNCzPseoPqJuiz1by8P1C',
  consumer_secret = 'axCPl3jKj7nZL2nv17YoyJWR3v4uFSQ8wX752kLFT4mdSmymFC',
  access_token = '2751208720-ZhfcRSljFyJ5boUY8zRKRERL5Y5nL1FQzRSWeKF',
  access_secret = '3anl2d1AGpqleey1dlTOfQq16pOwSVzw0t0wFWjc4rrPc',
  set_renv = FALSE)
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#download data
#download all tweets containing given hastags
#virus <- searchTwitter('#China + #Coronavirus', n = 1000, since = '2020-01-01', retryOnRateLimit = 1e3)
#download each account  - commented out as shinyapps is not able to connect to twitter
trzaskowski <- get_timeline("@trzaskowski_", n= 2000, since_id = "1211970307403272192", max_id = "1285185569761439748")
duda <- get_timeline("@AndrzejDuda", n= 2000, includeRts = FALSE, since_id = "1211970307403272192", max_id = "1285185569761439748")
#manually getting the data previously saved
#duda <- load("duda.Rdata")
#duda <- readRDS("duda.rds")
#trzaskowski <- load("trzaskowski.Rdata")
#trzaskowski <- readRDS("trzaskowski.rds")
pol_politics <- rbind(trzaskowski, duda)

#converting the downloaded data into dataframes
trzaskowski_df <-as.data.frame(trzaskowski)
duda_df <-as.data.frame(duda)

#word-count chart data preparation
#Create a simple function to clean-up/ remove the punctuation, numbers, html-links and unecessary spaces:
textScrubber <- function(dataframe) {
  
  dataframe$text <-  gsub("—", " ", dataframe$text)
  dataframe$text <-  gsub("&", " ", dataframe$text)
  dataframe$text <-  gsub("[[:punct:]]", " ", dataframe$text)
  dataframe$text <-  gsub("http\\w+", " ", dataframe$text)
  dataframe$text <-  gsub("\n", " ", dataframe$text)
  dataframe$text <-  gsub("[ \t]{2,}", " ", dataframe$text)
  dataframe$text <-  gsub("^\\s+|\\s+$", " ", dataframe$text)
  dataframe$text <-  tolower(dataframe$text)
  
  return(dataframe)
}
trzaskowski_tweets <- textScrubber(trzaskowski)
duda_tweets <- textScrubber(duda)
#additional libraries to work with text
library(ggplot2)
library(httr)
library(rjson)
library(tm)
library(gridExtra)
library(lubridate)
#install.packages("stopwords")
#install.packages("stemdoc")
library(stopwords)
#simple function to remove stopwords, convert into Terms Matrix
tdmCreator <- function(dataframe, stemDoc = T, rmStopwords = T){
  tdm <- Corpus(VectorSource(dataframe$text))
  if (isTRUE(rmStopwords)) {
    tdm <- tm_map(tdm, removeWords, stopwords(language = "pl", source = "stopwords-iso"))
  }
  if (isTRUE(stemDoc)) {
    tdm <- tm_map(tdm, stemDocument)
  }
  tdm <- TermDocumentMatrix(tdm,
                            control = list(wordLengths = c(1, Inf)))
  tdm <- rowSums(as.matrix(tdm))
  tdm <- sort(tdm, decreasing = T)
  df <- data.frame(term = names(tdm), freq = tdm)
  return(df)
}
duda_tweets <- tdmCreator(duda_tweets)
trzaskowski_tweets <- tdmCreator(trzaskowski_tweets)
#cutting the output for 15 most used for visibility
trzaskowski_tweets <- trzaskowski_tweets[2:15,]
trzaskowski_tweets <- trzaskowski_tweets[-c(5), ]
duda_tweets <- duda_tweets[2:15,]


#publication hour graph
#duda_df$hour = duda_df$created_at
#trzaskowski_df$hour = trzaskowski_df$created_at
#install.packages("tidyr")
#duda_df <- separate(data = duda_df, col = created_at, into = c("date", "time"), sep = "\\s")
duda_df$created_at2 <- ymd_hms(duda_df$created_at)
duda_df$hours <- hour(duda_df$created_at2) + minute(duda_df$created_at2)/60
trzaskowski_df$created_at2 <- ymd_hms(trzaskowski_df$created_at)
trzaskowski_df$hours <- hour(trzaskowski_df$created_at2) + minute(trzaskowski_df$created_at2)/60
both_hourPlot <- rbind(trzaskowski_df, duda_df)
#define days of the week
Sys.setlocale("LC_ALL","English")
both_hourPlot$day <- weekdays(as.Date(both_hourPlot$created_at2))
both_hourPlot$day <- factor(both_hourPlot$day, levels = c("Monday",
                               "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday"))

#hashtagscloud
#install.packages("wordcloud") # word-cloud generator 
library("wordcloud")
duda_df$hashtags <- as.character(duda_df$hashtags)
duda_df$hashtags <- gsub("c\\(", "", duda_df$hashtags)
trzaskowski_df$hashtags <- as.character(trzaskowski_df$hashtags)
trzaskowski_df$hashtags <- gsub("c\\(", "", trzaskowski_df$hashtags)
set.seed(1234)

#retweet barplot
retweet_t <- as.data.frame(table(trzaskowski_df$retweet_screen_name))  
retweet_t <- retweet_t[order(-retweet_t$Freq),]
retweet_t_limited <- retweet_t[1:5,]
retweet_d <- as.data.frame(table(duda_df$retweet_screen_name))  
retweet_d <- retweet_d[order(-retweet_d$Freq),]
retweet_d_limited <- retweet_d[1:5,]

#save(duda, file = "duda.Rdata")
#save(trzaskowski, file = "trzaskowski.Rdata")
#saveRDS(duda, file = "duda.rds")
#saveRDS(trzaskowski, file = "trzaskowski.rds")

#save(duda_df, file = "duda_df.Rdata")
#save(trzaskowski_df, file = "trzaskowski_df.Rdata")
#save(duda_tweets, file = "duda_tweets.Rdata")
#save(trzaskowski_tweets, file = "trzaskowski_tweets.Rdata")



#file.edit(".Rprofile")
