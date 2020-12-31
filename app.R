# necessary R packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(shinyjs)
#options(scipen = 999) #option for disabling scientific notatation in axes - uncomment to disable scientific notation
#source("Data/data_download.R", local = TRUE)
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

  # Define UI
  ui <- fluidPage(theme = shinytheme("yeti"),useShinyjs(),
    navbarPage(
      theme = "cyborg",
      "Tweets analysis",
      tabPanel("Numeric data",
               sidebarPanel(
                 #tags$h3("Input:"),
                 #textInput("txt1", "Analyzed word:", "great"),
                 #textInput("txt2", "Who:", "trzaskowski$created_at"),
                 #uiOutput("polSelector"),
                 selectInput(inputId = "histogram", label = "Politcian to display on histogram:",
                             choices = c("Rafal Trzaskowski" = "trzaskowski_df$created_at", 
                                         "Andrzej Duda" = "duda_df$created_at")),
                 #selectInput("politcian2", "Politcian:",choices = names(pol_politics)),
                 #hr(),
                 helpText("Data from last 1000 Tweets before the election date in Poland in 2020."),
                 sliderInput(inputId = "bins",
                             label = "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30),
                 
                 selectInput(inputId = "timePlot", label = "Choose politician for density plot:",
                             choices = c("Rafal Trzaskowski" = "trzaskowski_df", "Andrzej Duda" = "duda_df"),
                             selected = "duda_df"),
                 
               ), # sidebarPanel
               mainPanel(
                            plotOutput("frequencyPlot2"),
                            plotOutput("timePlot2"),
                            plotOutput("timePlot"),
                            plotOutput("timePlot3"),
                            #plotOutput("frequencyPlot"),
                            #plotOutput("barplot"),
               ) # mainPanel
               
      ), # Navbar 1, tabPanel
      
      tabPanel("Content and hashtags", 
               sidebarPanel(
                 radioButtons("politician3", "Hashtags of:",
                              c("Andrzej Duda" = "duda_df$hashtags",
                                "Rafal Trzaskowski" = "trzaskowski_df$hashtags")),
               ), # sidebarPanel
               mainPanel(
                 plotOutput("wordCount1Plot"),
                 plotOutput("wordCount2Plot"),
                 plotOutput("hashtagPlot"),
                 h3("Options for hashtag bar plot"),
                 div(style="display: inline-block;vertical-align:top; width: 150px;",radioButtons("limit", "Limit the tags used more than once",
                                                                                                  c("Yes" = "limited","No" = "not_limited"
                                                                                                    ))),
                 div(style="display: inline-block;vertical-align:top; width: 150px;",radioButtons("order", "Order descending",
                                                                                                  c("Yes" = "order",
                                                                                                    "No" = "no_order"), selected = "no_order")),
                 div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
                 
                 plotOutput("hashtagPlot2"),
                 
                 
               ) # mainPanel
               
      ),
      
      tabPanel("About", 
               mainPanel(
                 h2("About page"),
                 h5("The page is created for Querrying, Visualization and Data Presentation Class in R"),
                 h5("Marek Wrucha, Fall 2020"),
                 h5("mw75664@sgh.waw.pl"),
                 h3("References"),
                 h5("here links will be pasted"),
                 
               ) #mainPanel
              ) #tabPanel 
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    
    dataInput <- reactive({
      switch(input$histogram,
             "trzaskowski_df$created_at" = trzaskowski_df$created_at,
             "duda_df$created_at" = duda_df$created_at)
    })
    dataInput2 <- reactive({
      switch(input$timePlot,
             "trzaskowski_df" = trzaskowski_df,
             "duda_df" = duda_df)
    })
    dataInput3 <- reactive({
      switch(input$politician3,
             "Andrzej Duda" = duda_df$hashtags,
             "Rafal Trzaskowski" = trzaskowski_df$hashtags)
    })
    dataInput4 <- reactive({
      switch(input$limit,
             "No" = not_limited,
             "Yes" = limited)
    })
    dataInput5 <- reactive({
      switch(input$order,
             "Yes" = order,
             "No" = no_order)
    })
    output$frequencyPlot <- renderPlot({
      #spróbuj zrobic jeden df z oboma politykami i tylko wskaz wartosc w kolumnie
      x <- as.numeric(duda_df$created_at)
      #x <- as.numeric(pol_politics$user_id)
      #x    <- na.omit(x)
      #if (input$politician == "duda$created_at"){
      #  x <- duda$created_at
      #}
       # else if (input$politician == "duda$status_id"){
        #  x <- duda$status_id
        #}
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins)
      #ggplot(duda_df, aes(x=created_at)) + geom_histogram()
      
    })
    output$frequencyPlot2 <- renderPlot({
      if (input$histogram == "duda_df$created_at"){
        x <- duda_df$created_at
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins, main = "Frequency of Tweets among the time")
        #ggplot(duda_df, aes(x=created_at)) + geom_histogram()
      } else if (input$histogram == "trzaskowski_df$created_at"){
        x <- trzaskowski_df$created_at
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins, main = "Frequency of Tweets among the time", col = ifelse( is.null(trzaskowski_df$retweet_name), "green", "red"))
        #ggplot(trzaskowski_df, aes(x=created_at)) + geom_histogram(fill = ifelse( is.null(trzaskowski_df$retweet_name), "green", "red"))
        
      }
    })
    output$barplot <- renderPlot({
      qplot(input$txt2, geom="histogram", stat = "count") 
    })
    output$wordCount1Plot <- renderPlot({
      ggplot(trzaskowski_tweets, aes(x = reorder(term, freq), y = freq)) +
        geom_bar(stat = "identity", fill = ifelse( !is.null(duda_df$hashtags), "green", "red")) +
        xlab("Most Used") + ylab("How Often") +
        coord_flip() + theme(text=element_text(size=25,face="bold"))
    })
    output$wordCount2Plot <- renderPlot({
      ggplot(duda_tweets, aes(x = reorder(term, freq), y = freq)) +
        geom_bar(stat = "identity", fill = "red") +
        xlab("Most Used") + ylab("How Often") +
        coord_flip() + theme(text=element_text(size=25,face="bold"))
      })
    output$timePlot <- renderPlot({
      ggplot(data=duda_df, aes(hours)) + #data=dataInput2() for reactive plot
        geom_density(colour = "blue") +
        geom_density(data=trzaskowski_df, colour="orange")
    })
    output$timePlot2 <- renderPlot({
      if (input$timePlot == "duda_df"){
        ggplot(data=duda_df, aes(hours)) +
          geom_density(colour = "blue")
      } else if (input$timePlot == "trzaskowski_df"){
        ggplot(data=trzaskowski_df, aes(hours)) +
          geom_density(colour = "orange")
        }
      }) 
    output$timePlot3 <- renderPlot({
      ggplot(both_hourPlot, aes(hours)) + scale_fill_manual(labels = c("Andrzej Duda", "Rafal Trzaskowski"), values = c("blue","orange"))+
        geom_density(aes(fill = user_id), alpha = 0.4) + facet_wrap(~day, nrow = 3) + labs(title = "Tweeting hours among days\n") 
        # + scale_color_manual(labels = c("Andrzej Duda", "Rafal Trzaskowski"), values = c("blue", "orange")
        
    }) 
    output$hashtagPlot <- renderPlot({
      set.seed(1234)
      if (input$politician3 == "duda_df$hashtags"){
        wordcloud(duda_df$hashtags, min.freq=3, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))
      } else if (input$politician3 == "trzaskowski_df$hashtags"){
        wordcloud(trzaskowski_df$hashtags, min.freq=5, scale=c(2.7, .5), random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))
      }
    })
    observe({
      if (input$limit == "limited") { 
        shinyjs::hide("order")
      } else {
        shinyjs::show("order")
      }
    })
    output$hashtagPlot2 <- renderPlot({
      if (input$limit == "not_limited"&input$order == "no_order"){
        p1 <- ggplot(retweet_t, aes(y = Var1, x = Freq, fill = Freq)) +
          geom_bar(stat = "identity") + coord_cartesian(xlim=c(1,20))
        p2 <- ggplot(retweet_d, aes(y = Var1, x = Freq, fill = Freq)) +
          geom_bar(stat = "identity") + coord_cartesian(xlim=c(1,20))
        grid.arrange(p1,p2, nrow=1)
      } else if (input$limit == "limited"){
        p1 <- ggplot(retweet_t_limited, aes(y = reorder(Var1,Freq), x = Freq, fill = Freq)) +
          geom_bar(stat = "identity") + coord_cartesian(xlim=c(1,20))
        p2 <- ggplot(retweet_d_limited, aes(y = reorder(Var1,Freq), x = Freq, fill = Freq)) +
          geom_bar(stat = "identity") + coord_cartesian(xlim=c(1,20))
        grid.arrange(p1,p2, nrow=1)
      } else if (input$limit == "not_limited"&input$order == "order"){
        p1 <- ggplot(retweet_t, aes(y = reorder(Var1,Freq), x = Freq, fill = Freq)) +
          geom_bar(stat = "identity") + coord_cartesian(xlim=c(1,20))
        p2 <- ggplot(retweet_d, aes(y = reorder(Var1,Freq), x = Freq, fill = Freq)) +
          geom_bar(stat = "identity") + coord_cartesian(xlim=c(1,20))
        grid.arrange(p1,p2, nrow=1)
      }
    })
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
