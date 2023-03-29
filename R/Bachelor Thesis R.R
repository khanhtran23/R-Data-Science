library(car)
library(coinmarketcapr)
library(data.table)
library(dplyr)
library(httpuv)
library(geoR)
library(ggplot2)
library(MASS,stats)
library(mvtnorm)
library(nlme)
library(PerformanceAnalytics)
library(purrr)
library(qqplotr)
library(quantmod)
library(readr)
library(rtweet)
library(tidyr)
library(tidytext)
library(textdata)
library(scales)
library(vader)
#------------------------------------------------------------------------------------------------------------------------------------------------#
#WE USE BTC, ETH, LTC, AS THESE HAVE HOURLY DATA
#S1: DAILY CRYPTOS DATA -----------------------------------------------------
#Downloaded from Yahoo! Finance, from 2016-05-31 to 2021-05-31

data.BTC<-read.csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Daily Data/BTC-USD.csv",header = TRUE)
data.ETH<-read.csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Daily Data/ETH-USD.csv",header = TRUE)
data.XRP<-read.csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Daily Data/XRP-USD.csv",header = TRUE)
data.DOGE<-read.csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Daily Data/DOGE-USD.csv",header = TRUE)
#------------------------------------------------------------------------------------------------------------------------------------------------#

#S2: HOURLY CRYPTOS DATA -----------------------------------------------------
#Downloaded and enriched from https://www.cryptodatadownload.com/data/gemini/
options(scipen = 100)
BTCUSD_1hr<-read_csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Hourly Data/BTCUSD_1hr_woline1.csv")
ETHUSD_1hr<-read_csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Hourly Data/ETHUSD_1hr_woline1.csv")
XRPUSD_1hr<-read_csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Hourly Data/XRPUSD_1hr_woline1.csv")
DOGEUSD_1hr<-read_csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Hourly Data/DOGEUSD_1hr_woline1.csv")

#------------------------------------------------------------------------------------------------------------------------------------------------#
#S3: OBTAINING AND USING ACCESS TOKENS ON TWITTER ----------------------------
  #Step 1: Load package "httpuv"

install.packages("httpuv")

    #Store api keys
library(rtweet)
twt_api_key <- "7LmXzxbfH9iS7gZpj3z3p6Iyr"
twt_api_secret_key <- "dEC6PPridfXIpzXH78zMyRHdXVGDn8tDPBQLM6po28tMhQrfd0"

  #Step 2: Authenticate via web browser
token <- create_token(
  app = "khanhtranaugsburg",
  consumer_key = twt_api_key,
  consumer_secret = twt_api_secret_key)

    ## view token (you should see the correct app name)
token
#------------------------------------------------------------------------------------------------------------------------------------------------#
#S4: OBTAINING AND USING API ON COINMARKETCAP --------------------------------
library(coinmarketcapr)

cmc_api_key<-"2e8fe6ea-7f3b-4f01-b450-84eb50e31167"
coinmarketcapr::setup(cmc_api_key)

#------------------------------------------------------------------------------------------------------------------------------------------------#

#S5: DEMO: GET COUNTRY DATA FROM TWITTER AND DO A SIMPLE SENTIMENT SIMPLE SENTIMENT ANALYSIS --------

  #Step 1: Search Tweets on the chosen topic and SAVE IT -------------------------------
country1<-search_tweets(
  "#Canada", n = 100, include_rts = FALSE) #Search 100 Tweets with hashtag #Canada, with no retweet (rts)
write_as_csv(country1, "country1.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(country1, "country1.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

country2<-search_tweets(
  "#Scotland", n = 100, include_rts = FALSE) #Search 100 Tweets with hashtag #Scotland, with no retweet (rts)
write_as_csv(country2, "country2.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(country2, "country2.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

country1<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/country1.csv",header = TRUE)
country2<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/country2.csv",header = TRUE)


  #Step 2: Process each set of tweets into tidy text or corpus object --------
tweets.Country1<-country1 %>% dplyr::select(screen_name,text) #Create a table with "screen_name" and "text" columns from country1

tweets.Country2<-country2 %>% dplyr::select(screen_name,text)

  #Step 3: Use pre-processing text transformations to clean up the tweets  --------
#For country 1
head(tweets.Country1$text)

  #Remove hyperlink (http) elements
tweets.Country1$stripped_text1<-gsub("http\\S+","",tweets.Country1$text) 

  #Convert tweets to lowercase using "unnest_tokens" command + remove punctuation + add id for each tweet
tweets.Country1_stem<-tweets.Country1 %>%
  dplyr::select(stripped_text1) %>%
  unnest_tokens(word,stripped_text1)
head(tweets.Country1_stem)

  #Remove stop words from your list of words
cleaned_tweets.Country1<-tweets.Country1_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.Country1)

head(tweets.Country1$text)

#For country2
head(tweets.Country2$text)

  #Remove hyperlink (http) elements
tweets.Country2$stripped_text2<-gsub("http\\S+","",tweets.Country2$text) 

  #Convert tweets to lowercase using "unnest_tokens" command + remove punctuation + add id for each tweet
tweets.Country2_stem<-tweets.Country2 %>%
  dplyr::select(stripped_text2) %>%
  unnest_tokens(word,stripped_text2)
head(tweets.Country2_stem)

  #Remove stop words from your list of words
cleaned_tweets.Country2<-tweets.Country2_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.Country2)

head(tweets.Country2$text)

  #Step 4: Find the top 10 commonly used words in the set of tweet --------

#Top 10 words in #Canada tweets
cleaned_tweets.Country1 %>%
  count(word,sort = TRUE) %>%
  top_n(10) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(x=word,y=n)) +
  geom_col() +
  xlab(NULL) + 
  coord_flip() +
  theme_classic() +
  labs(x="Count",
       y="Unique words",
       title = "Unique word counts found in #Canada tweets")

#Top 10 words in #Scotland tweets
cleaned_tweets.Country2 %>%
  count(word,sort = TRUE) %>%
  top_n(10) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(x=word,y=n)) +
  geom_col() +
  xlab(NULL) + 
  coord_flip() +
  theme_classic() +
  labs(x="Count",
       y="Unique words",
       title = "Unique word counts found in #Scotland tweets")

  #Step 5: Perform sentiment analysis using the Bing lexicon and get_sentiments function from the tidytext package --------
#In tidytext & textdata we have dictionaries and lexicons for sentiment analysis, namely "bing" and "AFINN"
library(tidytext)
library(textdata)

#For "bing" lexicons, the words can be divided into 2 categories, "positive" or "negative"
#For "AFINN" lexicons, the words sentiment can be >0 (positive), <0 (negative), or =0 (neutral)

  #Sentiment analysis of country 1 using "bing" lexicon
bing_country1 = cleaned_tweets.Country1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%
  ungroup()
bing_country1
  #Plot the new finding by top 10
bing_country1 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment,scales = "free_y")+
  labs(title = "Tweets containing '#Canada'",
       y="Contribution to sentiment",
       x=NULL) +
  coord_flip() + theme_bw()

  #Sentiment analysis of country 2 using "bing" lexicon
bing_country2 = cleaned_tweets.Country2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%
  ungroup()
bing_country2
#Plot the new finding by top 10
bing_country2 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment,scales = "free_y")+
  labs(title = "Tweets containing '#Scotland'",
       y="Contribution to sentiment",
       x=NULL) +
  coord_flip() + theme_bw()

  #Step 6: Get the sentiment score for each tweet -------------------------

sentiment_bing = function(twt){
  #Perform basic text cleaning on the tweet
  twt_tbl=tibble(text=twt) %>%
    mutate(
      #Remove http elements manually
      stripped_text = gsub("http\\S+","",text)
    ) %>%
    unnest_tokens(word,stripped_text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word,sentiment,sort = TRUE) %>%
    ungroup() %>%
    #Create a column "score" that assigns a "-1" to negative words, and "1" to positive words
    mutate(
      score=case_when(
        sentiment=="negative"~n*(-1),
        sentiment=="positive"~n*1)
    )
    #Calculate total score
  sent.score = case_when(
    nrow(twt_tbl)==0~0, #if there are no words, score = 0
    nrow(twt_tbl)>0~sum(twt_tbl$score) #Otherwise, sum the positive & negative
  )
  #this is to keep track of which tweets contained no words at all from the bing list
  zero.type=case_when(
    nrow(twt_tbl)==0~"Type 1", #Type 1: no words at all, zero = no
    nrow(twt_tbl)>0~"Type 2" #Type 2: zero means sum of words = 0
  )
  list(score=sent.score,type=zero.type,twt_tbl=twt_tbl)
}

  #Apply the above function to both sets of tweets
country1_sent = lapply(country1$text, function(x){sentiment_bing(x)})
country1_sent
country2_sent = lapply(country2$text, function(x){sentiment_bing(x)})
country2_sent

  #Create a tibble that specifies the country, the scope and the type
country_sentiment<-bind_rows(
  tibble(
    country="#Canada",
    score=unlist(map(country1_sent,"score")),
    type=unlist(map(country1_sent,"type"))
  ),
  tibble(
    country="#Scotland",
    score=unlist(map(country2_sent,"score")),
    type=unlist(map(country2_sent,"type"))
  )
)
country_sentiment
  #plot the two sets of tweet sentiments
ggplot(country_sentiment,aes(x=score,fill=country)) + geom_histogram(bins = 15,alpha=.6)+
  facet_grid(~country) + theme_bw()

#------------------------------------------------------------------------------------------------------------------------------------------------#

#S6: TWEETS SENTIMENT ANALYSIS -------
  #Step 1: Download Necessary Tweets --------------------------------------
#Search 300000 Tweets with hashtags, with no retweets (rts), no replies, language = English

  #BTC
BTC_twt<-search_tweets(
  "#BTC", n = 300000, include_rts = FALSE, '-filter' = "replies",
  lang="en",retryonratelimit = TRUE)

write_as_csv(BTC_twt, "BTC_twt.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(BTC_twt, "BTC_twt.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

  #ETH
ETH_twt<-search_tweets(
  "#ETH", n = 300000, include_rts = FALSE, '-filter' = "replies",
  lang="en",retryonratelimit = TRUE)

write_as_csv(ETH_twt, "ETH_twt.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(ETH_twt, "ETH_twt.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

  #XRP
XRP_twt<-search_tweets(
  "#XRP", n = 300000, include_rts = FALSE, '-filter' = "replies",
  lang="en",retryonratelimit = TRUE)

write_as_csv(XRP_twt, "XRP_twt.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(XRP_twt, "XRP_twt.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")


  #DOGE
DOGE_twt<-search_tweets(
  "#Dogecoin", n = 300000, include_rts = FALSE, '-filter' = "replies",
  lang="en",retryonratelimit = TRUE)

write_as_csv(DOGE_twt, "DOGE_twt.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(DOGE_twt, "DOGE_twt.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

BTC_twt<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/Tweets data/BTC_twt.csv",header = TRUE)
ETH_twt<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/Tweets data/ETH_twt.csv",header = TRUE)
XRP_twt<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/Tweets data/XRP_twt.csv",header = TRUE)
DOGE_twt<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/Tweets data/DOGE_twt.csv",header = TRUE)

  #Step 2: Plot the Timeline of tweets ----------------------------------------------

#Plot the frequency of tweets over a variety of time intervals

ts_plot(BTC_twt, "hours") +
  labs(x = NULL, y = NULL,
       title = "Frequency of #BTC tweets",
       subtitle = paste0(format(min(as.Date(BTC_twt$created_at)), "%d %B %Y"), " to ", format(max(as.Date(BTC_twt$created_at)),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet") +
  theme_minimal()

  #step 3: Extract BTC data from 2021-05-13 -------------------------------
btc.trial<-subset(BTCUSD_1hr,BTCUSD_1hr$Date >= "2021-05-13" &
                    BTCUSD_1hr$Date <= "2021-05-22")
btc.trial<-btc.trial[c(1:194),c(2,7)]
btc.trial<-btc.trial[c(-1:-13),]

  #Step 4: Plot both tweets number and BTC adj close for the 5 days -------

p1 <- ts_plot(BTC_twt, "hours") +
  labs(x = NULL, y = "Number of tweets",
       title = "Frequency of #BTC tweets",
       subtitle = paste0(format(min(as.Date(BTC_twt$created_at)), "%d %B %Y"), " to ", format(max(as.Date(BTC_twt$created_at)),"%d %B %Y"))) +
  theme_minimal()

p2 <- ggplot(btc.trial, aes(Date,group=1)) + geom_line(aes(y=Close)) + theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Date", y = "Price ($)")+
  labs(title = "BTC Price", 
       subtitle =paste0(format(min(as.Date(btc.trial$Date)), "%d %B %Y"), " to ", format(max(as.Date(btc.trial$Date)),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))


#S7: TRIAL: APPLY "VADER" & "BING" LEXICON TO DO SENTIMENT ANALYSIS ON RIPPLE TWEETS -------------------
  #VADER is a is a lexicon and rule-based sentiment analysis tool that is specifically attuned to sentiments expressed in social media.
  # Step 1: Install package "vader" -----------------------------------------

install.packages("vader")
library(vader)

  # Step 2: Load RIPPLE data ---------------------------------------
data.XRP[c(1:3,nrow(data.XRP)),] #price data
XRP_twt[c(1:3,nrow(XRP_twt)),] #tweets data

  # Step 3: Process each set of tweets into tidy text or corpus object (Non-VADER approach) --------
tweets.XRP<- XRP_twt  %>% dplyr::select(screen_name,text) #Create a table with "screen_name" and "text" columns from country1
tweets.XRP[c(1:3,nrow(tweets.XRP)),]

head(tweets.XRP$text)

#Remove hyperlink (http) elements
tweets.XRP$stripped_text1<-gsub("http\\S+","",tweets.XRP$text) 

#Convert tweets to lowercase using "unnest_tokens" command + remove punctuation + add id for each tweet
tweets.XRP_stem<-tweets.XRP %>%
  dplyr::select(stripped_text1) %>%
  unnest_tokens(word,stripped_text1)
head(tweets.XRP_stem)

#Remove stop words from your list of words
cleaned_tweets.XRP<-tweets.XRP_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.XRP)

#Find the top 10 commonly used words in #XRP
cleaned_tweets.XRP %>%
  count(word,sort = TRUE) %>%
  top_n(10) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(x=word,y=n)) +
  geom_col() +
  xlab(NULL) + 
  coord_flip() +
  theme_classic() +
  labs(x="Count",
       y="Unique words",
       title = "Unique word counts found in #XRP tweets")

  # Step 4: Perform sentiment analysis using the Bing and VADER lexicon and get_sentiments function from the tidytext package --------
#In tidytext & textdata we have dictionaries and lexicons for sentiment analysis, namely "bing", "AFINN", and "VADER"
library(tidytext)
library(textdata)
library(vader)

#For "bing" lexicon, the words can be divided into 2 categories, "positive" or "negative"
#For "AFINN" lexicon, the words sentiment can be >0 (positive), <0 (negative), or =0 (neutral)
#For "VADER" lexicon, the words sentiment can be between -1 (most negative) and +1 (most positive)

    # 4.1: Sentiment analysis of #XRP using "bing" lexicon ------------------------
bing_xrp = cleaned_tweets.XRP %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%
  ungroup()
bing_xrp
#Plot the new finding by top 10
bing_xrp %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment,scales = "free_y")+
  labs(title = "Tweets containing '#XRP'",
       y="Contribution to sentiment",
       x=NULL) +
  coord_flip() + theme_bw()

    # 4.2: Sentiment analysis of #XRP using "VADER" lexicon -----------------------
#Sentiment of each words (already distached and cleaned from the tweets)
vader_xrp = vader_df(cleaned_tweets.XRP$word)
vader_xrp[c(1:3,nrow(vader_xrp)),]
write_as_csv(vader_xrp, "vader_xrp.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(vader_xrp, "vader_xrp.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

#Sentiment of the whole tweet
vader_xrp.2 = vader_df(tweets.XRP$stripped_text1)
vader_xrp.2[c(1:3,nrow(vader_xrp.2)),]
write_as_csv(vader_xrp.2, "vader_xrp.2.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(vader_xrp.2, "vader_xrp.2.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

vader_xrp<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/vader_xrp.csv",header = TRUE)
vader_xrp.2<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/vader_xrp.2.csv",header = TRUE)

  # Step 5: Get the sentiment score for #XRP -------------------------
    # 5.1: Bing lexicon approach --------------------------------------------------
sentiment_bing = function(twt){
  #Perform basic text cleaning on the tweet
  twt_tbl=tibble(text=twt) %>%
    mutate(
      #Remove http elements manually
      stripped_text = gsub("http\\S+","",text)
    ) %>%
    unnest_tokens(word,stripped_text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word,sentiment,sort = TRUE) %>%
    ungroup() %>%
    #Create a column "score" that assigns a "-1" to negative words, and "1" to positive words
    mutate(
      score=case_when(
        sentiment=="negative"~n*(-1),
        sentiment=="positive"~n*1)
    )
  #Calculate total score
  sent.score = case_when(
    nrow(twt_tbl)==0~0, #if there are no words, score = 0
    nrow(twt_tbl)>0~sum(twt_tbl$score) #Otherwise, sum the positive & negative
  )
  #this is to keep track of which tweets contained no words at all from the bing list
  zero.type=case_when(
    nrow(twt_tbl)==0~"Type 1", #Type 1: no words at all, zero = no
    nrow(twt_tbl)>0~"Type 2" #Type 2: zero means sum of words = 0
  )
  list(score=sent.score,type=zero.type,twt_tbl=twt_tbl)
}

#Apply the above function to XRP_twt
XRP_sent = lapply(XRP_twt$text, function(x){sentiment_bing(x)})

#Create a tibble that specifies the coins, the scope and the type
coin_sentiment<-bind_rows(
  tibble(
    coin="#XRP",
    score=unlist(map(XRP_sent,"score")),
    type=unlist(map(XRP_sent,"type"))
  )
)
#plot the tweet sentiments
ggplot(coin_sentiment,aes(x=score)) + 
  geom_histogram(bins = 15,alpha=.6) + 
  theme_bw() +
  labs(x="Score",y="Number of tweets")+
  labs(title = "Sentiment analysis of #XRP Tweets")+
  labs(subtitle = "Using Bing lexicon")
#------------------------------------------------------------------------------------------------------------------------------------------------#
    # 5.2: VADER Lexicon approach --------------------------------------------------
XRP_sent.vader<-vader_xrp.2[,c(3:4)]
 
ggplot(XRP_sent.vader,aes(x=compound)) + 
  geom_histogram(bins = 15,alpha=.6) + 
  theme_bw() +
  labs(x="Score",y="Number of tweets")+
  labs(title = "Sentiment analysis of #XRP Tweets")+
  labs(subtitle = "Using VADER lexicon")

#S8: COMBINE WITH S6: #XRP ANALYSIS ----------------------------------------------

  # Step 1: Plot the Timeline of #XRP tweets ----------------------------------------------

    #Plot the frequency of tweets over a variety of time intervals

ts_plot(XRP_twt, "hours") +
  labs(x = NULL, y = NULL,
       title = "Frequency of #XRP tweets",
       subtitle = paste0(format(min(as.Date(XRP_twt$created_at)), "%d %B %Y"), " to ", format(max(as.Date(XRP_twt$created_at)),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet") +
  theme_minimal()

  # Step 2: Extract #XRP data from 2021-05-27 to 2021-05-31 -------------------------------
xrp.trial<-subset(data.XRP,data.XRP$Date >= "2021-05-27" &
                    data.XRP$Date <= "2021-05-31")

  # Step 3: Plot both tweets number and XRP adj close for the 5 days -------

p1 <- ts_plot(XRP_twt, "hours") +
  labs(x = NULL, y = "Number of tweets",
       title = "Frequency of #XRP tweets",
       subtitle = paste0(format(min(as.Date(XRP_twt$created_at)), "%d %B %Y"), " to ", format(max(as.Date(XRP_twt$created_at)),"%d %B %Y"))) +
  theme_minimal()

p2 <- ggplot(xrp.trial, aes(Date,group=1)) + geom_line(aes(y=Adj.Close)) + theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Date", y = "Price ($)")+
  labs(title = "XRP Price", 
       subtitle =paste0(format(min(as.Date(xrp.trial$Date)), "%d %B %Y"), " to ", format(max(as.Date(xrp.trial$Date)),"%d %B %Y")))
p3 <- ggplot(XRP_sent.vader,aes(x=compound)) + 
  geom_histogram(bins = 15,alpha=.6) + 
  theme_bw() +
  labs(x="Score",y="Number of tweets")+
  labs(title = "Sentiment analysis of #XRP Tweets (VADER)",caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")
library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3), size = "last"))

