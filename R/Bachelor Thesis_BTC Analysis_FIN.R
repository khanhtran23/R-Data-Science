#BITCOIN (BTC) FROM 26 MAY 21H -31 MAY 11H 2021
# 1 Asterisk (*): Execute only ONCE!
# 2 Asterisks (**): Execute EVERY TIME!

#------------------------------------------------------------------------------------------------------------------------------------------------#
# **LIBRARIES ---------------------------------------------------------------
library(astsa)
library(car)
library(coinmarketcapr)
library(corrplot)
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
library(tidyverse)
library(testcorr)
library(textdata)
library(TSstudio)
library(scales)
library(vader)
#------------------------------------------------------------------------------------------------------------------------------------------------#
# PART I: PRICE ANALYSIS --------------------------------------------------
# *S1: GET NECESSARY PRICE DATASET -----------------------------------------------
data.BTC<-getSymbols("BTC-USD",from="2016-05-31",to="2021-05-31",auto.assign = FALSE)

BTCUSD_1hr<-read_csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Hourly Data/BTCUSD_1hr_woline1.csv")
BTCUSD_1hr<-subset(BTCUSD_1hr,BTCUSD_1hr$Date>="2016-05-31" &
                     BTCUSD_1hr$Date<="2021-06-01")
BTCUSD_1hr<-BTCUSD_1hr[c(-43823:-43825),]

# *S2: DAILY PRICE RETURNS 5 YEARS -------------------------------------------------------
#DAILY LOGARITHMIC TOTAL RETURNS
BTC.log.ret <- data.BTC[,6]
BTC.log.ret$BTC.log.ret <- diff(log(BTC.log.ret$`BTC-USD.Adjusted`))
options(digits = 3)
BTC.log.ret <- BTC.log.ret[,2]
BTC.log.ret[1,1]<-0
BTC.log.ret$Gross.ret<-BTC.log.ret$BTC.log.ret+1
BTC.log.ret$Gross.ret[1]<-1
BTC.log.ret$cum.ret<-cumprod(BTC.log.ret$Gross.ret)

#CUMULATED DAILY RETURN AFTER 5 YEARS
BTC.logcumret <- sum(BTC.log.ret$BTC.log.ret,na.rm=TRUE)
BTC.cumret <- exp(BTC.logcumret)-1
BTC.cumret #65.4

#Mean returns, Std-Dev, Min, Max, Skewness, Kurtosis
BTC.mean<-mean(BTC.log.ret$BTC.log.ret,na.rm=TRUE)
BTC.sd<-sd(BTC.log.ret$BTC.log.ret,na.rm=TRUE)
BTC.min<-min(BTC.log.ret$BTC.log.ret,na.rm=TRUE)
BTC.max<-max(BTC.log.ret$BTC.log.ret,na.rm=TRUE)
BTC.skew<-skewness(BTC.log.ret$BTC.log.ret,na.rm=TRUE)
BTC.kurt<-kurtosis(BTC.log.ret$BTC.log.ret,na.rm=TRUE)
BTC.summary.table<-cbind(BTC.cumret,BTC.mean,BTC.sd,BTC.min,BTC.max,BTC.skew,BTC.kurt)
rownames(BTC.summary.table)<-c("2016-2021")
BTC.summary.table

# *S3: SIMPLE MOVING AVERAGE & TREND ---------------------------------------------------

#Step 1: Create BTC.sma
BTC.sma <- data.BTC[,6]
BTC.sma[is.na(BTC.sma)] <- 0
colnames(BTC.sma)<-paste(c("Adjusted"))
#Step 2: Create 50-day and 100-day rolling average columns
BTC.sma$sma50 <- rollmeanr(BTC.sma$Adjusted, k =50)
BTC.sma$sma100 <- rollmeanr(BTC.sma$Adjusted, k = 100)
BTC.sma$no<-c(1:nrow(BTC.sma)) #This supports creation of Trendline

#**S4: DAILY EWMA VOLATILITY -----------------------------------------------------
#Step 1,2: Create squared logarithmic price return of BTC
BTC_squared <- data.BTC[,6]
BTC_squared$BTC.squared <- (diff(log(BTC_squared$`BTC-USD.Adjusted`)))^2
options(digits = 3)
BTC_squared <- BTC_squared[,2]
BTC_squared[1,1]<-0

BTC_squared2 <- as.numeric(BTC_squared) # Squared returns as a numeric vector

#Step 3: Create EWMA function
ewma <- function(BTC_squared2, lambda){
  stats::filter(BTC_squared2*(1-lambda), lambda, "recursive", init = BTC_squared2[1])
}

# Step 4: Calculate variance with lambda=0.94
BTC_var_ewma94 <- ewma(BTC_squared2, 0.94)

# Step 5: Transform back to a zoo object (time index from object BTC_squared)
BTC_var_ewma <- zoo(BTC_var_ewma94,index(BTC_squared))

#Step 6: Volatility
BTC_vola_ewma <- sqrt(BTC_var_ewma)
# *S5: ALPHA & BETA USING CAPM, MARKET MODEL, AND ROLLING WINDOW REGRESSION --------
#Knowledge from Ang 2015, Chapter 5
#CAPM
#Step 0: Construct a monthly returns of BTC
data.BTC[c(1:3,nrow(data.BTC)),]

BTC.monthly<-to.monthly(data.BTC)
BTC.monthly[c(1:3,nrow(BTC.monthly)),]

BTC.monthly<-BTC.monthly[,6]

BTC.ret<-Delt(BTC.monthly$data.BTC.Adjusted)
names(BTC.ret)<-paste("BTC.ret")
BTC.ret<-BTC.ret[-1,]
BTC.ret[c(1:3,nrow(BTC.ret)),]

csv.BTC<-cbind(data.frame(index(BTC.ret)),data.frame(BTC.ret))
names(csv.BTC)[1]<-paste("date")
rownames(csv.BTC)<-seq(1,nrow(csv.BTC),by=1)
csv.BTC[c(1:3,nrow(csv.BTC)),]

write.csv(csv.BTC,"BTC Returns (Monthly).csv") #Now, we have the csv file for the next steps.

#Step 1: Import Portfolio Returns and Convert to a data.frame Object
BTC<-read.csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Others/BTC Returns (Monthly).csv", header = TRUE)
BTC[c(1:3,nrow(BTC)),]
BTC$date<-as.yearmon(as.character(BTC$date),"%b %Y")
BTC[c(1:3,nrow(BTC)),]
BTC.df<-data.frame(BTC)
BTC.df[c(1:3,nrow(BTC.df)),]

#Step 2: Import S&P 500 Index Data from Yahoo Finance and Calculate Monthly Market Returns
data.mkt<-getSymbols("^GSPC",from="2016-05-31", to="2021-05-31",auto.assign = FALSE)
data.mkt[c(1:3,nrow(data.mkt)),]
mkt.monthly<-to.monthly(data.mkt)
mkt.monthly[c(1:3,nrow(mkt.monthly)),]
mkt.monthly<-mkt.monthly[,6]
mkt.ret<-Delt(mkt.monthly$data.mkt.Adjusted)
names(mkt.ret)<-paste("mkt.ret")
mkt.ret<-mkt.ret[-1,]
mkt.ret[c(1:3,nrow(mkt.ret)),]
market.df<-data.frame(mkt.ret)
head(market.df)
tail(market.df)

#Step 3: Import Risk-Free Rate Data from FRED and Setup Data to Contain Monthly Risk-Free Returns
#For rf data, go to https://fred.stlouisfed.org/ and download the 3-Month Constant Maturity Treasury rate. Save as "DGS3MOFRED.csv"
rf<-read.csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Others/DGS3MOFRED.csv", skip = 10)
rf[1:3,]
rf$date<-as.Date(rf$X2016.04.14,"%Y-%m-%d")
rf$X0.22<-as.numeric(as.character(rf$X0.22))
names(rf)[2]<-paste("DGS3MO")
names(rf)[1]<-paste("observation_date")
rf[c(1:3,nrow(rf)),]
rf<-subset(rf,rf$date >= "2016-05-31" &
             rf$date <= "2021-05-31")
rf[c(1:3,nrow(rf)),]

#Step 3a: Convert to xts Object 
rf<-xts(rf$DGS3MO,order.by = rf$date)
names(rf)<-paste("DGS3MO")
rf[c(1:3,nrow(rf)),]
#Step 3b: Apply to.monthly Command to Identify First Yield for Each Month
rf.monthly<-to.monthly(rf)
rf.monthly[c(1:3,nrow(rf.monthly)),]
#Step 3c: Convert Opening Annualized Yield for Each Month Into a Monthly Yield
#The Yield shown in 3b is annualized Yield. Instead, we want monthly yield, using geometric average calculation
options(scipen = 100) #Our results will be small; "scipens" prevent R from showing notations (0.010E+...), but the long result instead (0.01054179266)
rf.monthly<-(1+rf.monthly[,1]/100)^(1/12)-1
rf.monthly<-rf.monthly[-1,]
rf.monthly[c(1:3,nrow(rf.monthly)),]

#Step 4: Combine BTC, mkt, and rf Data Into One Data Object
combo.BTC<-cbind(market.df,data.frame(rf.monthly),BTC.df$BTC.ret)
combo.BTC[c(1:3,nrow(combo.BTC)),]
names(combo.BTC)<-paste(c("mkt.ret","rf","BTC.ret"))
combo.BTC[c(1:3,nrow(combo.BTC)),]

#Step 5: Calculate Excess BTC Return and Excess Market Return
#Excess BTC Return (exret) = port.ret - rf
#Excess Market Return (exmkt) = mkt.ret - rf
combo.BTC$exret<-combo.BTC$BTC.ret-combo.BTC$rf
combo.BTC$exmkt<-combo.BTC$mkt.ret-combo.BTC$rf
combo.BTC[c(1:3,nrow(combo.BTC)),]

#Step 6: Run Regression of Excess BTC Return on Excess Market Return
#We use "lm" command to calculate OLS Regression between exret & exmkt
options(digits = 3)
CAPM<-lm(combo.BTC$exret~combo.BTC$exmkt) #lm stands for "linear model", the function that creates a simple regression model
summary(CAPM)

#Rolling Window Regression
#In this section, we run regression through a rolling window, which calculate Alphas and Betas in different periods.
#=> Thus we can see how variable these two are, over a time period.

#Step 1: Import BTC and S&P 500 Index Data 
data.BTC[c(1:3,nrow(data.BTC)),]
data.mkt[c(1:3,nrow(data.mkt)),]

#Step 2: Calculate the BTC and Market Returns
rets<-diff(log(data.BTC$`BTC-USD.Adjusted`))
rets$GSPC<-diff(log(data.mkt$GSPC.Adjusted))
names(rets)[1]<-"BTC"
rets<-rets[-1,]
rets[c(1:3,nrow(rets)),]

#Step 3: Create the Rolling Window Regression Function
require(zoo)
coeffs<-rollapply(rets,
                  width = 252,
                  FUN = function(X)
                  {
                    roll.reg=lm(BTC~GSPC,
                                data=as.data.frame(X))
                    return(roll.reg$coef) #used to output the parameter estimates (i.e., the intercept term and the coefficient for the market return) of the regression.
                  },
                  by.column = FALSE)
coeffs[c(1,251:253,nrow(coeffs)),]

#Step 4: Remove NAs From the Data
coeffs<-na.omit(coeffs)
coeffs[c(1:3,nrow(coeffs)),]

#Step 5: Clean-Up Data
#We want: Rename the columns to "Alpha" & "Beta"
coeffs<-coeffs[-1,]
names(coeffs)<-c("Alpha","Beta")
coeffs[c(1:3,nrow(coeffs)),]
# PLOTS -------------------------------------------------------------------
##**Daily Price movement of BTC 2016-2021 ---------------------------------
ggplot(data.BTC,aes(x=index(data.BTC)))+
  geom_line(aes(y=data.BTC$`BTC-USD.Adjusted`))+
  labs(x = "Date", y = "Value ($)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + # centers headline
  labs(title = "Value of $1 Investment in Bitcoin May 31, 2016 - May 31, 2021",
       caption = "Data retrieved from Yahoo! Finance")

##**Hourly Price movement of BTC 2016-2021 (the same with daily price movement) --------
ggplot(BTCUSD_1hr, aes(Date,group=1)) + geom_line(aes(y=Close)) + theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Date", y = "Price ($)")+
  labs(title = " BTC Hourly Price", 
       subtitle =paste0(format(min(as.Date(BTCUSD_1hr$Date)), "%d %B %Y"), " to ", format(max(as.Date(BTCUSD_1hr$Date)),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

##**Daily Log return volatility 2016-2021 ---------------------------------
BTC.log.ret<-as.data.frame(BTC.log.ret)
BTC.volatility<-ggplot(BTC.log.ret,aes(x=index(BTC.log.ret)))+
  geom_line(aes(y=BTC.log.ret))+
  labs(x = "Date", y = "Value of Investment ($)") +
  labs(title = "Volatility of BTC price
May 31, 2016 - May 31, 2021")+
  theme_bw()# white background
BTC.volatility<-BTC.volatility+geom_hline(yintercept = 0,col="red")
BTC.volatility
##**SMA + Trendline ---------------------------------------------------
ggplot(BTC.sma,aes(x=index(BTC.sma)))+
  geom_line(aes(y=Adjusted))+
  theme_bw()+
  labs(x="Date",y="Price ($)",
       title = "BTC-USD Simple Moving Average",
       subtitle = "May 31, 2016 - May 31, 2021")+
  geom_line(aes(y=predict(lm(Adjusted~no))),col="red")+ #trendline
  geom_line(aes(y=sma50), col="green",alpha=.4,size=.1)+
  geom_line(aes(y=sma100),col="red",alpha=.4,size=.1)

##**DAILY EWMA --------------------------------------------------------------
plot(BTC_var_ewma, main="BTC - Daily Variance with EWMA, lambda=0.94", xlab="Date", ylab="Variance", col="blue", type="l")
plot(BTC_vola_ewma,  main="BTC - Daily Volatility with EWMA, lambda=0.94", xlab="Date", ylab="Volatility", col="blue", type="l")

##**ALPHA-BETA --------------------------------------------------------------
p1 <- ggplot(coeffs, aes(index(coeffs), coeffs$Alpha)) + geom_line() + theme_minimal() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(angle = 90))+
  labs(x = "Date", y = "Alpha") +
  labs(title = "Bitcoin Alpha and Beta
Using Rolling 252-Day Windowns and
Daily Returns From 2016 to 2021")
p2 <- ggplot(coeffs, aes(index(coeffs), coeffs$Beta)) + geom_line() + theme_minimal() + 
  theme(axis.text.x = element_text())+
  geom_hline(yintercept = 0,col="red")+
  labs(x = "Date", y = "Beta")
library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

# PART II: SENTIMENT ANALYSIS USING VADER ---------------------------------
# *S1: GET TWEETS DATASET --------------------------------------------------
BTC_twt<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/Tweets data/BTC_twt2.csv",header = TRUE)

# *S2: Clean Tweets ------------------------------------------------------
tweets.BTC<- BTC_twt  %>% dplyr::select(created_at, screen_name,text) #Create a table with "created_at", "screen_name", and "text" columns from BTC_twt
tweets.BTC[c(1:3,nrow(tweets.BTC)),]

tweets.BTC$stripped_text1<-gsub("https(.*)*$","",tweets.BTC$text) #Remove URLs
tweets.BTC$stripped_text1<-tolower(tweets.BTC$stripped_text1) #To lowercase
tweets.BTC$stripped_text1<-gsub("\\.\\.","",tweets.BTC$stripped_text1) #replace ..... with .
tweets.BTC$stripped_text1<-gsub("(.)\\1{2,}", "\\1",tweets.BTC$stripped_text1) #Transform "Goooooo" to "go"
tweets.BTC$stripped_text1<-gsub("([[:punct:]])\\1+", "\\1", tweets.BTC$stripped_text1) #Reduce !!!!! to !
tweets.BTC$stripped_text1<-gsub("#","", tweets.BTC$stripped_text1) #Remove hashtags
tweets.BTC$stripped_text1<-gsub("@[[:alnum:]]+","", tweets.BTC$stripped_text1) #Remove mentions
tweets.BTC$stripped_text1<-gsub("&amp","and", tweets.BTC$stripped_text1) #Replace &apm with and
tweets.BTC$stripped_text1<-gsub("<(.*)>","", tweets.BTC$stripped_text1) #Remove unicodes <U+...>
tweets.BTC$stripped_text1<-iconv(tweets.BTC$stripped_text1, "latin1", "ASCII", sub="") #remove weird letters
tweets.BTC$stripped_text1<- gsub("%%", "\'", tweets.BTC$stripped_text1) # Changing %% back to apostrophes
tweets.BTC$stripped_text1<-str_squish(tweets.BTC$stripped_text1) #Remove excessive spaces

# *S3: Run and save VADER analysis -----------------------------------------------------------
vader_btc = vader_df(tweets.BTC$stripped_text1)
vader_btc[c(1:3,nrow(vader_btc)),]
write_as_csv(vader_btc, "vader_btc2.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(vader_btc, "vader_btc2.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

# *S3 bis: Load sentiment data ---------------------------------------------
vader_btc<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/vader_btc2.csv",header = TRUE)
BTC_sent<-vader_btc[,c(3:4)]
date<-tweets.BTC$created_at
BTC_sent$date<-date

# *S4: AVERAGE SENTIMENT SCORE OVER THE PERIOD -----------------------------
BTC_sent[c(1:3,nrow(BTC_sent)),]
BTC_sent.avg<-mean(BTC_sent$compound,na.rm=TRUE)
BTC_sent.avg #0.24

# *S5: AVERAGE SENTIMENT SCORE PER HOUR ----------------------------------------

BTC_sent.1<-subset(BTC_sent,BTC_sent$date >= "2021-05-26 21:00:00" &
                     BTC_sent$date <= "2021-05-26 21:59:59")
BTC_sent.1$index<-seq(1,nrow(BTC_sent.1),by=1)
BTC_sent.1.sent<-mean(BTC_sent.1$compound,na.rm = TRUE)
BTC_sent.1.sent<-cbind(BTC_sent.1.sent,BTC_sent.1[nrow(BTC_sent.1),4])
colnames(BTC_sent.1.sent)<-paste(c("avg.sent","volume"))
BTC_sent.1.sent

BTC_sent.2<-subset(BTC_sent,BTC_sent$date >= "2021-05-26 22:00:00" &
                     BTC_sent$date <= "2021-05-26 22:59:59")
BTC_sent.2$index<-seq(1,nrow(BTC_sent.2),by=1)
BTC_sent.2.sent<-mean(BTC_sent.2$compound,na.rm = TRUE)
BTC_sent.2.sent<-cbind(BTC_sent.2.sent,BTC_sent.2[nrow(BTC_sent.2),4])
colnames(BTC_sent.2.sent)<-paste(c("avg.sent","volume"))
BTC_sent.2.sent

BTC_sent.3<-subset(BTC_sent,BTC_sent$date >= "2021-05-26 23:00:00" &
                     BTC_sent$date <= "2021-05-26 23:59:59")
BTC_sent.3$index<-seq(1,nrow(BTC_sent.3),by=1)
BTC_sent.3.sent<-mean(BTC_sent.3$compound,na.rm = TRUE)
BTC_sent.3.sent<-cbind(BTC_sent.3.sent,BTC_sent.3[nrow(BTC_sent.3),4])
colnames(BTC_sent.3.sent)<-paste(c("avg.sent","volume"))
BTC_sent.3.sent

BTC_sent.4<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 00:00:00" &
                     BTC_sent$date <= "2021-05-27 00:59:59")
BTC_sent.4$index<-seq(1,nrow(BTC_sent.4),by=1)
BTC_sent.4.sent<-mean(BTC_sent.4$compound,na.rm = TRUE)
BTC_sent.4.sent<-cbind(BTC_sent.4.sent,BTC_sent.4[nrow(BTC_sent.4),4])
colnames(BTC_sent.4.sent)<-paste(c("avg.sent","volume"))
BTC_sent.4.sent

BTC_sent.5<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 01:00:00" &
                     BTC_sent$date <= "2021-05-27 01:59:59")
BTC_sent.5$index<-seq(1,nrow(BTC_sent.5),by=1)
BTC_sent.5.sent<-mean(BTC_sent.5$compound,na.rm = TRUE)
BTC_sent.5.sent<-cbind(BTC_sent.5.sent,BTC_sent.5[nrow(BTC_sent.5),4])
colnames(BTC_sent.5.sent)<-paste(c("avg.sent","volume"))
BTC_sent.5.sent

BTC_sent.6<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 02:00:00" &
                     BTC_sent$date <= "2021-05-27 02:59:59")
BTC_sent.6$index<-seq(1,nrow(BTC_sent.6),by=1)
BTC_sent.6.sent<-mean(BTC_sent.6$compound,na.rm = TRUE)
BTC_sent.6.sent<-cbind(BTC_sent.6.sent,BTC_sent.6[nrow(BTC_sent.6),4])
colnames(BTC_sent.6.sent)<-paste(c("avg.sent","volume"))
BTC_sent.6.sent

BTC_sent.7<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 03:00:00" &
                     BTC_sent$date <= "2021-05-27 03:59:59")
BTC_sent.7$index<-seq(1,nrow(BTC_sent.7),by=1)
BTC_sent.7.sent<-mean(BTC_sent.7$compound,na.rm = TRUE)
BTC_sent.7.sent<-cbind(BTC_sent.7.sent,BTC_sent.7[nrow(BTC_sent.7),4])
colnames(BTC_sent.7.sent)<-paste(c("avg.sent","volume"))
BTC_sent.7.sent

BTC_sent.8<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 04:00:00" &
                     BTC_sent$date <= "2021-05-27 04:59:59")
BTC_sent.8$index<-seq(1,nrow(BTC_sent.8),by=1)
BTC_sent.8.sent<-mean(BTC_sent.8$compound,na.rm = TRUE)
BTC_sent.8.sent<-cbind(BTC_sent.8.sent,BTC_sent.8[nrow(BTC_sent.8),4])
colnames(BTC_sent.8.sent)<-paste(c("avg.sent","volume"))
BTC_sent.8.sent

BTC_sent.9<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 05:00:00" &
                     BTC_sent$date <= "2021-05-27 05:59:59")
BTC_sent.9$index<-seq(1,nrow(BTC_sent.9),by=1)
BTC_sent.9.sent<-mean(BTC_sent.9$compound,na.rm = TRUE)
BTC_sent.9.sent<-cbind(BTC_sent.9.sent,BTC_sent.9[nrow(BTC_sent.9),4])
colnames(BTC_sent.9.sent)<-paste(c("avg.sent","volume"))
BTC_sent.9.sent

BTC_sent.10<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 06:00:00" &
                     BTC_sent$date <= "2021-05-27 06:59:59")
BTC_sent.10$index<-seq(1,nrow(BTC_sent.10),by=1)
BTC_sent.10.sent<-mean(BTC_sent.10$compound,na.rm = TRUE)
BTC_sent.10.sent<-cbind(BTC_sent.10.sent,BTC_sent.10[nrow(BTC_sent.10),4])
colnames(BTC_sent.10.sent)<-paste(c("avg.sent","volume"))
BTC_sent.10.sent

BTC_sent.11<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 07:00:00" &
                      BTC_sent$date <= "2021-05-27 07:59:59")
BTC_sent.11$index<-seq(1,nrow(BTC_sent.11),by=1)
BTC_sent.11.sent<-mean(BTC_sent.11$compound,na.rm = TRUE)
BTC_sent.11.sent<-cbind(BTC_sent.11.sent,BTC_sent.11[nrow(BTC_sent.11),4])
colnames(BTC_sent.11.sent)<-paste(c("avg.sent","volume"))
BTC_sent.11.sent

BTC_sent.12<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 08:00:00" &
                      BTC_sent$date <= "2021-05-27 08:59:59")
BTC_sent.12$index<-seq(1,nrow(BTC_sent.12),by=1)
BTC_sent.12.sent<-mean(BTC_sent.12$compound,na.rm = TRUE)
BTC_sent.12.sent<-cbind(BTC_sent.12.sent,BTC_sent.12[nrow(BTC_sent.12),4])
colnames(BTC_sent.12.sent)<-paste(c("avg.sent","volume"))
BTC_sent.12.sent

BTC_sent.13<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 09:00:00" &
                      BTC_sent$date <= "2021-05-27 09:59:59")
BTC_sent.13$index<-seq(1,nrow(BTC_sent.13),by=1)
BTC_sent.13.sent<-mean(BTC_sent.13$compound,na.rm = TRUE)
BTC_sent.13.sent<-cbind(BTC_sent.13.sent,BTC_sent.13[nrow(BTC_sent.13),4])
colnames(BTC_sent.13.sent)<-paste(c("avg.sent","volume"))
BTC_sent.13.sent

BTC_sent.14<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 10:00:00" &
                      BTC_sent$date <= "2021-05-27 10:59:59")
BTC_sent.14$index<-seq(1,nrow(BTC_sent.14),by=1)
BTC_sent.14.sent<-mean(BTC_sent.14$compound,na.rm = TRUE)
BTC_sent.14.sent<-cbind(BTC_sent.14.sent,BTC_sent.14[nrow(BTC_sent.14),4])
colnames(BTC_sent.14.sent)<-paste(c("avg.sent","volume"))
BTC_sent.14.sent

BTC_sent.15<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 11:00:00" &
                      BTC_sent$date <= "2021-05-27 11:59:59")
BTC_sent.15$index<-seq(1,nrow(BTC_sent.15),by=1)
BTC_sent.15.sent<-mean(BTC_sent.15$compound,na.rm = TRUE)
BTC_sent.15.sent<-cbind(BTC_sent.15.sent,BTC_sent.15[nrow(BTC_sent.15),4])
colnames(BTC_sent.15.sent)<-paste(c("avg.sent","volume"))
BTC_sent.15.sent

BTC_sent.16<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 12:00:00" &
                      BTC_sent$date <= "2021-05-27 12:59:59")
BTC_sent.16$index<-seq(1,nrow(BTC_sent.16),by=1)
BTC_sent.16.sent<-mean(BTC_sent.16$compound,na.rm = TRUE)
BTC_sent.16.sent<-cbind(BTC_sent.16.sent,BTC_sent.16[nrow(BTC_sent.16),4])
colnames(BTC_sent.16.sent)<-paste(c("avg.sent","volume"))
BTC_sent.16.sent

BTC_sent.17<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 13:00:00" &
                      BTC_sent$date <= "2021-05-27 13:59:59")
BTC_sent.17$index<-seq(1,nrow(BTC_sent.17),by=1)
BTC_sent.17.sent<-mean(BTC_sent.17$compound,na.rm = TRUE)
BTC_sent.17.sent<-cbind(BTC_sent.17.sent,BTC_sent.17[nrow(BTC_sent.17),4])
colnames(BTC_sent.17.sent)<-paste(c("avg.sent","volume"))
BTC_sent.17.sent

BTC_sent.18<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 14:00:00" &
                      BTC_sent$date <= "2021-05-27 14:59:59")
BTC_sent.18$index<-seq(1,nrow(BTC_sent.18),by=1)
BTC_sent.18.sent<-mean(BTC_sent.18$compound,na.rm = TRUE)
BTC_sent.18.sent<-cbind(BTC_sent.18.sent,BTC_sent.18[nrow(BTC_sent.18),4])
colnames(BTC_sent.18.sent)<-paste(c("avg.sent","volume"))
BTC_sent.18.sent

BTC_sent.19<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 15:00:00" &
                      BTC_sent$date <= "2021-05-27 15:59:59")
BTC_sent.19$index<-seq(1,nrow(BTC_sent.19),by=1)
BTC_sent.19.sent<-mean(BTC_sent.19$compound,na.rm = TRUE)
BTC_sent.19.sent<-cbind(BTC_sent.19.sent,BTC_sent.19[nrow(BTC_sent.19),4])
colnames(BTC_sent.19.sent)<-paste(c("avg.sent","volume"))
BTC_sent.19.sent

BTC_sent.20<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 16:00:00" &
                      BTC_sent$date <= "2021-05-27 16:59:59")
BTC_sent.20$index<-seq(1,nrow(BTC_sent.20),by=1)
BTC_sent.20.sent<-mean(BTC_sent.20$compound,na.rm = TRUE)
BTC_sent.20.sent<-cbind(BTC_sent.20.sent,BTC_sent.20[nrow(BTC_sent.20),4])
colnames(BTC_sent.20.sent)<-paste(c("avg.sent","volume"))
BTC_sent.20.sent

BTC_sent.21<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 17:00:00" &
                      BTC_sent$date <= "2021-05-27 17:59:59")
BTC_sent.21$index<-seq(1,nrow(BTC_sent.21),by=1)
BTC_sent.21.sent<-mean(BTC_sent.21$compound,na.rm = TRUE)
BTC_sent.21.sent<-cbind(BTC_sent.21.sent,BTC_sent.21[nrow(BTC_sent.21),4])
colnames(BTC_sent.21.sent)<-paste(c("avg.sent","volume"))
BTC_sent.21.sent

BTC_sent.22<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 18:00:00" &
                      BTC_sent$date <= "2021-05-27 18:59:59")
BTC_sent.22$index<-seq(1,nrow(BTC_sent.22),by=1)
BTC_sent.22.sent<-mean(BTC_sent.22$compound,na.rm = TRUE)
BTC_sent.22.sent<-cbind(BTC_sent.22.sent,BTC_sent.22[nrow(BTC_sent.22),4])
colnames(BTC_sent.22.sent)<-paste(c("avg.sent","volume"))
BTC_sent.22.sent

BTC_sent.23<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 19:00:00" &
                      BTC_sent$date <= "2021-05-27 19:59:59")
BTC_sent.23$index<-seq(1,nrow(BTC_sent.23),by=1)
BTC_sent.23.sent<-mean(BTC_sent.23$compound,na.rm = TRUE)
BTC_sent.23.sent<-cbind(BTC_sent.23.sent,BTC_sent.23[nrow(BTC_sent.23),4])
colnames(BTC_sent.23.sent)<-paste(c("avg.sent","volume"))
BTC_sent.23.sent

BTC_sent.24<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 20:00:00" &
                      BTC_sent$date <= "2021-05-27 20:59:59")
BTC_sent.24$index<-seq(1,nrow(BTC_sent.24),by=1)
BTC_sent.24.sent<-mean(BTC_sent.24$compound,na.rm = TRUE)
BTC_sent.24.sent<-cbind(BTC_sent.24.sent,BTC_sent.24[nrow(BTC_sent.24),4])
colnames(BTC_sent.24.sent)<-paste(c("avg.sent","volume"))
BTC_sent.24.sent

BTC_sent.25<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 21:00:00" &
                      BTC_sent$date <= "2021-05-27 21:59:59")
BTC_sent.25$index<-seq(1,nrow(BTC_sent.25),by=1)
BTC_sent.25.sent<-mean(BTC_sent.25$compound,na.rm = TRUE)
BTC_sent.25.sent<-cbind(BTC_sent.25.sent,BTC_sent.25[nrow(BTC_sent.25),4])
colnames(BTC_sent.25.sent)<-paste(c("avg.sent","volume"))
BTC_sent.25.sent

BTC_sent.26<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 22:00:00" &
                      BTC_sent$date <= "2021-05-27 22:59:59")
BTC_sent.26$index<-seq(1,nrow(BTC_sent.26),by=1)
BTC_sent.26.sent<-mean(BTC_sent.26$compound,na.rm = TRUE)
BTC_sent.26.sent<-cbind(BTC_sent.26.sent,BTC_sent.26[nrow(BTC_sent.26),4])
colnames(BTC_sent.26.sent)<-paste(c("avg.sent","volume"))
BTC_sent.26.sent

BTC_sent.27<-subset(BTC_sent,BTC_sent$date >= "2021-05-27 23:00:00" &
                      BTC_sent$date <= "2021-05-27 23:59:59")
BTC_sent.27$index<-seq(1,nrow(BTC_sent.27),by=1)
BTC_sent.27.sent<-mean(BTC_sent.27$compound,na.rm = TRUE)
BTC_sent.27.sent<-cbind(BTC_sent.27.sent,BTC_sent.27[nrow(BTC_sent.27),4])
colnames(BTC_sent.27.sent)<-paste(c("avg.sent","volume"))
BTC_sent.27.sent

BTC_sent.28<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 00:00:00" &
                      BTC_sent$date <= "2021-05-28 00:59:59")
BTC_sent.28$index<-seq(1,nrow(BTC_sent.28),by=1)
BTC_sent.28.sent<-mean(BTC_sent.28$compound,na.rm = TRUE)
BTC_sent.28.sent<-cbind(BTC_sent.28.sent,BTC_sent.28[nrow(BTC_sent.28),4])
colnames(BTC_sent.28.sent)<-paste(c("avg.sent","volume"))
BTC_sent.28.sent

BTC_sent.29<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 01:00:00" &
                      BTC_sent$date <= "2021-05-28 01:59:59")
BTC_sent.29$index<-seq(1,nrow(BTC_sent.29),by=1)
BTC_sent.29.sent<-mean(BTC_sent.29$compound,na.rm = TRUE)
BTC_sent.29.sent<-cbind(BTC_sent.29.sent,BTC_sent.29[nrow(BTC_sent.29),4])
colnames(BTC_sent.29.sent)<-paste(c("avg.sent","volume"))
BTC_sent.29.sent

BTC_sent.30<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 02:00:00" &
                      BTC_sent$date <= "2021-05-28 02:59:59")
BTC_sent.30$index<-seq(1,nrow(BTC_sent.30),by=1)
BTC_sent.30.sent<-mean(BTC_sent.30$compound,na.rm = TRUE)
BTC_sent.30.sent<-cbind(BTC_sent.30.sent,BTC_sent.30[nrow(BTC_sent.30),4])
colnames(BTC_sent.30.sent)<-paste(c("avg.sent","volume"))
BTC_sent.30.sent

BTC_sent.31<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 03:00:00" &
                      BTC_sent$date <= "2021-05-28 03:59:59")
BTC_sent.31$index<-seq(1,nrow(BTC_sent.31),by=1)
BTC_sent.31.sent<-mean(BTC_sent.31$compound,na.rm = TRUE)
BTC_sent.31.sent<-cbind(BTC_sent.31.sent,BTC_sent.31[nrow(BTC_sent.31),4])
colnames(BTC_sent.31.sent)<-paste(c("avg.sent","volume"))
BTC_sent.31.sent

BTC_sent.32<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 04:00:00" &
                      BTC_sent$date <= "2021-05-28 04:59:59")
BTC_sent.32$index<-seq(1,nrow(BTC_sent.32),by=1)
BTC_sent.32.sent<-mean(BTC_sent.32$compound,na.rm = TRUE)
BTC_sent.32.sent<-cbind(BTC_sent.32.sent,BTC_sent.32[nrow(BTC_sent.32),4])
colnames(BTC_sent.32.sent)<-paste(c("avg.sent","volume"))
BTC_sent.32.sent

BTC_sent.33<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 05:00:00" &
                      BTC_sent$date <= "2021-05-28 05:59:59")
BTC_sent.33$index<-seq(1,nrow(BTC_sent.33),by=1)
BTC_sent.33.sent<-mean(BTC_sent.33$compound,na.rm = TRUE)
BTC_sent.33.sent<-cbind(BTC_sent.33.sent,BTC_sent.33[nrow(BTC_sent.33),4])
colnames(BTC_sent.33.sent)<-paste(c("avg.sent","volume"))
BTC_sent.33.sent

BTC_sent.34<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 06:00:00" &
                      BTC_sent$date <= "2021-05-28 06:59:59")
BTC_sent.34$index<-seq(1,nrow(BTC_sent.34),by=1)
BTC_sent.34.sent<-mean(BTC_sent.34$compound,na.rm = TRUE)
BTC_sent.34.sent<-cbind(BTC_sent.34.sent,BTC_sent.34[nrow(BTC_sent.34),4])
colnames(BTC_sent.34.sent)<-paste(c("avg.sent","volume"))
BTC_sent.34.sent

BTC_sent.35<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 07:00:00" &
                      BTC_sent$date <= "2021-05-28 07:59:59")
BTC_sent.35$index<-seq(1,nrow(BTC_sent.35),by=1)
BTC_sent.35.sent<-mean(BTC_sent.35$compound,na.rm = TRUE)
BTC_sent.35.sent<-cbind(BTC_sent.35.sent,BTC_sent.35[nrow(BTC_sent.35),4])
colnames(BTC_sent.35.sent)<-paste(c("avg.sent","volume"))
BTC_sent.35.sent

BTC_sent.36<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 08:00:00" &
                      BTC_sent$date <= "2021-05-28 08:59:59")
BTC_sent.36$index<-seq(1,nrow(BTC_sent.36),by=1)
BTC_sent.36.sent<-mean(BTC_sent.36$compound,na.rm = TRUE)
BTC_sent.36.sent<-cbind(BTC_sent.36.sent,BTC_sent.36[nrow(BTC_sent.36),4])
colnames(BTC_sent.36.sent)<-paste(c("avg.sent","volume"))
BTC_sent.36.sent

BTC_sent.37<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 09:00:00" &
                      BTC_sent$date <= "2021-05-28 09:59:59")
BTC_sent.37$index<-seq(1,nrow(BTC_sent.37),by=1)
BTC_sent.37.sent<-mean(BTC_sent.37$compound,na.rm = TRUE)
BTC_sent.37.sent<-cbind(BTC_sent.37.sent,BTC_sent.37[nrow(BTC_sent.37),4])
colnames(BTC_sent.37.sent)<-paste(c("avg.sent","volume"))
BTC_sent.37.sent

BTC_sent.38<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 10:00:00" &
                      BTC_sent$date <= "2021-05-28 10:59:59")
BTC_sent.38$index<-seq(1,nrow(BTC_sent.38),by=1)
BTC_sent.38.sent<-mean(BTC_sent.38$compound,na.rm = TRUE)
BTC_sent.38.sent<-cbind(BTC_sent.38.sent,BTC_sent.38[nrow(BTC_sent.38),4])
colnames(BTC_sent.38.sent)<-paste(c("avg.sent","volume"))
BTC_sent.38.sent

BTC_sent.39<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 11:00:00" &
                      BTC_sent$date <= "2021-05-28 11:59:59")
BTC_sent.39$index<-seq(1,nrow(BTC_sent.39),by=1)
BTC_sent.39.sent<-mean(BTC_sent.39$compound,na.rm = TRUE)
BTC_sent.39.sent<-cbind(BTC_sent.39.sent,BTC_sent.39[nrow(BTC_sent.39),4])
colnames(BTC_sent.39.sent)<-paste(c("avg.sent","volume"))
BTC_sent.39.sent

BTC_sent.40<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 12:00:00" &
                      BTC_sent$date <= "2021-05-28 12:59:59")
BTC_sent.40$index<-seq(1,nrow(BTC_sent.40),by=1)
BTC_sent.40.sent<-mean(BTC_sent.40$compound,na.rm = TRUE)
BTC_sent.40.sent<-cbind(BTC_sent.40.sent,BTC_sent.40[nrow(BTC_sent.40),4])
colnames(BTC_sent.40.sent)<-paste(c("avg.sent","volume"))
BTC_sent.40.sent

BTC_sent.41<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 13:00:00" &
                      BTC_sent$date <= "2021-05-28 13:59:59")
BTC_sent.41$index<-seq(1,nrow(BTC_sent.41),by=1)
BTC_sent.41.sent<-mean(BTC_sent.41$compound,na.rm = TRUE)
BTC_sent.41.sent<-cbind(BTC_sent.41.sent,BTC_sent.41[nrow(BTC_sent.41),4])
colnames(BTC_sent.41.sent)<-paste(c("avg.sent","volume"))
BTC_sent.41.sent

BTC_sent.42<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 14:00:00" &
                      BTC_sent$date <= "2021-05-28 14:59:59")
BTC_sent.42$index<-seq(1,nrow(BTC_sent.42),by=1)
BTC_sent.42.sent<-mean(BTC_sent.42$compound,na.rm = TRUE)
BTC_sent.42.sent<-cbind(BTC_sent.42.sent,BTC_sent.42[nrow(BTC_sent.42),4])
colnames(BTC_sent.42.sent)<-paste(c("avg.sent","volume"))
BTC_sent.42.sent

BTC_sent.43<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 15:00:00" &
                      BTC_sent$date <= "2021-05-28 15:59:59")
BTC_sent.43$index<-seq(1,nrow(BTC_sent.43),by=1)
BTC_sent.43.sent<-mean(BTC_sent.43$compound,na.rm = TRUE)
BTC_sent.43.sent<-cbind(BTC_sent.43.sent,BTC_sent.43[nrow(BTC_sent.43),4])
colnames(BTC_sent.43.sent)<-paste(c("avg.sent","volume"))
BTC_sent.43.sent

BTC_sent.44<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 16:00:00" &
                      BTC_sent$date <= "2021-05-28 16:59:59")
BTC_sent.44$index<-seq(1,nrow(BTC_sent.44),by=1)
BTC_sent.44.sent<-mean(BTC_sent.44$compound,na.rm = TRUE)
BTC_sent.44.sent<-cbind(BTC_sent.44.sent,BTC_sent.44[nrow(BTC_sent.44),4])
colnames(BTC_sent.44.sent)<-paste(c("avg.sent","volume"))
BTC_sent.44.sent

BTC_sent.45<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 17:00:00" &
                      BTC_sent$date <= "2021-05-28 17:59:59")
BTC_sent.45$index<-seq(1,nrow(BTC_sent.45),by=1)
BTC_sent.45.sent<-mean(BTC_sent.45$compound,na.rm = TRUE)
BTC_sent.45.sent<-cbind(BTC_sent.45.sent,BTC_sent.45[nrow(BTC_sent.45),4])
colnames(BTC_sent.45.sent)<-paste(c("avg.sent","volume"))
BTC_sent.45.sent

BTC_sent.46<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 18:00:00" &
                      BTC_sent$date <= "2021-05-28 18:59:59")
BTC_sent.46$index<-seq(1,nrow(BTC_sent.46),by=1)
BTC_sent.46.sent<-mean(BTC_sent.46$compound,na.rm = TRUE)
BTC_sent.46.sent<-cbind(BTC_sent.46.sent,BTC_sent.46[nrow(BTC_sent.46),4])
colnames(BTC_sent.46.sent)<-paste(c("avg.sent","volume"))
BTC_sent.46.sent

BTC_sent.47<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 19:00:00" &
                      BTC_sent$date <= "2021-05-28 19:59:59")
BTC_sent.47$index<-seq(1,nrow(BTC_sent.47),by=1)
BTC_sent.47.sent<-mean(BTC_sent.47$compound,na.rm = TRUE)
BTC_sent.47.sent<-cbind(BTC_sent.47.sent,BTC_sent.47[nrow(BTC_sent.47),4])
colnames(BTC_sent.47.sent)<-paste(c("avg.sent","volume"))
BTC_sent.47.sent

BTC_sent.48<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 20:00:00" &
                      BTC_sent$date <= "2021-05-28 20:59:59")
BTC_sent.48$index<-seq(1,nrow(BTC_sent.48),by=1)
BTC_sent.48.sent<-mean(BTC_sent.48$compound,na.rm = TRUE)
BTC_sent.48.sent<-cbind(BTC_sent.48.sent,BTC_sent.48[nrow(BTC_sent.48),4])
colnames(BTC_sent.48.sent)<-paste(c("avg.sent","volume"))
BTC_sent.48.sent

BTC_sent.49<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 21:00:00" &
                      BTC_sent$date <= "2021-05-28 21:59:59")
BTC_sent.49$index<-seq(1,nrow(BTC_sent.49),by=1)
BTC_sent.49.sent<-mean(BTC_sent.49$compound,na.rm = TRUE)
BTC_sent.49.sent<-cbind(BTC_sent.49.sent,BTC_sent.49[nrow(BTC_sent.49),4])
colnames(BTC_sent.49.sent)<-paste(c("avg.sent","volume"))
BTC_sent.49.sent

BTC_sent.50<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 22:00:00" &
                      BTC_sent$date <= "2021-05-28 22:59:59")
BTC_sent.50$index<-seq(1,nrow(BTC_sent.50),by=1)
BTC_sent.50.sent<-mean(BTC_sent.50$compound,na.rm = TRUE)
BTC_sent.50.sent<-cbind(BTC_sent.50.sent,BTC_sent.50[nrow(BTC_sent.50),4])
colnames(BTC_sent.50.sent)<-paste(c("avg.sent","volume"))
BTC_sent.50.sent

BTC_sent.51<-subset(BTC_sent,BTC_sent$date >= "2021-05-28 23:00:00" &
                      BTC_sent$date <= "2021-05-28 23:59:59")
BTC_sent.51$index<-seq(1,nrow(BTC_sent.51),by=1)
BTC_sent.51.sent<-mean(BTC_sent.51$compound,na.rm = TRUE)
BTC_sent.51.sent<-cbind(BTC_sent.51.sent,BTC_sent.51[nrow(BTC_sent.51),4])
colnames(BTC_sent.51.sent)<-paste(c("avg.sent","volume"))
BTC_sent.51.sent

BTC_sent.52<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 00:00:00" &
                      BTC_sent$date <= "2021-05-29 00:59:59")
BTC_sent.52$index<-seq(1,nrow(BTC_sent.52),by=1)
BTC_sent.52.sent<-mean(BTC_sent.52$compound,na.rm = TRUE)
BTC_sent.52.sent<-cbind(BTC_sent.52.sent,BTC_sent.52[nrow(BTC_sent.52),4])
colnames(BTC_sent.52.sent)<-paste(c("avg.sent","volume"))
BTC_sent.52.sent

BTC_sent.53<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 01:00:00" &
                      BTC_sent$date <= "2021-05-29 01:59:59")
BTC_sent.53$index<-seq(1,nrow(BTC_sent.53),by=1)
BTC_sent.53.sent<-mean(BTC_sent.53$compound,na.rm = TRUE)
BTC_sent.53.sent<-cbind(BTC_sent.53.sent,BTC_sent.53[nrow(BTC_sent.53),4])
colnames(BTC_sent.53.sent)<-paste(c("avg.sent","volume"))
BTC_sent.53.sent

BTC_sent.54<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 02:00:00" &
                      BTC_sent$date <= "2021-05-29 02:59:59")
BTC_sent.54$index<-seq(1,nrow(BTC_sent.54),by=1)
BTC_sent.54.sent<-mean(BTC_sent.54$compound,na.rm = TRUE)
BTC_sent.54.sent<-cbind(BTC_sent.54.sent,BTC_sent.54[nrow(BTC_sent.54),4])
colnames(BTC_sent.54.sent)<-paste(c("avg.sent","volume"))
BTC_sent.54.sent

BTC_sent.55<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 03:00:00" &
                      BTC_sent$date <= "2021-05-29 03:59:59")
BTC_sent.55$index<-seq(1,nrow(BTC_sent.55),by=1)
BTC_sent.55.sent<-mean(BTC_sent.55$compound,na.rm = TRUE)
BTC_sent.55.sent<-cbind(BTC_sent.55.sent,BTC_sent.55[nrow(BTC_sent.55),4])
colnames(BTC_sent.55.sent)<-paste(c("avg.sent","volume"))
BTC_sent.55.sent

BTC_sent.56<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 04:00:00" &
                      BTC_sent$date <= "2021-05-29 04:59:59")
BTC_sent.56$index<-seq(1,nrow(BTC_sent.56),by=1)
BTC_sent.56.sent<-mean(BTC_sent.56$compound,na.rm = TRUE)
BTC_sent.56.sent<-cbind(BTC_sent.56.sent,BTC_sent.56[nrow(BTC_sent.56),4])
colnames(BTC_sent.56.sent)<-paste(c("avg.sent","volume"))
BTC_sent.56.sent

BTC_sent.57<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 05:00:00" &
                      BTC_sent$date <= "2021-05-29 05:59:59")
BTC_sent.57$index<-seq(1,nrow(BTC_sent.57),by=1)
BTC_sent.57.sent<-mean(BTC_sent.57$compound,na.rm = TRUE)
BTC_sent.57.sent<-cbind(BTC_sent.57.sent,BTC_sent.57[nrow(BTC_sent.57),4])
colnames(BTC_sent.57.sent)<-paste(c("avg.sent","volume"))
BTC_sent.57.sent

BTC_sent.58<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 06:00:00" &
                      BTC_sent$date <= "2021-05-29 06:59:59")
BTC_sent.58$index<-seq(1,nrow(BTC_sent.58),by=1)
BTC_sent.58.sent<-mean(BTC_sent.58$compound,na.rm = TRUE)
BTC_sent.58.sent<-cbind(BTC_sent.58.sent,BTC_sent.58[nrow(BTC_sent.58),4])
colnames(BTC_sent.58.sent)<-paste(c("avg.sent","volume"))
BTC_sent.58.sent

BTC_sent.59<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 07:00:00" &
                      BTC_sent$date <= "2021-05-29 07:59:59")
BTC_sent.59$index<-seq(1,nrow(BTC_sent.59),by=1)
BTC_sent.59.sent<-mean(BTC_sent.59$compound,na.rm = TRUE)
BTC_sent.59.sent<-cbind(BTC_sent.59.sent,BTC_sent.59[nrow(BTC_sent.59),4])
colnames(BTC_sent.59.sent)<-paste(c("avg.sent","volume"))
BTC_sent.59.sent

BTC_sent.60<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 08:00:00" &
                      BTC_sent$date <= "2021-05-29 08:59:59")
BTC_sent.60$index<-seq(1,nrow(BTC_sent.60),by=1)
BTC_sent.60.sent<-mean(BTC_sent.60$compound,na.rm = TRUE)
BTC_sent.60.sent<-cbind(BTC_sent.60.sent,BTC_sent.60[nrow(BTC_sent.60),4])
colnames(BTC_sent.60.sent)<-paste(c("avg.sent","volume"))
BTC_sent.60.sent

BTC_sent.61<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 09:00:00" &
                      BTC_sent$date <= "2021-05-29 09:59:59")
BTC_sent.61$index<-seq(1,nrow(BTC_sent.61),by=1)
BTC_sent.61.sent<-mean(BTC_sent.61$compound,na.rm = TRUE)
BTC_sent.61.sent<-cbind(BTC_sent.61.sent,BTC_sent.61[nrow(BTC_sent.61),4])
colnames(BTC_sent.61.sent)<-paste(c("avg.sent","volume"))
BTC_sent.61.sent

BTC_sent.62<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 10:00:00" &
                      BTC_sent$date <= "2021-05-29 10:59:59")
BTC_sent.62$index<-seq(1,nrow(BTC_sent.62),by=1)
BTC_sent.62.sent<-mean(BTC_sent.62$compound,na.rm = TRUE)
BTC_sent.62.sent<-cbind(BTC_sent.62.sent,BTC_sent.62[nrow(BTC_sent.62),4])
colnames(BTC_sent.62.sent)<-paste(c("avg.sent","volume"))
BTC_sent.62.sent

BTC_sent.63<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 11:00:00" &
                      BTC_sent$date <= "2021-05-29 11:59:59")
BTC_sent.63$index<-seq(1,nrow(BTC_sent.63),by=1)
BTC_sent.63.sent<-mean(BTC_sent.63$compound,na.rm = TRUE)
BTC_sent.63.sent<-cbind(BTC_sent.63.sent,BTC_sent.63[nrow(BTC_sent.63),4])
colnames(BTC_sent.63.sent)<-paste(c("avg.sent","volume"))
BTC_sent.63.sent

BTC_sent.64<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 12:00:00" &
                      BTC_sent$date <= "2021-05-29 12:59:59")
BTC_sent.64$index<-seq(1,nrow(BTC_sent.64),by=1)
BTC_sent.64.sent<-mean(BTC_sent.64$compound,na.rm = TRUE)
BTC_sent.64.sent<-cbind(BTC_sent.64.sent,BTC_sent.64[nrow(BTC_sent.64),4])
colnames(BTC_sent.64.sent)<-paste(c("avg.sent","volume"))
BTC_sent.64.sent

BTC_sent.65<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 13:00:00" &
                      BTC_sent$date <= "2021-05-29 13:59:59")
BTC_sent.65$index<-seq(1,nrow(BTC_sent.65),by=1)
BTC_sent.65.sent<-mean(BTC_sent.65$compound,na.rm = TRUE)
BTC_sent.65.sent<-cbind(BTC_sent.65.sent,BTC_sent.65[nrow(BTC_sent.65),4])
colnames(BTC_sent.65.sent)<-paste(c("avg.sent","volume"))
BTC_sent.65.sent

BTC_sent.66<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 14:00:00" &
                      BTC_sent$date <= "2021-05-29 14:59:59")
BTC_sent.66$index<-seq(1,nrow(BTC_sent.66),by=1)
BTC_sent.66.sent<-mean(BTC_sent.66$compound,na.rm = TRUE)
BTC_sent.66.sent<-cbind(BTC_sent.66.sent,BTC_sent.66[nrow(BTC_sent.66),4])
colnames(BTC_sent.66.sent)<-paste(c("avg.sent","volume"))
BTC_sent.66.sent

BTC_sent.67<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 15:00:00" &
                      BTC_sent$date <= "2021-05-29 15:59:59")
BTC_sent.67$index<-seq(1,nrow(BTC_sent.67),by=1)
BTC_sent.67.sent<-mean(BTC_sent.67$compound,na.rm = TRUE)
BTC_sent.67.sent<-cbind(BTC_sent.67.sent,BTC_sent.67[nrow(BTC_sent.67),4])
colnames(BTC_sent.67.sent)<-paste(c("avg.sent","volume"))
BTC_sent.67.sent

BTC_sent.68<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 16:00:00" &
                      BTC_sent$date <= "2021-05-29 16:59:59")
BTC_sent.68$index<-seq(1,nrow(BTC_sent.68),by=1)
BTC_sent.68.sent<-mean(BTC_sent.68$compound,na.rm = TRUE)
BTC_sent.68.sent<-cbind(BTC_sent.68.sent,BTC_sent.68[nrow(BTC_sent.68),4])
colnames(BTC_sent.68.sent)<-paste(c("avg.sent","volume"))
BTC_sent.68.sent

BTC_sent.69<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 17:00:00" &
                      BTC_sent$date <= "2021-05-29 17:59:59")
BTC_sent.69$index<-seq(1,nrow(BTC_sent.69),by=1)
BTC_sent.69.sent<-mean(BTC_sent.69$compound,na.rm = TRUE)
BTC_sent.69.sent<-cbind(BTC_sent.69.sent,BTC_sent.69[nrow(BTC_sent.69),4])
colnames(BTC_sent.69.sent)<-paste(c("avg.sent","volume"))
BTC_sent.69.sent

BTC_sent.70<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 18:00:00" &
                      BTC_sent$date <= "2021-05-29 18:59:59")
BTC_sent.70$index<-seq(1,nrow(BTC_sent.70),by=1)
BTC_sent.70.sent<-mean(BTC_sent.70$compound,na.rm = TRUE)
BTC_sent.70.sent<-cbind(BTC_sent.70.sent,BTC_sent.70[nrow(BTC_sent.70),4])
colnames(BTC_sent.70.sent)<-paste(c("avg.sent","volume"))
BTC_sent.70.sent

BTC_sent.71<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 19:00:00" &
                      BTC_sent$date <= "2021-05-29 19:59:59")
BTC_sent.71$index<-seq(1,nrow(BTC_sent.71),by=1)
BTC_sent.71.sent<-mean(BTC_sent.71$compound,na.rm = TRUE)
BTC_sent.71.sent<-cbind(BTC_sent.71.sent,BTC_sent.71[nrow(BTC_sent.71),4])
colnames(BTC_sent.71.sent)<-paste(c("avg.sent","volume"))
BTC_sent.71.sent

BTC_sent.72<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 20:00:00" &
                      BTC_sent$date <= "2021-05-29 20:59:59")
BTC_sent.72$index<-seq(1,nrow(BTC_sent.72),by=1)
BTC_sent.72.sent<-mean(BTC_sent.72$compound,na.rm = TRUE)
BTC_sent.72.sent<-cbind(BTC_sent.72.sent,BTC_sent.72[nrow(BTC_sent.72),4])
colnames(BTC_sent.72.sent)<-paste(c("avg.sent","volume"))
BTC_sent.72.sent

BTC_sent.73<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 21:00:00" &
                      BTC_sent$date <= "2021-05-29 21:59:59")
BTC_sent.73$index<-seq(1,nrow(BTC_sent.73),by=1)
BTC_sent.73.sent<-mean(BTC_sent.73$compound,na.rm = TRUE)
BTC_sent.73.sent<-cbind(BTC_sent.73.sent,BTC_sent.73[nrow(BTC_sent.73),4])
colnames(BTC_sent.73.sent)<-paste(c("avg.sent","volume"))
BTC_sent.73.sent

BTC_sent.74<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 22:00:00" &
                      BTC_sent$date <= "2021-05-29 22:59:59")
BTC_sent.74$index<-seq(1,nrow(BTC_sent.74),by=1)
BTC_sent.74.sent<-mean(BTC_sent.74$compound,na.rm = TRUE)
BTC_sent.74.sent<-cbind(BTC_sent.74.sent,BTC_sent.74[nrow(BTC_sent.74),4])
colnames(BTC_sent.74.sent)<-paste(c("avg.sent","volume"))
BTC_sent.74.sent

BTC_sent.75<-subset(BTC_sent,BTC_sent$date >= "2021-05-29 23:00:00" &
                      BTC_sent$date <= "2021-05-29 23:59:59")
BTC_sent.75$index<-seq(1,nrow(BTC_sent.75),by=1)
BTC_sent.75.sent<-mean(BTC_sent.75$compound,na.rm = TRUE)
BTC_sent.75.sent<-cbind(BTC_sent.75.sent,BTC_sent.75[nrow(BTC_sent.75),4])
colnames(BTC_sent.75.sent)<-paste(c("avg.sent","volume"))
BTC_sent.75.sent

BTC_sent.76<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 00:00:00" &
                      BTC_sent$date <= "2021-05-30 00:59:59")
BTC_sent.76$index<-seq(1,nrow(BTC_sent.76),by=1)
BTC_sent.76.sent<-mean(BTC_sent.76$compound,na.rm = TRUE)
BTC_sent.76.sent<-cbind(BTC_sent.76.sent,BTC_sent.76[nrow(BTC_sent.76),4])
colnames(BTC_sent.76.sent)<-paste(c("avg.sent","volume"))
BTC_sent.76.sent

BTC_sent.77<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 01:00:00" &
                      BTC_sent$date <= "2021-05-30 01:59:59")
BTC_sent.77$index<-seq(1,nrow(BTC_sent.77),by=1)
BTC_sent.77.sent<-mean(BTC_sent.77$compound,na.rm = TRUE)
BTC_sent.77.sent<-cbind(BTC_sent.77.sent,BTC_sent.77[nrow(BTC_sent.77),4])
colnames(BTC_sent.77.sent)<-paste(c("avg.sent","volume"))
BTC_sent.77.sent

BTC_sent.78<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 02:00:00" &
                      BTC_sent$date <= "2021-05-30 02:59:59")
BTC_sent.78$index<-seq(1,nrow(BTC_sent.78),by=1)
BTC_sent.78.sent<-mean(BTC_sent.78$compound,na.rm = TRUE)
BTC_sent.78.sent<-cbind(BTC_sent.78.sent,BTC_sent.78[nrow(BTC_sent.78),4])
colnames(BTC_sent.78.sent)<-paste(c("avg.sent","volume"))
BTC_sent.78.sent

BTC_sent.79<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 03:00:00" &
                      BTC_sent$date <= "2021-05-30 03:59:59")
BTC_sent.79$index<-seq(1,nrow(BTC_sent.79),by=1)
BTC_sent.79.sent<-mean(BTC_sent.79$compound,na.rm = TRUE)
BTC_sent.79.sent<-cbind(BTC_sent.79.sent,BTC_sent.79[nrow(BTC_sent.79),4])
colnames(BTC_sent.79.sent)<-paste(c("avg.sent","volume"))
BTC_sent.79.sent

BTC_sent.80<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 04:00:00" &
                      BTC_sent$date <= "2021-05-30 04:59:59")
BTC_sent.80$index<-seq(1,nrow(BTC_sent.80),by=1)
BTC_sent.80.sent<-mean(BTC_sent.80$compound,na.rm = TRUE)
BTC_sent.80.sent<-cbind(BTC_sent.80.sent,BTC_sent.80[nrow(BTC_sent.80),4])
colnames(BTC_sent.80.sent)<-paste(c("avg.sent","volume"))
BTC_sent.80.sent

BTC_sent.81<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 05:00:00" &
                      BTC_sent$date <= "2021-05-30 05:59:59")
BTC_sent.81$index<-seq(1,nrow(BTC_sent.81),by=1)
BTC_sent.81.sent<-mean(BTC_sent.81$compound,na.rm = TRUE)
BTC_sent.81.sent<-cbind(BTC_sent.81.sent,BTC_sent.81[nrow(BTC_sent.81),4])
colnames(BTC_sent.81.sent)<-paste(c("avg.sent","volume"))
BTC_sent.81.sent

BTC_sent.82<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 06:00:00" &
                      BTC_sent$date <= "2021-05-30 06:59:59")
BTC_sent.82$index<-seq(1,nrow(BTC_sent.82),by=1)
BTC_sent.82.sent<-mean(BTC_sent.82$compound,na.rm = TRUE)
BTC_sent.82.sent<-cbind(BTC_sent.82.sent,BTC_sent.82[nrow(BTC_sent.82),4])
colnames(BTC_sent.82.sent)<-paste(c("avg.sent","volume"))
BTC_sent.82.sent

BTC_sent.83<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 07:00:00" &
                      BTC_sent$date <= "2021-05-30 07:59:59")
BTC_sent.83$index<-seq(1,nrow(BTC_sent.83),by=1)
BTC_sent.83.sent<-mean(BTC_sent.83$compound,na.rm = TRUE)
BTC_sent.83.sent<-cbind(BTC_sent.83.sent,BTC_sent.83[nrow(BTC_sent.83),4])
colnames(BTC_sent.83.sent)<-paste(c("avg.sent","volume"))
BTC_sent.83.sent

BTC_sent.84<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 08:00:00" &
                      BTC_sent$date <= "2021-05-30 08:59:59")
BTC_sent.84$index<-seq(1,nrow(BTC_sent.84),by=1)
BTC_sent.84.sent<-mean(BTC_sent.84$compound,na.rm = TRUE)
BTC_sent.84.sent<-cbind(BTC_sent.84.sent,BTC_sent.84[nrow(BTC_sent.84),4])
colnames(BTC_sent.84.sent)<-paste(c("avg.sent","volume"))
BTC_sent.84.sent

BTC_sent.85<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 09:00:00" &
                      BTC_sent$date <= "2021-05-30 09:59:59")
BTC_sent.85$index<-seq(1,nrow(BTC_sent.85),by=1)
BTC_sent.85.sent<-mean(BTC_sent.85$compound,na.rm = TRUE)
BTC_sent.85.sent<-cbind(BTC_sent.85.sent,BTC_sent.85[nrow(BTC_sent.85),4])
colnames(BTC_sent.85.sent)<-paste(c("avg.sent","volume"))
BTC_sent.85.sent

BTC_sent.86<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 10:00:00" &
                      BTC_sent$date <= "2021-05-30 10:59:59")
BTC_sent.86$index<-seq(1,nrow(BTC_sent.86),by=1)
BTC_sent.86.sent<-mean(BTC_sent.86$compound,na.rm = TRUE)
BTC_sent.86.sent<-cbind(BTC_sent.86.sent,BTC_sent.86[nrow(BTC_sent.86),4])
colnames(BTC_sent.86.sent)<-paste(c("avg.sent","volume"))
BTC_sent.86.sent

BTC_sent.87<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 11:00:00" &
                      BTC_sent$date <= "2021-05-30 11:59:59")
BTC_sent.87$index<-seq(1,nrow(BTC_sent.87),by=1)
BTC_sent.87.sent<-mean(BTC_sent.87$compound,na.rm = TRUE)
BTC_sent.87.sent<-cbind(BTC_sent.87.sent,BTC_sent.87[nrow(BTC_sent.87),4])
colnames(BTC_sent.87.sent)<-paste(c("avg.sent","volume"))
BTC_sent.87.sent

BTC_sent.88<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 12:00:00" &
                      BTC_sent$date <= "2021-05-30 12:59:59")
BTC_sent.88$index<-seq(1,nrow(BTC_sent.88),by=1)
BTC_sent.88.sent<-mean(BTC_sent.88$compound,na.rm = TRUE)
BTC_sent.88.sent<-cbind(BTC_sent.88.sent,BTC_sent.88[nrow(BTC_sent.88),4])
colnames(BTC_sent.88.sent)<-paste(c("avg.sent","volume"))
BTC_sent.88.sent

BTC_sent.89<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 13:00:00" &
                      BTC_sent$date <= "2021-05-30 13:59:59")
BTC_sent.89$index<-seq(1,nrow(BTC_sent.89),by=1)
BTC_sent.89.sent<-mean(BTC_sent.89$compound,na.rm = TRUE)
BTC_sent.89.sent<-cbind(BTC_sent.89.sent,BTC_sent.89[nrow(BTC_sent.89),4])
colnames(BTC_sent.89.sent)<-paste(c("avg.sent","volume"))
BTC_sent.89.sent

BTC_sent.90<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 14:00:00" &
                      BTC_sent$date <= "2021-05-30 14:59:59")
BTC_sent.90$index<-seq(1,nrow(BTC_sent.90),by=1)
BTC_sent.90.sent<-mean(BTC_sent.90$compound,na.rm = TRUE)
BTC_sent.90.sent<-cbind(BTC_sent.90.sent,BTC_sent.90[nrow(BTC_sent.90),4])
colnames(BTC_sent.90.sent)<-paste(c("avg.sent","volume"))
BTC_sent.90.sent

BTC_sent.91<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 15:00:00" &
                      BTC_sent$date <= "2021-05-30 15:59:59")
BTC_sent.91$index<-seq(1,nrow(BTC_sent.91),by=1)
BTC_sent.91.sent<-mean(BTC_sent.91$compound,na.rm = TRUE)
BTC_sent.91.sent<-cbind(BTC_sent.91.sent,BTC_sent.91[nrow(BTC_sent.91),4])
colnames(BTC_sent.91.sent)<-paste(c("avg.sent","volume"))
BTC_sent.91.sent

BTC_sent.92<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 16:00:00" &
                      BTC_sent$date <= "2021-05-30 16:59:59")
BTC_sent.92$index<-seq(1,nrow(BTC_sent.92),by=1)
BTC_sent.92.sent<-mean(BTC_sent.92$compound,na.rm = TRUE)
BTC_sent.92.sent<-cbind(BTC_sent.92.sent,BTC_sent.92[nrow(BTC_sent.92),4])
colnames(BTC_sent.92.sent)<-paste(c("avg.sent","volume"))
BTC_sent.92.sent

BTC_sent.93<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 17:00:00" &
                      BTC_sent$date <= "2021-05-30 17:59:59")
BTC_sent.93$index<-seq(1,nrow(BTC_sent.93),by=1)
BTC_sent.93.sent<-mean(BTC_sent.93$compound,na.rm = TRUE)
BTC_sent.93.sent<-cbind(BTC_sent.93.sent,BTC_sent.93[nrow(BTC_sent.93),4])
colnames(BTC_sent.93.sent)<-paste(c("avg.sent","volume"))
BTC_sent.93.sent

BTC_sent.94<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 18:00:00" &
                      BTC_sent$date <= "2021-05-30 18:59:59")
BTC_sent.94$index<-seq(1,nrow(BTC_sent.94),by=1)
BTC_sent.94.sent<-mean(BTC_sent.94$compound,na.rm = TRUE)
BTC_sent.94.sent<-cbind(BTC_sent.94.sent,BTC_sent.94[nrow(BTC_sent.94),4])
colnames(BTC_sent.94.sent)<-paste(c("avg.sent","volume"))
BTC_sent.94.sent

BTC_sent.95<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 19:00:00" &
                      BTC_sent$date <= "2021-05-30 19:59:59")
BTC_sent.95$index<-seq(1,nrow(BTC_sent.95),by=1)
BTC_sent.95.sent<-mean(BTC_sent.95$compound,na.rm = TRUE)
BTC_sent.95.sent<-cbind(BTC_sent.95.sent,BTC_sent.95[nrow(BTC_sent.95),4])
colnames(BTC_sent.95.sent)<-paste(c("avg.sent","volume"))
BTC_sent.95.sent

BTC_sent.96<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 20:00:00" &
                      BTC_sent$date <= "2021-05-30 20:59:59")
BTC_sent.96$index<-seq(1,nrow(BTC_sent.96),by=1)
BTC_sent.96.sent<-mean(BTC_sent.96$compound,na.rm = TRUE)
BTC_sent.96.sent<-cbind(BTC_sent.96.sent,BTC_sent.96[nrow(BTC_sent.96),4])
colnames(BTC_sent.96.sent)<-paste(c("avg.sent","volume"))
BTC_sent.96.sent

BTC_sent.97<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 21:00:00" &
                      BTC_sent$date <= "2021-05-30 21:59:59")
BTC_sent.97$index<-seq(1,nrow(BTC_sent.97),by=1)
BTC_sent.97.sent<-mean(BTC_sent.97$compound,na.rm = TRUE)
BTC_sent.97.sent<-cbind(BTC_sent.97.sent,BTC_sent.97[nrow(BTC_sent.97),4])
colnames(BTC_sent.97.sent)<-paste(c("avg.sent","volume"))
BTC_sent.97.sent

BTC_sent.98<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 22:00:00" &
                      BTC_sent$date <= "2021-05-30 22:59:59")
BTC_sent.98$index<-seq(1,nrow(BTC_sent.98),by=1)
BTC_sent.98.sent<-mean(BTC_sent.98$compound,na.rm = TRUE)
BTC_sent.98.sent<-cbind(BTC_sent.98.sent,BTC_sent.98[nrow(BTC_sent.98),4])
colnames(BTC_sent.98.sent)<-paste(c("avg.sent","volume"))
BTC_sent.98.sent

BTC_sent.99<-subset(BTC_sent,BTC_sent$date >= "2021-05-30 23:00:00" &
                      BTC_sent$date <= "2021-05-30 23:59:59")
BTC_sent.99$index<-seq(1,nrow(BTC_sent.99),by=1)
BTC_sent.99.sent<-mean(BTC_sent.99$compound,na.rm = TRUE)
BTC_sent.99.sent<-cbind(BTC_sent.99.sent,BTC_sent.99[nrow(BTC_sent.99),4])
colnames(BTC_sent.99.sent)<-paste(c("avg.sent","volume"))
BTC_sent.99.sent

BTC_sent.100<-subset(BTC_sent,BTC_sent$date >= "2021-05-31 00:00:00" &
                      BTC_sent$date <= "2021-05-31 00:59:59")
BTC_sent.100$index<-seq(1,nrow(BTC_sent.100),by=1)
BTC_sent.100.sent<-mean(BTC_sent.100$compound,na.rm = TRUE)
BTC_sent.100.sent<-cbind(BTC_sent.100.sent,BTC_sent.100[nrow(BTC_sent.100),4])
colnames(BTC_sent.100.sent)<-paste(c("avg.sent","volume"))
BTC_sent.100.sent

BTC_sent.101<-subset(BTC_sent,BTC_sent$date >= "2021-05-31 01:00:00" &
                       BTC_sent$date <= "2021-05-31 01:59:59")
BTC_sent.101$index<-seq(1,nrow(BTC_sent.101),by=1)
BTC_sent.101.sent<-mean(BTC_sent.101$compound,na.rm = TRUE)
BTC_sent.101.sent<-cbind(BTC_sent.101.sent,BTC_sent.101[nrow(BTC_sent.101),4])
colnames(BTC_sent.101.sent)<-paste(c("avg.sent","volume"))
BTC_sent.101.sent

BTC_sent.102<-subset(BTC_sent,BTC_sent$date >= "2021-05-31 02:00:00" &
                       BTC_sent$date <= "2021-05-31 02:59:59")
BTC_sent.102$index<-seq(1,nrow(BTC_sent.102),by=1)
BTC_sent.102.sent<-mean(BTC_sent.102$compound,na.rm = TRUE)
BTC_sent.102.sent<-cbind(BTC_sent.102.sent,BTC_sent.102[nrow(BTC_sent.102),4])
colnames(BTC_sent.102.sent)<-paste(c("avg.sent","volume"))
BTC_sent.102.sent

BTC_sent.103<-subset(BTC_sent,BTC_sent$date >= "2021-05-31 03:00:00" &
                       BTC_sent$date <= "2021-05-31 03:59:59")
BTC_sent.103$index<-seq(1,nrow(BTC_sent.103),by=1)
BTC_sent.103.sent<-mean(BTC_sent.103$compound,na.rm = TRUE)
BTC_sent.103.sent<-cbind(BTC_sent.103.sent,BTC_sent.103[nrow(BTC_sent.103),4])
colnames(BTC_sent.103.sent)<-paste(c("avg.sent","volume"))
BTC_sent.103.sent

BTC_sent.104<-subset(BTC_sent,BTC_sent$date >= "2021-05-31 04:00:00" &
                       BTC_sent$date <= "2021-05-31 04:59:59")
BTC_sent.104$index<-seq(1,nrow(BTC_sent.104),by=1)
BTC_sent.104.sent<-mean(BTC_sent.104$compound,na.rm = TRUE)
BTC_sent.104.sent<-cbind(BTC_sent.104.sent,BTC_sent.104[nrow(BTC_sent.104),4])
colnames(BTC_sent.104.sent)<-paste(c("avg.sent","volume"))
BTC_sent.104.sent

BTC_sent.105<-subset(BTC_sent,BTC_sent$date >= "2021-05-31 05:00:00" &
                       BTC_sent$date <= "2021-05-31 05:59:59")
BTC_sent.105$index<-seq(1,nrow(BTC_sent.105),by=1)
BTC_sent.105.sent<-mean(BTC_sent.105$compound,na.rm = TRUE)
BTC_sent.105.sent<-cbind(BTC_sent.105.sent,BTC_sent.105[nrow(BTC_sent.105),4])
colnames(BTC_sent.105.sent)<-paste(c("avg.sent","volume"))
BTC_sent.105.sent

BTC_sent.106<-subset(BTC_sent,BTC_sent$date >= "2021-05-31 06:00:00" &
                       BTC_sent$date <= "2021-05-31 06:59:59")
BTC_sent.106$index<-seq(1,nrow(BTC_sent.106),by=1)
BTC_sent.106.sent<-mean(BTC_sent.106$compound,na.rm = TRUE)
BTC_sent.106.sent<-cbind(BTC_sent.106.sent,BTC_sent.106[nrow(BTC_sent.106),4])
colnames(BTC_sent.106.sent)<-paste(c("avg.sent","volume"))
BTC_sent.106.sent

BTC_sent.107<-subset(BTC_sent,BTC_sent$date >= "2021-05-31 07:00:00" &
                       BTC_sent$date <= "2021-05-31 07:59:59")
BTC_sent.107$index<-seq(1,nrow(BTC_sent.107),by=1)
BTC_sent.107.sent<-mean(BTC_sent.107$compound,na.rm = TRUE)
BTC_sent.107.sent<-cbind(BTC_sent.107.sent,BTC_sent.107[nrow(BTC_sent.107),4])
colnames(BTC_sent.107.sent)<-paste(c("avg.sent","volume"))
BTC_sent.107.sent

BTC_sent.108<-subset(BTC_sent,BTC_sent$date >= "2021-05-31 08:00:00" &
                       BTC_sent$date <= "2021-05-31 08:59:59")
BTC_sent.108$index<-seq(1,nrow(BTC_sent.108),by=1)
BTC_sent.108.sent<-mean(BTC_sent.108$compound,na.rm = TRUE)
BTC_sent.108.sent<-cbind(BTC_sent.108.sent,BTC_sent.108[nrow(BTC_sent.108),4])
colnames(BTC_sent.108.sent)<-paste(c("avg.sent","volume"))
BTC_sent.108.sent

BTC_sent.109<-subset(BTC_sent,BTC_sent$date >= "2021-05-31 09:00:00" &
                       BTC_sent$date <= "2021-05-31 09:59:59")
BTC_sent.109$index<-seq(1,nrow(BTC_sent.109),by=1)
BTC_sent.109.sent<-mean(BTC_sent.109$compound,na.rm = TRUE)
BTC_sent.109.sent<-cbind(BTC_sent.109.sent,BTC_sent.109[nrow(BTC_sent.109),4])
colnames(BTC_sent.109.sent)<-paste(c("avg.sent","volume"))
BTC_sent.109.sent

BTC_sent.110<-subset(BTC_sent,BTC_sent$date >= "2021-05-31 10:00:00" &
                       BTC_sent$date <= "2021-05-31 10:59:59")
BTC_sent.110$index<-seq(1,nrow(BTC_sent.110),by=1)
BTC_sent.110.sent<-mean(BTC_sent.110$compound,na.rm = TRUE)
BTC_sent.110.sent<-cbind(BTC_sent.110.sent,BTC_sent.110[nrow(BTC_sent.110),4])
colnames(BTC_sent.110.sent)<-paste(c("avg.sent","volume"))
BTC_sent.110.sent

BTC_sent.111<-subset(BTC_sent,BTC_sent$date >= "2021-05-31 11:00:00" &
                       BTC_sent$date <= "2021-05-31 11:59:59")
BTC_sent.111$index<-seq(1,nrow(BTC_sent.111),by=1)
BTC_sent.111.sent<-mean(BTC_sent.111$compound,na.rm = TRUE)
BTC_sent.111.sent<-cbind(BTC_sent.111.sent,BTC_sent.111[nrow(BTC_sent.111),4])
colnames(BTC_sent.111.sent)<-paste(c("avg.sent","volume"))
BTC_sent.111.sent


BTC_sent.hourly<-rbind(BTC_sent.1.sent,BTC_sent.2.sent,BTC_sent.3.sent,BTC_sent.4.sent,BTC_sent.5.sent,BTC_sent.6.sent,BTC_sent.7.sent,BTC_sent.8.sent,
                       BTC_sent.9.sent,BTC_sent.10.sent,BTC_sent.11.sent,BTC_sent.12.sent,BTC_sent.13.sent,BTC_sent.14.sent,BTC_sent.15.sent,BTC_sent.16.sent,
                       BTC_sent.17.sent,BTC_sent.18.sent,BTC_sent.19.sent,BTC_sent.20.sent,BTC_sent.21.sent,BTC_sent.22.sent,BTC_sent.23.sent,
                       BTC_sent.24.sent,BTC_sent.25.sent,BTC_sent.26.sent,BTC_sent.27.sent,BTC_sent.28.sent,BTC_sent.29.sent,BTC_sent.30.sent,
                       BTC_sent.31.sent,BTC_sent.32.sent,BTC_sent.33.sent,BTC_sent.34.sent,BTC_sent.35.sent,BTC_sent.36.sent,BTC_sent.37.sent,
                       BTC_sent.38.sent,BTC_sent.39.sent,BTC_sent.40.sent,BTC_sent.41.sent,BTC_sent.42.sent,BTC_sent.43.sent,BTC_sent.44.sent,
                       BTC_sent.45.sent,BTC_sent.46.sent,BTC_sent.47.sent,BTC_sent.48.sent,BTC_sent.49.sent,BTC_sent.50.sent,BTC_sent.51.sent,
                       BTC_sent.52.sent,BTC_sent.53.sent,BTC_sent.54.sent,BTC_sent.55.sent,BTC_sent.56.sent,BTC_sent.57.sent,BTC_sent.58.sent,
                       BTC_sent.59.sent,BTC_sent.60.sent,BTC_sent.61.sent,BTC_sent.62.sent,BTC_sent.63.sent,BTC_sent.64.sent,BTC_sent.65.sent,
                       BTC_sent.66.sent,BTC_sent.67.sent,BTC_sent.68.sent,BTC_sent.69.sent,BTC_sent.70.sent,BTC_sent.71.sent,BTC_sent.72.sent,
                       BTC_sent.73.sent,BTC_sent.74.sent,BTC_sent.75.sent,BTC_sent.76.sent,BTC_sent.77.sent,BTC_sent.78.sent,BTC_sent.79.sent,
                       BTC_sent.80.sent,BTC_sent.81.sent,BTC_sent.82.sent,BTC_sent.83.sent,BTC_sent.84.sent,BTC_sent.85.sent,BTC_sent.86.sent,
                       BTC_sent.87.sent,BTC_sent.88.sent,BTC_sent.89.sent,BTC_sent.90.sent,BTC_sent.91.sent,BTC_sent.92.sent,BTC_sent.93.sent,
                       BTC_sent.94.sent,BTC_sent.95.sent,BTC_sent.96.sent,BTC_sent.97.sent,BTC_sent.98.sent,BTC_sent.99.sent,BTC_sent.100.sent,
                       BTC_sent.101.sent,BTC_sent.102.sent,BTC_sent.103.sent,BTC_sent.104.sent,BTC_sent.105.sent,BTC_sent.106.sent,BTC_sent.107.sent,
                       BTC_sent.108.sent,BTC_sent.109.sent,BTC_sent.110.sent,BTC_sent.111.sent)

date<-seq(from=as.POSIXct("2021-05-26 21:00:00", tz="UTC"), 
          to=as.POSIXct("2021-05-31 11:00:00", tz="UTC"), by="hour")
date<- as.data.frame(date)

BTC_sent.hourly <- cbind(date, BTC_sent.hourly)
head(BTC_sent.hourly)

write_as_csv(BTC_sent.hourly, "BTC_sent.hourly.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(BTC_sent.hourly, "BTC_sent.hourly.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
BTC_sent.hourly<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/BTC_sent.hourly.csv",header = TRUE)


# *S6: AVERAGE SENTIMENT SCORE PER DAY -------------------------------------
BTC_sent.hourly[c(1:3,nrow(BTC_sent.hourly)),]
BTC_sent.daily<-BTC_sent.hourly[c(4:nrow(BTC_sent.hourly)),]

BTC_sent.daily1<-subset(BTC_sent.daily,BTC_sent.daily$date >= "2021-05-27 00:00:00" &
                          BTC_sent.daily$date <= "2021-05-28 02:00:00")
BTC_sent.daily1.sent<-mean(BTC_sent.daily1$avg.sent,na.rm = TRUE)
BTC_sent.daily1.vol<-sum(BTC_sent.daily1$volume)
BTC_sent.daily1<-cbind(BTC_sent.daily1.sent,BTC_sent.daily1.vol)
colnames(BTC_sent.daily1)<-paste(c("Daily.sent","Daily.vol"))
BTC_sent.daily1

BTC_sent.daily2<-subset(BTC_sent.daily,BTC_sent.daily$date >= "2021-05-28 03:00:00" &
                          BTC_sent.daily$date <= "2021-05-29 02:00:00")
BTC_sent.daily2.sent<-mean(BTC_sent.daily2$avg.sent,na.rm = TRUE)
BTC_sent.daily2.vol<-sum(BTC_sent.daily2$volume)
BTC_sent.daily2<-cbind(BTC_sent.daily2.sent,BTC_sent.daily2.vol)
colnames(BTC_sent.daily2)<-paste(c("Daily.sent","Daily.vol"))
BTC_sent.daily2

BTC_sent.daily3<-subset(BTC_sent.daily,BTC_sent.daily$date >= "2021-05-29 03:00:00" &
                          BTC_sent.daily$date <= "2021-05-30 02:00:00")
BTC_sent.daily3.sent<-mean(BTC_sent.daily3$avg.sent,na.rm = TRUE)
BTC_sent.daily3.vol<-sum(BTC_sent.daily3$volume)
BTC_sent.daily3<-cbind(BTC_sent.daily3.sent,BTC_sent.daily3.vol)
colnames(BTC_sent.daily3)<-paste(c("Daily.sent","Daily.vol"))
BTC_sent.daily3

BTC_sent.daily4<-subset(BTC_sent.daily,BTC_sent.daily$date >= "2021-05-30 03:00:00" &
                          BTC_sent.daily$date <= "2021-05-31 02:00:00")
BTC_sent.daily4.sent<-mean(BTC_sent.daily4$avg.sent,na.rm = TRUE)
BTC_sent.daily4.vol<-sum(BTC_sent.daily4$volume)
BTC_sent.daily4<-cbind(BTC_sent.daily4.sent,BTC_sent.daily4.vol)
colnames(BTC_sent.daily4)<-paste(c("Daily.sent","Daily.vol"))
BTC_sent.daily4

BTC_sent.daily.tot<-rbind(BTC_sent.daily1,BTC_sent.daily2,BTC_sent.daily3,BTC_sent.daily4)

date<-seq(from=as.POSIXct("2021-05-27 00:00:00", tz="UTC"), 
          to=as.POSIXct("2021-05-30 00:00:00", tz="UTC"), by="day")
date<- as.data.frame(date)

BTC_sent.daily.tot <- cbind(date, BTC_sent.daily.tot)

options(scipen = 1)
BTC_sent.daily.tot$Daily.sent.diff<-Delt(BTC_sent.daily.tot$Daily.sent)
BTC_sent.daily.tot$Daily.vol.diff<-Delt(BTC_sent.daily.tot$Daily.vol)
BTC_sent.daily.tot[is.na(BTC_sent.daily.tot)] <- 0


write_as_csv(BTC_sent.daily.tot, "BTC_sent.daily.tot.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(BTC_sent.daily.tot, "BTC_sent.daily.tot.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
BTC_sent.daily.tot<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/BTC_sent.daily.tot.csv",header = TRUE)

# PLOTS -------------------------------------------------------------------
##**Plot the frequency of tweets over a variety of time intervals ---------
ggplot(BTC_sent.hourly,aes(x=date))+
  geom_line(aes(y=volume))+
  theme_bw()+
  labs(x = NULL, y = NULL,
       title = "Frequency of #BTC tweets",
       subtitle = paste0(format(min(as.Date(BTC_twt$created_at)), "%d %B %Y"), " to ", format(max(as.Date(BTC_twt$created_at)),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet")
##**Plot the tweets sentiment score ---------------------------------------
ggplot(BTC_sent,aes(x=compound)) + 
  geom_histogram(bins = 15,alpha=.6) + 
  theme_bw() +
  labs(x="Score",y="Number of tweets")+
  labs(title = "Sentiment analysis of #BTC Tweets from 26-31 May 2021")+
  labs(subtitle = "Using VADER lexicon")

##**Plot the hourly tweets sentiment score + volume -----------------------------
# Value used to transform the data
coeff <- 0.0001

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(BTC_sent.hourly, aes(x=date)) +
  
  geom_bar(aes(y=volume), stat="identity", size=.1, fill=temperatureColor, color="black", alpha=.4) + 
  geom_line(aes(y=avg.sent/coeff), size=1, color=priceColor,) +
  
  scale_y_continuous(
    # Features of the first axis
    name = "Volume (Tweets)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Hourly sentiment score")
  ) + 
  theme_bw() +
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  labs(title = "Hourly tweets sentiment scores and volume of #BTC",
       caption = "Using  VADER appproach")

# PART III: ANALYSIS FROM PART I + II -------------------------------------
# *S1: Extract necessary data ----------------------------------------------
BTC.price.hourly<-subset(BTCUSD_1hr,BTCUSD_1hr$Date >= "2021-05-27 00:00:00" &
                           BTCUSD_1hr$Date <= "2021-05-31 14:00:00")
BTC.price.hourly<-BTC.price.hourly[,c(2,7,8)]
BTC.price.hourly<-BTC.price.hourly[order(BTC.price.hourly$Date),]

BTC.price.hourly[c(1:3,nrow(BTC.price.hourly)),]

BTC_sent.hourly[c(1:3,nrow(BTC_sent.hourly)),]


# *S2: CALCULATE HOURLY CHANGES IN PRICE/SENTIMENT -------------------------------

#HOURLY LOGARITHMIC PRICE RETURNS
BTC.hourly.log.ret <- as.data.frame(BTC.price.hourly)
BTC.hourly.log.ret$BTC.log.ret <- c(0,diff(log(BTC.hourly.log.ret$Close)))
options(digits = 3)

#HOURLY LOGARITHMIC SENTIMENT
BTC_sent.diff<-BTC_sent.hourly
BTC_sent.diff$log.sent<-c(diff(log(BTC_sent.diff$avg.sent)),0)

#TWEETS VOLUME LOGARITHMIC CHANGES
BTC_vol.diff<-BTC_sent.hourly
BTC_vol.diff$vol.log<- c(diff(log(BTC_vol.diff$volume)),0)

#COMBINE THE RESULTS
BTC.log.change<-cbind(BTC.hourly.log.ret$BTC.log.ret,BTC_sent.diff$log.sent,BTC_vol.diff$vol.log)
colnames(BTC.log.change)<-paste(c("BTCUSD","BTC","Volume"))
BTC.log.change<-as.data.frame(BTC.log.change)
# *S3: SMA -----------------------------------------------------------------
BTC.sma.3<-BTC.price.hourly[,c(1:2)]
BTC.sma.3$sma5 <- rollmeanr(BTC.sma.3$Close, k =5, fill=NA)
BTC.sma.3$sma10 <- rollmeanr(BTC.sma.3$Close, k = 10, fill=NA)

# **S4: CORRELATION MATRIX: VOLUME, SENTIMENT, PRICE ------------------------
#HOURLY TABLE
hourly.table<-BTC_sent.diff[,-4]
hourly.table<-cbind(hourly.table,BTC.hourly.log.ret$Close,BTC.hourly.log.ret$BTC.log.ret)
colnames(hourly.table)<-paste(c("Date","Hourly.sent","Hourly.vol","Hourly.close","Hourly.log.ret"))

hourly.table$sent.idx<-hourly.table$Hourly.sent/hourly.table$Hourly.sent[1]
hourly.table$vol.idx<-hourly.table$Hourly.vol/hourly.table$Hourly.vol[1]
hourly.table$close.idx<-hourly.table$Hourly.close/hourly.table$Hourly.close[1]

#DAILY TABLE
daily.table<-BTC_sent.daily.tot[,c(-4,-5)]
daily.table<-cbind(daily.table,BTC.price.daily$BTC.USD.Adjusted[c(2:5),])
daily.table$Daily.log.ret<-c(0,diff(log(daily.table$BTC.USD.Adjusted)))
colnames(daily.table)<-paste(c("Date","Daily.sent","Daily.vol","Daily.close","Daily.log.ret"))

daily.table$sent.idx<-daily.table$Daily.sent/daily.table$Daily.sent[1]
daily.table$vol.idx<-daily.table$Daily.vol/daily.table$Daily.vol[1]
daily.table$close.idx<-daily.table$Daily.close/daily.table$Daily.close[1]


# **S5: CROSS-CORRELATION ANALYSIS WITH LAGS USING ACF/CCF ---------------------------------------------
#Step 1: Get daily & Hourly sentiment & crypto price data
hourly.table[c(1:3,nrow(hourly.table)),]

daily.table[c(1:3,nrow(daily.table)),]

#Step 2: convert data to "Time series" format

#Hourly
hourly.table_ts <- ts(data = hourly.table[, c(2,3,5)],
                      start = c(1),
                      end = c(nrow(hourly.table)),
                      frequency=1)
#Daily
daily.table_ts <- ts(data = daily.table[, c(2,3,5)],
                     start = c(1),
                     end = c(nrow(daily.table)),
                     frequency=1)

#Step 3: Lag analysis
par(mfrow = c(1, 2))

# PLOTS -------------------------------------------------------------------
##**Correlation Scatterplot Between #BTC Sentiment changes and BTC Price Return--------
plot(BTC.log.change$BTCUSD, BTC.log.change$BTC, pch = 19, 
     main = "Correlation Matrix Between Hourly #BTC Sentiment changes and BTC Price return", xlab = "BTC-USD", ylab = "#BTC")
abline(lm( BTC.log.change$BTCUSD ~ BTC.log.change$BTC), col = "red")
text(x = 0.03, y = -0.5, label = "r = 0.003390", col = "red")

##**Correlation Scatterplot Between #BTC Volume changes and BTC Price Return --------
plot(BTC.log.change$BTCUSD, BTC.log.change$Volume, pch = 19, 
     main = "Correlation Matrix Between Hourly #BTC Volume changes and BTC Price return", xlab = "BTC-USD", ylab = "#BTC Volume")
abline(lm( BTC.log.change$BTCUSD ~ BTC.log.change$Volume), col = "red")
text(x = 0.03, y = -0.3, label = "r = -0.005409", col = "red")

##**Plot the hourly price returns + volume --------------------------------
return.volume.plot<-BTC_sent.hourly
return.volume.plot$price.ret<-BTC_sent.diff$log.sent

# Value used to transform the data
coeff <- 0.001

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(return.volume.plot, aes(x=date)) +
  
  geom_bar(aes(y=volume), stat="identity", size=.1, fill=temperatureColor, color="black", alpha=.4) + 
  geom_line(aes(y=price.ret/coeff), size=1, color=priceColor,) +
  
  scale_y_continuous(
    # Features of the first axis
    name = "Volume (Tweets)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Hourly Logarithmic price returns")
  ) + 
  theme_bw() +
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  labs(title = "Hourly Logarithmic price returns and volume of #BTC",
       subtitle = "Using VADER appproach")

##**Tweets volume - Price returns - Price of BTC --------------------------
p1 <- ggplot(BTC_sent.hourly,aes(x=date))+
  geom_bar(aes(y=volume), stat="identity", size=.1, color="black", alpha=.4)+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(x="Hours",y="Tweets Volume")+
  labs(title = "Hourly Tweets Volume of #BTC")

p2 <- ggplot(BTC.hourly.log.ret, aes(x=Date)) + 
  geom_line(aes(y=Close)) + 
  theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Hours", y = "Price ($)")+
  labs(title = "Hourly Price of BTC-USD", 
       subtitle =paste0(format(min(as.Date(BTC.hourly.log.ret$Date)), "%d %B %Y"), " to ", format(max(as.Date(BTC.hourly.log.ret$Date)),"%d %B %Y")))

p3 <- ggplot(BTC.hourly.log.ret,aes(x=Date)) + 
  geom_line(aes(y=BTC.log.ret)) +
  geom_hline(yintercept = 0,col="red") +
  theme_bw() +
  labs(x="Hours",y="Price Returns (%)")+
  labs(title = "Hourly Logarithmic Price Returns of BTC-USD",caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3), size = "last"))

##**Daily Sentiment score - Tweets volume - Close Prices of BTC -----------
BTC.price.daily<- subset(data.BTC,index(data.BTC) >= "2021-05-26" &
                           index(data.BTC) <= "2021-05-31")
BTC.price.daily<-BTC.price.daily[,6]
BTC.price.daily$ret<-diff(log(BTC.price.daily$`BTC-USD.Adjusted`))
BTC.price.daily[1,2]<-0

p1 <- ggplot(BTC_sent.daily.tot,aes(x=date))+
  geom_line(aes(y=Daily.sent), stat="identity", size=.1, color="black", alpha=.4)+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(x="Days",y="Daily Sentiment Score")+
  labs(title = "Daily Sentiment Scores of #BTC")

p2 <- ggplot(BTC_sent.daily.tot, aes(x=date)) + 
  geom_line(aes(y=Daily.vol)) + 
  theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Days", y = "Tweets Volume")+
  labs(title = "Daily Volume of #BTC Tweets")

p3 <- ggplot(BTC.price.daily,aes(x=index(BTC.price.daily))) + 
  geom_line(aes(y=BTC.USD.Adjusted)) +
  theme_bw() +
  labs(x="Days",y="Prices ($)", 
       subtitle =paste0(format(min(as.Date(BTC_sent.daily.tot$date)), "%d %B %Y"), " to ", format(max(as.Date(BTC_sent.daily.tot$date)),"%d %B %Y")))+
  labs(title = "Daily Prices of BTC-USD",caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3), size = "last"))

##**CORRELATION MATRIX: VOLUME, SENTIMENT, PRICE --------------------------
#Hourly data
hourly.table.corr.mat <- as.data.frame(cbind(hourly.table$sent.idx,hourly.table$vol.idx,hourly.table$close.idx))
colnames(hourly.table.corr.mat)<-paste(c("Sent","Vol","Close Price"))
corrplot(cor(hourly.table.corr.mat), method = "number", title = "Hourly Correlation Matrix #BTC", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

#Daily data
daily.table.corr.mat <- as.data.frame(cbind(daily.table$sent.idx,daily.table$vol.idx,daily.table$close.idx))
colnames(daily.table.corr.mat)<-paste(c("Sent","Vol","Close Price"))
corrplot(cor(daily.table.corr.mat), method = "number", title = "Daily Correlation Matrix #BTC", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

##**SMA + TRENDLINE during the period 2021-05-26 -> 2021-05-31 ------------------------
ggplot(BTC.sma.3,aes(x=Date))+
  geom_line(aes(y=Close))+
  theme_bw()+
  labs(x="Date",y="Price ($)",
       title = "BTC-USD Simple Moving Average",
       subtitle = "May 26, 2021 - May 31, 2021")+
  geom_line(aes(y=predict(lm(Close~index(BTC.sma.3)))),col="red")+ #trendline
  geom_line(aes(y=sma5), col="green",alpha=.4,size=.1)+
  geom_line(aes(y=sma10),col="red",alpha=.4,size=.1)
##**LAG ANALYSIS + CROSS-CORRELATION ANALYSIS -------------------------------
par(mfrow = c(1, 2))
#Hourly
#Hourly Sentiment Score
# acf Sentiment time series
hourly.table_ts[, c("Hourly.sent")] %>% 
  acf(lag.max = 180, 
      main = "Autocorrelation - Hourly Sentiment")

# pacf R time series
hourly.table_ts[, c("Hourly.sent")] %>%
  pacf(lag.max = 180,
       main = "Partial Autocorrelation- Hourly Sentiment")

#Hourly Crypto Log ret
# acf Sentiment time series
hourly.table_ts[, c("Hourly.log.ret")] %>% 
  acf(lag.max = 180, 
      main = "Autocorrelation - Hourly Logarithmic Return")

# pacf R time series
hourly.table_ts[, c("Hourly.log.ret")] %>%
  pacf(lag.max = 180,
       main = "Partial Autocorrelation- Hourly Logarithmic Return")

#Daily
#Daily Sentiment Score
# acf Sentiment time series
daily.table_ts[, c("Daily.sent")] %>% 
  acf(lag.max = 180, 
      main = "Autocorrelation - Daily Sentiment")

# pacf R time series
daily.table_ts[, c("Daily.sent")] %>%
  pacf(lag.max = 180,
       main = "Partial Autocorrelation- Daily Sentiment")

#Daily Crypto Log ret
# acf Sentiment time series
daily.table_ts[, c("Daily.log.ret")] %>% 
  acf(lag.max = 180, 
      main = "Autocorrelation - Daily Logarithmic Return")

# pacf R time series
daily.table_ts[, c("Daily.log.ret")] %>%
  pacf(lag.max = 180,
       main = "Partial Autocorrelation- Daily Logarithmic Return")

#Step 4: Cross-correlation analysis
#Hourly
par(mfrow=c(1,1))
ccf(hourly.table_ts[, c("Hourly.sent")], hourly.table_ts[, c("Hourly.log.ret")], 
    lag.max = 180,
    main = "Cross-Correlation Plot between Hourly Sentiment Score & Crypto Logarithmic Return",
    ylab = "CCF")

hourly.ccfvalues = ccf(hourly.table_ts[, c("Hourly.sent")], hourly.table_ts[, c("Hourly.log.ret")])
hourly.ccfvalues
#Hourly sentiment score affects significantly positively on later hourly crypto price

#Daily
par(mfrow=c(1,1))
ccf(daily.table_ts[, c("Daily.sent")], daily.table_ts[, c("Daily.log.ret")], 
    lag.max = 180,
    main = "Cross-Correlation Plot between Daily Sentiment Score & Crypto Logarithmic Return",
    ylab = "CCF")

Daily.ccfvalues = ccf(daily.table_ts[, c("Daily.sent")], daily.table_ts[, c("Daily.log.ret")])
Daily.ccfvalues
#Daily sentiment score affects positively on later daily crypto price

#Step 5: Significance test
#Hourly
hourly.cc<-cc.test(hourly.table_ts[, c("Hourly.sent")], hourly.table_ts[, c("Hourly.log.ret")], 
                       max.lag = 30, alpha = 0.05, lambda = 2.576, plot = TRUE,
                       table = TRUE, var.names = NULL)

#Daily
daily.cc<-cc.test(daily.table_ts[, c("Daily.sent")], daily.table_ts[, c("Daily.log.ret")], 
                      max.lag =3, alpha = 0.05, lambda = 2.576, plot = TRUE,
                      table = TRUE, var.names = NULL)