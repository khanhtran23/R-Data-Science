#DOGECOIN 2021-05-27 00:00:00 -> 2021-05-31 14:00:00
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
library(stringr)
library(vader)
#------------------------------------------------------------------------------------------------------------------------------------------------#
# PART I: PRICE ANALYSIS --------------------------------------------------
# *S1: GET NECESSARY PRICE DATASET -----------------------------------------------
data.DOGE<-getSymbols("DOGE-USD",from="2016-05-31",to="2021-05-31",auto.assign = FALSE)

DOGEUSD_1hr<-read_csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Hourly Data/DOGEUSD_1hr_woline1.csv")

# *S2: DAILY PRICE RETURNS 5 YEARS -------------------------------------------------------
#DAILY LOGARITHMIC TOTAL RETURNS
DOGE.log.ret <- data.DOGE[,6]
DOGE.log.ret$DOGE.log.ret <- diff(log(DOGE.log.ret$`DOGE-USD.Adjusted`))
options(digits = 3)
DOGE.log.ret <- DOGE.log.ret[,2]
DOGE.log.ret[1,1]<-0
DOGE.log.ret$Gross.ret<-DOGE.log.ret$DOGE.log.ret+1
DOGE.log.ret$Gross.ret[1]<-1
DOGE.log.ret$cum.ret<-cumprod(DOGE.log.ret$Gross.ret)

#CUMULATED DAILY RETURN AFTER 5 YEARS
DOGE.logcumret <- sum(DOGE.log.ret$DOGE.log.ret,na.rm=TRUE)
DOGE.cumret <- exp(DOGE.logcumret)-1
DOGE.cumret #1379

#Mean returns, Std-Dev, Min, Max, Skewness, Kurtosis
DOGE.mean<-mean(DOGE.log.ret$DOGE.log.ret,na.rm=TRUE)
DOGE.sd<-sd(DOGE.log.ret$DOGE.log.ret,na.rm=TRUE)
DOGE.min<-min(DOGE.log.ret$DOGE.log.ret,na.rm=TRUE)
DOGE.max<-max(DOGE.log.ret$DOGE.log.ret,na.rm=TRUE)
DOGE.skew<-skewness(DOGE.log.ret$DOGE.log.ret,na.rm=TRUE)
DOGE.kurt<-kurtosis(DOGE.log.ret$DOGE.log.ret,na.rm=TRUE)
DOGE.summary.table<-cbind(DOGE.cumret,DOGE.mean,DOGE.sd,DOGE.min,DOGE.max,DOGE.skew,DOGE.kurt)
rownames(DOGE.summary.table)<-c("2016-2021")
DOGE.summary.table

# *S3: SIMPLE MOVING AVERAGE & TREND ---------------------------------------------------

#Step 1: Create ETH.sma
DOGE.sma <- data.DOGE[,6]
DOGE.sma[is.na(DOGE.sma)] <- 0
colnames(DOGE.sma)<-paste(c("Adjusted"))
#Step 2: Create 50-day and 100-day rolling average columns
DOGE.sma$sma50 <- rollmeanr(DOGE.sma$Adjusted, k =50)
DOGE.sma$sma100 <- rollmeanr(DOGE.sma$Adjusted, k = 100)
DOGE.sma$no<-c(1:nrow(DOGE.sma)) #This supports creation of Trendline

#**S4: DAILY EWMA VOLATILITY -----------------------------------------------------
#Step 1,2: Create squared logarithmic price return of ETH
DOGE_squared <- data.DOGE[,6]
DOGE_squared$DOGE_squared <- (diff(log(DOGE_squared$`DOGE-USD.Adjusted`)))^2
options(digits = 3)
DOGE_squared <- DOGE_squared[,2]
DOGE_squared[1,1]<-0

DOGE_squared2 <- as.numeric(DOGE_squared) # Squared returns as a numeric vector

#Step 3: Create EWMA function
ewma <- function(DOGE_squared2, lambda){
  stats::filter(DOGE_squared2*(1-lambda), lambda, "recursive", init = DOGE_squared2[1])
}

# Step 4: Calculate variance with lambda=0.94
DOGE_var_ewma94 <- ewma(DOGE_squared2, 0.94)

# Step 5: Transform back to a zoo object (time index from object ETH_squared)
DOGE_var_ewma <- zoo(DOGE_var_ewma94,index(DOGE_squared))

#Step 6: Volatility
DOGE_vola_ewma <- sqrt(DOGE_var_ewma)
# *S5: ALPHA & BETA USING CAPM, MARKET MODEL, AND ROLLING WINDOW REGRESSION --------
#Knowledge from Ang 2015, Chapter 5
#CAPM
#Step 0: Construct a monthly returns of Crypto
data.DOGE[c(1:3,nrow(data.DOGE)),]

DOGE.monthly<-to.monthly(data.DOGE)
DOGE.monthly[c(1:3,nrow(DOGE.monthly)),]

DOGE.monthly<-DOGE.monthly[,6]

DOGE.ret<-Delt(DOGE.monthly$data.DOGE.Adjusted)
names(DOGE.ret)<-paste("DOGE.ret")
DOGE.ret<-DOGE.ret[-1,]
DOGE.ret[c(1:3,nrow(DOGE.ret)),]

csv.DOGE<-cbind(data.frame(index(DOGE.ret)),data.frame(DOGE.ret))
names(csv.DOGE)[1]<-paste("date")
rownames(csv.DOGE)<-seq(1,nrow(csv.DOGE),by=1)
csv.DOGE[c(1:3,nrow(csv.DOGE)),]

write.csv(csv.DOGE,"DOGE Returns (Monthly).csv") #Now, we have the csv file for the next steps.

#Step 1: Import Portfolio Returns and Convert to a data.frame Object
DOGE<-read.csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Others/DOGE Returns (Monthly).csv", header = TRUE)
DOGE[c(1:3,nrow(DOGE)),]
DOGE$date<-as.yearmon(as.character(DOGE$date),"%b %Y")
DOGE[c(1:3,nrow(DOGE)),]
DOGE.df<-data.frame(DOGE)
DOGE.df[c(1:3,nrow(DOGE.df)),]

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

#Step 4: Combine XRP, mkt, and rf Data Into One Data Object
combo.DOGE<-cbind(market.df,data.frame(rf.monthly),DOGE.df$DOGE.ret)
combo.DOGE[c(1:3,nrow(combo.DOGE)),]
names(combo.DOGE)<-paste(c("mkt.ret","rf","DOGE.ret"))
combo.DOGE[c(1:3,nrow(combo.DOGE)),]

#Step 5: Calculate Excess Return and Excess Market Return
#Excess Return (exret) = port.ret - rf
#Excess Market Return (exmkt) = mkt.ret - rf
combo.DOGE$exret<-combo.DOGE$DOGE.ret -combo.DOGE$rf
combo.DOGE$exmkt<-combo.DOGE$mkt.ret-combo.DOGE$rf
combo.DOGE[c(1:3,nrow(combo.DOGE)),]

#Step 6: Run Regression of Excess Return on Excess Market Return
#We use "lm" command to calculate OLS Regression between exret & exmkt
options(digits = 3)
CAPM<-lm(combo.DOGE$exret~combo.DOGE$exmkt) #lm stands for "linear model", the function that creates a simple regression model
summary(CAPM)

#Rolling Window Regression
#In this section, we run regression through a rolling window, which calculate Alphas and Betas in different periods.
#=> Thus we can see how variable these two are, over a time period.

#Step 1: Import DOGE and S&P 500 Index Data 
data.DOGE[c(1:3,nrow(data.DOGE)),]
data.mkt[c(1:3,nrow(data.mkt)),]

#Step 2: Calculate the DOGE and Market Returns
rets<-diff(log(data.DOGE$`DOGE-USD.Adjusted`))
rets$GSPC<-diff(log(data.mkt$GSPC.Adjusted))
names(rets)[1]<-"DOGE"
rets<-rets[-1,]
rets[c(1:3,nrow(rets)),]

#Step 3: Create the Rolling Window Regression Function
require(zoo)
coeffs<-rollapply(rets,
                  width = 252,
                  FUN = function(X)
                  {
                    roll.reg=lm(DOGE~GSPC,
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
##**Daily Price movement of DOGE 2016-2021 ---------------------------------
ggplot(data.DOGE,aes(x=index(data.DOGE)))+
  geom_line(aes(y=data.DOGE$`DOGE-USD.Adjusted`))+
  labs(x = "Date", y = "Value ($)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + # centers headline
  labs(title = "Value of $1 Investment in Dogecoin 
       May 31, 2016 - May 31, 2021",
       caption = "Data retrieved from Yahoo! Finance")

##**Daily Price movement of DOGE 2021 ---------------------------------
data.DOGE.may<-subset(data.DOGE,index(data.DOGE)>= "2021-01-01" &
                        index(data.DOGE) <= "2021-05-31")
ggplot(data.DOGE.may,aes(x=index(data.DOGE.may)))+
  geom_line(aes(y=data.DOGE.may$`DOGE-USD.Adjusted`))+
  labs(x = "Date", y = "Value ($)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + # centers headline
  labs(title = "Value of $1 Investment in Dogecoin 
       Jan 01, 2021 - May 31, 2021",
       caption = "Data retrieved from Yahoo! Finance")

##**Hourly Price movement of DOGE May 2021 --------
ggplot(DOGEUSD_1hr, aes(Date,group=1)) + geom_line(aes(y=Close)) + theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Date", y = "Price ($)")+
  labs(title = "DOGE Hourly Price", 
       subtitle =paste0(format(min(as.Date(DOGEUSD_1hr$Date)), "%d %B %Y"), " to ", format(max(as.Date(DOGEUSD_1hr$Date)),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

##**Daily Log return volatility 2016-2021 ---------------------------------
DOGE.log.ret<-as.data.frame(DOGE.log.ret)
DOGE.volatility<-ggplot(DOGE.log.ret,aes(x=index(DOGE.log.ret)))+
  geom_line(aes(y=DOGE.log.ret))+
  labs(x = "Date", y = "Value of Investment ($)") +
  labs(title = "Volatility of DOGE price
May 31, 2016 - May 31, 2021")+
  theme_bw()# white background
DOGE.volatility<-DOGE.volatility+geom_hline(yintercept = 0,col="red")
DOGE.volatility
##**SMA + Trendline 2016-2021 ---------------------------------------------------
ggplot(DOGE.sma,aes(x=index(DOGE.sma)))+
  geom_line(aes(y=Adjusted))+
  theme_bw()+
  labs(x="Date",y="Price ($)",
       title = "DOGE-USD Simple Moving Average",
       subtitle = "May 31, 2016 - May 31, 2021")+
  geom_line(aes(y=predict(lm(Adjusted~no))),col="red")+ #trendline
  geom_line(aes(y=sma50), col="green",alpha=.4,size=.1)+
  geom_line(aes(y=sma100),col="red",alpha=.4,size=.1)
##**SMA + Trendline 2021 ---------------------------------------------------
DOGE.sma.2021<-subset(DOGE.sma,index(DOGE.sma) >= "2021-01-01" &
                        index(DOGE.sma) <= "2021-05-31")
ggplot(DOGE.sma.2021,aes(x=index(DOGE.sma.2021)))+
  geom_line(aes(y=Adjusted))+
  theme_bw()+
  labs(x="Date",y="Price ($)",
       title = "DOGE-USD Simple Moving Average",
       subtitle = "Jan 01, 2021 - May 31, 2021")+
  geom_line(aes(y=predict(lm(Adjusted~no))),col="red")+ #trendline
  geom_line(aes(y=sma50), col="green",alpha=.4,size=.1)+
  geom_line(aes(y=sma100),col="red",alpha=.4,size=.1)
##**DAILY EWMA --------------------------------------------------------------
plot(DOGE_var_ewma, main="DOGE - Daily Variance with EWMA, lambda=0.94", xlab="Date", ylab="Variance", col="blue", type="l")
plot(DOGE_vola_ewma,  main="DOGE - Daily Volatility with EWMA, lambda=0.94", xlab="Date", ylab="Volatility", col="blue", type="l")

##**ALPHA-BETA --------------------------------------------------------------
p1 <- ggplot(coeffs, aes(index(coeffs), coeffs$Alpha)) + geom_line() + theme_minimal() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(angle = 90))+
  labs(x = "Date", y = "Alpha") +
  labs(title = "Dogecoin Alpha and Beta
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
DOGE_twt<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/Tweets data/DOGE_twt.csv",header = TRUE)

# *S2: Clean Tweets ------------------------------------------------------
tweets.DOGE<- DOGE_twt  %>% dplyr::select(created_at, screen_name,text) #Create a table with "created_at", "screen_name", and "text" columns from BTC_twt
tweets.DOGE[c(1:3,nrow(tweets.DOGE)),]

tweets.DOGE$stripped_text1<-gsub("https(.*)*$","",tweets.DOGE$text) #Remove URLs
tweets.DOGE$stripped_text1<-tolower(tweets.DOGE$stripped_text1) #To lowercase
tweets.DOGE$stripped_text1<-gsub("\\.\\.","",tweets.DOGE$stripped_text1) #replace ..... with .
tweets.DOGE$stripped_text1<-gsub("(.)\\1{2,}", "\\1",tweets.DOGE$stripped_text1) #Transform "Goooooo" to "go"
tweets.DOGE$stripped_text1<-gsub("([[:punct:]])\\1+", "\\1", tweets.DOGE$stripped_text1) #Reduce !!!!! to !
tweets.DOGE$stripped_text1<-gsub("#","", tweets.DOGE$stripped_text1) #Remove hashtags
tweets.DOGE$stripped_text1<-gsub("@[[:alnum:]]+","", tweets.DOGE$stripped_text1) #Remove mentions
tweets.DOGE$stripped_text1<-gsub("&amp","and", tweets.DOGE$stripped_text1) #Replace &apm with and
tweets.DOGE$stripped_text1<-gsub("<(.*)>","", tweets.DOGE$stripped_text1) #Remove unicodes <U+...>
tweets.DOGE$stripped_text1<-iconv(tweets.DOGE$stripped_text1, "latin1", "ASCII", sub="") #remove weird letters
tweets.DOGE$stripped_text1<- gsub("%%", "\'", tweets.DOGE$stripped_text1) # Changing %% back to apostrophes
tweets.DOGE$stripped_text1<-str_squish(tweets.DOGE$stripped_text1) #Remove excessive spaces

# *S3: Run and save VADER analysis -----------------------------------------------------------
vader_doge = vader_df(tweets.DOGE$stripped_text1)
vader_doge[c(1:3,nrow(vader_doge)),]
write_as_csv(vader_doge, "vader_doge.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(vader_doge, "vader_doge.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

#**S3 bis: Load sentiment data ---------------------------------------------
vader_doge<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/vader_doge.csv",header = TRUE)
DOGE_sent<-vader_doge[,c(3:4)]
date<-tweets.DOGE$created_at
DOGE_sent$date<-date

# *S4: AVERAGE SENTIMENT SCORE OVER THE PERIOD -----------------------------
DOGE_sent[c(1:3,nrow(DOGE_sent)),]
DOGE_sent.avg<-mean(DOGE_sent$compound,na.rm=TRUE)
DOGE_sent.avg #0.291

#**S5: AVERAGE SENTIMENT SCORE PER HOUR ----------------------------------------

DOGE_sent.1<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 00:00:00" &
                      DOGE_sent$date <= "2021-05-27 00:59:59")
DOGE_sent.1$index<-seq(1,nrow(DOGE_sent.1),by=1)
DOGE_sent.1.sent<-mean(DOGE_sent.1$compound,na.rm = TRUE)
DOGE_sent.1.sent<-cbind(DOGE_sent.1.sent,DOGE_sent.1[nrow(DOGE_sent.1),4])
colnames(DOGE_sent.1.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.1.sent

DOGE_sent.2<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 01:00:00" &
                      DOGE_sent$date <= "2021-05-27 01:59:59")
DOGE_sent.2$index<-seq(1,nrow(DOGE_sent.2),by=1)
DOGE_sent.2.sent<-mean(DOGE_sent.2$compound,na.rm = TRUE)
DOGE_sent.2.sent<-cbind(DOGE_sent.2.sent,DOGE_sent.2[nrow(DOGE_sent.2),4])
colnames(DOGE_sent.2.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.2.sent

DOGE_sent.3<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 02:00:00" &
                      DOGE_sent$date <= "2021-05-27 02:59:59")
DOGE_sent.3$index<-seq(1,nrow(DOGE_sent.3),by=1)
DOGE_sent.3.sent<-mean(DOGE_sent.3$compound,na.rm = TRUE)
DOGE_sent.3.sent<-cbind(DOGE_sent.3.sent,DOGE_sent.3[nrow(DOGE_sent.3),4])
colnames(DOGE_sent.3.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.3.sent

DOGE_sent.4<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 03:00:00" &
                      DOGE_sent$date <= "2021-05-27 03:59:59")
DOGE_sent.4$index<-seq(1,nrow(DOGE_sent.4),by=1)
DOGE_sent.4.sent<-mean(DOGE_sent.4$compound,na.rm = TRUE)
DOGE_sent.4.sent<-cbind(DOGE_sent.4.sent,DOGE_sent.4[nrow(DOGE_sent.4),4])
colnames(DOGE_sent.4.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.4.sent

DOGE_sent.5<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 04:00:00" &
                      DOGE_sent$date <= "2021-05-27 04:59:59")
DOGE_sent.5$index<-seq(1,nrow(DOGE_sent.5),by=1)
DOGE_sent.5.sent<-mean(DOGE_sent.5$compound,na.rm = TRUE)
DOGE_sent.5.sent<-cbind(DOGE_sent.5.sent,DOGE_sent.5[nrow(DOGE_sent.5),4])
colnames(DOGE_sent.5.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.5.sent

DOGE_sent.6<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 05:00:00" &
                      DOGE_sent$date <= "2021-05-27 05:59:59")
DOGE_sent.6$index<-seq(1,nrow(DOGE_sent.6),by=1)
DOGE_sent.6.sent<-mean(DOGE_sent.6$compound,na.rm = TRUE)
DOGE_sent.6.sent<-cbind(DOGE_sent.6.sent,DOGE_sent.6[nrow(DOGE_sent.6),4])
colnames(DOGE_sent.6.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.6.sent

DOGE_sent.7<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 06:00:00" &
                      DOGE_sent$date <= "2021-05-27 06:59:59")
DOGE_sent.7$index<-seq(1,nrow(DOGE_sent.7),by=1)
DOGE_sent.7.sent<-mean(DOGE_sent.7$compound,na.rm = TRUE)
DOGE_sent.7.sent<-cbind(DOGE_sent.7.sent,DOGE_sent.7[nrow(DOGE_sent.7),4])
colnames(DOGE_sent.7.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.7.sent

DOGE_sent.8<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 07:00:00" &
                      DOGE_sent$date <= "2021-05-27 07:59:59")
DOGE_sent.8$index<-seq(1,nrow(DOGE_sent.8),by=1)
DOGE_sent.8.sent<-mean(DOGE_sent.8$compound,na.rm = TRUE)
DOGE_sent.8.sent<-cbind(DOGE_sent.8.sent,DOGE_sent.8[nrow(DOGE_sent.8),4])
colnames(DOGE_sent.8.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.8.sent

DOGE_sent.9<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 08:00:00" &
                      DOGE_sent$date <= "2021-05-27 08:59:59")
DOGE_sent.9$index<-seq(1,nrow(DOGE_sent.9),by=1)
DOGE_sent.9.sent<-mean(DOGE_sent.9$compound,na.rm = TRUE)
DOGE_sent.9.sent<-cbind(DOGE_sent.9.sent,DOGE_sent.9[nrow(DOGE_sent.9),4])
colnames(DOGE_sent.9.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.9.sent

DOGE_sent.10<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 09:00:00" &
                      DOGE_sent$date <= "2021-05-27 09:59:59")
DOGE_sent.10$index<-seq(1,nrow(DOGE_sent.10),by=1)
DOGE_sent.10.sent<-mean(DOGE_sent.10$compound,na.rm = TRUE)
DOGE_sent.10.sent<-cbind(DOGE_sent.10.sent,DOGE_sent.10[nrow(DOGE_sent.10),4])
colnames(DOGE_sent.10.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.10.sent

DOGE_sent.11<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 10:00:00" &
                       DOGE_sent$date <= "2021-05-27 10:59:59")
DOGE_sent.11$index<-seq(1,nrow(DOGE_sent.11),by=1)
DOGE_sent.11.sent<-mean(DOGE_sent.11$compound,na.rm = TRUE)
DOGE_sent.11.sent<-cbind(DOGE_sent.11.sent,DOGE_sent.11[nrow(DOGE_sent.11),4])
colnames(DOGE_sent.11.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.11.sent

DOGE_sent.12<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 11:00:00" &
                       DOGE_sent$date <= "2021-05-27 11:59:59")
DOGE_sent.12$index<-seq(1,nrow(DOGE_sent.12),by=1)
DOGE_sent.12.sent<-mean(DOGE_sent.12$compound,na.rm = TRUE)
DOGE_sent.12.sent<-cbind(DOGE_sent.12.sent,DOGE_sent.12[nrow(DOGE_sent.12),4])
colnames(DOGE_sent.12.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.12.sent

DOGE_sent.13<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 12:00:00" &
                       DOGE_sent$date <= "2021-05-27 12:59:59")
DOGE_sent.13$index<-seq(1,nrow(DOGE_sent.13),by=1)
DOGE_sent.13.sent<-mean(DOGE_sent.13$compound,na.rm = TRUE)
DOGE_sent.13.sent<-cbind(DOGE_sent.13.sent,DOGE_sent.13[nrow(DOGE_sent.13),4])
colnames(DOGE_sent.13.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.13.sent

DOGE_sent.14<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 13:00:00" &
                       DOGE_sent$date <= "2021-05-27 13:59:59")
DOGE_sent.14$index<-seq(1,nrow(DOGE_sent.14),by=1)
DOGE_sent.14.sent<-mean(DOGE_sent.14$compound,na.rm = TRUE)
DOGE_sent.14.sent<-cbind(DOGE_sent.14.sent,DOGE_sent.14[nrow(DOGE_sent.14),4])
colnames(DOGE_sent.14.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.14.sent

DOGE_sent.15<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 14:00:00" &
                       DOGE_sent$date <= "2021-05-27 14:59:59")
DOGE_sent.15$index<-seq(1,nrow(DOGE_sent.15),by=1)
DOGE_sent.15.sent<-mean(DOGE_sent.15$compound,na.rm = TRUE)
DOGE_sent.15.sent<-cbind(DOGE_sent.15.sent,DOGE_sent.15[nrow(DOGE_sent.15),4])
colnames(DOGE_sent.15.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.15.sent

DOGE_sent.16<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 15:00:00" &
                       DOGE_sent$date <= "2021-05-27 15:59:59")
DOGE_sent.16$index<-seq(1,nrow(DOGE_sent.16),by=1)
DOGE_sent.16.sent<-mean(DOGE_sent.16$compound,na.rm = TRUE)
DOGE_sent.16.sent<-cbind(DOGE_sent.16.sent,DOGE_sent.16[nrow(DOGE_sent.16),4])
colnames(DOGE_sent.16.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.16.sent

DOGE_sent.17<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 16:00:00" &
                       DOGE_sent$date <= "2021-05-27 16:59:59")
DOGE_sent.17$index<-seq(1,nrow(DOGE_sent.17),by=1)
DOGE_sent.17.sent<-mean(DOGE_sent.17$compound,na.rm = TRUE)
DOGE_sent.17.sent<-cbind(DOGE_sent.17.sent,DOGE_sent.17[nrow(DOGE_sent.17),4])
colnames(DOGE_sent.17.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.17.sent

DOGE_sent.18<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 17:00:00" &
                       DOGE_sent$date <= "2021-05-27 17:59:59")
DOGE_sent.18$index<-seq(1,nrow(DOGE_sent.18),by=1)
DOGE_sent.18.sent<-mean(DOGE_sent.18$compound,na.rm = TRUE)
DOGE_sent.18.sent<-cbind(DOGE_sent.18.sent,DOGE_sent.18[nrow(DOGE_sent.18),4])
colnames(DOGE_sent.18.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.18.sent

DOGE_sent.19<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 18:00:00" &
                       DOGE_sent$date <= "2021-05-27 18:59:59")
DOGE_sent.19$index<-seq(1,nrow(DOGE_sent.19),by=1)
DOGE_sent.19.sent<-mean(DOGE_sent.19$compound,na.rm = TRUE)
DOGE_sent.19.sent<-cbind(DOGE_sent.19.sent,DOGE_sent.19[nrow(DOGE_sent.19),4])
colnames(DOGE_sent.19.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.19.sent

DOGE_sent.20<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 19:00:00" &
                       DOGE_sent$date <= "2021-05-27 19:59:59")
DOGE_sent.20$index<-seq(1,nrow(DOGE_sent.20),by=1)
DOGE_sent.20.sent<-mean(DOGE_sent.20$compound,na.rm = TRUE)
DOGE_sent.20.sent<-cbind(DOGE_sent.20.sent,DOGE_sent.20[nrow(DOGE_sent.20),4])
colnames(DOGE_sent.20.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.20.sent

DOGE_sent.21<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 20:00:00" &
                       DOGE_sent$date <= "2021-05-27 20:59:59")
DOGE_sent.21$index<-seq(1,nrow(DOGE_sent.21),by=1)
DOGE_sent.21.sent<-mean(DOGE_sent.21$compound,na.rm = TRUE)
DOGE_sent.21.sent<-cbind(DOGE_sent.21.sent,DOGE_sent.21[nrow(DOGE_sent.21),4])
colnames(DOGE_sent.21.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.21.sent

DOGE_sent.22<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 21:00:00" &
                       DOGE_sent$date <= "2021-05-27 21:59:59")
DOGE_sent.22$index<-seq(1,nrow(DOGE_sent.22),by=1)
DOGE_sent.22.sent<-mean(DOGE_sent.22$compound,na.rm = TRUE)
DOGE_sent.22.sent<-cbind(DOGE_sent.22.sent,DOGE_sent.22[nrow(DOGE_sent.22),4])
colnames(DOGE_sent.22.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.22.sent

DOGE_sent.23<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 22:00:00" &
                       DOGE_sent$date <= "2021-05-27 22:59:59")
DOGE_sent.23$index<-seq(1,nrow(DOGE_sent.23),by=1)
DOGE_sent.23.sent<-mean(DOGE_sent.23$compound,na.rm = TRUE)
DOGE_sent.23.sent<-cbind(DOGE_sent.23.sent,DOGE_sent.23[nrow(DOGE_sent.23),4])
colnames(DOGE_sent.23.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.23.sent

DOGE_sent.24<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-27 23:00:00" &
                       DOGE_sent$date <= "2021-05-27 23:59:59")
DOGE_sent.24$index<-seq(1,nrow(DOGE_sent.24),by=1)
DOGE_sent.24.sent<-mean(DOGE_sent.24$compound,na.rm = TRUE)
DOGE_sent.24.sent<-cbind(DOGE_sent.24.sent,DOGE_sent.24[nrow(DOGE_sent.24),4])
colnames(DOGE_sent.24.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.24.sent

DOGE_sent.25<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 00:00:00" &
                       DOGE_sent$date <= "2021-05-28 00:59:59")
DOGE_sent.25$index<-seq(1,nrow(DOGE_sent.25),by=1)
DOGE_sent.25.sent<-mean(DOGE_sent.25$compound,na.rm = TRUE)
DOGE_sent.25.sent<-cbind(DOGE_sent.25.sent,DOGE_sent.25[nrow(DOGE_sent.25),4])
colnames(DOGE_sent.25.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.25.sent

DOGE_sent.26<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 01:00:00" &
                       DOGE_sent$date <= "2021-05-28 01:59:59")
DOGE_sent.26$index<-seq(1,nrow(DOGE_sent.26),by=1)
DOGE_sent.26.sent<-mean(DOGE_sent.26$compound,na.rm = TRUE)
DOGE_sent.26.sent<-cbind(DOGE_sent.26.sent,DOGE_sent.26[nrow(DOGE_sent.26),4])
colnames(DOGE_sent.26.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.26.sent

DOGE_sent.27<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 02:00:00" &
                       DOGE_sent$date <= "2021-05-28 02:59:59")
DOGE_sent.27$index<-seq(1,nrow(DOGE_sent.27),by=1)
DOGE_sent.27.sent<-mean(DOGE_sent.27$compound,na.rm = TRUE)
DOGE_sent.27.sent<-cbind(DOGE_sent.27.sent,DOGE_sent.27[nrow(DOGE_sent.27),4])
colnames(DOGE_sent.27.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.27.sent

DOGE_sent.28<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 03:00:00" &
                       DOGE_sent$date <= "2021-05-28 03:59:59")
DOGE_sent.28$index<-seq(1,nrow(DOGE_sent.28),by=1)
DOGE_sent.28.sent<-mean(DOGE_sent.28$compound,na.rm = TRUE)
DOGE_sent.28.sent<-cbind(DOGE_sent.28.sent,DOGE_sent.28[nrow(DOGE_sent.28),4])
colnames(DOGE_sent.28.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.28.sent

DOGE_sent.29<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 04:00:00" &
                       DOGE_sent$date <= "2021-05-28 04:59:59")
DOGE_sent.29$index<-seq(1,nrow(DOGE_sent.29),by=1)
DOGE_sent.29.sent<-mean(DOGE_sent.29$compound,na.rm = TRUE)
DOGE_sent.29.sent<-cbind(DOGE_sent.29.sent,DOGE_sent.29[nrow(DOGE_sent.29),4])
colnames(DOGE_sent.29.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.29.sent

DOGE_sent.30<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 05:00:00" &
                       DOGE_sent$date <= "2021-05-28 05:59:59")
DOGE_sent.30$index<-seq(1,nrow(DOGE_sent.30),by=1)
DOGE_sent.30.sent<-mean(DOGE_sent.30$compound,na.rm = TRUE)
DOGE_sent.30.sent<-cbind(DOGE_sent.30.sent,DOGE_sent.30[nrow(DOGE_sent.30),4])
colnames(DOGE_sent.30.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.30.sent

DOGE_sent.31<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 06:00:00" &
                       DOGE_sent$date <= "2021-05-28 06:59:59")
DOGE_sent.31$index<-seq(1,nrow(DOGE_sent.31),by=1)
DOGE_sent.31.sent<-mean(DOGE_sent.31$compound,na.rm = TRUE)
DOGE_sent.31.sent<-cbind(DOGE_sent.31.sent,DOGE_sent.31[nrow(DOGE_sent.31),4])
colnames(DOGE_sent.31.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.31.sent

DOGE_sent.32<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 07:00:00" &
                       DOGE_sent$date <= "2021-05-28 07:59:59")
DOGE_sent.32$index<-seq(1,nrow(DOGE_sent.32),by=1)
DOGE_sent.32.sent<-mean(DOGE_sent.32$compound,na.rm = TRUE)
DOGE_sent.32.sent<-cbind(DOGE_sent.32.sent,DOGE_sent.32[nrow(DOGE_sent.32),4])
colnames(DOGE_sent.32.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.32.sent

DOGE_sent.33<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 08:00:00" &
                       DOGE_sent$date <= "2021-05-28 08:59:59")
DOGE_sent.33$index<-seq(1,nrow(DOGE_sent.33),by=1)
DOGE_sent.33.sent<-mean(DOGE_sent.33$compound,na.rm = TRUE)
DOGE_sent.33.sent<-cbind(DOGE_sent.33.sent,DOGE_sent.33[nrow(DOGE_sent.33),4])
colnames(DOGE_sent.33.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.33.sent

DOGE_sent.34<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 09:00:00" &
                       DOGE_sent$date <= "2021-05-28 09:59:59")
DOGE_sent.34$index<-seq(1,nrow(DOGE_sent.34),by=1)
DOGE_sent.34.sent<-mean(DOGE_sent.34$compound,na.rm = TRUE)
DOGE_sent.34.sent<-cbind(DOGE_sent.34.sent,DOGE_sent.34[nrow(DOGE_sent.34),4])
colnames(DOGE_sent.34.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.34.sent

DOGE_sent.35<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 10:00:00" &
                       DOGE_sent$date <= "2021-05-28 10:59:59")
DOGE_sent.35$index<-seq(1,nrow(DOGE_sent.35),by=1)
DOGE_sent.35.sent<-mean(DOGE_sent.35$compound,na.rm = TRUE)
DOGE_sent.35.sent<-cbind(DOGE_sent.35.sent,DOGE_sent.35[nrow(DOGE_sent.35),4])
colnames(DOGE_sent.35.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.35.sent

DOGE_sent.36<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 11:00:00" &
                       DOGE_sent$date <= "2021-05-28 11:59:59")
DOGE_sent.36$index<-seq(1,nrow(DOGE_sent.36),by=1)
DOGE_sent.36.sent<-mean(DOGE_sent.36$compound,na.rm = TRUE)
DOGE_sent.36.sent<-cbind(DOGE_sent.36.sent,DOGE_sent.36[nrow(DOGE_sent.36),4])
colnames(DOGE_sent.36.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.36.sent

DOGE_sent.37<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 12:00:00" &
                       DOGE_sent$date <= "2021-05-28 12:59:59")
DOGE_sent.37$index<-seq(1,nrow(DOGE_sent.37),by=1)
DOGE_sent.37.sent<-mean(DOGE_sent.37$compound,na.rm = TRUE)
DOGE_sent.37.sent<-cbind(DOGE_sent.37.sent,DOGE_sent.37[nrow(DOGE_sent.37),4])
colnames(DOGE_sent.37.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.37.sent

DOGE_sent.38<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 13:00:00" &
                       DOGE_sent$date <= "2021-05-28 13:59:59")
DOGE_sent.38$index<-seq(1,nrow(DOGE_sent.38),by=1)
DOGE_sent.38.sent<-mean(DOGE_sent.38$compound,na.rm = TRUE)
DOGE_sent.38.sent<-cbind(DOGE_sent.38.sent,DOGE_sent.38[nrow(DOGE_sent.38),4])
colnames(DOGE_sent.38.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.38.sent

DOGE_sent.39<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 14:00:00" &
                       DOGE_sent$date <= "2021-05-28 14:59:59")
DOGE_sent.39$index<-seq(1,nrow(DOGE_sent.39),by=1)
DOGE_sent.39.sent<-mean(DOGE_sent.39$compound,na.rm = TRUE)
DOGE_sent.39.sent<-cbind(DOGE_sent.39.sent,DOGE_sent.39[nrow(DOGE_sent.39),4])
colnames(DOGE_sent.39.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.39.sent

DOGE_sent.40<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 15:00:00" &
                       DOGE_sent$date <= "2021-05-28 15:59:59")
DOGE_sent.40$index<-seq(1,nrow(DOGE_sent.40),by=1)
DOGE_sent.40.sent<-mean(DOGE_sent.40$compound,na.rm = TRUE)
DOGE_sent.40.sent<-cbind(DOGE_sent.40.sent,DOGE_sent.40[nrow(DOGE_sent.40),4])
colnames(DOGE_sent.40.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.40.sent

DOGE_sent.41<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 16:00:00" &
                       DOGE_sent$date <= "2021-05-28 16:59:59")
DOGE_sent.41$index<-seq(1,nrow(DOGE_sent.41),by=1)
DOGE_sent.41.sent<-mean(DOGE_sent.41$compound,na.rm = TRUE)
DOGE_sent.41.sent<-cbind(DOGE_sent.41.sent,DOGE_sent.41[nrow(DOGE_sent.41),4])
colnames(DOGE_sent.41.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.41.sent

DOGE_sent.42<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 17:00:00" &
                       DOGE_sent$date <= "2021-05-28 17:59:59")
DOGE_sent.42$index<-seq(1,nrow(DOGE_sent.42),by=1)
DOGE_sent.42.sent<-mean(DOGE_sent.42$compound,na.rm = TRUE)
DOGE_sent.42.sent<-cbind(DOGE_sent.42.sent,DOGE_sent.42[nrow(DOGE_sent.42),4])
colnames(DOGE_sent.42.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.42.sent

DOGE_sent.43<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 18:00:00" &
                       DOGE_sent$date <= "2021-05-28 18:59:59")
DOGE_sent.43$index<-seq(1,nrow(DOGE_sent.43),by=1)
DOGE_sent.43.sent<-mean(DOGE_sent.43$compound,na.rm = TRUE)
DOGE_sent.43.sent<-cbind(DOGE_sent.43.sent,DOGE_sent.43[nrow(DOGE_sent.43),4])
colnames(DOGE_sent.43.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.43.sent

DOGE_sent.44<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 19:00:00" &
                       DOGE_sent$date <= "2021-05-28 19:59:59")
DOGE_sent.44$index<-seq(1,nrow(DOGE_sent.44),by=1)
DOGE_sent.44.sent<-mean(DOGE_sent.44$compound,na.rm = TRUE)
DOGE_sent.44.sent<-cbind(DOGE_sent.44.sent,DOGE_sent.44[nrow(DOGE_sent.44),4])
colnames(DOGE_sent.44.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.44.sent

DOGE_sent.45<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 20:00:00" &
                       DOGE_sent$date <= "2021-05-28 20:59:59")
DOGE_sent.45$index<-seq(1,nrow(DOGE_sent.45),by=1)
DOGE_sent.45.sent<-mean(DOGE_sent.45$compound,na.rm = TRUE)
DOGE_sent.45.sent<-cbind(DOGE_sent.45.sent,DOGE_sent.45[nrow(DOGE_sent.45),4])
colnames(DOGE_sent.45.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.45.sent

DOGE_sent.46<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 21:00:00" &
                       DOGE_sent$date <= "2021-05-28 21:59:59")
DOGE_sent.46$index<-seq(1,nrow(DOGE_sent.46),by=1)
DOGE_sent.46.sent<-mean(DOGE_sent.46$compound,na.rm = TRUE)
DOGE_sent.46.sent<-cbind(DOGE_sent.46.sent,DOGE_sent.46[nrow(DOGE_sent.46),4])
colnames(DOGE_sent.46.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.46.sent

DOGE_sent.47<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 22:00:00" &
                       DOGE_sent$date <= "2021-05-28 22:59:59")
DOGE_sent.47$index<-seq(1,nrow(DOGE_sent.47),by=1)
DOGE_sent.47.sent<-mean(DOGE_sent.47$compound,na.rm = TRUE)
DOGE_sent.47.sent<-cbind(DOGE_sent.47.sent,DOGE_sent.47[nrow(DOGE_sent.47),4])
colnames(DOGE_sent.47.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.47.sent

DOGE_sent.48<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-28 23:00:00" &
                       DOGE_sent$date <= "2021-05-28 23:59:59")
DOGE_sent.48$index<-seq(1,nrow(DOGE_sent.48),by=1)
DOGE_sent.48.sent<-mean(DOGE_sent.48$compound,na.rm = TRUE)
DOGE_sent.48.sent<-cbind(DOGE_sent.48.sent,DOGE_sent.48[nrow(DOGE_sent.48),4])
colnames(DOGE_sent.48.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.48.sent

DOGE_sent.49<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 00:00:00" &
                       DOGE_sent$date <= "2021-05-29 00:59:59")
DOGE_sent.49$index<-seq(1,nrow(DOGE_sent.49),by=1)
DOGE_sent.49.sent<-mean(DOGE_sent.49$compound,na.rm = TRUE)
DOGE_sent.49.sent<-cbind(DOGE_sent.49.sent,DOGE_sent.49[nrow(DOGE_sent.49),4])
colnames(DOGE_sent.49.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.49.sent

DOGE_sent.50<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 01:00:00" &
                       DOGE_sent$date <= "2021-05-29 01:59:59")
DOGE_sent.50$index<-seq(1,nrow(DOGE_sent.50),by=1)
DOGE_sent.50.sent<-mean(DOGE_sent.50$compound,na.rm = TRUE)
DOGE_sent.50.sent<-cbind(DOGE_sent.50.sent,DOGE_sent.50[nrow(DOGE_sent.50),4])
colnames(DOGE_sent.50.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.50.sent

DOGE_sent.51<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 02:00:00" &
                       DOGE_sent$date <= "2021-05-29 02:59:59")
DOGE_sent.51$index<-seq(1,nrow(DOGE_sent.51),by=1)
DOGE_sent.51.sent<-mean(DOGE_sent.51$compound,na.rm = TRUE)
DOGE_sent.51.sent<-cbind(DOGE_sent.51.sent,DOGE_sent.51[nrow(DOGE_sent.51),4])
colnames(DOGE_sent.51.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.51.sent

DOGE_sent.52<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 03:00:00" &
                       DOGE_sent$date <= "2021-05-29 03:59:59")
DOGE_sent.52$index<-seq(1,nrow(DOGE_sent.52),by=1)
DOGE_sent.52.sent<-mean(DOGE_sent.52$compound,na.rm = TRUE)
DOGE_sent.52.sent<-cbind(DOGE_sent.52.sent,DOGE_sent.52[nrow(DOGE_sent.52),4])
colnames(DOGE_sent.52.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.52.sent

DOGE_sent.53<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 04:00:00" &
                       DOGE_sent$date <= "2021-05-29 04:59:59")
DOGE_sent.53$index<-seq(1,nrow(DOGE_sent.53),by=1)
DOGE_sent.53.sent<-mean(DOGE_sent.53$compound,na.rm = TRUE)
DOGE_sent.53.sent<-cbind(DOGE_sent.53.sent,DOGE_sent.53[nrow(DOGE_sent.53),4])
colnames(DOGE_sent.53.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.53.sent

DOGE_sent.54<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 05:00:00" &
                       DOGE_sent$date <= "2021-05-29 05:59:59")
DOGE_sent.54$index<-seq(1,nrow(DOGE_sent.54),by=1)
DOGE_sent.54.sent<-mean(DOGE_sent.54$compound,na.rm = TRUE)
DOGE_sent.54.sent<-cbind(DOGE_sent.54.sent,DOGE_sent.54[nrow(DOGE_sent.54),4])
colnames(DOGE_sent.54.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.54.sent

DOGE_sent.55<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 06:00:00" &
                       DOGE_sent$date <= "2021-05-29 06:59:59")
DOGE_sent.55$index<-seq(1,nrow(DOGE_sent.55),by=1)
DOGE_sent.55.sent<-mean(DOGE_sent.55$compound,na.rm = TRUE)
DOGE_sent.55.sent<-cbind(DOGE_sent.55.sent,DOGE_sent.55[nrow(DOGE_sent.55),4])
colnames(DOGE_sent.55.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.55.sent

DOGE_sent.56<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 07:00:00" &
                       DOGE_sent$date <= "2021-05-29 07:59:59")
DOGE_sent.56$index<-seq(1,nrow(DOGE_sent.56),by=1)
DOGE_sent.56.sent<-mean(DOGE_sent.56$compound,na.rm = TRUE)
DOGE_sent.56.sent<-cbind(DOGE_sent.56.sent,DOGE_sent.56[nrow(DOGE_sent.56),4])
colnames(DOGE_sent.56.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.56.sent

DOGE_sent.57<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 08:00:00" &
                       DOGE_sent$date <= "2021-05-29 08:59:59")
DOGE_sent.57$index<-seq(1,nrow(DOGE_sent.57),by=1)
DOGE_sent.57.sent<-mean(DOGE_sent.57$compound,na.rm = TRUE)
DOGE_sent.57.sent<-cbind(DOGE_sent.57.sent,DOGE_sent.57[nrow(DOGE_sent.57),4])
colnames(DOGE_sent.57.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.57.sent

DOGE_sent.58<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 09:00:00" &
                       DOGE_sent$date <= "2021-05-29 09:59:59")
DOGE_sent.58$index<-seq(1,nrow(DOGE_sent.58),by=1)
DOGE_sent.58.sent<-mean(DOGE_sent.58$compound,na.rm = TRUE)
DOGE_sent.58.sent<-cbind(DOGE_sent.58.sent,DOGE_sent.58[nrow(DOGE_sent.58),4])
colnames(DOGE_sent.58.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.58.sent

DOGE_sent.59<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 10:00:00" &
                       DOGE_sent$date <= "2021-05-29 10:59:59")
DOGE_sent.59$index<-seq(1,nrow(DOGE_sent.59),by=1)
DOGE_sent.59.sent<-mean(DOGE_sent.59$compound,na.rm = TRUE)
DOGE_sent.59.sent<-cbind(DOGE_sent.59.sent,DOGE_sent.59[nrow(DOGE_sent.59),4])
colnames(DOGE_sent.59.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.59.sent

DOGE_sent.60<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 11:00:00" &
                       DOGE_sent$date <= "2021-05-29 11:59:59")
DOGE_sent.60$index<-seq(1,nrow(DOGE_sent.60),by=1)
DOGE_sent.60.sent<-mean(DOGE_sent.60$compound,na.rm = TRUE)
DOGE_sent.60.sent<-cbind(DOGE_sent.60.sent,DOGE_sent.60[nrow(DOGE_sent.60),4])
colnames(DOGE_sent.60.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.60.sent

DOGE_sent.61<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 12:00:00" &
                       DOGE_sent$date <= "2021-05-29 12:59:59")
DOGE_sent.61$index<-seq(1,nrow(DOGE_sent.61),by=1)
DOGE_sent.61.sent<-mean(DOGE_sent.61$compound,na.rm = TRUE)
DOGE_sent.61.sent<-cbind(DOGE_sent.61.sent,DOGE_sent.61[nrow(DOGE_sent.61),4])
colnames(DOGE_sent.61.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.61.sent

DOGE_sent.62<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 13:00:00" &
                       DOGE_sent$date <= "2021-05-29 13:59:59")
DOGE_sent.62$index<-seq(1,nrow(DOGE_sent.62),by=1)
DOGE_sent.62.sent<-mean(DOGE_sent.62$compound,na.rm = TRUE)
DOGE_sent.62.sent<-cbind(DOGE_sent.62.sent,DOGE_sent.62[nrow(DOGE_sent.62),4])
colnames(DOGE_sent.62.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.62.sent

DOGE_sent.63<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 14:00:00" &
                       DOGE_sent$date <= "2021-05-29 14:59:59")
DOGE_sent.63$index<-seq(1,nrow(DOGE_sent.63),by=1)
DOGE_sent.63.sent<-mean(DOGE_sent.63$compound,na.rm = TRUE)
DOGE_sent.63.sent<-cbind(DOGE_sent.63.sent,DOGE_sent.63[nrow(DOGE_sent.63),4])
colnames(DOGE_sent.63.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.63.sent

DOGE_sent.64<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 15:00:00" &
                       DOGE_sent$date <= "2021-05-29 15:59:59")
DOGE_sent.64$index<-seq(1,nrow(DOGE_sent.64),by=1)
DOGE_sent.64.sent<-mean(DOGE_sent.64$compound,na.rm = TRUE)
DOGE_sent.64.sent<-cbind(DOGE_sent.64.sent,DOGE_sent.64[nrow(DOGE_sent.64),4])
colnames(DOGE_sent.64.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.64.sent

DOGE_sent.65<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 16:00:00" &
                       DOGE_sent$date <= "2021-05-29 16:59:59")
DOGE_sent.65$index<-seq(1,nrow(DOGE_sent.65),by=1)
DOGE_sent.65.sent<-mean(DOGE_sent.65$compound,na.rm = TRUE)
DOGE_sent.65.sent<-cbind(DOGE_sent.65.sent,DOGE_sent.65[nrow(DOGE_sent.65),4])
colnames(DOGE_sent.65.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.65.sent

DOGE_sent.66<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 17:00:00" &
                       DOGE_sent$date <= "2021-05-29 17:59:59")
DOGE_sent.66$index<-seq(1,nrow(DOGE_sent.66),by=1)
DOGE_sent.66.sent<-mean(DOGE_sent.66$compound,na.rm = TRUE)
DOGE_sent.66.sent<-cbind(DOGE_sent.66.sent,DOGE_sent.66[nrow(DOGE_sent.66),4])
colnames(DOGE_sent.66.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.66.sent

DOGE_sent.67<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 18:00:00" &
                       DOGE_sent$date <= "2021-05-29 18:59:59")
DOGE_sent.67$index<-seq(1,nrow(DOGE_sent.67),by=1)
DOGE_sent.67.sent<-mean(DOGE_sent.67$compound,na.rm = TRUE)
DOGE_sent.67.sent<-cbind(DOGE_sent.67.sent,DOGE_sent.67[nrow(DOGE_sent.67),4])
colnames(DOGE_sent.67.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.67.sent

DOGE_sent.68<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 19:00:00" &
                       DOGE_sent$date <= "2021-05-29 19:59:59")
DOGE_sent.68$index<-seq(1,nrow(DOGE_sent.68),by=1)
DOGE_sent.68.sent<-mean(DOGE_sent.68$compound,na.rm = TRUE)
DOGE_sent.68.sent<-cbind(DOGE_sent.68.sent,DOGE_sent.68[nrow(DOGE_sent.68),4])
colnames(DOGE_sent.68.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.68.sent

DOGE_sent.69<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 20:00:00" &
                       DOGE_sent$date <= "2021-05-29 20:59:59")
DOGE_sent.69$index<-seq(1,nrow(DOGE_sent.69),by=1)
DOGE_sent.69.sent<-mean(DOGE_sent.69$compound,na.rm = TRUE)
DOGE_sent.69.sent<-cbind(DOGE_sent.69.sent,DOGE_sent.69[nrow(DOGE_sent.69),4])
colnames(DOGE_sent.69.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.69.sent

DOGE_sent.70<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 21:00:00" &
                       DOGE_sent$date <= "2021-05-29 21:59:59")
DOGE_sent.70$index<-seq(1,nrow(DOGE_sent.70),by=1)
DOGE_sent.70.sent<-mean(DOGE_sent.70$compound,na.rm = TRUE)
DOGE_sent.70.sent<-cbind(DOGE_sent.70.sent,DOGE_sent.70[nrow(DOGE_sent.70),4])
colnames(DOGE_sent.70.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.70.sent

DOGE_sent.71<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 22:00:00" &
                       DOGE_sent$date <= "2021-05-29 22:59:59")
DOGE_sent.71$index<-seq(1,nrow(DOGE_sent.71),by=1)
DOGE_sent.71.sent<-mean(DOGE_sent.71$compound,na.rm = TRUE)
DOGE_sent.71.sent<-cbind(DOGE_sent.71.sent,DOGE_sent.71[nrow(DOGE_sent.71),4])
colnames(DOGE_sent.71.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.71.sent

DOGE_sent.72<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-29 23:00:00" &
                       DOGE_sent$date <= "2021-05-29 23:59:59")
DOGE_sent.72$index<-seq(1,nrow(DOGE_sent.72),by=1)
DOGE_sent.72.sent<-mean(DOGE_sent.72$compound,na.rm = TRUE)
DOGE_sent.72.sent<-cbind(DOGE_sent.72.sent,DOGE_sent.72[nrow(DOGE_sent.72),4])
colnames(DOGE_sent.72.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.72.sent

DOGE_sent.73<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 00:00:00" &
                       DOGE_sent$date <= "2021-05-30 00:59:59")
DOGE_sent.73$index<-seq(1,nrow(DOGE_sent.73),by=1)
DOGE_sent.73.sent<-mean(DOGE_sent.73$compound,na.rm = TRUE)
DOGE_sent.73.sent<-cbind(DOGE_sent.73.sent,DOGE_sent.73[nrow(DOGE_sent.73),4])
colnames(DOGE_sent.73.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.73.sent

DOGE_sent.74<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 01:00:00" &
                       DOGE_sent$date <= "2021-05-30 01:59:59")
DOGE_sent.74$index<-seq(1,nrow(DOGE_sent.74),by=1)
DOGE_sent.74.sent<-mean(DOGE_sent.74$compound,na.rm = TRUE)
DOGE_sent.74.sent<-cbind(DOGE_sent.74.sent,DOGE_sent.74[nrow(DOGE_sent.74),4])
colnames(DOGE_sent.74.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.74.sent

DOGE_sent.75<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 02:00:00" &
                       DOGE_sent$date <= "2021-05-30 02:59:59")
DOGE_sent.75$index<-seq(1,nrow(DOGE_sent.75),by=1)
DOGE_sent.75.sent<-mean(DOGE_sent.75$compound,na.rm = TRUE)
DOGE_sent.75.sent<-cbind(DOGE_sent.75.sent,DOGE_sent.75[nrow(DOGE_sent.75),4])
colnames(DOGE_sent.75.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.75.sent

DOGE_sent.76<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 03:00:00" &
                       DOGE_sent$date <= "2021-05-30 03:59:59")
DOGE_sent.76$index<-seq(1,nrow(DOGE_sent.76),by=1)
DOGE_sent.76.sent<-mean(DOGE_sent.76$compound,na.rm = TRUE)
DOGE_sent.76.sent<-cbind(DOGE_sent.76.sent,DOGE_sent.76[nrow(DOGE_sent.76),4])
colnames(DOGE_sent.76.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.76.sent

DOGE_sent.77<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 04:00:00" &
                       DOGE_sent$date <= "2021-05-30 04:59:59")
DOGE_sent.77$index<-seq(1,nrow(DOGE_sent.77),by=1)
DOGE_sent.77.sent<-mean(DOGE_sent.77$compound,na.rm = TRUE)
DOGE_sent.77.sent<-cbind(DOGE_sent.77.sent,DOGE_sent.77[nrow(DOGE_sent.77),4])
colnames(DOGE_sent.77.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.77.sent

DOGE_sent.78<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 05:00:00" &
                       DOGE_sent$date <= "2021-05-30 05:59:59")
DOGE_sent.78$index<-seq(1,nrow(DOGE_sent.78),by=1)
DOGE_sent.78.sent<-mean(DOGE_sent.78$compound,na.rm = TRUE)
DOGE_sent.78.sent<-cbind(DOGE_sent.78.sent,DOGE_sent.78[nrow(DOGE_sent.78),4])
colnames(DOGE_sent.78.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.78.sent

DOGE_sent.79<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 06:00:00" &
                       DOGE_sent$date <= "2021-05-30 06:59:59")
DOGE_sent.79$index<-seq(1,nrow(DOGE_sent.79),by=1)
DOGE_sent.79.sent<-mean(DOGE_sent.79$compound,na.rm = TRUE)
DOGE_sent.79.sent<-cbind(DOGE_sent.79.sent,DOGE_sent.79[nrow(DOGE_sent.79),4])
colnames(DOGE_sent.79.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.79.sent

DOGE_sent.80<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 07:00:00" &
                       DOGE_sent$date <= "2021-05-30 07:59:59")
DOGE_sent.80$index<-seq(1,nrow(DOGE_sent.80),by=1)
DOGE_sent.80.sent<-mean(DOGE_sent.80$compound,na.rm = TRUE)
DOGE_sent.80.sent<-cbind(DOGE_sent.80.sent,DOGE_sent.80[nrow(DOGE_sent.80),4])
colnames(DOGE_sent.80.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.80.sent

DOGE_sent.81<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 08:00:00" &
                       DOGE_sent$date <= "2021-05-30 08:59:59")
DOGE_sent.81$index<-seq(1,nrow(DOGE_sent.81),by=1)
DOGE_sent.81.sent<-mean(DOGE_sent.81$compound,na.rm = TRUE)
DOGE_sent.81.sent<-cbind(DOGE_sent.81.sent,DOGE_sent.81[nrow(DOGE_sent.81),4])
colnames(DOGE_sent.81.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.81.sent

DOGE_sent.82<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 09:00:00" &
                       DOGE_sent$date <= "2021-05-30 09:59:59")
DOGE_sent.82$index<-seq(1,nrow(DOGE_sent.82),by=1)
DOGE_sent.82.sent<-mean(DOGE_sent.82$compound,na.rm = TRUE)
DOGE_sent.82.sent<-cbind(DOGE_sent.82.sent,DOGE_sent.82[nrow(DOGE_sent.82),4])
colnames(DOGE_sent.82.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.82.sent

DOGE_sent.83<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 10:00:00" &
                       DOGE_sent$date <= "2021-05-30 10:59:59")
DOGE_sent.83$index<-seq(1,nrow(DOGE_sent.83),by=1)
DOGE_sent.83.sent<-mean(DOGE_sent.83$compound,na.rm = TRUE)
DOGE_sent.83.sent<-cbind(DOGE_sent.83.sent,DOGE_sent.83[nrow(DOGE_sent.83),4])
colnames(DOGE_sent.83.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.83.sent

DOGE_sent.84<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 11:00:00" &
                       DOGE_sent$date <= "2021-05-30 11:59:59")
DOGE_sent.84$index<-seq(1,nrow(DOGE_sent.84),by=1)
DOGE_sent.84.sent<-mean(DOGE_sent.84$compound,na.rm = TRUE)
DOGE_sent.84.sent<-cbind(DOGE_sent.84.sent,DOGE_sent.84[nrow(DOGE_sent.84),4])
colnames(DOGE_sent.84.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.84.sent

DOGE_sent.85<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 12:00:00" &
                       DOGE_sent$date <= "2021-05-30 12:59:59")
DOGE_sent.85$index<-seq(1,nrow(DOGE_sent.85),by=1)
DOGE_sent.85.sent<-mean(DOGE_sent.85$compound,na.rm = TRUE)
DOGE_sent.85.sent<-cbind(DOGE_sent.85.sent,DOGE_sent.85[nrow(DOGE_sent.85),4])
colnames(DOGE_sent.85.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.85.sent

DOGE_sent.86<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 13:00:00" &
                       DOGE_sent$date <= "2021-05-30 13:59:59")
DOGE_sent.86$index<-seq(1,nrow(DOGE_sent.86),by=1)
DOGE_sent.86.sent<-mean(DOGE_sent.86$compound,na.rm = TRUE)
DOGE_sent.86.sent<-cbind(DOGE_sent.86.sent,DOGE_sent.86[nrow(DOGE_sent.86),4])
colnames(DOGE_sent.86.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.86.sent

DOGE_sent.87<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 14:00:00" &
                       DOGE_sent$date <= "2021-05-30 14:59:59")
DOGE_sent.87$index<-seq(1,nrow(DOGE_sent.87),by=1)
DOGE_sent.87.sent<-mean(DOGE_sent.87$compound,na.rm = TRUE)
DOGE_sent.87.sent<-cbind(DOGE_sent.87.sent,DOGE_sent.87[nrow(DOGE_sent.87),4])
colnames(DOGE_sent.87.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.87.sent

DOGE_sent.88<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 15:00:00" &
                       DOGE_sent$date <= "2021-05-30 15:59:59")
DOGE_sent.88$index<-seq(1,nrow(DOGE_sent.88),by=1)
DOGE_sent.88.sent<-mean(DOGE_sent.88$compound,na.rm = TRUE)
DOGE_sent.88.sent<-cbind(DOGE_sent.88.sent,DOGE_sent.88[nrow(DOGE_sent.88),4])
colnames(DOGE_sent.88.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.88.sent

DOGE_sent.89<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 16:00:00" &
                       DOGE_sent$date <= "2021-05-30 16:59:59")
DOGE_sent.89$index<-seq(1,nrow(DOGE_sent.89),by=1)
DOGE_sent.89.sent<-mean(DOGE_sent.89$compound,na.rm = TRUE)
DOGE_sent.89.sent<-cbind(DOGE_sent.89.sent,DOGE_sent.89[nrow(DOGE_sent.89),4])
colnames(DOGE_sent.89.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.89.sent

DOGE_sent.90<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 17:00:00" &
                       DOGE_sent$date <= "2021-05-30 17:59:59")
DOGE_sent.90$index<-seq(1,nrow(DOGE_sent.90),by=1)
DOGE_sent.90.sent<-mean(DOGE_sent.90$compound,na.rm = TRUE)
DOGE_sent.90.sent<-cbind(DOGE_sent.90.sent,DOGE_sent.90[nrow(DOGE_sent.90),4])
colnames(DOGE_sent.90.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.90.sent

DOGE_sent.91<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 18:00:00" &
                       DOGE_sent$date <= "2021-05-30 18:59:59")
DOGE_sent.91$index<-seq(1,nrow(DOGE_sent.91),by=1)
DOGE_sent.91.sent<-mean(DOGE_sent.91$compound,na.rm = TRUE)
DOGE_sent.91.sent<-cbind(DOGE_sent.91.sent,DOGE_sent.91[nrow(DOGE_sent.91),4])
colnames(DOGE_sent.91.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.91.sent

DOGE_sent.92<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 19:00:00" &
                       DOGE_sent$date <= "2021-05-30 19:59:59")
DOGE_sent.92$index<-seq(1,nrow(DOGE_sent.92),by=1)
DOGE_sent.92.sent<-mean(DOGE_sent.92$compound,na.rm = TRUE)
DOGE_sent.92.sent<-cbind(DOGE_sent.92.sent,DOGE_sent.92[nrow(DOGE_sent.92),4])
colnames(DOGE_sent.92.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.92.sent

DOGE_sent.93<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 20:00:00" &
                       DOGE_sent$date <= "2021-05-30 20:59:59")
DOGE_sent.93$index<-seq(1,nrow(DOGE_sent.93),by=1)
DOGE_sent.93.sent<-mean(DOGE_sent.93$compound,na.rm = TRUE)
DOGE_sent.93.sent<-cbind(DOGE_sent.93.sent,DOGE_sent.93[nrow(DOGE_sent.93),4])
colnames(DOGE_sent.93.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.93.sent

DOGE_sent.94<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 21:00:00" &
                       DOGE_sent$date <= "2021-05-30 21:59:59")
DOGE_sent.94$index<-seq(1,nrow(DOGE_sent.94),by=1)
DOGE_sent.94.sent<-mean(DOGE_sent.94$compound,na.rm = TRUE)
DOGE_sent.94.sent<-cbind(DOGE_sent.94.sent,DOGE_sent.94[nrow(DOGE_sent.94),4])
colnames(DOGE_sent.94.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.94.sent

DOGE_sent.95<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 22:00:00" &
                       DOGE_sent$date <= "2021-05-30 22:59:59")
DOGE_sent.95$index<-seq(1,nrow(DOGE_sent.95),by=1)
DOGE_sent.95.sent<-mean(DOGE_sent.95$compound,na.rm = TRUE)
DOGE_sent.95.sent<-cbind(DOGE_sent.95.sent,DOGE_sent.95[nrow(DOGE_sent.95),4])
colnames(DOGE_sent.95.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.95.sent

DOGE_sent.96<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-30 23:00:00" &
                       DOGE_sent$date <= "2021-05-30 23:59:59")
DOGE_sent.96$index<-seq(1,nrow(DOGE_sent.96),by=1)
DOGE_sent.96.sent<-mean(DOGE_sent.96$compound,na.rm = TRUE)
DOGE_sent.96.sent<-cbind(DOGE_sent.96.sent,DOGE_sent.96[nrow(DOGE_sent.96),4])
colnames(DOGE_sent.96.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.96.sent

DOGE_sent.97<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-31 00:00:00" &
                       DOGE_sent$date <= "2021-05-31 00:59:59")
DOGE_sent.97$index<-seq(1,nrow(DOGE_sent.97),by=1)
DOGE_sent.97.sent<-mean(DOGE_sent.97$compound,na.rm = TRUE)
DOGE_sent.97.sent<-cbind(DOGE_sent.97.sent,DOGE_sent.97[nrow(DOGE_sent.97),4])
colnames(DOGE_sent.97.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.97.sent

DOGE_sent.98<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-31 01:00:00" &
                       DOGE_sent$date <= "2021-05-31 01:59:59")
DOGE_sent.98$index<-seq(1,nrow(DOGE_sent.98),by=1)
DOGE_sent.98.sent<-mean(DOGE_sent.98$compound,na.rm = TRUE)
DOGE_sent.98.sent<-cbind(DOGE_sent.98.sent,DOGE_sent.98[nrow(DOGE_sent.98),4])
colnames(DOGE_sent.98.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.98.sent

DOGE_sent.99<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-31 02:00:00" &
                       DOGE_sent$date <= "2021-05-31 02:59:59")
DOGE_sent.99$index<-seq(1,nrow(DOGE_sent.99),by=1)
DOGE_sent.99.sent<-mean(DOGE_sent.99$compound,na.rm = TRUE)
DOGE_sent.99.sent<-cbind(DOGE_sent.99.sent,DOGE_sent.99[nrow(DOGE_sent.99),4])
colnames(DOGE_sent.99.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.99.sent

DOGE_sent.100<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-31 03:00:00" &
                       DOGE_sent$date <= "2021-05-31 03:59:59")
DOGE_sent.100$index<-seq(1,nrow(DOGE_sent.100),by=1)
DOGE_sent.100.sent<-mean(DOGE_sent.100$compound,na.rm = TRUE)
DOGE_sent.100.sent<-cbind(DOGE_sent.100.sent,DOGE_sent.100[nrow(DOGE_sent.100),4])
colnames(DOGE_sent.100.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.100.sent

DOGE_sent.101<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-31 04:00:00" &
                        DOGE_sent$date <= "2021-05-31 04:59:59")
DOGE_sent.101$index<-seq(1,nrow(DOGE_sent.101),by=1)
DOGE_sent.101.sent<-mean(DOGE_sent.101$compound,na.rm = TRUE)
DOGE_sent.101.sent<-cbind(DOGE_sent.101.sent,DOGE_sent.101[nrow(DOGE_sent.101),4])
colnames(DOGE_sent.101.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.101.sent

DOGE_sent.102<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-31 05:00:00" &
                        DOGE_sent$date <= "2021-05-31 05:59:59")
DOGE_sent.102$index<-seq(1,nrow(DOGE_sent.102),by=1)
DOGE_sent.102.sent<-mean(DOGE_sent.102$compound,na.rm = TRUE)
DOGE_sent.102.sent<-cbind(DOGE_sent.102.sent,DOGE_sent.102[nrow(DOGE_sent.102),4])
colnames(DOGE_sent.102.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.102.sent

DOGE_sent.103<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-31 06:00:00" &
                        DOGE_sent$date <= "2021-05-31 06:59:59")
DOGE_sent.103$index<-seq(1,nrow(DOGE_sent.103),by=1)
DOGE_sent.103.sent<-mean(DOGE_sent.103$compound,na.rm = TRUE)
DOGE_sent.103.sent<-cbind(DOGE_sent.103.sent,DOGE_sent.103[nrow(DOGE_sent.103),4])
colnames(DOGE_sent.103.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.103.sent

DOGE_sent.104<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-31 07:00:00" &
                        DOGE_sent$date <= "2021-05-31 07:59:59")
DOGE_sent.104$index<-seq(1,nrow(DOGE_sent.104),by=1)
DOGE_sent.104.sent<-mean(DOGE_sent.104$compound,na.rm = TRUE)
DOGE_sent.104.sent<-cbind(DOGE_sent.104.sent,DOGE_sent.104[nrow(DOGE_sent.104),4])
colnames(DOGE_sent.104.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.104.sent

DOGE_sent.105<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-31 08:00:00" &
                        DOGE_sent$date <= "2021-05-31 08:59:59")
DOGE_sent.105$index<-seq(1,nrow(DOGE_sent.105),by=1)
DOGE_sent.105.sent<-mean(DOGE_sent.105$compound,na.rm = TRUE)
DOGE_sent.105.sent<-cbind(DOGE_sent.105.sent,DOGE_sent.105[nrow(DOGE_sent.105),4])
colnames(DOGE_sent.105.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.105.sent

DOGE_sent.106<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-31 09:00:00" &
                        DOGE_sent$date <= "2021-05-31 09:59:59")
DOGE_sent.106$index<-seq(1,nrow(DOGE_sent.106),by=1)
DOGE_sent.106.sent<-mean(DOGE_sent.106$compound,na.rm = TRUE)
DOGE_sent.106.sent<-cbind(DOGE_sent.106.sent,DOGE_sent.106[nrow(DOGE_sent.106),4])
colnames(DOGE_sent.106.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.106.sent

DOGE_sent.107<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-31 10:00:00" &
                        DOGE_sent$date <= "2021-05-31 10:59:59")
DOGE_sent.107$index<-seq(1,nrow(DOGE_sent.107),by=1)
DOGE_sent.107.sent<-mean(DOGE_sent.107$compound,na.rm = TRUE)
DOGE_sent.107.sent<-cbind(DOGE_sent.107.sent,DOGE_sent.107[nrow(DOGE_sent.107),4])
colnames(DOGE_sent.107.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.107.sent

DOGE_sent.108<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-31 11:00:00" &
                        DOGE_sent$date <= "2021-05-31 11:59:59")
DOGE_sent.108$index<-seq(1,nrow(DOGE_sent.108),by=1)
DOGE_sent.108.sent<-mean(DOGE_sent.108$compound,na.rm = TRUE)
DOGE_sent.108.sent<-cbind(DOGE_sent.108.sent,DOGE_sent.108[nrow(DOGE_sent.108),4])
colnames(DOGE_sent.108.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.108.sent

DOGE_sent.109<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-31 12:00:00" &
                        DOGE_sent$date <= "2021-05-31 12:59:59")
DOGE_sent.109$index<-seq(1,nrow(DOGE_sent.109),by=1)
DOGE_sent.109.sent<-mean(DOGE_sent.109$compound,na.rm = TRUE)
DOGE_sent.109.sent<-cbind(DOGE_sent.109.sent,DOGE_sent.109[nrow(DOGE_sent.109),4])
colnames(DOGE_sent.109.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.109.sent

DOGE_sent.110<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-31 13:00:00" &
                        DOGE_sent$date <= "2021-05-31 13:59:59")
DOGE_sent.110$index<-seq(1,nrow(DOGE_sent.110),by=1)
DOGE_sent.110.sent<-mean(DOGE_sent.110$compound,na.rm = TRUE)
DOGE_sent.110.sent<-cbind(DOGE_sent.110.sent,DOGE_sent.110[nrow(DOGE_sent.110),4])
colnames(DOGE_sent.110.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.110.sent

DOGE_sent.111<-subset(DOGE_sent,DOGE_sent$date >= "2021-05-31 14:00:00" &
                        DOGE_sent$date <= "2021-05-31 14:59:59")
DOGE_sent.111$index<-seq(1,nrow(DOGE_sent.111),by=1)
DOGE_sent.111.sent<-mean(DOGE_sent.111$compound,na.rm = TRUE)
DOGE_sent.111.sent<-cbind(DOGE_sent.111.sent,DOGE_sent.111[nrow(DOGE_sent.111),4])
colnames(DOGE_sent.111.sent)<-paste(c("avg.sent","volume"))
DOGE_sent.111.sent

DOGE_sent.hourly<-rbind(DOGE_sent.1.sent,DOGE_sent.2.sent,DOGE_sent.3.sent,DOGE_sent.4.sent,DOGE_sent.5.sent,DOGE_sent.6.sent,
                        DOGE_sent.7.sent,DOGE_sent.8.sent,DOGE_sent.9.sent,DOGE_sent.10.sent,DOGE_sent.11.sent,DOGE_sent.12.sent,
                        DOGE_sent.13.sent,DOGE_sent.14.sent,DOGE_sent.15.sent,DOGE_sent.16.sent,DOGE_sent.17.sent,DOGE_sent.18.sent,
                        DOGE_sent.19.sent,DOGE_sent.20.sent,DOGE_sent.21.sent,DOGE_sent.22.sent,DOGE_sent.23.sent,DOGE_sent.24.sent,
                        DOGE_sent.25.sent,DOGE_sent.26.sent,DOGE_sent.27.sent,DOGE_sent.28.sent,DOGE_sent.29.sent,DOGE_sent.30.sent,
                        DOGE_sent.31.sent,DOGE_sent.32.sent,DOGE_sent.33.sent,DOGE_sent.34.sent,DOGE_sent.35.sent,DOGE_sent.36.sent,
                        DOGE_sent.37.sent,DOGE_sent.38.sent,DOGE_sent.39.sent,DOGE_sent.40.sent,DOGE_sent.41.sent,DOGE_sent.42.sent,
                        DOGE_sent.43.sent,DOGE_sent.44.sent,DOGE_sent.45.sent,DOGE_sent.46.sent,DOGE_sent.47.sent,DOGE_sent.48.sent,
                        DOGE_sent.49.sent,DOGE_sent.50.sent,DOGE_sent.51.sent,DOGE_sent.52.sent,DOGE_sent.53.sent,DOGE_sent.54.sent,
                        DOGE_sent.55.sent,DOGE_sent.56.sent,DOGE_sent.57.sent,DOGE_sent.58.sent,DOGE_sent.59.sent,DOGE_sent.60.sent,
                        DOGE_sent.61.sent,DOGE_sent.62.sent,DOGE_sent.63.sent,DOGE_sent.64.sent,DOGE_sent.65.sent,DOGE_sent.66.sent,
                        DOGE_sent.67.sent,DOGE_sent.68.sent,DOGE_sent.69.sent,DOGE_sent.70.sent,DOGE_sent.71.sent,DOGE_sent.72.sent,
                        DOGE_sent.73.sent,DOGE_sent.74.sent,DOGE_sent.75.sent,DOGE_sent.76.sent,DOGE_sent.77.sent,DOGE_sent.78.sent,
                        DOGE_sent.79.sent,DOGE_sent.80.sent,DOGE_sent.81.sent,DOGE_sent.82.sent,DOGE_sent.83.sent,DOGE_sent.84.sent,
                        DOGE_sent.85.sent,DOGE_sent.86.sent,DOGE_sent.87.sent,DOGE_sent.88.sent,DOGE_sent.89.sent,DOGE_sent.90.sent,
                        DOGE_sent.91.sent,DOGE_sent.92.sent,DOGE_sent.93.sent,DOGE_sent.94.sent,DOGE_sent.95.sent,DOGE_sent.96.sent,
                        DOGE_sent.97.sent,DOGE_sent.98.sent,DOGE_sent.99.sent,DOGE_sent.100.sent,DOGE_sent.101.sent,DOGE_sent.102.sent,
                        DOGE_sent.103.sent,DOGE_sent.104.sent,DOGE_sent.105.sent,DOGE_sent.106.sent,DOGE_sent.107.sent,DOGE_sent.108.sent,
                        DOGE_sent.109.sent,DOGE_sent.110.sent,DOGE_sent.111.sent)

date<-seq(from=as.POSIXct("2021-05-27 00:00:00", tz="UTC"), 
          to=as.POSIXct("2021-05-31 14:00:00", tz="UTC"), by="hour")
date<- as.data.frame(date)

DOGE_sent.hourly <- cbind(date, DOGE_sent.hourly)
head(DOGE_sent.hourly)

write_as_csv(DOGE_sent.hourly, "DOGE_sent.hourly.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(DOGE_sent.hourly, "DOGE_sent.hourly.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
DOGE_sent.hourly<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/DOGE_sent.hourly.csv",header = TRUE)


#**S6: AVERAGE SENTIMENT SCORE PER DAY -------------------------------------
DOGE_sent.hourly[c(1:3,nrow(DOGE_sent.hourly)),]
DOGE_sent.daily<-DOGE_sent.hourly[c(1:96),]

DOGE_sent.daily1<-subset(DOGE_sent.daily,DOGE_sent.daily$date >= "2021-05-27 00:00:00" &
                           DOGE_sent.daily$date <= "2021-05-27 23:00:00")
DOGE_sent.daily1.sent<-mean(DOGE_sent.daily1$avg.sent,na.rm = TRUE)
DOGE_sent.daily1.vol<-sum(DOGE_sent.daily1$volume)
DOGE_sent.daily1<-cbind(DOGE_sent.daily1.sent,DOGE_sent.daily1.vol)
colnames(DOGE_sent.daily1)<-paste(c("Daily.sent","Daily.vol"))
DOGE_sent.daily1

DOGE_sent.daily2<-subset(DOGE_sent.daily,DOGE_sent.daily$date >= "2021-05-28 00:00:00" &
                           DOGE_sent.daily$date <= "2021-05-28 23:00:00")
DOGE_sent.daily2.sent<-mean(DOGE_sent.daily2$avg.sent,na.rm = TRUE)
DOGE_sent.daily2.vol<-sum(DOGE_sent.daily2$volume)
DOGE_sent.daily2<-cbind(DOGE_sent.daily2.sent,DOGE_sent.daily2.vol)
colnames(DOGE_sent.daily2)<-paste(c("Daily.sent","Daily.vol"))
DOGE_sent.daily2

DOGE_sent.daily3<-subset(DOGE_sent.daily,DOGE_sent.daily$date >= "2021-05-29 00:00:00" &
                           DOGE_sent.daily$date <= "2021-05-29 23:00:00")
DOGE_sent.daily3.sent<-mean(DOGE_sent.daily3$avg.sent,na.rm = TRUE)
DOGE_sent.daily3.vol<-sum(DOGE_sent.daily3$volume)
DOGE_sent.daily3<-cbind(DOGE_sent.daily3.sent,DOGE_sent.daily3.vol)
colnames(DOGE_sent.daily3)<-paste(c("Daily.sent","Daily.vol"))
DOGE_sent.daily3

DOGE_sent.daily4<-subset(DOGE_sent.daily,DOGE_sent.daily$date >= "2021-05-30 00:00:00" &
                           DOGE_sent.daily$date <= "2021-05-30 23:00:00")
DOGE_sent.daily4.sent<-mean(DOGE_sent.daily4$avg.sent,na.rm = TRUE)
DOGE_sent.daily4.vol<-sum(DOGE_sent.daily4$volume)
DOGE_sent.daily4<-cbind(DOGE_sent.daily4.sent,DOGE_sent.daily4.vol)
colnames(DOGE_sent.daily4)<-paste(c("Daily.sent","Daily.vol"))
DOGE_sent.daily4

DOGE_sent.daily.tot<-rbind(DOGE_sent.daily1,DOGE_sent.daily2,DOGE_sent.daily3,DOGE_sent.daily4)

date<-seq(from=as.POSIXct("2021-05-27 00:00:00", tz="UTC"), 
          to=as.POSIXct("2021-05-30 00:00:00", tz="UTC"), by="day")
date<- as.data.frame(date)

DOGE_sent.daily.tot <- cbind(date, DOGE_sent.daily.tot)

options(scipen = 1)
DOGE_sent.daily.tot$Daily.sent.diff<-Delt(DOGE_sent.daily.tot$Daily.sent)
DOGE_sent.daily.tot$Daily.vol.diff<-Delt(DOGE_sent.daily.tot$Daily.vol)
colnames(DOGE_sent.daily.tot)<-paste(c("date","Daily.sent","Daily.vol","Daily.sent.diff","Daily.vol.diff"))
DOGE_sent.daily.tot[is.na(DOGE_sent.daily.tot)] <- 0

write_as_csv(DOGE_sent.daily.tot, "DOGE_sent.daily.tot.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(DOGE_sent.daily.tot, "DOGE_sent.daily.tot.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
DOGE_sent.daily.tot<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/DOGE_sent.daily.tot.csv",header = TRUE)


# PLOTS -------------------------------------------------------------------
##**Plot the frequency of tweets over a variety of time intervals ---------
ggplot(DOGE_sent.hourly,aes(x=date))+
  geom_line(aes(y=volume))+
  theme_bw()+
  labs(x = NULL, y = NULL,
       title = "Frequency of #DOGE tweets",
       subtitle = paste0(format(min(as.Date(DOGE_twt$created_at)), "%d %B %Y"), " to ", format(max(as.Date(DOGE_twt$created_at)),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet")

##**Plot the tweets sentiment score ---------------------------------------
ggplot(DOGE_sent,aes(x=compound)) + 
  geom_histogram(bins = 15,alpha=.6) + 
  theme_bw() +
  labs(x="Score",y="Number of tweets")+
  labs(title = "Sentiment analysis of #DOGE Tweets from 27-31 May 2021")+
  labs(subtitle = "Using VADER lexicon")

##**Plot the hourly tweets sentiment score + volume -----------------------------
# Value used to transform the data
coeff <- 0.0001

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(DOGE_sent.hourly, aes(x=date)) +
  
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
  labs(title = "Hourly tweets sentiment scores and volume of #DOGE",
       caption = "Using  VADER appproach")

# PART III: ANALYSIS FROM PART I + II -------------------------------------
# *S1: Extract necessary data ----------------------------------------------
DOGE.price.hourly<-DOGEUSD_1hr[c(10:nrow(DOGEUSD_1hr)),]
DOGE.price.hourly<-DOGE.price.hourly[,c(2,7,8)]
names(DOGE.price.hourly)<-paste(c("Date","Close","Volume"))
DOGE.price.hourly<-DOGE.price.hourly[order(DOGE.price.hourly$Date),]

DOGE.price.hourly[c(1:3,nrow(DOGE.price.hourly)),]

DOGE_sent.hourly[c(1:3,nrow(DOGE_sent.hourly)),]


# *S2: CALCULATE HOURLY CHANGES IN PRICE/SENTIMENT -------------------------------

#HOURLY LOGARITHMIC PRICE RETURNS
DOGE.hourly.log.ret <- as.data.frame(DOGE.price.hourly)
DOGE.hourly.log.ret$DOGE.log.ret <- c(diff(log(DOGE.hourly.log.ret$Close)),0)
options(digits = 3)

#HOURLY LOGARITHMIC SENTIMENT
DOGE_sent.diff<-DOGE_sent.hourly
DOGE_sent.diff$log.sent<-c(diff(log(DOGE_sent.diff$avg.sent)),0)

#TWEETS VOLUME LOGARITHMIC CHANGES
DOGE_vol.diff<-DOGE_sent.hourly
DOGE_vol.diff$vol.log<- c(diff(log(DOGE_vol.diff$volume)),0)

#COMBINE THE RESULTS
DOGE.log.change<-cbind(DOGE.hourly.log.ret$DOGE.log.ret,DOGE_sent.diff$log.sent,DOGE_vol.diff$vol.log)
colnames(DOGE.log.change)<-paste(c("DOGEUSD","DOGE","Volume"))
DOGE.log.change<-as.data.frame(DOGE.log.change)
# *S3: SMA -----------------------------------------------------------------
DOGE.sma.3<-DOGE.price.hourly[,c(1:2)]
DOGE.sma.3$sma5 <- rollmeanr(DOGE.sma.3$Close, k =5, fill=NA)
DOGE.sma.3$sma10 <- rollmeanr(DOGE.sma.3$Close, k = 10, fill=NA)

#**S4: CORRELATION MATRIX: VOLUME, SENTIMENT, PRICE ------------------------
#HOURLY TABLE
hourly.table<-DOGE_sent.diff[,-4]
hourly.table<-cbind(hourly.table,DOGE.hourly.log.ret$Close,DOGE.hourly.log.ret$DOGE.log.ret)
colnames(hourly.table)<-paste(c("Date","Hourly.sent","Hourly.vol","Hourly.close","Hourly.log.ret"))

hourly.table$sent.idx<-hourly.table$Hourly.sent/hourly.table$Hourly.sent[1]
hourly.table$vol.idx<-hourly.table$Hourly.vol/hourly.table$Hourly.vol[1]
hourly.table$close.idx<-hourly.table$Hourly.close/hourly.table$Hourly.close[1]

#DAILY TABLE
DOGE.price.daily<-subset(data.DOGE,index(data.DOGE)>="2021-05-27" &
                          index(data.DOGE)<="2021-05-30")
DOGE.price.daily<-DOGE.price.daily[,6]
daily.table<-DOGE_sent.daily.tot[,c(-4,-5)]
daily.table<-cbind(daily.table,DOGE.price.daily$`DOGE-USD.Adjusted`)
daily.table$Daily.log.ret<-c(0,diff(log(daily.table$`DOGE-USD.Adjusted`)))
colnames(daily.table)<-paste(c("Date","Daily.sent","Daily.vol","Daily.close","Daily.log.ret"))

daily.table$sent.idx<-daily.table$Daily.sent/daily.table$Daily.sent[1]
daily.table$vol.idx<-daily.table$Daily.vol/daily.table$Daily.vol[1]
daily.table$close.idx<-daily.table$Daily.close/daily.table$Daily.close[1]

#**S5: CROSS-CORRELATION ANALYSIS WITH LAGS USING ACF/CCF ---------------------------------------------
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
##**Correlation Scatterplot Between #DOGE Sentiment changes and DOGE Price Return--------
plot(DOGE.log.change$DOGEUSD, DOGE.log.change$DOGE, pch = 19, 
     main = "Correlation Matrix 
     Between Hourly #DOGE Sentiment changes and DOGE Price return", xlab = "DOGE-USD", ylab = "#DOGE")
abline(lm(DOGE.log.change$DOGEUSD ~ DOGE.log.change$DOGE), col = "red")
text(x = 0.025, y = -0.2, label = "r = -0.00528 ", col = "red")

##**Correlation Scatterplot Between #DOGE Volume changes and DOGE Price Return --------
plot(DOGE.log.change$DOGEUSD, DOGE.log.change$Volume, pch = 19, 
     main = "Correlation Matrix Between Hourly #DOGE Volume changes and DOGE Price return", xlab = "DOGE-USD", ylab = "#DOGE Volume")
abline(lm( DOGE.log.change$DOGEUSD ~ DOGE.log.change$Volume), col = "red")
text(x = 0.05, y = -0.3, label = "r = -0.00121", col = "red")

##**Plot the hourly price returns + volume --------------------------------
return.volume.plot<-DOGE_sent.hourly
return.volume.plot$price.ret<-DOGE_sent.diff$log.sent

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
  labs(title = "Hourly Logarithmic price returns and volume of #DOGE",
       subtitle = "Using VADER appproach")

##**Tweets volume - Price returns - Price of DOGE --------------------------
p1 <- ggplot(DOGE_sent.hourly,aes(x=date))+
  geom_bar(aes(y=volume), stat="identity", size=.1, color="black", alpha=.4)+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(x="Hours",y="Tweets Volume")+
  labs(title = "Hourly Tweets Volume of #DOGE")

p2 <- ggplot(DOGE.hourly.log.ret, aes(x=Date)) + 
  geom_line(aes(y=Close)) + 
  theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Hours", y = "Price ($)")+
  labs(title = "Hourly Price of DOGE-USD", 
       subtitle =paste0(format(min(as.Date(DOGE.hourly.log.ret$Date)), "%d %B %Y"), " to ", format(max(as.Date(DOGE.hourly.log.ret$Date)),"%d %B %Y")))

p3 <- ggplot(DOGE.hourly.log.ret,aes(x=Date)) + 
  geom_line(aes(y=DOGE.log.ret)) +
  geom_hline(yintercept = 0,col="red") +
  theme_bw() +
  labs(x="Hours",y="Price Returns (%)")+
  labs(title = "Hourly Logarithmic Price Returns of DOGE-USD",caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3), size = "last"))

##**Daily Sentiment score - Tweets volume - Close Prices of DOGE -----------
DOGE.price.daily<- subset(data.DOGE,index(data.DOGE) >= "2021-05-27" &
                           index(data.DOGE) <= "2021-05-31")
DOGE.price.daily<-DOGE.price.daily[,6]
DOGE.price.daily$ret<-diff(log(DOGE.price.daily$`DOGE-USD.Adjusted`))
DOGE.price.daily[1,2]<-0

p1 <- ggplot(DOGE_sent.daily.tot,aes(x=date))+
  geom_line(aes(y=Daily.sent), stat="identity", size=.1, color="black", alpha=.4)+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(x="Days",y="Daily Sentiment Score")+
  labs(title = "Daily Sentiment Scores of #DOGE")

p2 <- ggplot(DOGE_sent.daily.tot, aes(x=date)) + 
  geom_line(aes(y=Daily.vol)) + 
  theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Days", y = "Tweets Volume")+
  labs(title = "Daily Volume of #DOGE Tweets")

p3 <- ggplot(DOGE.price.daily,aes(x=index(DOGE.price.daily))) + 
  geom_line(aes(y=DOGE.USD.Adjusted)) +
  theme_bw() +
  labs(x="Days",y="Prices ($)", 
       subtitle =paste0(format(min(as.Date(DOGE_sent.daily.tot$date)), "%d %B %Y"), " to ", format(max(as.Date(DOGE_sent.daily.tot$date)),"%d %B %Y")))+
  labs(title = "Daily Prices of DOGE-USD",caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3), size = "last"))

##**CORRELATION MATRIX: VOLUME, SENTIMENT, PRICE --------------------------
#Hourly data
hourly.table.corr.mat <- as.data.frame(cbind(hourly.table$sent.idx,hourly.table$vol.idx,hourly.table$close.idx))
colnames(hourly.table.corr.mat)<-paste(c("Sent","Vol","Close Price"))
corrplot(cor(hourly.table.corr.mat), method = "number", title = "Hourly Correlation Matrix #DOGE", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

#Daily data
daily.table.corr.mat <- as.data.frame(cbind(daily.table$sent.idx,daily.table$vol.idx,daily.table$close.idx))
colnames(daily.table.corr.mat)<-paste(c("Sent","Vol","Close Price"))
corrplot(cor(daily.table.corr.mat), method = "number", title = "Daily Correlation Matrix #DOGE", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

##**SMA + TRENDLINE during the period 2021-05-27 -> 2021-05-31 ------------------------
ggplot(DOGE.sma.3,aes(x=Date))+
  geom_line(aes(y=Close))+
  theme_bw()+
  labs(x="Date",y="Price ($)",
       title = "DOGE-USD Simple Moving Average",
       subtitle = "May 27, 2021 - May 31, 2021")+
  geom_line(aes(y=predict(lm(Close~index(DOGE.sma.3)))),col="red")+ #trendline
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
                   max.lag = 100, alpha = 0.05, lambda = 2.576, plot = TRUE,
                   table = TRUE, var.names = NULL)

#Daily
daily.cc<-cc.test(daily.table_ts[, c("Daily.sent")], daily.table_ts[, c("Daily.log.ret")], 
                  max.lag =3, alpha = 0.05, lambda = 2.576, plot = TRUE,
                  table = TRUE, var.names = NULL)