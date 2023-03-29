#ETHERIUM (ETH) 23 MAY 12H -31 MAY 12H 2021
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
data.ETH<-getSymbols("ETH-USD",from="2016-05-31",to="2021-05-31",auto.assign = FALSE)

ETHUSD_1hr<-read_csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Hourly Data/ETHUSD_1hr_woline1.csv")
ETHUSD_1hr<-subset(ETHUSD_1hr,ETHUSD_1hr$Date>="2016-05-31" &
                     ETHUSD_1hr$Date<="2021-06-01")
ETHUSD_1hr<-ETHUSD_1hr[c(10:43843),] #Clear out those rows with unnecessary dates

# *S2: DAILY PRICE RETURNS 5 YEARS -------------------------------------------------------
#DAILY LOGARITHMIC TOTAL RETURNS
ETH.log.ret <- data.ETH[,6]
ETH.log.ret$ETH.log.ret <- diff(log(ETH.log.ret$`ETH-USD.Adjusted`))
options(digits = 3)
ETH.log.ret <- ETH.log.ret[,2]
ETH.log.ret[1,1]<-0
ETH.log.ret$Gross.ret<-ETH.log.ret$ETH.log.ret+1
ETH.log.ret$Gross.ret[1]<-1
ETH.log.ret$cum.ret<-cumprod(ETH.log.ret$Gross.ret)

#CUMULATED DAILY RETURN AFTER 5 YEARS
ETH.logcumret <- sum(ETH.log.ret$ETH.log.ret,na.rm=TRUE)
ETH.cumret <- exp(ETH.logcumret)-1
ETH.cumret #165

#Mean returns, Std-Dev, Min, Max, Skewness, Kurtosis
ETH.mean<-mean(ETH.log.ret$ETH.log.ret,na.rm=TRUE)
ETH.sd<-sd(ETH.log.ret$ETH.log.ret,na.rm=TRUE)
ETH.min<-min(ETH.log.ret$ETH.log.ret,na.rm=TRUE)
ETH.max<-max(ETH.log.ret$ETH.log.ret,na.rm=TRUE)
ETH.skew<-skewness(ETH.log.ret$ETH.log.ret,na.rm=TRUE)
ETH.kurt<-kurtosis(ETH.log.ret$ETH.log.ret,na.rm=TRUE)
ETH.summary.table<-cbind(ETH.cumret,ETH.mean,ETH.sd,ETH.min,ETH.max,ETH.skew,ETH.kurt)
rownames(ETH.summary.table)<-c("2016-2021")
ETH.summary.table

# *S3: SIMPLE MOVING AVERAGE & TREND ---------------------------------------------------

#Step 1: Create ETH.sma
ETH.sma <- data.ETH[,6]
ETH.sma[is.na(ETH.sma)] <- 0
colnames(ETH.sma)<-paste(c("Adjusted"))
#Step 2: Create 50-day and 100-day rolling average columns
ETH.sma$sma50 <- rollmeanr(ETH.sma$Adjusted, k =50)
ETH.sma$sma100 <- rollmeanr(ETH.sma$Adjusted, k = 100)
ETH.sma$no<-c(1:nrow(ETH.sma)) #This supports creation of Trendline

#**S4: DAILY EWMA VOLATILITY -----------------------------------------------------
#Step 1,2: Create squared logarithmic price return of ETH
ETH_squared <- data.ETH[,6]
ETH_squared$ETH.squared <- (diff(log(ETH_squared$`ETH-USD.Adjusted`)))^2
options(digits = 3)
ETH_squared <- ETH_squared[,2]
ETH_squared[1,1]<-0

ETH_squared2 <- as.numeric(ETH_squared) # Squared returns as a numeric vector

#Step 3: Create EWMA function
ewma <- function(ETH_squared2, lambda){
  stats::filter(ETH_squared2*(1-lambda), lambda, "recursive", init = ETH_squared2[1])
}

# Step 4: Calculate variance with lambda=0.94
ETH_var_ewma94 <- ewma(ETH_squared2, 0.94)

# Step 5: Transform back to a zoo object (time index from object ETH_squared)
ETH_var_ewma <- zoo(ETH_var_ewma94,index(ETH_squared))

#Step 6: Volatility
ETH_vola_ewma <- sqrt(ETH_var_ewma)
# *S5: ALPHA & BETA USING CAPM, MARKET MODEL, AND ROLLING WINDOW REGRESSION --------
#Knowledge from Ang 2015, Chapter 5
#CAPM
#Step 0: Construct a monthly returns of ETH
data.ETH[c(1:3,nrow(data.ETH)),]

ETH.monthly<-to.monthly(data.ETH)
ETH.monthly[c(1:3,nrow(ETH.monthly)),]

ETH.monthly<-ETH.monthly[,6]

ETH.ret<-Delt(ETH.monthly$data.ETH.Adjusted)
names(ETH.ret)<-paste("ETH.ret")
ETH.ret<-ETH.ret[-1,]
ETH.ret[c(1:3,nrow(ETH.ret)),]

csv.ETH<-cbind(data.frame(index(ETH.ret)),data.frame(ETH.ret))
names(csv.ETH)[1]<-paste("date")
rownames(csv.ETH)<-seq(1,nrow(csv.ETH),by=1)
csv.ETH[c(1:3,nrow(csv.ETH)),]

write.csv(csv.ETH,"ETH Returns (Monthly).csv") #Now, we have the csv file for the next steps.

#Step 1: Import Portfolio Returns and Convert to a data.frame Object
ETH<-read.csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Others/ETH Returns (Monthly).csv", header = TRUE)
ETH[c(1:3,nrow(ETH)),]
ETH$date<-as.yearmon(as.character(ETH$date),"%b %Y")
ETH[c(1:3,nrow(ETH)),]
ETH.df<-data.frame(ETH)
ETH.df[c(1:3,nrow(ETH.df)),]

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
combo.ETH<-cbind(market.df,data.frame(rf.monthly),ETH.df$ETH.ret)
combo.ETH[c(1:3,nrow(combo.ETH)),]
names(combo.ETH)<-paste(c("mkt.ret","rf","ETH.ret"))
combo.ETH[c(1:3,nrow(combo.ETH)),]

#Step 5: Calculate Excess BTC Return and Excess Market Return
#Excess BTC Return (exret) = port.ret - rf
#Excess Market Return (exmkt) = mkt.ret - rf
combo.ETH$exret<-combo.ETH$ETH.ret -combo.ETH$rf
combo.ETH$exmkt<-combo.ETH$mkt.ret-combo.ETH$rf
combo.ETH[c(1:3,nrow(combo.ETH)),]

#Step 6: Run Regression of Excess BTC Return on Excess Market Return
#We use "lm" command to calculate OLS Regression between exret & exmkt
options(digits = 3)
CAPM<-lm(combo.ETH$exret~combo.ETH$exmkt) #lm stands for "linear model", the function that creates a simple regression model
summary(CAPM)

#Rolling Window Regression
#In this section, we run regression through a rolling window, which calculate Alphas and Betas in different periods.
#=> Thus we can see how variable these two are, over a time period.

#Step 1: Import ETH and S&P 500 Index Data 
data.ETH[c(1:3,nrow(data.ETH)),]
data.mkt[c(1:3,nrow(data.mkt)),]

#Step 2: Calculate the BTC and Market Returns
rets<-diff(log(data.ETH$`ETH-USD.Adjusted`))
rets$GSPC<-diff(log(data.mkt$GSPC.Adjusted))
names(rets)[1]<-"ETH"
rets<-rets[-1,]
rets[c(1:3,nrow(rets)),]

#Step 3: Create the Rolling Window Regression Function
require(zoo)
coeffs<-rollapply(rets,
                  width = 252,
                  FUN = function(X)
                  {
                    roll.reg=lm(ETH~GSPC,
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
##**Daily Price movement of ETH 2016-2021 ---------------------------------
ggplot(data.ETH,aes(x=index(data.ETH)))+
  geom_line(aes(y=data.ETH$`ETH-USD.Adjusted`))+
  labs(x = "Date", y = "Value ($)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + # centers headline
  labs(title = "Value of $1 Investment in Etherium May 31, 2016 - May 31, 2021",
       caption = "Data retrieved from Yahoo! Finance")

##**Hourly Price movement of ETH 2016-2021 (the same with daily price movement) --------
ggplot(ETHUSD_1hr, aes(Date,group=1)) + geom_line(aes(y=Close)) + theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Date", y = "Price ($)")+
  labs(title = " ETH Hourly Price", 
       subtitle =paste0(format(min(as.Date(ETHUSD_1hr$Date)), "%d %B %Y"), " to ", format(max(as.Date(ETHUSD_1hr$Date)),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

##**Daily Log return volatility 2016-2021 ---------------------------------
ETH.log.ret<-as.data.frame(ETH.log.ret)
ETH.volatility<-ggplot(ETH.log.ret,aes(x=index(ETH.log.ret)))+
  geom_line(aes(y=ETH.log.ret))+
  labs(x = "Date", y = "Value of Investment ($)") +
  labs(title = "Volatility of ETH price
May 31, 2016 - May 31, 2021")+
  theme_bw()# white background
ETH.volatility<-ETH.volatility+geom_hline(yintercept = 0,col="red")
ETH.volatility
##**SMA + Trendline ---------------------------------------------------
ggplot(ETH.sma,aes(x=index(ETH.sma)))+
  geom_line(aes(y=Adjusted))+
  theme_bw()+
  labs(x="Date",y="Price ($)",
       title = "ETH-USD Simple Moving Average",
       subtitle = "May 21, 2016 - May 21, 2021")+
  geom_line(aes(y=predict(lm(Adjusted~no))),col="red")+ #trendline
  geom_line(aes(y=sma50), col="green",alpha=.4,size=.1)+
  geom_line(aes(y=sma100),col="red",alpha=.4,size=.1)

##**DAILY EWMA --------------------------------------------------------------
plot(ETH_var_ewma, main="ETH - Daily Variance with EWMA, lambda=0.94", xlab="Date", ylab="Variance", col="blue", type="l")
plot(ETH_vola_ewma,  main="ETH - Daily Volatility with EWMA, lambda=0.94", xlab="Date", ylab="Volatility", col="blue", type="l")

##**ALPHA-BETA --------------------------------------------------------------
p1 <- ggplot(coeffs, aes(index(coeffs), coeffs$Alpha)) + geom_line() + theme_minimal() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(angle = 90))+
  labs(x = "Date", y = "Alpha") +
  labs(title = "Etherium Alpha and Beta
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
ETH_twt<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/Tweets data/ETH_twt.csv",header = TRUE)

# *S2: Clean Tweets ------------------------------------------------------
tweets.ETH<- ETH_twt  %>% dplyr::select(created_at, screen_name,text) #Create a table with "created_at", "screen_name", and "text" columns from BTC_twt
tweets.ETH[c(1:3,nrow(tweets.ETH)),]

tweets.ETH$stripped_text1<-gsub("https(.*)*$","",tweets.ETH$text) #Remove URLs
tweets.ETH$stripped_text1<-tolower(tweets.ETH$stripped_text1) #To lowercase
tweets.ETH$stripped_text1<-gsub("\\.\\.","",tweets.ETH$stripped_text1) #replace ..... with .
tweets.ETH$stripped_text1<-gsub("(.)\\1{2,}", "\\1",tweets.ETH$stripped_text1) #Transform "Goooooo" to "go"
tweets.ETH$stripped_text1<-gsub("([[:punct:]])\\1+", "\\1", tweets.ETH$stripped_text1) #Reduce !!!!! to !
tweets.ETH$stripped_text1<-gsub("#","", tweets.ETH$stripped_text1) #Remove hashtags
tweets.ETH$stripped_text1<-gsub("@[[:alnum:]]+","", tweets.ETH$stripped_text1) #Remove mentions
tweets.ETH$stripped_text1<-gsub("&amp","and", tweets.ETH$stripped_text1) #Replace &apm with and
tweets.ETH$stripped_text1<-gsub("<(.*)>","", tweets.ETH$stripped_text1) #Remove unicodes <U+...>
tweets.ETH$stripped_text1<-iconv(tweets.ETH$stripped_text1, "latin1", "ASCII", sub="") #remove weird letters
tweets.ETH$stripped_text1<- gsub("%%", "\'", tweets.ETH$stripped_text1) # Changing %% back to apostrophes
tweets.ETH$stripped_text1<-str_squish(tweets.ETH$stripped_text1) #Remove excessive spaces

# *S3: Run and save VADER analysis -----------------------------------------------------------
vader_eth = vader_df(tweets.ETH$stripped_text1)
vader_eth[c(1:3,nrow(vader_eth)),]
write_as_csv(vader_eth, "vader_eth.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(vader_eth, "vader_eth.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

# *S3 bis: Load sentiment data ---------------------------------------------
vader_eth<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/vader_eth.csv",header = TRUE)
ETH_sent<-vader_eth[,c(3:4)]
date<-tweets.ETH$created_at
ETH_sent$date<-date

# *S4: AVERAGE SENTIMENT SCORE OVER THE PERIOD -----------------------------
ETH_sent[c(1:3,nrow(ETH_sent)),]
ETH_sent.avg<-mean(ETH_sent$compound,na.rm=TRUE)
ETH_sent.avg #0.304

# *S5: AVERAGE SENTIMENT SCORE PER HOUR ----------------------------------------

ETH_sent.1<-subset(ETH_sent,ETH_sent$date >= "2021-05-23 12:00:00" &
                     ETH_sent$date <= "2021-05-23 12:59:59")
ETH_sent.1$index<-seq(1,nrow(ETH_sent.1),by=1)
ETH_sent.1.sent<-mean(ETH_sent.1$compound,na.rm = TRUE)
ETH_sent.1.sent<-cbind(ETH_sent.1.sent,ETH_sent.1[nrow(ETH_sent.1),4])
colnames(ETH_sent.1.sent)<-paste(c("avg.sent","volume"))
ETH_sent.1.sent

ETH_sent.2<-subset(ETH_sent,ETH_sent$date >= "2021-05-23 13:00:00" &
                     ETH_sent$date <= "2021-05-23 13:59:59")
ETH_sent.2$index<-seq(1,nrow(ETH_sent.2),by=1)
ETH_sent.2.sent<-mean(ETH_sent.2$compound,na.rm = TRUE)
ETH_sent.2.sent<-cbind(ETH_sent.2.sent,ETH_sent.2[nrow(ETH_sent.2),4])
colnames(ETH_sent.2.sent)<-paste(c("avg.sent","volume"))
ETH_sent.2.sent

ETH_sent.3<-subset(ETH_sent,ETH_sent$date >= "2021-05-23 14:00:00" &
                     ETH_sent$date <= "2021-05-23 14:59:59")
ETH_sent.3$index<-seq(1,nrow(ETH_sent.3),by=1)
ETH_sent.3.sent<-mean(ETH_sent.3$compound,na.rm = TRUE)
ETH_sent.3.sent<-cbind(ETH_sent.3.sent,ETH_sent.3[nrow(ETH_sent.3),4])
colnames(ETH_sent.3.sent)<-paste(c("avg.sent","volume"))
ETH_sent.3.sent

ETH_sent.4<-subset(ETH_sent,ETH_sent$date >= "2021-05-23 15:00:00" &
                     ETH_sent$date <= "2021-05-23 15:59:59")
ETH_sent.4$index<-seq(1,nrow(ETH_sent.4),by=1)
ETH_sent.4.sent<-mean(ETH_sent.4$compound,na.rm = TRUE)
ETH_sent.4.sent<-cbind(ETH_sent.4.sent,ETH_sent.4[nrow(ETH_sent.4),4])
colnames(ETH_sent.4.sent)<-paste(c("avg.sent","volume"))
ETH_sent.4.sent

ETH_sent.5<-subset(ETH_sent,ETH_sent$date >= "2021-05-23 16:00:00" &
                     ETH_sent$date <= "2021-05-23 16:59:59")
ETH_sent.5$index<-seq(1,nrow(ETH_sent.5),by=1)
ETH_sent.5.sent<-mean(ETH_sent.5$compound,na.rm = TRUE)
ETH_sent.5.sent<-cbind(ETH_sent.5.sent,ETH_sent.5[nrow(ETH_sent.5),4])
colnames(ETH_sent.5.sent)<-paste(c("avg.sent","volume"))
ETH_sent.5.sent

ETH_sent.6<-subset(ETH_sent,ETH_sent$date >= "2021-05-23 17:00:00" &
                     ETH_sent$date <= "2021-05-23 17:59:59")
ETH_sent.6$index<-seq(1,nrow(ETH_sent.6),by=1)
ETH_sent.6.sent<-mean(ETH_sent.6$compound,na.rm = TRUE)
ETH_sent.6.sent<-cbind(ETH_sent.6.sent,ETH_sent.6[nrow(ETH_sent.6),4])
colnames(ETH_sent.6.sent)<-paste(c("avg.sent","volume"))
ETH_sent.6.sent

ETH_sent.7<-subset(ETH_sent,ETH_sent$date >= "2021-05-23 18:00:00" &
                     ETH_sent$date <= "2021-05-23 18:59:59")
ETH_sent.7$index<-seq(1,nrow(ETH_sent.7),by=1)
ETH_sent.7.sent<-mean(ETH_sent.7$compound,na.rm = TRUE)
ETH_sent.7.sent<-cbind(ETH_sent.7.sent,ETH_sent.7[nrow(ETH_sent.7),4])
colnames(ETH_sent.7.sent)<-paste(c("avg.sent","volume"))
ETH_sent.7.sent

ETH_sent.8<-subset(ETH_sent,ETH_sent$date >= "2021-05-23 19:00:00" &
                     ETH_sent$date <= "2021-05-23 19:59:59")
ETH_sent.8$index<-seq(1,nrow(ETH_sent.8),by=1)
ETH_sent.8.sent<-mean(ETH_sent.8$compound,na.rm = TRUE)
ETH_sent.8.sent<-cbind(ETH_sent.8.sent,ETH_sent.8[nrow(ETH_sent.8),4])
colnames(ETH_sent.8.sent)<-paste(c("avg.sent","volume"))
ETH_sent.8.sent

ETH_sent.9<-subset(ETH_sent,ETH_sent$date >= "2021-05-23 20:00:00" &
                     ETH_sent$date <= "2021-05-23 20:59:59")
ETH_sent.9$index<-seq(1,nrow(ETH_sent.9),by=1)
ETH_sent.9.sent<-mean(ETH_sent.9$compound,na.rm = TRUE)
ETH_sent.9.sent<-cbind(ETH_sent.9.sent,ETH_sent.9[nrow(ETH_sent.9),4])
colnames(ETH_sent.9.sent)<-paste(c("avg.sent","volume"))
ETH_sent.9.sent

ETH_sent.10<-subset(ETH_sent,ETH_sent$date >= "2021-05-23 21:00:00" &
                     ETH_sent$date <= "2021-05-23 21:59:59")
ETH_sent.10$index<-seq(1,nrow(ETH_sent.10),by=1)
ETH_sent.10.sent<-mean(ETH_sent.10$compound,na.rm = TRUE)
ETH_sent.10.sent<-cbind(ETH_sent.10.sent,ETH_sent.10[nrow(ETH_sent.10),4])
colnames(ETH_sent.10.sent)<-paste(c("avg.sent","volume"))
ETH_sent.10.sent

ETH_sent.11<-subset(ETH_sent,ETH_sent$date >= "2021-05-23 22:00:00" &
                     ETH_sent$date <= "2021-05-23 22:59:59")
ETH_sent.11$index<-seq(1,nrow(ETH_sent.11),by=1)
ETH_sent.11.sent<-mean(ETH_sent.11$compound,na.rm = TRUE)
ETH_sent.11.sent<-cbind(ETH_sent.11.sent,ETH_sent.11[nrow(ETH_sent.11),4])
colnames(ETH_sent.11.sent)<-paste(c("avg.sent","volume"))
ETH_sent.11.sent

ETH_sent.12<-subset(ETH_sent,ETH_sent$date >= "2021-05-23 23:00:00" &
                     ETH_sent$date <= "2021-05-23 23:59:59")
ETH_sent.12$index<-seq(1,nrow(ETH_sent.12),by=1)
ETH_sent.12.sent<-mean(ETH_sent.12$compound,na.rm = TRUE)
ETH_sent.12.sent<-cbind(ETH_sent.12.sent,ETH_sent.12[nrow(ETH_sent.12),4])
colnames(ETH_sent.12.sent)<-paste(c("avg.sent","volume"))
ETH_sent.12.sent

ETH_sent.13<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 00:00:00" &
                     ETH_sent$date <= "2021-05-24 00:59:59")
ETH_sent.13$index<-seq(1,nrow(ETH_sent.13),by=1)
ETH_sent.13.sent<-mean(ETH_sent.13$compound,na.rm = TRUE)
ETH_sent.13.sent<-cbind(ETH_sent.13.sent,ETH_sent.13[nrow(ETH_sent.13),4])
colnames(ETH_sent.13.sent)<-paste(c("avg.sent","volume"))
ETH_sent.13.sent

ETH_sent.14<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 01:00:00" &
                      ETH_sent$date <= "2021-05-24 01:59:59")
ETH_sent.14$index<-seq(1,nrow(ETH_sent.14),by=1)
ETH_sent.14.sent<-mean(ETH_sent.14$compound,na.rm = TRUE)
ETH_sent.14.sent<-cbind(ETH_sent.14.sent,ETH_sent.14[nrow(ETH_sent.14),4])
colnames(ETH_sent.14.sent)<-paste(c("avg.sent","volume"))
ETH_sent.14.sent

ETH_sent.15<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 02:00:00" &
                      ETH_sent$date <= "2021-05-24 02:59:59")
ETH_sent.15$index<-seq(1,nrow(ETH_sent.15),by=1)
ETH_sent.15.sent<-mean(ETH_sent.15$compound,na.rm = TRUE)
ETH_sent.15.sent<-cbind(ETH_sent.15.sent,ETH_sent.15[nrow(ETH_sent.15),4])
colnames(ETH_sent.15.sent)<-paste(c("avg.sent","volume"))
ETH_sent.15.sent

ETH_sent.16<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 03:00:00" &
                      ETH_sent$date <= "2021-05-24 03:59:59")
ETH_sent.16$index<-seq(1,nrow(ETH_sent.16),by=1)
ETH_sent.16.sent<-mean(ETH_sent.16$compound,na.rm = TRUE)
ETH_sent.16.sent<-cbind(ETH_sent.16.sent,ETH_sent.16[nrow(ETH_sent.16),4])
colnames(ETH_sent.16.sent)<-paste(c("avg.sent","volume"))
ETH_sent.16.sent

ETH_sent.17<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 04:00:00" &
                      ETH_sent$date <= "2021-05-24 04:59:59")
ETH_sent.17$index<-seq(1,nrow(ETH_sent.17),by=1)
ETH_sent.17.sent<-mean(ETH_sent.17$compound,na.rm = TRUE)
ETH_sent.17.sent<-cbind(ETH_sent.17.sent,ETH_sent.17[nrow(ETH_sent.17),4])
colnames(ETH_sent.17.sent)<-paste(c("avg.sent","volume"))
ETH_sent.17.sent

ETH_sent.18<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 05:00:00" &
                      ETH_sent$date <= "2021-05-24 05:59:59")
ETH_sent.18$index<-seq(1,nrow(ETH_sent.18),by=1)
ETH_sent.18.sent<-mean(ETH_sent.18$compound,na.rm = TRUE)
ETH_sent.18.sent<-cbind(ETH_sent.18.sent,ETH_sent.18[nrow(ETH_sent.18),4])
colnames(ETH_sent.18.sent)<-paste(c("avg.sent","volume"))
ETH_sent.18.sent

ETH_sent.19<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 06:00:00" &
                      ETH_sent$date <= "2021-05-24 06:59:59")
ETH_sent.19$index<-seq(1,nrow(ETH_sent.19),by=1)
ETH_sent.19.sent<-mean(ETH_sent.19$compound,na.rm = TRUE)
ETH_sent.19.sent<-cbind(ETH_sent.19.sent,ETH_sent.19[nrow(ETH_sent.19),4])
colnames(ETH_sent.19.sent)<-paste(c("avg.sent","volume"))
ETH_sent.19.sent

ETH_sent.20<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 07:00:00" &
                      ETH_sent$date <= "2021-05-24 07:59:59")
ETH_sent.20$index<-seq(1,nrow(ETH_sent.20),by=1)
ETH_sent.20.sent<-mean(ETH_sent.20$compound,na.rm = TRUE)
ETH_sent.20.sent<-cbind(ETH_sent.20.sent,ETH_sent.20[nrow(ETH_sent.20),4])
colnames(ETH_sent.20.sent)<-paste(c("avg.sent","volume"))
ETH_sent.20.sent

ETH_sent.21<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 08:00:00" &
                      ETH_sent$date <= "2021-05-24 08:59:59")
ETH_sent.21$index<-seq(1,nrow(ETH_sent.21),by=1)
ETH_sent.21.sent<-mean(ETH_sent.21$compound,na.rm = TRUE)
ETH_sent.21.sent<-cbind(ETH_sent.21.sent,ETH_sent.21[nrow(ETH_sent.21),4])
colnames(ETH_sent.21.sent)<-paste(c("avg.sent","volume"))
ETH_sent.21.sent

ETH_sent.22<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 09:00:00" &
                      ETH_sent$date <= "2021-05-24 09:59:59")
ETH_sent.22$index<-seq(1,nrow(ETH_sent.22),by=1)
ETH_sent.22.sent<-mean(ETH_sent.22$compound,na.rm = TRUE)
ETH_sent.22.sent<-cbind(ETH_sent.22.sent,ETH_sent.22[nrow(ETH_sent.22),4])
colnames(ETH_sent.22.sent)<-paste(c("avg.sent","volume"))
ETH_sent.22.sent

ETH_sent.23<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 10:00:00" &
                      ETH_sent$date <= "2021-05-24 10:59:59")
ETH_sent.23$index<-seq(1,nrow(ETH_sent.23),by=1)
ETH_sent.23.sent<-mean(ETH_sent.23$compound,na.rm = TRUE)
ETH_sent.23.sent<-cbind(ETH_sent.23.sent,ETH_sent.23[nrow(ETH_sent.23),4])
colnames(ETH_sent.23.sent)<-paste(c("avg.sent","volume"))
ETH_sent.23.sent

ETH_sent.24<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 11:00:00" &
                      ETH_sent$date <= "2021-05-24 11:59:59")
ETH_sent.24$index<-seq(1,nrow(ETH_sent.24),by=1)
ETH_sent.24.sent<-mean(ETH_sent.24$compound,na.rm = TRUE)
ETH_sent.24.sent<-cbind(ETH_sent.24.sent,ETH_sent.24[nrow(ETH_sent.24),4])
colnames(ETH_sent.24.sent)<-paste(c("avg.sent","volume"))
ETH_sent.24.sent

ETH_sent.25<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 12:00:00" &
                      ETH_sent$date <= "2021-05-24 12:59:59")
ETH_sent.25$index<-seq(1,nrow(ETH_sent.25),by=1)
ETH_sent.25.sent<-mean(ETH_sent.25$compound,na.rm = TRUE)
ETH_sent.25.sent<-cbind(ETH_sent.25.sent,ETH_sent.25[nrow(ETH_sent.25),4])
colnames(ETH_sent.25.sent)<-paste(c("avg.sent","volume"))
ETH_sent.25.sent

ETH_sent.26<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 13:00:00" &
                      ETH_sent$date <= "2021-05-24 13:59:59")
ETH_sent.26$index<-seq(1,nrow(ETH_sent.26),by=1)
ETH_sent.26.sent<-mean(ETH_sent.26$compound,na.rm = TRUE)
ETH_sent.26.sent<-cbind(ETH_sent.26.sent,ETH_sent.26[nrow(ETH_sent.26),4])
colnames(ETH_sent.26.sent)<-paste(c("avg.sent","volume"))
ETH_sent.26.sent

ETH_sent.27<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 14:00:00" &
                      ETH_sent$date <= "2021-05-24 14:59:59")
ETH_sent.27$index<-seq(1,nrow(ETH_sent.27),by=1)
ETH_sent.27.sent<-mean(ETH_sent.27$compound,na.rm = TRUE)
ETH_sent.27.sent<-cbind(ETH_sent.27.sent,ETH_sent.27[nrow(ETH_sent.27),4])
colnames(ETH_sent.27.sent)<-paste(c("avg.sent","volume"))
ETH_sent.27.sent

ETH_sent.28<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 15:00:00" &
                      ETH_sent$date <= "2021-05-24 15:59:59")
ETH_sent.28$index<-seq(1,nrow(ETH_sent.28),by=1)
ETH_sent.28.sent<-mean(ETH_sent.28$compound,na.rm = TRUE)
ETH_sent.28.sent<-cbind(ETH_sent.28.sent,ETH_sent.28[nrow(ETH_sent.28),4])
colnames(ETH_sent.28.sent)<-paste(c("avg.sent","volume"))
ETH_sent.28.sent

ETH_sent.29<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 16:00:00" &
                      ETH_sent$date <= "2021-05-24 16:59:59")
ETH_sent.29$index<-seq(1,nrow(ETH_sent.29),by=1)
ETH_sent.29.sent<-mean(ETH_sent.29$compound,na.rm = TRUE)
ETH_sent.29.sent<-cbind(ETH_sent.29.sent,ETH_sent.29[nrow(ETH_sent.29),4])
colnames(ETH_sent.29.sent)<-paste(c("avg.sent","volume"))
ETH_sent.29.sent

ETH_sent.30<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 17:00:00" &
                      ETH_sent$date <= "2021-05-24 17:59:59")
ETH_sent.30$index<-seq(1,nrow(ETH_sent.30),by=1)
ETH_sent.30.sent<-mean(ETH_sent.30$compound,na.rm = TRUE)
ETH_sent.30.sent<-cbind(ETH_sent.30.sent,ETH_sent.30[nrow(ETH_sent.30),4])
colnames(ETH_sent.30.sent)<-paste(c("avg.sent","volume"))
ETH_sent.30.sent

ETH_sent.31<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 18:00:00" &
                      ETH_sent$date <= "2021-05-24 18:59:59")
ETH_sent.31$index<-seq(1,nrow(ETH_sent.31),by=1)
ETH_sent.31.sent<-mean(ETH_sent.31$compound,na.rm = TRUE)
ETH_sent.31.sent<-cbind(ETH_sent.31.sent,ETH_sent.31[nrow(ETH_sent.31),4])
colnames(ETH_sent.31.sent)<-paste(c("avg.sent","volume"))
ETH_sent.31.sent

ETH_sent.32<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 19:00:00" &
                      ETH_sent$date <= "2021-05-24 19:59:59")
ETH_sent.32$index<-seq(1,nrow(ETH_sent.32),by=1)
ETH_sent.32.sent<-mean(ETH_sent.32$compound,na.rm = TRUE)
ETH_sent.32.sent<-cbind(ETH_sent.32.sent,ETH_sent.32[nrow(ETH_sent.32),4])
colnames(ETH_sent.32.sent)<-paste(c("avg.sent","volume"))
ETH_sent.32.sent


ETH_sent.33<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 20:00:00" &
                      ETH_sent$date <= "2021-05-24 20:59:59")
ETH_sent.33$index<-seq(1,nrow(ETH_sent.33),by=1)
ETH_sent.33.sent<-mean(ETH_sent.33$compound,na.rm = TRUE)
ETH_sent.33.sent<-cbind(ETH_sent.33.sent,ETH_sent.33[nrow(ETH_sent.33),4])
colnames(ETH_sent.33.sent)<-paste(c("avg.sent","volume"))
ETH_sent.33.sent


ETH_sent.34<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 21:00:00" &
                      ETH_sent$date <= "2021-05-24 21:59:59")
ETH_sent.34$index<-seq(1,nrow(ETH_sent.34),by=1)
ETH_sent.34.sent<-mean(ETH_sent.34$compound,na.rm = TRUE)
ETH_sent.34.sent<-cbind(ETH_sent.34.sent,ETH_sent.34[nrow(ETH_sent.34),4])
colnames(ETH_sent.34.sent)<-paste(c("avg.sent","volume"))
ETH_sent.34.sent


ETH_sent.35<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 22:00:00" &
                      ETH_sent$date <= "2021-05-24 22:59:59")
ETH_sent.35$index<-seq(1,nrow(ETH_sent.35),by=1)
ETH_sent.35.sent<-mean(ETH_sent.35$compound,na.rm = TRUE)
ETH_sent.35.sent<-cbind(ETH_sent.35.sent,ETH_sent.35[nrow(ETH_sent.35),4])
colnames(ETH_sent.35.sent)<-paste(c("avg.sent","volume"))
ETH_sent.35.sent


ETH_sent.36<-subset(ETH_sent,ETH_sent$date >= "2021-05-24 23:00:00" &
                      ETH_sent$date <= "2021-05-24 23:59:59")
ETH_sent.36$index<-seq(1,nrow(ETH_sent.36),by=1)
ETH_sent.36.sent<-mean(ETH_sent.36$compound,na.rm = TRUE)
ETH_sent.36.sent<-cbind(ETH_sent.36.sent,ETH_sent.36[nrow(ETH_sent.36),4])
colnames(ETH_sent.36.sent)<-paste(c("avg.sent","volume"))
ETH_sent.36.sent

ETH_sent.37<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 00:00:00" &
                      ETH_sent$date <= "2021-05-25 00:59:59")
ETH_sent.37$index<-seq(1,nrow(ETH_sent.37),by=1)
ETH_sent.37.sent<-mean(ETH_sent.37$compound,na.rm = TRUE)
ETH_sent.37.sent<-cbind(ETH_sent.37.sent,ETH_sent.37[nrow(ETH_sent.37),4])
colnames(ETH_sent.37.sent)<-paste(c("avg.sent","volume"))
ETH_sent.37.sent

ETH_sent.38<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 01:00:00" &
                      ETH_sent$date <= "2021-05-25 01:59:59")
ETH_sent.38$index<-seq(1,nrow(ETH_sent.38),by=1)
ETH_sent.38.sent<-mean(ETH_sent.38$compound,na.rm = TRUE)
ETH_sent.38.sent<-cbind(ETH_sent.38.sent,ETH_sent.38[nrow(ETH_sent.38),4])
colnames(ETH_sent.38.sent)<-paste(c("avg.sent","volume"))
ETH_sent.38.sent


ETH_sent.39<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 02:00:00" &
                      ETH_sent$date <= "2021-05-25 02:59:59")
ETH_sent.39$index<-seq(1,nrow(ETH_sent.39),by=1)
ETH_sent.39.sent<-mean(ETH_sent.39$compound,na.rm = TRUE)
ETH_sent.39.sent<-cbind(ETH_sent.39.sent,ETH_sent.39[nrow(ETH_sent.39),4])
colnames(ETH_sent.39.sent)<-paste(c("avg.sent","volume"))
ETH_sent.39.sent


ETH_sent.40<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 03:00:00" &
                      ETH_sent$date <= "2021-05-25 03:59:59")
ETH_sent.40$index<-seq(1,nrow(ETH_sent.40),by=1)
ETH_sent.40.sent<-mean(ETH_sent.40$compound,na.rm = TRUE)
ETH_sent.40.sent<-cbind(ETH_sent.40.sent,ETH_sent.40[nrow(ETH_sent.40),4])
colnames(ETH_sent.40.sent)<-paste(c("avg.sent","volume"))
ETH_sent.40.sent


ETH_sent.41<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 04:00:00" &
                      ETH_sent$date <= "2021-05-25 04:59:59")
ETH_sent.41$index<-seq(1,nrow(ETH_sent.41),by=1)
ETH_sent.41.sent<-mean(ETH_sent.41$compound,na.rm = TRUE)
ETH_sent.41.sent<-cbind(ETH_sent.41.sent,ETH_sent.41[nrow(ETH_sent.41),4])
colnames(ETH_sent.41.sent)<-paste(c("avg.sent","volume"))
ETH_sent.41.sent


ETH_sent.42<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 05:00:00" &
                      ETH_sent$date <= "2021-05-25 05:59:59")
ETH_sent.42$index<-seq(1,nrow(ETH_sent.42),by=1)
ETH_sent.42.sent<-mean(ETH_sent.42$compound,na.rm = TRUE)
ETH_sent.42.sent<-cbind(ETH_sent.42.sent,ETH_sent.42[nrow(ETH_sent.42),4])
colnames(ETH_sent.42.sent)<-paste(c("avg.sent","volume"))
ETH_sent.42.sent


ETH_sent.43<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 06:00:00" &
                      ETH_sent$date <= "2021-05-25 06:59:59")
ETH_sent.43$index<-seq(1,nrow(ETH_sent.43),by=1)
ETH_sent.43.sent<-mean(ETH_sent.43$compound,na.rm = TRUE)
ETH_sent.43.sent<-cbind(ETH_sent.43.sent,ETH_sent.43[nrow(ETH_sent.43),4])
colnames(ETH_sent.43.sent)<-paste(c("avg.sent","volume"))
ETH_sent.43.sent


ETH_sent.44<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 07:00:00" &
                      ETH_sent$date <= "2021-05-25 07:59:59")
ETH_sent.44$index<-seq(1,nrow(ETH_sent.44),by=1)
ETH_sent.44.sent<-mean(ETH_sent.44$compound,na.rm = TRUE)
ETH_sent.44.sent<-cbind(ETH_sent.44.sent,ETH_sent.44[nrow(ETH_sent.44),4])
colnames(ETH_sent.44.sent)<-paste(c("avg.sent","volume"))
ETH_sent.44.sent


ETH_sent.45<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 08:00:00" &
                      ETH_sent$date <= "2021-05-25 08:59:59")
ETH_sent.45$index<-seq(1,nrow(ETH_sent.45),by=1)
ETH_sent.45.sent<-mean(ETH_sent.45$compound,na.rm = TRUE)
ETH_sent.45.sent<-cbind(ETH_sent.45.sent,ETH_sent.45[nrow(ETH_sent.45),4])
colnames(ETH_sent.45.sent)<-paste(c("avg.sent","volume"))
ETH_sent.45.sent

ETH_sent.46<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 09:00:00" &
                      ETH_sent$date <= "2021-05-25 09:59:59")
ETH_sent.46$index<-seq(1,nrow(ETH_sent.46),by=1)
ETH_sent.46.sent<-mean(ETH_sent.46$compound,na.rm = TRUE)
ETH_sent.46.sent<-cbind(ETH_sent.46.sent,ETH_sent.46[nrow(ETH_sent.46),4])
colnames(ETH_sent.46.sent)<-paste(c("avg.sent","volume"))
ETH_sent.46.sent


ETH_sent.47<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 10:00:00" &
                      ETH_sent$date <= "2021-05-25 10:59:59")
ETH_sent.47$index<-seq(1,nrow(ETH_sent.47),by=1)
ETH_sent.47.sent<-mean(ETH_sent.47$compound,na.rm = TRUE)
ETH_sent.47.sent<-cbind(ETH_sent.47.sent,ETH_sent.47[nrow(ETH_sent.47),4])
colnames(ETH_sent.47.sent)<-paste(c("avg.sent","volume"))
ETH_sent.47.sent


ETH_sent.48<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 11:00:00" &
                      ETH_sent$date <= "2021-05-25 11:59:59")
ETH_sent.48$index<-seq(1,nrow(ETH_sent.48),by=1)
ETH_sent.48.sent<-mean(ETH_sent.48$compound,na.rm = TRUE)
ETH_sent.48.sent<-cbind(ETH_sent.48.sent,ETH_sent.48[nrow(ETH_sent.48),4])
colnames(ETH_sent.48.sent)<-paste(c("avg.sent","volume"))
ETH_sent.48.sent


ETH_sent.49<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 12:00:00" &
                      ETH_sent$date <= "2021-05-25 12:59:59")
ETH_sent.49$index<-seq(1,nrow(ETH_sent.49),by=1)
ETH_sent.49.sent<-mean(ETH_sent.49$compound,na.rm = TRUE)
ETH_sent.49.sent<-cbind(ETH_sent.49.sent,ETH_sent.49[nrow(ETH_sent.49),4])
colnames(ETH_sent.49.sent)<-paste(c("avg.sent","volume"))
ETH_sent.49.sent


ETH_sent.50<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 13:00:00" &
                      ETH_sent$date <= "2021-05-25 13:59:59")
ETH_sent.50$index<-seq(1,nrow(ETH_sent.50),by=1)
ETH_sent.50.sent<-mean(ETH_sent.50$compound,na.rm = TRUE)
ETH_sent.50.sent<-cbind(ETH_sent.50.sent,ETH_sent.50[nrow(ETH_sent.50),4])
colnames(ETH_sent.50.sent)<-paste(c("avg.sent","volume"))
ETH_sent.50.sent

ETH_sent.51<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 14:00:00" &
                      ETH_sent$date <= "2021-05-25 14:59:59")
ETH_sent.51$index<-seq(1,nrow(ETH_sent.51),by=1)
ETH_sent.51.sent<-mean(ETH_sent.51$compound,na.rm = TRUE)
ETH_sent.51.sent<-cbind(ETH_sent.51.sent,ETH_sent.51[nrow(ETH_sent.51),4])
colnames(ETH_sent.51.sent)<-paste(c("avg.sent","volume"))
ETH_sent.51.sent

ETH_sent.52<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 15:00:00" &
                      ETH_sent$date <= "2021-05-25 15:59:59")
ETH_sent.52$index<-seq(1,nrow(ETH_sent.52),by=1)
ETH_sent.52.sent<-mean(ETH_sent.52$compound,na.rm = TRUE)
ETH_sent.52.sent<-cbind(ETH_sent.52.sent,ETH_sent.52[nrow(ETH_sent.52),4])
colnames(ETH_sent.52.sent)<-paste(c("avg.sent","volume"))
ETH_sent.52.sent

ETH_sent.53<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 16:00:00" &
                      ETH_sent$date <= "2021-05-25 16:59:59")
ETH_sent.53$index<-seq(1,nrow(ETH_sent.53),by=1)
ETH_sent.53.sent<-mean(ETH_sent.53$compound,na.rm = TRUE)
ETH_sent.53.sent<-cbind(ETH_sent.53.sent,ETH_sent.53[nrow(ETH_sent.53),4])
colnames(ETH_sent.53.sent)<-paste(c("avg.sent","volume"))
ETH_sent.53.sent

ETH_sent.54<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 17:00:00" &
                      ETH_sent$date <= "2021-05-25 17:59:59")
ETH_sent.54$index<-seq(1,nrow(ETH_sent.54),by=1)
ETH_sent.54.sent<-mean(ETH_sent.54$compound,na.rm = TRUE)
ETH_sent.54.sent<-cbind(ETH_sent.54.sent,ETH_sent.54[nrow(ETH_sent.54),4])
colnames(ETH_sent.54.sent)<-paste(c("avg.sent","volume"))
ETH_sent.54.sent

ETH_sent.55<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 18:00:00" &
                      ETH_sent$date <= "2021-05-25 18:59:59")
ETH_sent.55$index<-seq(1,nrow(ETH_sent.55),by=1)
ETH_sent.55.sent<-mean(ETH_sent.55$compound,na.rm = TRUE)
ETH_sent.55.sent<-cbind(ETH_sent.55.sent,ETH_sent.55[nrow(ETH_sent.55),4])
colnames(ETH_sent.55.sent)<-paste(c("avg.sent","volume"))
ETH_sent.55.sent


ETH_sent.56<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 19:00:00" &
                      ETH_sent$date <= "2021-05-25 19:59:59")
ETH_sent.56$index<-seq(1,nrow(ETH_sent.56),by=1)
ETH_sent.56.sent<-mean(ETH_sent.56$compound,na.rm = TRUE)
ETH_sent.56.sent<-cbind(ETH_sent.56.sent,ETH_sent.56[nrow(ETH_sent.56),4])
colnames(ETH_sent.56.sent)<-paste(c("avg.sent","volume"))
ETH_sent.56.sent

ETH_sent.57<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 20:00:00" &
                      ETH_sent$date <= "2021-05-25 20:59:59")
ETH_sent.57$index<-seq(1,nrow(ETH_sent.57),by=1)
ETH_sent.57.sent<-mean(ETH_sent.57$compound,na.rm = TRUE)
ETH_sent.57.sent<-cbind(ETH_sent.57.sent,ETH_sent.57[nrow(ETH_sent.57),4])
colnames(ETH_sent.57.sent)<-paste(c("avg.sent","volume"))
ETH_sent.57.sent

ETH_sent.58<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 21:00:00" &
                      ETH_sent$date <= "2021-05-25 21:59:59")
ETH_sent.58$index<-seq(1,nrow(ETH_sent.58),by=1)
ETH_sent.58.sent<-mean(ETH_sent.58$compound,na.rm = TRUE)
ETH_sent.58.sent<-cbind(ETH_sent.58.sent,ETH_sent.58[nrow(ETH_sent.58),4])
colnames(ETH_sent.58.sent)<-paste(c("avg.sent","volume"))
ETH_sent.58.sent


ETH_sent.59<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 22:00:00" &
                      ETH_sent$date <= "2021-05-25 22:59:59")
ETH_sent.59$index<-seq(1,nrow(ETH_sent.59),by=1)
ETH_sent.59.sent<-mean(ETH_sent.59$compound,na.rm = TRUE)
ETH_sent.59.sent<-cbind(ETH_sent.59.sent,ETH_sent.59[nrow(ETH_sent.59),4])
colnames(ETH_sent.59.sent)<-paste(c("avg.sent","volume"))
ETH_sent.59.sent

ETH_sent.60<-subset(ETH_sent,ETH_sent$date >= "2021-05-25 23:00:00" &
                      ETH_sent$date <= "2021-05-25 23:59:59")
ETH_sent.60$index<-seq(1,nrow(ETH_sent.60),by=1)
ETH_sent.60.sent<-mean(ETH_sent.60$compound,na.rm = TRUE)
ETH_sent.60.sent<-cbind(ETH_sent.60.sent,ETH_sent.60[nrow(ETH_sent.60),4])
colnames(ETH_sent.60.sent)<-paste(c("avg.sent","volume"))
ETH_sent.60.sent

ETH_sent.61<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 00:00:00" &
                      ETH_sent$date <= "2021-05-26 00:59:59")
ETH_sent.61$index<-seq(1,nrow(ETH_sent.61),by=1)
ETH_sent.61.sent<-mean(ETH_sent.61$compound,na.rm = TRUE)
ETH_sent.61.sent<-cbind(ETH_sent.61.sent,ETH_sent.61[nrow(ETH_sent.61),4])
colnames(ETH_sent.61.sent)<-paste(c("avg.sent","volume"))
ETH_sent.61.sent

ETH_sent.62<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 01:00:00" &
                      ETH_sent$date <= "2021-05-26 01:59:59")
ETH_sent.62$index<-seq(1,nrow(ETH_sent.62),by=1)
ETH_sent.62.sent<-mean(ETH_sent.62$compound,na.rm = TRUE)
ETH_sent.62.sent<-cbind(ETH_sent.62.sent,ETH_sent.62[nrow(ETH_sent.62),4])
colnames(ETH_sent.62.sent)<-paste(c("avg.sent","volume"))
ETH_sent.62.sent


ETH_sent.63<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 02:00:00" &
                      ETH_sent$date <= "2021-05-26 02:59:59")
ETH_sent.63$index<-seq(1,nrow(ETH_sent.63),by=1)
ETH_sent.63.sent<-mean(ETH_sent.63$compound,na.rm = TRUE)
ETH_sent.63.sent<-cbind(ETH_sent.63.sent,ETH_sent.63[nrow(ETH_sent.63),4])
colnames(ETH_sent.63.sent)<-paste(c("avg.sent","volume"))
ETH_sent.63.sent


ETH_sent.64<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 03:00:00" &
                      ETH_sent$date <= "2021-05-26 03:59:59")
ETH_sent.64$index<-seq(1,nrow(ETH_sent.64),by=1)
ETH_sent.64.sent<-mean(ETH_sent.64$compound,na.rm = TRUE)
ETH_sent.64.sent<-cbind(ETH_sent.64.sent,ETH_sent.64[nrow(ETH_sent.64),4])
colnames(ETH_sent.64.sent)<-paste(c("avg.sent","volume"))
ETH_sent.64.sent

ETH_sent.65<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 04:00:00" &
                      ETH_sent$date <= "2021-05-26 04:59:59")
ETH_sent.65$index<-seq(1,nrow(ETH_sent.65),by=1)
ETH_sent.65.sent<-mean(ETH_sent.65$compound,na.rm = TRUE)
ETH_sent.65.sent<-cbind(ETH_sent.65.sent,ETH_sent.65[nrow(ETH_sent.65),4])
colnames(ETH_sent.65.sent)<-paste(c("avg.sent","volume"))
ETH_sent.65.sent

ETH_sent.66<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 05:00:00" &
                      ETH_sent$date <= "2021-05-26 05:59:59")
ETH_sent.66$index<-seq(1,nrow(ETH_sent.66),by=1)
ETH_sent.66.sent<-mean(ETH_sent.66$compound,na.rm = TRUE)
ETH_sent.66.sent<-cbind(ETH_sent.66.sent,ETH_sent.66[nrow(ETH_sent.66),4])
colnames(ETH_sent.66.sent)<-paste(c("avg.sent","volume"))
ETH_sent.66.sent

ETH_sent.67<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 06:00:00" &
                      ETH_sent$date <= "2021-05-26 06:59:59")
ETH_sent.67$index<-seq(1,nrow(ETH_sent.67),by=1)
ETH_sent.67.sent<-mean(ETH_sent.67$compound,na.rm = TRUE)
ETH_sent.67.sent<-cbind(ETH_sent.67.sent,ETH_sent.67[nrow(ETH_sent.67),4])
colnames(ETH_sent.67.sent)<-paste(c("avg.sent","volume"))
ETH_sent.67.sent

ETH_sent.68<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 07:00:00" &
                      ETH_sent$date <= "2021-05-26 07:59:59")
ETH_sent.68$index<-seq(1,nrow(ETH_sent.68),by=1)
ETH_sent.68.sent<-mean(ETH_sent.68$compound,na.rm = TRUE)
ETH_sent.68.sent<-cbind(ETH_sent.68.sent,ETH_sent.68[nrow(ETH_sent.68),4])
colnames(ETH_sent.68.sent)<-paste(c("avg.sent","volume"))
ETH_sent.68.sent

ETH_sent.69<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 08:00:00" &
                      ETH_sent$date <= "2021-05-26 08:59:59")
ETH_sent.69$index<-seq(1,nrow(ETH_sent.69),by=1)
ETH_sent.69.sent<-mean(ETH_sent.69$compound,na.rm = TRUE)
ETH_sent.69.sent<-cbind(ETH_sent.69.sent,ETH_sent.69[nrow(ETH_sent.69),4])
colnames(ETH_sent.69.sent)<-paste(c("avg.sent","volume"))
ETH_sent.69.sent


ETH_sent.70<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 09:00:00" &
                      ETH_sent$date <= "2021-05-26 09:59:59")
ETH_sent.70$index<-seq(1,nrow(ETH_sent.70),by=1)
ETH_sent.70.sent<-mean(ETH_sent.70$compound,na.rm = TRUE)
ETH_sent.70.sent<-cbind(ETH_sent.70.sent,ETH_sent.70[nrow(ETH_sent.70),4])
colnames(ETH_sent.70.sent)<-paste(c("avg.sent","volume"))
ETH_sent.70.sent


ETH_sent.71<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 10:00:00" &
                      ETH_sent$date <= "2021-05-26 10:59:59")
ETH_sent.71$index<-seq(1,nrow(ETH_sent.71),by=1)
ETH_sent.71.sent<-mean(ETH_sent.71$compound,na.rm = TRUE)
ETH_sent.71.sent<-cbind(ETH_sent.71.sent,ETH_sent.71[nrow(ETH_sent.71),4])
colnames(ETH_sent.71.sent)<-paste(c("avg.sent","volume"))
ETH_sent.71.sent


ETH_sent.72<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 11:00:00" &
                      ETH_sent$date <= "2021-05-26 11:59:59")
ETH_sent.72$index<-seq(1,nrow(ETH_sent.72),by=1)
ETH_sent.72.sent<-mean(ETH_sent.72$compound,na.rm = TRUE)
ETH_sent.72.sent<-cbind(ETH_sent.72.sent,ETH_sent.72[nrow(ETH_sent.72),4])
colnames(ETH_sent.72.sent)<-paste(c("avg.sent","volume"))
ETH_sent.72.sent


ETH_sent.73<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 12:00:00" &
                      ETH_sent$date <= "2021-05-26 12:59:59")
ETH_sent.73$index<-seq(1,nrow(ETH_sent.73),by=1)
ETH_sent.73.sent<-mean(ETH_sent.73$compound,na.rm = TRUE)
ETH_sent.73.sent<-cbind(ETH_sent.73.sent,ETH_sent.73[nrow(ETH_sent.73),4])
colnames(ETH_sent.73.sent)<-paste(c("avg.sent","volume"))
ETH_sent.73.sent


ETH_sent.74<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 13:00:00" &
                      ETH_sent$date <= "2021-05-26 13:59:59")
ETH_sent.74$index<-seq(1,nrow(ETH_sent.74),by=1)
ETH_sent.74.sent<-mean(ETH_sent.74$compound,na.rm = TRUE)
ETH_sent.74.sent<-cbind(ETH_sent.74.sent,ETH_sent.74[nrow(ETH_sent.74),4])
colnames(ETH_sent.74.sent)<-paste(c("avg.sent","volume"))
ETH_sent.74.sent


ETH_sent.75<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 14:00:00" &
                      ETH_sent$date <= "2021-05-26 14:59:59")
ETH_sent.75$index<-seq(1,nrow(ETH_sent.75),by=1)
ETH_sent.75.sent<-mean(ETH_sent.75$compound,na.rm = TRUE)
ETH_sent.75.sent<-cbind(ETH_sent.75.sent,ETH_sent.75[nrow(ETH_sent.75),4])
colnames(ETH_sent.75.sent)<-paste(c("avg.sent","volume"))
ETH_sent.75.sent


ETH_sent.76<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 15:00:00" &
                      ETH_sent$date <= "2021-05-26 15:59:59")
ETH_sent.76$index<-seq(1,nrow(ETH_sent.76),by=1)
ETH_sent.76.sent<-mean(ETH_sent.76$compound,na.rm = TRUE)
ETH_sent.76.sent<-cbind(ETH_sent.76.sent,ETH_sent.76[nrow(ETH_sent.76),4])
colnames(ETH_sent.76.sent)<-paste(c("avg.sent","volume"))
ETH_sent.76.sent


ETH_sent.77<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 16:00:00" &
                      ETH_sent$date <= "2021-05-26 16:59:59")
ETH_sent.77$index<-seq(1,nrow(ETH_sent.77),by=1)
ETH_sent.77.sent<-mean(ETH_sent.77$compound,na.rm = TRUE)
ETH_sent.77.sent<-cbind(ETH_sent.77.sent,ETH_sent.77[nrow(ETH_sent.77),4])
colnames(ETH_sent.77.sent)<-paste(c("avg.sent","volume"))
ETH_sent.77.sent


ETH_sent.78<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 17:00:00" &
                      ETH_sent$date <= "2021-05-26 17:59:59")
ETH_sent.78$index<-seq(1,nrow(ETH_sent.78),by=1)
ETH_sent.78.sent<-mean(ETH_sent.78$compound,na.rm = TRUE)
ETH_sent.78.sent<-cbind(ETH_sent.78.sent,ETH_sent.78[nrow(ETH_sent.78),4])
colnames(ETH_sent.78.sent)<-paste(c("avg.sent","volume"))
ETH_sent.78.sent


ETH_sent.79<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 18:00:00" &
                      ETH_sent$date <= "2021-05-26 18:59:59")
ETH_sent.79$index<-seq(1,nrow(ETH_sent.79),by=1)
ETH_sent.79.sent<-mean(ETH_sent.79$compound,na.rm = TRUE)
ETH_sent.79.sent<-cbind(ETH_sent.79.sent,ETH_sent.79[nrow(ETH_sent.79),4])
colnames(ETH_sent.79.sent)<-paste(c("avg.sent","volume"))
ETH_sent.79.sent


ETH_sent.80<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 19:00:00" &
                      ETH_sent$date <= "2021-05-26 19:59:59")
ETH_sent.80$index<-seq(1,nrow(ETH_sent.80),by=1)
ETH_sent.80.sent<-mean(ETH_sent.80$compound,na.rm = TRUE)
ETH_sent.80.sent<-cbind(ETH_sent.80.sent,ETH_sent.80[nrow(ETH_sent.80),4])
colnames(ETH_sent.80.sent)<-paste(c("avg.sent","volume"))
ETH_sent.80.sent


ETH_sent.81<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 20:00:00" &
                      ETH_sent$date <= "2021-05-26 20:59:59")
ETH_sent.81$index<-seq(1,nrow(ETH_sent.81),by=1)
ETH_sent.81.sent<-mean(ETH_sent.81$compound,na.rm = TRUE)
ETH_sent.81.sent<-cbind(ETH_sent.81.sent,ETH_sent.81[nrow(ETH_sent.81),4])
colnames(ETH_sent.81.sent)<-paste(c("avg.sent","volume"))
ETH_sent.81.sent


ETH_sent.82<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 21:00:00" &
                      ETH_sent$date <= "2021-05-26 21:59:59")
ETH_sent.82$index<-seq(1,nrow(ETH_sent.82),by=1)
ETH_sent.82.sent<-mean(ETH_sent.82$compound,na.rm = TRUE)
ETH_sent.82.sent<-cbind(ETH_sent.82.sent,ETH_sent.82[nrow(ETH_sent.82),4])
colnames(ETH_sent.82.sent)<-paste(c("avg.sent","volume"))
ETH_sent.82.sent


ETH_sent.83<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 22:00:00" &
                      ETH_sent$date <= "2021-05-26 22:59:59")
ETH_sent.83$index<-seq(1,nrow(ETH_sent.83),by=1)
ETH_sent.83.sent<-mean(ETH_sent.83$compound,na.rm = TRUE)
ETH_sent.83.sent<-cbind(ETH_sent.83.sent,ETH_sent.83[nrow(ETH_sent.83),4])
colnames(ETH_sent.83.sent)<-paste(c("avg.sent","volume"))
ETH_sent.83.sent


ETH_sent.84<-subset(ETH_sent,ETH_sent$date >= "2021-05-26 23:00:00" &
                      ETH_sent$date <= "2021-05-26 23:59:59")
ETH_sent.84$index<-seq(1,nrow(ETH_sent.84),by=1)
ETH_sent.84.sent<-mean(ETH_sent.84$compound,na.rm = TRUE)
ETH_sent.84.sent<-cbind(ETH_sent.84.sent,ETH_sent.84[nrow(ETH_sent.84),4])
colnames(ETH_sent.84.sent)<-paste(c("avg.sent","volume"))
ETH_sent.84.sent

ETH_sent.85<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 00:00:00" &
                      ETH_sent$date <= "2021-05-27 00:59:59")
ETH_sent.85$index<-seq(1,nrow(ETH_sent.85),by=1)
ETH_sent.85.sent<-mean(ETH_sent.85$compound,na.rm = TRUE)
ETH_sent.85.sent<-cbind(ETH_sent.85.sent,ETH_sent.85[nrow(ETH_sent.85),4])
colnames(ETH_sent.85.sent)<-paste(c("avg.sent","volume"))
ETH_sent.85.sent

ETH_sent.86<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 01:00:00" &
                      ETH_sent$date <= "2021-05-27 01:59:59")
ETH_sent.86$index<-seq(1,nrow(ETH_sent.86),by=1)
ETH_sent.86.sent<-mean(ETH_sent.86$compound,na.rm = TRUE)
ETH_sent.86.sent<-cbind(ETH_sent.86.sent,ETH_sent.86[nrow(ETH_sent.86),4])
colnames(ETH_sent.86.sent)<-paste(c("avg.sent","volume"))
ETH_sent.86.sent


ETH_sent.87<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 02:00:00" &
                      ETH_sent$date <= "2021-05-27 02:59:59")
ETH_sent.87$index<-seq(1,nrow(ETH_sent.87),by=1)
ETH_sent.87.sent<-mean(ETH_sent.87$compound,na.rm = TRUE)
ETH_sent.87.sent<-cbind(ETH_sent.87.sent,ETH_sent.87[nrow(ETH_sent.87),4])
colnames(ETH_sent.87.sent)<-paste(c("avg.sent","volume"))
ETH_sent.87.sent


ETH_sent.88<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 03:00:00" &
                      ETH_sent$date <= "2021-05-27 03:59:59")
ETH_sent.88$index<-seq(1,nrow(ETH_sent.88),by=1)
ETH_sent.88.sent<-mean(ETH_sent.88$compound,na.rm = TRUE)
ETH_sent.88.sent<-cbind(ETH_sent.88.sent,ETH_sent.88[nrow(ETH_sent.88),4])
colnames(ETH_sent.88.sent)<-paste(c("avg.sent","volume"))
ETH_sent.88.sent


ETH_sent.89<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 04:00:00" &
                      ETH_sent$date <= "2021-05-27 04:59:59")
ETH_sent.89$index<-seq(1,nrow(ETH_sent.89),by=1)
ETH_sent.89.sent<-mean(ETH_sent.89$compound,na.rm = TRUE)
ETH_sent.89.sent<-cbind(ETH_sent.89.sent,ETH_sent.89[nrow(ETH_sent.89),4])
colnames(ETH_sent.89.sent)<-paste(c("avg.sent","volume"))
ETH_sent.89.sent


ETH_sent.90<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 05:00:00" &
                      ETH_sent$date <= "2021-05-27 05:59:59")
ETH_sent.90$index<-seq(1,nrow(ETH_sent.90),by=1)
ETH_sent.90.sent<-mean(ETH_sent.90$compound,na.rm = TRUE)
ETH_sent.90.sent<-cbind(ETH_sent.90.sent,ETH_sent.90[nrow(ETH_sent.90),4])
colnames(ETH_sent.90.sent)<-paste(c("avg.sent","volume"))
ETH_sent.90.sent


ETH_sent.91<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 06:00:00" &
                      ETH_sent$date <= "2021-05-27 06:59:59")
ETH_sent.91$index<-seq(1,nrow(ETH_sent.91),by=1)
ETH_sent.91.sent<-mean(ETH_sent.91$compound,na.rm = TRUE)
ETH_sent.91.sent<-cbind(ETH_sent.91.sent,ETH_sent.91[nrow(ETH_sent.91),4])
colnames(ETH_sent.91.sent)<-paste(c("avg.sent","volume"))
ETH_sent.91.sent


ETH_sent.92<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 07:00:00" &
                      ETH_sent$date <= "2021-05-27 07:59:59")
ETH_sent.92$index<-seq(1,nrow(ETH_sent.92),by=1)
ETH_sent.92.sent<-mean(ETH_sent.92$compound,na.rm = TRUE)
ETH_sent.92.sent<-cbind(ETH_sent.92.sent,ETH_sent.92[nrow(ETH_sent.92),4])
colnames(ETH_sent.92.sent)<-paste(c("avg.sent","volume"))
ETH_sent.92.sent


ETH_sent.93<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 08:00:00" &
                      ETH_sent$date <= "2021-05-27 08:59:59")
ETH_sent.93$index<-seq(1,nrow(ETH_sent.93),by=1)
ETH_sent.93.sent<-mean(ETH_sent.93$compound,na.rm = TRUE)
ETH_sent.93.sent<-cbind(ETH_sent.93.sent,ETH_sent.93[nrow(ETH_sent.93),4])
colnames(ETH_sent.93.sent)<-paste(c("avg.sent","volume"))
ETH_sent.93.sent


ETH_sent.94<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 09:00:00" &
                      ETH_sent$date <= "2021-05-27 09:59:59")
ETH_sent.94$index<-seq(1,nrow(ETH_sent.94),by=1)
ETH_sent.94.sent<-mean(ETH_sent.94$compound,na.rm = TRUE)
ETH_sent.94.sent<-cbind(ETH_sent.94.sent,ETH_sent.94[nrow(ETH_sent.94),4])
colnames(ETH_sent.94.sent)<-paste(c("avg.sent","volume"))
ETH_sent.94.sent


ETH_sent.95<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 10:00:00" &
                      ETH_sent$date <= "2021-05-27 10:59:59")
ETH_sent.95$index<-seq(1,nrow(ETH_sent.95),by=1)
ETH_sent.95.sent<-mean(ETH_sent.95$compound,na.rm = TRUE)
ETH_sent.95.sent<-cbind(ETH_sent.95.sent,ETH_sent.95[nrow(ETH_sent.95),4])
colnames(ETH_sent.95.sent)<-paste(c("avg.sent","volume"))
ETH_sent.95.sent


ETH_sent.96<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 11:00:00" &
                      ETH_sent$date <= "2021-05-27 11:59:59")
ETH_sent.96$index<-seq(1,nrow(ETH_sent.96),by=1)
ETH_sent.96.sent<-mean(ETH_sent.96$compound,na.rm = TRUE)
ETH_sent.96.sent<-cbind(ETH_sent.96.sent,ETH_sent.96[nrow(ETH_sent.96),4])
colnames(ETH_sent.96.sent)<-paste(c("avg.sent","volume"))
ETH_sent.96.sent


ETH_sent.97<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 12:00:00" &
                      ETH_sent$date <= "2021-05-27 12:59:59")
ETH_sent.97$index<-seq(1,nrow(ETH_sent.97),by=1)
ETH_sent.97.sent<-mean(ETH_sent.97$compound,na.rm = TRUE)
ETH_sent.97.sent<-cbind(ETH_sent.97.sent,ETH_sent.97[nrow(ETH_sent.97),4])
colnames(ETH_sent.97.sent)<-paste(c("avg.sent","volume"))
ETH_sent.97.sent


ETH_sent.98<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 13:00:00" &
                      ETH_sent$date <= "2021-05-27 13:59:59")
ETH_sent.98$index<-seq(1,nrow(ETH_sent.98),by=1)
ETH_sent.98.sent<-mean(ETH_sent.98$compound,na.rm = TRUE)
ETH_sent.98.sent<-cbind(ETH_sent.98.sent,ETH_sent.98[nrow(ETH_sent.98),4])
colnames(ETH_sent.98.sent)<-paste(c("avg.sent","volume"))
ETH_sent.98.sent


ETH_sent.99<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 14:00:00" &
                      ETH_sent$date <= "2021-05-27 14:59:59")
ETH_sent.99$index<-seq(1,nrow(ETH_sent.99),by=1)
ETH_sent.99.sent<-mean(ETH_sent.99$compound,na.rm = TRUE)
ETH_sent.99.sent<-cbind(ETH_sent.99.sent,ETH_sent.99[nrow(ETH_sent.99),4])
colnames(ETH_sent.99.sent)<-paste(c("avg.sent","volume"))
ETH_sent.99.sent


ETH_sent.100<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 15:00:00" &
                      ETH_sent$date <= "2021-05-27 15:59:59")
ETH_sent.100$index<-seq(1,nrow(ETH_sent.100),by=1)
ETH_sent.100.sent<-mean(ETH_sent.100$compound,na.rm = TRUE)
ETH_sent.100.sent<-cbind(ETH_sent.100.sent,ETH_sent.100[nrow(ETH_sent.100),4])
colnames(ETH_sent.100.sent)<-paste(c("avg.sent","volume"))
ETH_sent.100.sent

ETH_sent.101<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 16:00:00" &
                       ETH_sent$date <= "2021-05-27 16:59:59")
ETH_sent.101$index<-seq(1,nrow(ETH_sent.101),by=1)
ETH_sent.101.sent<-mean(ETH_sent.101$compound,na.rm = TRUE)
ETH_sent.101.sent<-cbind(ETH_sent.101.sent,ETH_sent.101[nrow(ETH_sent.101),4])
colnames(ETH_sent.101.sent)<-paste(c("avg.sent","volume"))
ETH_sent.101.sent

ETH_sent.102<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 17:00:00" &
                       ETH_sent$date <= "2021-05-27 17:59:59")
ETH_sent.102$index<-seq(1,nrow(ETH_sent.102),by=1)
ETH_sent.102.sent<-mean(ETH_sent.102$compound,na.rm = TRUE)
ETH_sent.102.sent<-cbind(ETH_sent.102.sent,ETH_sent.102[nrow(ETH_sent.102),4])
colnames(ETH_sent.102.sent)<-paste(c("avg.sent","volume"))
ETH_sent.102.sent


ETH_sent.103<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 18:00:00" &
                       ETH_sent$date <= "2021-05-27 18:59:59")
ETH_sent.103$index<-seq(1,nrow(ETH_sent.103),by=1)
ETH_sent.103.sent<-mean(ETH_sent.103$compound,na.rm = TRUE)
ETH_sent.103.sent<-cbind(ETH_sent.103.sent,ETH_sent.103[nrow(ETH_sent.103),4])
colnames(ETH_sent.103.sent)<-paste(c("avg.sent","volume"))
ETH_sent.103.sent

ETH_sent.104<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 19:00:00" &
                       ETH_sent$date <= "2021-05-27 19:59:59")
ETH_sent.104$index<-seq(1,nrow(ETH_sent.104),by=1)
ETH_sent.104.sent<-mean(ETH_sent.104$compound,na.rm = TRUE)
ETH_sent.104.sent<-cbind(ETH_sent.104.sent,ETH_sent.104[nrow(ETH_sent.104),4])
colnames(ETH_sent.104.sent)<-paste(c("avg.sent","volume"))
ETH_sent.104.sent

ETH_sent.105<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 20:00:00" &
                       ETH_sent$date <= "2021-05-27 20:59:59")
ETH_sent.105$index<-seq(1,nrow(ETH_sent.105),by=1)
ETH_sent.105.sent<-mean(ETH_sent.105$compound,na.rm = TRUE)
ETH_sent.105.sent<-cbind(ETH_sent.105.sent,ETH_sent.105[nrow(ETH_sent.105),4])
colnames(ETH_sent.105.sent)<-paste(c("avg.sent","volume"))
ETH_sent.105.sent


ETH_sent.106<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 21:00:00" &
                       ETH_sent$date <= "2021-05-27 21:59:59")
ETH_sent.106$index<-seq(1,nrow(ETH_sent.106),by=1)
ETH_sent.106.sent<-mean(ETH_sent.106$compound,na.rm = TRUE)
ETH_sent.106.sent<-cbind(ETH_sent.106.sent,ETH_sent.106[nrow(ETH_sent.106),4])
colnames(ETH_sent.106.sent)<-paste(c("avg.sent","volume"))
ETH_sent.106.sent


ETH_sent.107<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 22:00:00" &
                       ETH_sent$date <= "2021-05-27 22:59:59")
ETH_sent.107$index<-seq(1,nrow(ETH_sent.107),by=1)
ETH_sent.107.sent<-mean(ETH_sent.107$compound,na.rm = TRUE)
ETH_sent.107.sent<-cbind(ETH_sent.107.sent,ETH_sent.107[nrow(ETH_sent.107),4])
colnames(ETH_sent.107.sent)<-paste(c("avg.sent","volume"))
ETH_sent.107.sent

ETH_sent.108<-subset(ETH_sent,ETH_sent$date >= "2021-05-27 23:00:00" &
                       ETH_sent$date <= "2021-05-27 23:59:59")
ETH_sent.108$index<-seq(1,nrow(ETH_sent.108),by=1)
ETH_sent.108.sent<-mean(ETH_sent.108$compound,na.rm = TRUE)
ETH_sent.108.sent<-cbind(ETH_sent.108.sent,ETH_sent.108[nrow(ETH_sent.108),4])
colnames(ETH_sent.108.sent)<-paste(c("avg.sent","volume"))
ETH_sent.108.sent

ETH_sent.109<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 00:00:00" &
                       ETH_sent$date <= "2021-05-28 00:59:59")
ETH_sent.109$index<-seq(1,nrow(ETH_sent.109),by=1)
ETH_sent.109.sent<-mean(ETH_sent.109$compound,na.rm = TRUE)
ETH_sent.109.sent<-cbind(ETH_sent.109.sent,ETH_sent.109[nrow(ETH_sent.109),4])
colnames(ETH_sent.109.sent)<-paste(c("avg.sent","volume"))
ETH_sent.109.sent

ETH_sent.110<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 01:00:00" &
                       ETH_sent$date <= "2021-05-28 01:59:59")
ETH_sent.110$index<-seq(1,nrow(ETH_sent.110),by=1)
ETH_sent.110.sent<-mean(ETH_sent.110$compound,na.rm = TRUE)
ETH_sent.110.sent<-cbind(ETH_sent.110.sent,ETH_sent.110[nrow(ETH_sent.110),4])
colnames(ETH_sent.110.sent)<-paste(c("avg.sent","volume"))
ETH_sent.110.sent

ETH_sent.111<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 02:00:00" &
                       ETH_sent$date <= "2021-05-28 02:59:59")
ETH_sent.111$index<-seq(1,nrow(ETH_sent.111),by=1)
ETH_sent.111.sent<-mean(ETH_sent.111$compound,na.rm = TRUE)
ETH_sent.111.sent<-cbind(ETH_sent.111.sent,ETH_sent.111[nrow(ETH_sent.111),4])
colnames(ETH_sent.111.sent)<-paste(c("avg.sent","volume"))
ETH_sent.111.sent

ETH_sent.112<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 03:00:00" &
                       ETH_sent$date <= "2021-05-28 03:59:59")
ETH_sent.112$index<-seq(1,nrow(ETH_sent.112),by=1)
ETH_sent.112.sent<-mean(ETH_sent.112$compound,na.rm = TRUE)
ETH_sent.112.sent<-cbind(ETH_sent.112.sent,ETH_sent.112[nrow(ETH_sent.112),4])
colnames(ETH_sent.112.sent)<-paste(c("avg.sent","volume"))
ETH_sent.112.sent

ETH_sent.113<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 04:00:00" &
                       ETH_sent$date <= "2021-05-28 04:59:59")
ETH_sent.113$index<-seq(1,nrow(ETH_sent.113),by=1)
ETH_sent.113.sent<-mean(ETH_sent.113$compound,na.rm = TRUE)
ETH_sent.113.sent<-cbind(ETH_sent.113.sent,ETH_sent.113[nrow(ETH_sent.113),4])
colnames(ETH_sent.113.sent)<-paste(c("avg.sent","volume"))
ETH_sent.113.sent

ETH_sent.114<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 05:00:00" &
                       ETH_sent$date <= "2021-05-28 05:59:59")
ETH_sent.114$index<-seq(1,nrow(ETH_sent.114),by=1)
ETH_sent.114.sent<-mean(ETH_sent.114$compound,na.rm = TRUE)
ETH_sent.114.sent<-cbind(ETH_sent.114.sent,ETH_sent.114[nrow(ETH_sent.114),4])
colnames(ETH_sent.114.sent)<-paste(c("avg.sent","volume"))
ETH_sent.114.sent

ETH_sent.115<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 06:00:00" &
                       ETH_sent$date <= "2021-05-28 06:59:59")
ETH_sent.115$index<-seq(1,nrow(ETH_sent.115),by=1)
ETH_sent.115.sent<-mean(ETH_sent.115$compound,na.rm = TRUE)
ETH_sent.115.sent<-cbind(ETH_sent.115.sent,ETH_sent.115[nrow(ETH_sent.115),4])
colnames(ETH_sent.115.sent)<-paste(c("avg.sent","volume"))
ETH_sent.115.sent

ETH_sent.116<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 07:00:00" &
                       ETH_sent$date <= "2021-05-28 07:59:59")
ETH_sent.116$index<-seq(1,nrow(ETH_sent.116),by=1)
ETH_sent.116.sent<-mean(ETH_sent.116$compound,na.rm = TRUE)
ETH_sent.116.sent<-cbind(ETH_sent.116.sent,ETH_sent.116[nrow(ETH_sent.116),4])
colnames(ETH_sent.116.sent)<-paste(c("avg.sent","volume"))
ETH_sent.116.sent

ETH_sent.117<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 08:00:00" &
                       ETH_sent$date <= "2021-05-28 08:59:59")
ETH_sent.117$index<-seq(1,nrow(ETH_sent.117),by=1)
ETH_sent.117.sent<-mean(ETH_sent.117$compound,na.rm = TRUE)
ETH_sent.117.sent<-cbind(ETH_sent.117.sent,ETH_sent.117[nrow(ETH_sent.117),4])
colnames(ETH_sent.117.sent)<-paste(c("avg.sent","volume"))
ETH_sent.117.sent

ETH_sent.118<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 09:00:00" &
                       ETH_sent$date <= "2021-05-28 09:59:59")
ETH_sent.118$index<-seq(1,nrow(ETH_sent.118),by=1)
ETH_sent.118.sent<-mean(ETH_sent.118$compound,na.rm = TRUE)
ETH_sent.118.sent<-cbind(ETH_sent.118.sent,ETH_sent.118[nrow(ETH_sent.118),4])
colnames(ETH_sent.118.sent)<-paste(c("avg.sent","volume"))
ETH_sent.118.sent

ETH_sent.119<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 10:00:00" &
                       ETH_sent$date <= "2021-05-28 10:59:59")
ETH_sent.119$index<-seq(1,nrow(ETH_sent.119),by=1)
ETH_sent.119.sent<-mean(ETH_sent.119$compound,na.rm = TRUE)
ETH_sent.119.sent<-cbind(ETH_sent.119.sent,ETH_sent.119[nrow(ETH_sent.119),4])
colnames(ETH_sent.119.sent)<-paste(c("avg.sent","volume"))
ETH_sent.119.sent

ETH_sent.120<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 11:00:00" &
                       ETH_sent$date <= "2021-05-28 11:59:59")
ETH_sent.120$index<-seq(1,nrow(ETH_sent.120),by=1)
ETH_sent.120.sent<-mean(ETH_sent.120$compound,na.rm = TRUE)
ETH_sent.120.sent<-cbind(ETH_sent.120.sent,ETH_sent.120[nrow(ETH_sent.120),4])
colnames(ETH_sent.120.sent)<-paste(c("avg.sent","volume"))
ETH_sent.120.sent

ETH_sent.121<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 12:00:00" &
                       ETH_sent$date <= "2021-05-28 12:59:59")
ETH_sent.121$index<-seq(1,nrow(ETH_sent.121),by=1)
ETH_sent.121.sent<-mean(ETH_sent.121$compound,na.rm = TRUE)
ETH_sent.121.sent<-cbind(ETH_sent.121.sent,ETH_sent.121[nrow(ETH_sent.121),4])
colnames(ETH_sent.121.sent)<-paste(c("avg.sent","volume"))
ETH_sent.121.sent

ETH_sent.122<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 13:00:00" &
                       ETH_sent$date <= "2021-05-28 13:59:59")
ETH_sent.122$index<-seq(1,nrow(ETH_sent.122),by=1)
ETH_sent.122.sent<-mean(ETH_sent.122$compound,na.rm = TRUE)
ETH_sent.122.sent<-cbind(ETH_sent.122.sent,ETH_sent.122[nrow(ETH_sent.122),4])
colnames(ETH_sent.122.sent)<-paste(c("avg.sent","volume"))
ETH_sent.122.sent

ETH_sent.123<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 14:00:00" &
                       ETH_sent$date <= "2021-05-28 14:59:59")
ETH_sent.123$index<-seq(1,nrow(ETH_sent.123),by=1)
ETH_sent.123.sent<-mean(ETH_sent.123$compound,na.rm = TRUE)
ETH_sent.123.sent<-cbind(ETH_sent.123.sent,ETH_sent.123[nrow(ETH_sent.123),4])
colnames(ETH_sent.123.sent)<-paste(c("avg.sent","volume"))
ETH_sent.123.sent

ETH_sent.124<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 15:00:00" &
                       ETH_sent$date <= "2021-05-28 15:59:59")
ETH_sent.124$index<-seq(1,nrow(ETH_sent.124),by=1)
ETH_sent.124.sent<-mean(ETH_sent.124$compound,na.rm = TRUE)
ETH_sent.124.sent<-cbind(ETH_sent.124.sent,ETH_sent.124[nrow(ETH_sent.124),4])
colnames(ETH_sent.124.sent)<-paste(c("avg.sent","volume"))
ETH_sent.124.sent

ETH_sent.125<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 16:00:00" &
                       ETH_sent$date <= "2021-05-28 16:59:59")
ETH_sent.125$index<-seq(1,nrow(ETH_sent.125),by=1)
ETH_sent.125.sent<-mean(ETH_sent.125$compound,na.rm = TRUE)
ETH_sent.125.sent<-cbind(ETH_sent.125.sent,ETH_sent.125[nrow(ETH_sent.125),4])
colnames(ETH_sent.125.sent)<-paste(c("avg.sent","volume"))
ETH_sent.125.sent

ETH_sent.126<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 17:00:00" &
                       ETH_sent$date <= "2021-05-28 17:59:59")
ETH_sent.126$index<-seq(1,nrow(ETH_sent.126),by=1)
ETH_sent.126.sent<-mean(ETH_sent.126$compound,na.rm = TRUE)
ETH_sent.126.sent<-cbind(ETH_sent.126.sent,ETH_sent.126[nrow(ETH_sent.126),4])
colnames(ETH_sent.126.sent)<-paste(c("avg.sent","volume"))
ETH_sent.126.sent

ETH_sent.127<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 18:00:00" &
                       ETH_sent$date <= "2021-05-28 18:59:59")
ETH_sent.127$index<-seq(1,nrow(ETH_sent.127),by=1)
ETH_sent.127.sent<-mean(ETH_sent.127$compound,na.rm = TRUE)
ETH_sent.127.sent<-cbind(ETH_sent.127.sent,ETH_sent.127[nrow(ETH_sent.127),4])
colnames(ETH_sent.127.sent)<-paste(c("avg.sent","volume"))
ETH_sent.127.sent

ETH_sent.128<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 19:00:00" &
                       ETH_sent$date <= "2021-05-28 19:59:59")
ETH_sent.128$index<-seq(1,nrow(ETH_sent.128),by=1)
ETH_sent.128.sent<-mean(ETH_sent.128$compound,na.rm = TRUE)
ETH_sent.128.sent<-cbind(ETH_sent.128.sent,ETH_sent.128[nrow(ETH_sent.128),4])
colnames(ETH_sent.128.sent)<-paste(c("avg.sent","volume"))
ETH_sent.128.sent

ETH_sent.129<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 20:00:00" &
                       ETH_sent$date <= "2021-05-28 20:59:59")
ETH_sent.129$index<-seq(1,nrow(ETH_sent.129),by=1)
ETH_sent.129.sent<-mean(ETH_sent.129$compound,na.rm = TRUE)
ETH_sent.129.sent<-cbind(ETH_sent.129.sent,ETH_sent.129[nrow(ETH_sent.129),4])
colnames(ETH_sent.129.sent)<-paste(c("avg.sent","volume"))
ETH_sent.129.sent

ETH_sent.130<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 21:00:00" &
                       ETH_sent$date <= "2021-05-28 21:59:59")
ETH_sent.130$index<-seq(1,nrow(ETH_sent.130),by=1)
ETH_sent.130.sent<-mean(ETH_sent.130$compound,na.rm = TRUE)
ETH_sent.130.sent<-cbind(ETH_sent.130.sent,ETH_sent.130[nrow(ETH_sent.130),4])
colnames(ETH_sent.130.sent)<-paste(c("avg.sent","volume"))
ETH_sent.130.sent

ETH_sent.131<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 22:00:00" &
                       ETH_sent$date <= "2021-05-28 22:59:59")
ETH_sent.131$index<-seq(1,nrow(ETH_sent.131),by=1)
ETH_sent.131.sent<-mean(ETH_sent.131$compound,na.rm = TRUE)
ETH_sent.131.sent<-cbind(ETH_sent.131.sent,ETH_sent.131[nrow(ETH_sent.131),4])
colnames(ETH_sent.131.sent)<-paste(c("avg.sent","volume"))
ETH_sent.131.sent

ETH_sent.132<-subset(ETH_sent,ETH_sent$date >= "2021-05-28 23:00:00" &
                       ETH_sent$date <= "2021-05-28 23:59:59")
ETH_sent.132$index<-seq(1,nrow(ETH_sent.132),by=1)
ETH_sent.132.sent<-mean(ETH_sent.132$compound,na.rm = TRUE)
ETH_sent.132.sent<-cbind(ETH_sent.132.sent,ETH_sent.132[nrow(ETH_sent.132),4])
colnames(ETH_sent.132.sent)<-paste(c("avg.sent","volume"))
ETH_sent.132.sent

ETH_sent.133<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 00:00:00" &
                       ETH_sent$date <= "2021-05-29 00:59:59")
ETH_sent.133$index<-seq(1,nrow(ETH_sent.133),by=1)
ETH_sent.133.sent<-mean(ETH_sent.133$compound,na.rm = TRUE)
ETH_sent.133.sent<-cbind(ETH_sent.133.sent,ETH_sent.133[nrow(ETH_sent.133),4])
colnames(ETH_sent.133.sent)<-paste(c("avg.sent","volume"))
ETH_sent.133.sent

ETH_sent.134<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 01:00:00" &
                       ETH_sent$date <= "2021-05-29 01:59:59")
ETH_sent.134$index<-seq(1,nrow(ETH_sent.134),by=1)
ETH_sent.134.sent<-mean(ETH_sent.134$compound,na.rm = TRUE)
ETH_sent.134.sent<-cbind(ETH_sent.134.sent,ETH_sent.134[nrow(ETH_sent.134),4])
colnames(ETH_sent.134.sent)<-paste(c("avg.sent","volume"))
ETH_sent.134.sent

ETH_sent.135<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 02:00:00" &
                       ETH_sent$date <= "2021-05-29 02:59:59")
ETH_sent.135$index<-seq(1,nrow(ETH_sent.135),by=1)
ETH_sent.135.sent<-mean(ETH_sent.135$compound,na.rm = TRUE)
ETH_sent.135.sent<-cbind(ETH_sent.135.sent,ETH_sent.135[nrow(ETH_sent.135),4])
colnames(ETH_sent.135.sent)<-paste(c("avg.sent","volume"))
ETH_sent.135.sent

ETH_sent.136<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 03:00:00" &
                       ETH_sent$date <= "2021-05-29 03:59:59")
ETH_sent.136$index<-seq(1,nrow(ETH_sent.136),by=1)
ETH_sent.136.sent<-mean(ETH_sent.136$compound,na.rm = TRUE)
ETH_sent.136.sent<-cbind(ETH_sent.136.sent,ETH_sent.136[nrow(ETH_sent.136),4])
colnames(ETH_sent.136.sent)<-paste(c("avg.sent","volume"))
ETH_sent.136.sent

ETH_sent.137<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 04:00:00" &
                       ETH_sent$date <= "2021-05-29 04:59:59")
ETH_sent.137$index<-seq(1,nrow(ETH_sent.137),by=1)
ETH_sent.137.sent<-mean(ETH_sent.137$compound,na.rm = TRUE)
ETH_sent.137.sent<-cbind(ETH_sent.137.sent,ETH_sent.137[nrow(ETH_sent.137),4])
colnames(ETH_sent.137.sent)<-paste(c("avg.sent","volume"))
ETH_sent.137.sent

ETH_sent.138<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 05:00:00" &
                       ETH_sent$date <= "2021-05-29 05:59:59")
ETH_sent.138$index<-seq(1,nrow(ETH_sent.138),by=1)
ETH_sent.138.sent<-mean(ETH_sent.138$compound,na.rm = TRUE)
ETH_sent.138.sent<-cbind(ETH_sent.138.sent,ETH_sent.138[nrow(ETH_sent.138),4])
colnames(ETH_sent.138.sent)<-paste(c("avg.sent","volume"))
ETH_sent.138.sent

ETH_sent.139<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 06:00:00" &
                       ETH_sent$date <= "2021-05-29 06:59:59")
ETH_sent.139$index<-seq(1,nrow(ETH_sent.139),by=1)
ETH_sent.139.sent<-mean(ETH_sent.139$compound,na.rm = TRUE)
ETH_sent.139.sent<-cbind(ETH_sent.139.sent,ETH_sent.139[nrow(ETH_sent.139),4])
colnames(ETH_sent.139.sent)<-paste(c("avg.sent","volume"))
ETH_sent.139.sent

ETH_sent.140<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 07:00:00" &
                       ETH_sent$date <= "2021-05-29 07:59:59")
ETH_sent.140$index<-seq(1,nrow(ETH_sent.140),by=1)
ETH_sent.140.sent<-mean(ETH_sent.140$compound,na.rm = TRUE)
ETH_sent.140.sent<-cbind(ETH_sent.140.sent,ETH_sent.140[nrow(ETH_sent.140),4])
colnames(ETH_sent.140.sent)<-paste(c("avg.sent","volume"))
ETH_sent.140.sent

ETH_sent.141<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 08:00:00" &
                       ETH_sent$date <= "2021-05-29 08:59:59")
ETH_sent.141$index<-seq(1,nrow(ETH_sent.141),by=1)
ETH_sent.141.sent<-mean(ETH_sent.141$compound,na.rm = TRUE)
ETH_sent.141.sent<-cbind(ETH_sent.141.sent,ETH_sent.141[nrow(ETH_sent.141),4])
colnames(ETH_sent.141.sent)<-paste(c("avg.sent","volume"))
ETH_sent.141.sent

ETH_sent.142<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 09:00:00" &
                       ETH_sent$date <= "2021-05-29 09:59:59")
ETH_sent.142$index<-seq(1,nrow(ETH_sent.142),by=1)
ETH_sent.142.sent<-mean(ETH_sent.142$compound,na.rm = TRUE)
ETH_sent.142.sent<-cbind(ETH_sent.142.sent,ETH_sent.142[nrow(ETH_sent.142),4])
colnames(ETH_sent.142.sent)<-paste(c("avg.sent","volume"))
ETH_sent.142.sent

ETH_sent.143<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 10:00:00" &
                       ETH_sent$date <= "2021-05-29 10:59:59")
ETH_sent.143$index<-seq(1,nrow(ETH_sent.143),by=1)
ETH_sent.143.sent<-mean(ETH_sent.143$compound,na.rm = TRUE)
ETH_sent.143.sent<-cbind(ETH_sent.143.sent,ETH_sent.143[nrow(ETH_sent.143),4])
colnames(ETH_sent.143.sent)<-paste(c("avg.sent","volume"))
ETH_sent.143.sent

ETH_sent.144<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 11:00:00" &
                       ETH_sent$date <= "2021-05-29 11:59:59")
ETH_sent.144$index<-seq(1,nrow(ETH_sent.144),by=1)
ETH_sent.144.sent<-mean(ETH_sent.144$compound,na.rm = TRUE)
ETH_sent.144.sent<-cbind(ETH_sent.144.sent,ETH_sent.144[nrow(ETH_sent.144),4])
colnames(ETH_sent.144.sent)<-paste(c("avg.sent","volume"))
ETH_sent.144.sent

ETH_sent.145<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 12:00:00" &
                       ETH_sent$date <= "2021-05-29 12:59:59")
ETH_sent.145$index<-seq(1,nrow(ETH_sent.145),by=1)
ETH_sent.145.sent<-mean(ETH_sent.145$compound,na.rm = TRUE)
ETH_sent.145.sent<-cbind(ETH_sent.145.sent,ETH_sent.145[nrow(ETH_sent.145),4])
colnames(ETH_sent.145.sent)<-paste(c("avg.sent","volume"))
ETH_sent.145.sent

ETH_sent.146<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 13:00:00" &
                       ETH_sent$date <= "2021-05-29 13:59:59")
ETH_sent.146$index<-seq(1,nrow(ETH_sent.146),by=1)
ETH_sent.146.sent<-mean(ETH_sent.146$compound,na.rm = TRUE)
ETH_sent.146.sent<-cbind(ETH_sent.146.sent,ETH_sent.146[nrow(ETH_sent.146),4])
colnames(ETH_sent.146.sent)<-paste(c("avg.sent","volume"))
ETH_sent.146.sent

ETH_sent.147<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 14:00:00" &
                       ETH_sent$date <= "2021-05-29 14:59:59")
ETH_sent.147$index<-seq(1,nrow(ETH_sent.147),by=1)
ETH_sent.147.sent<-mean(ETH_sent.147$compound,na.rm = TRUE)
ETH_sent.147.sent<-cbind(ETH_sent.147.sent,ETH_sent.147[nrow(ETH_sent.147),4])
colnames(ETH_sent.147.sent)<-paste(c("avg.sent","volume"))
ETH_sent.147.sent

ETH_sent.148<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 15:00:00" &
                       ETH_sent$date <= "2021-05-29 15:59:59")
ETH_sent.148$index<-seq(1,nrow(ETH_sent.148),by=1)
ETH_sent.148.sent<-mean(ETH_sent.148$compound,na.rm = TRUE)
ETH_sent.148.sent<-cbind(ETH_sent.148.sent,ETH_sent.148[nrow(ETH_sent.148),4])
colnames(ETH_sent.148.sent)<-paste(c("avg.sent","volume"))
ETH_sent.148.sent

ETH_sent.149<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 16:00:00" &
                       ETH_sent$date <= "2021-05-29 16:59:59")
ETH_sent.149$index<-seq(1,nrow(ETH_sent.149),by=1)
ETH_sent.149.sent<-mean(ETH_sent.149$compound,na.rm = TRUE)
ETH_sent.149.sent<-cbind(ETH_sent.149.sent,ETH_sent.149[nrow(ETH_sent.149),4])
colnames(ETH_sent.149.sent)<-paste(c("avg.sent","volume"))
ETH_sent.149.sent

ETH_sent.150<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 17:00:00" &
                       ETH_sent$date <= "2021-05-29 17:59:59")
ETH_sent.150$index<-seq(1,nrow(ETH_sent.150),by=1)
ETH_sent.150.sent<-mean(ETH_sent.150$compound,na.rm = TRUE)
ETH_sent.150.sent<-cbind(ETH_sent.150.sent,ETH_sent.150[nrow(ETH_sent.150),4])
colnames(ETH_sent.150.sent)<-paste(c("avg.sent","volume"))
ETH_sent.150.sent

ETH_sent.151<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 18:00:00" &
                       ETH_sent$date <= "2021-05-29 18:59:59")
ETH_sent.151$index<-seq(1,nrow(ETH_sent.151),by=1)
ETH_sent.151.sent<-mean(ETH_sent.151$compound,na.rm = TRUE)
ETH_sent.151.sent<-cbind(ETH_sent.151.sent,ETH_sent.151[nrow(ETH_sent.151),4])
colnames(ETH_sent.151.sent)<-paste(c("avg.sent","volume"))
ETH_sent.151.sent

ETH_sent.152<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 19:00:00" &
                       ETH_sent$date <= "2021-05-29 19:59:59")
ETH_sent.152$index<-seq(1,nrow(ETH_sent.152),by=1)
ETH_sent.152.sent<-mean(ETH_sent.152$compound,na.rm = TRUE)
ETH_sent.152.sent<-cbind(ETH_sent.152.sent,ETH_sent.152[nrow(ETH_sent.152),4])
colnames(ETH_sent.152.sent)<-paste(c("avg.sent","volume"))
ETH_sent.152.sent

ETH_sent.153<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 20:00:00" &
                       ETH_sent$date <= "2021-05-29 20:59:59")
ETH_sent.153$index<-seq(1,nrow(ETH_sent.153),by=1)
ETH_sent.153.sent<-mean(ETH_sent.153$compound,na.rm = TRUE)
ETH_sent.153.sent<-cbind(ETH_sent.153.sent,ETH_sent.153[nrow(ETH_sent.153),4])
colnames(ETH_sent.153.sent)<-paste(c("avg.sent","volume"))
ETH_sent.153.sent

ETH_sent.154<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 21:00:00" &
                       ETH_sent$date <= "2021-05-29 21:59:59")
ETH_sent.154$index<-seq(1,nrow(ETH_sent.154),by=1)
ETH_sent.154.sent<-mean(ETH_sent.154$compound,na.rm = TRUE)
ETH_sent.154.sent<-cbind(ETH_sent.154.sent,ETH_sent.154[nrow(ETH_sent.154),4])
colnames(ETH_sent.154.sent)<-paste(c("avg.sent","volume"))
ETH_sent.154.sent

ETH_sent.155<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 22:00:00" &
                       ETH_sent$date <= "2021-05-29 22:59:59")
ETH_sent.155$index<-seq(1,nrow(ETH_sent.155),by=1)
ETH_sent.155.sent<-mean(ETH_sent.155$compound,na.rm = TRUE)
ETH_sent.155.sent<-cbind(ETH_sent.155.sent,ETH_sent.155[nrow(ETH_sent.155),4])
colnames(ETH_sent.155.sent)<-paste(c("avg.sent","volume"))
ETH_sent.155.sent

ETH_sent.156<-subset(ETH_sent,ETH_sent$date >= "2021-05-29 23:00:00" &
                       ETH_sent$date <= "2021-05-29 23:59:59")
ETH_sent.156$index<-seq(1,nrow(ETH_sent.156),by=1)
ETH_sent.156.sent<-mean(ETH_sent.156$compound,na.rm = TRUE)
ETH_sent.156.sent<-cbind(ETH_sent.156.sent,ETH_sent.156[nrow(ETH_sent.156),4])
colnames(ETH_sent.156.sent)<-paste(c("avg.sent","volume"))
ETH_sent.156.sent

ETH_sent.157<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 00:00:00" &
                       ETH_sent$date <= "2021-05-30 00:59:59")
ETH_sent.157$index<-seq(1,nrow(ETH_sent.157),by=1)
ETH_sent.157.sent<-mean(ETH_sent.157$compound,na.rm = TRUE)
ETH_sent.157.sent<-cbind(ETH_sent.157.sent,ETH_sent.157[nrow(ETH_sent.157),4])
colnames(ETH_sent.157.sent)<-paste(c("avg.sent","volume"))
ETH_sent.157.sent

ETH_sent.158<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 01:00:00" &
                       ETH_sent$date <= "2021-05-30 01:59:59")
ETH_sent.158$index<-seq(1,nrow(ETH_sent.158),by=1)
ETH_sent.158.sent<-mean(ETH_sent.158$compound,na.rm = TRUE)
ETH_sent.158.sent<-cbind(ETH_sent.158.sent,ETH_sent.158[nrow(ETH_sent.158),4])
colnames(ETH_sent.158.sent)<-paste(c("avg.sent","volume"))
ETH_sent.158.sent

ETH_sent.159<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 02:00:00" &
                       ETH_sent$date <= "2021-05-30 02:59:59")
ETH_sent.159$index<-seq(1,nrow(ETH_sent.159),by=1)
ETH_sent.159.sent<-mean(ETH_sent.159$compound,na.rm = TRUE)
ETH_sent.159.sent<-cbind(ETH_sent.159.sent,ETH_sent.159[nrow(ETH_sent.159),4])
colnames(ETH_sent.159.sent)<-paste(c("avg.sent","volume"))
ETH_sent.159.sent

ETH_sent.160<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 03:00:00" &
                       ETH_sent$date <= "2021-05-30 03:59:59")
ETH_sent.160$index<-seq(1,nrow(ETH_sent.160),by=1)
ETH_sent.160.sent<-mean(ETH_sent.160$compound,na.rm = TRUE)
ETH_sent.160.sent<-cbind(ETH_sent.160.sent,ETH_sent.160[nrow(ETH_sent.160),4])
colnames(ETH_sent.160.sent)<-paste(c("avg.sent","volume"))
ETH_sent.160.sent

ETH_sent.161<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 04:00:00" &
                       ETH_sent$date <= "2021-05-30 04:59:59")
ETH_sent.161$index<-seq(1,nrow(ETH_sent.161),by=1)
ETH_sent.161.sent<-mean(ETH_sent.161$compound,na.rm = TRUE)
ETH_sent.161.sent<-cbind(ETH_sent.161.sent,ETH_sent.161[nrow(ETH_sent.161),4])
colnames(ETH_sent.161.sent)<-paste(c("avg.sent","volume"))
ETH_sent.161.sent

ETH_sent.162<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 05:00:00" &
                       ETH_sent$date <= "2021-05-30 05:59:59")
ETH_sent.162$index<-seq(1,nrow(ETH_sent.162),by=1)
ETH_sent.162.sent<-mean(ETH_sent.162$compound,na.rm = TRUE)
ETH_sent.162.sent<-cbind(ETH_sent.162.sent,ETH_sent.162[nrow(ETH_sent.162),4])
colnames(ETH_sent.162.sent)<-paste(c("avg.sent","volume"))
ETH_sent.162.sent

ETH_sent.163<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 06:00:00" &
                       ETH_sent$date <= "2021-05-30 06:59:59")
ETH_sent.163$index<-seq(1,nrow(ETH_sent.163),by=1)
ETH_sent.163.sent<-mean(ETH_sent.163$compound,na.rm = TRUE)
ETH_sent.163.sent<-cbind(ETH_sent.163.sent,ETH_sent.163[nrow(ETH_sent.163),4])
colnames(ETH_sent.163.sent)<-paste(c("avg.sent","volume"))
ETH_sent.163.sent

ETH_sent.164<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 07:00:00" &
                       ETH_sent$date <= "2021-05-30 07:59:59")
ETH_sent.164$index<-seq(1,nrow(ETH_sent.164),by=1)
ETH_sent.164.sent<-mean(ETH_sent.164$compound,na.rm = TRUE)
ETH_sent.164.sent<-cbind(ETH_sent.164.sent,ETH_sent.164[nrow(ETH_sent.164),4])
colnames(ETH_sent.164.sent)<-paste(c("avg.sent","volume"))
ETH_sent.164.sent

ETH_sent.165<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 08:00:00" &
                       ETH_sent$date <= "2021-05-30 08:59:59")
ETH_sent.165$index<-seq(1,nrow(ETH_sent.165),by=1)
ETH_sent.165.sent<-mean(ETH_sent.165$compound,na.rm = TRUE)
ETH_sent.165.sent<-cbind(ETH_sent.165.sent,ETH_sent.165[nrow(ETH_sent.165),4])
colnames(ETH_sent.165.sent)<-paste(c("avg.sent","volume"))
ETH_sent.165.sent

ETH_sent.166<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 09:00:00" &
                       ETH_sent$date <= "2021-05-30 09:59:59")
ETH_sent.166$index<-seq(1,nrow(ETH_sent.166),by=1)
ETH_sent.166.sent<-mean(ETH_sent.166$compound,na.rm = TRUE)
ETH_sent.166.sent<-cbind(ETH_sent.166.sent,ETH_sent.166[nrow(ETH_sent.166),4])
colnames(ETH_sent.166.sent)<-paste(c("avg.sent","volume"))
ETH_sent.166.sent

ETH_sent.167<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 10:00:00" &
                       ETH_sent$date <= "2021-05-30 10:59:59")
ETH_sent.167$index<-seq(1,nrow(ETH_sent.167),by=1)
ETH_sent.167.sent<-mean(ETH_sent.167$compound,na.rm = TRUE)
ETH_sent.167.sent<-cbind(ETH_sent.167.sent,ETH_sent.167[nrow(ETH_sent.167),4])
colnames(ETH_sent.167.sent)<-paste(c("avg.sent","volume"))
ETH_sent.167.sent

ETH_sent.168<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 11:00:00" &
                       ETH_sent$date <= "2021-05-30 11:59:59")
ETH_sent.168$index<-seq(1,nrow(ETH_sent.168),by=1)
ETH_sent.168.sent<-mean(ETH_sent.168$compound,na.rm = TRUE)
ETH_sent.168.sent<-cbind(ETH_sent.168.sent,ETH_sent.168[nrow(ETH_sent.168),4])
colnames(ETH_sent.168.sent)<-paste(c("avg.sent","volume"))
ETH_sent.168.sent

ETH_sent.169<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 12:00:00" &
                       ETH_sent$date <= "2021-05-30 12:59:59")
ETH_sent.169$index<-seq(1,nrow(ETH_sent.169),by=1)
ETH_sent.169.sent<-mean(ETH_sent.169$compound,na.rm = TRUE)
ETH_sent.169.sent<-cbind(ETH_sent.169.sent,ETH_sent.169[nrow(ETH_sent.169),4])
colnames(ETH_sent.169.sent)<-paste(c("avg.sent","volume"))
ETH_sent.169.sent

ETH_sent.170<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 13:00:00" &
                       ETH_sent$date <= "2021-05-30 13:59:59")
ETH_sent.170$index<-seq(1,nrow(ETH_sent.170),by=1)
ETH_sent.170.sent<-mean(ETH_sent.170$compound,na.rm = TRUE)
ETH_sent.170.sent<-cbind(ETH_sent.170.sent,ETH_sent.170[nrow(ETH_sent.170),4])
colnames(ETH_sent.170.sent)<-paste(c("avg.sent","volume"))
ETH_sent.170.sent

ETH_sent.171<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 14:00:00" &
                       ETH_sent$date <= "2021-05-30 14:59:59")
ETH_sent.171$index<-seq(1,nrow(ETH_sent.171),by=1)
ETH_sent.171.sent<-mean(ETH_sent.171$compound,na.rm = TRUE)
ETH_sent.171.sent<-cbind(ETH_sent.171.sent,ETH_sent.171[nrow(ETH_sent.171),4])
colnames(ETH_sent.171.sent)<-paste(c("avg.sent","volume"))
ETH_sent.171.sent

ETH_sent.172<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 15:00:00" &
                       ETH_sent$date <= "2021-05-30 15:59:59")
ETH_sent.172$index<-seq(1,nrow(ETH_sent.172),by=1)
ETH_sent.172.sent<-mean(ETH_sent.172$compound,na.rm = TRUE)
ETH_sent.172.sent<-cbind(ETH_sent.172.sent,ETH_sent.172[nrow(ETH_sent.172),4])
colnames(ETH_sent.172.sent)<-paste(c("avg.sent","volume"))
ETH_sent.172.sent

ETH_sent.173<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 16:00:00" &
                       ETH_sent$date <= "2021-05-30 16:59:59")
ETH_sent.173$index<-seq(1,nrow(ETH_sent.173),by=1)
ETH_sent.173.sent<-mean(ETH_sent.173$compound,na.rm = TRUE)
ETH_sent.173.sent<-cbind(ETH_sent.173.sent,ETH_sent.173[nrow(ETH_sent.173),4])
colnames(ETH_sent.173.sent)<-paste(c("avg.sent","volume"))
ETH_sent.173.sent

ETH_sent.174<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 17:00:00" &
                       ETH_sent$date <= "2021-05-30 17:59:59")
ETH_sent.174$index<-seq(1,nrow(ETH_sent.174),by=1)
ETH_sent.174.sent<-mean(ETH_sent.174$compound,na.rm = TRUE)
ETH_sent.174.sent<-cbind(ETH_sent.174.sent,ETH_sent.174[nrow(ETH_sent.174),4])
colnames(ETH_sent.174.sent)<-paste(c("avg.sent","volume"))
ETH_sent.174.sent

ETH_sent.175<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 18:00:00" &
                       ETH_sent$date <= "2021-05-30 18:59:59")
ETH_sent.175$index<-seq(1,nrow(ETH_sent.175),by=1)
ETH_sent.175.sent<-mean(ETH_sent.175$compound,na.rm = TRUE)
ETH_sent.175.sent<-cbind(ETH_sent.175.sent,ETH_sent.175[nrow(ETH_sent.175),4])
colnames(ETH_sent.175.sent)<-paste(c("avg.sent","volume"))
ETH_sent.175.sent

ETH_sent.176<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 19:00:00" &
                       ETH_sent$date <= "2021-05-30 19:59:59")
ETH_sent.176$index<-seq(1,nrow(ETH_sent.176),by=1)
ETH_sent.176.sent<-mean(ETH_sent.176$compound,na.rm = TRUE)
ETH_sent.176.sent<-cbind(ETH_sent.176.sent,ETH_sent.176[nrow(ETH_sent.176),4])
colnames(ETH_sent.176.sent)<-paste(c("avg.sent","volume"))
ETH_sent.176.sent

ETH_sent.177<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 20:00:00" &
                       ETH_sent$date <= "2021-05-30 20:59:59")
ETH_sent.177$index<-seq(1,nrow(ETH_sent.177),by=1)
ETH_sent.177.sent<-mean(ETH_sent.177$compound,na.rm = TRUE)
ETH_sent.177.sent<-cbind(ETH_sent.177.sent,ETH_sent.177[nrow(ETH_sent.177),4])
colnames(ETH_sent.177.sent)<-paste(c("avg.sent","volume"))
ETH_sent.177.sent

ETH_sent.178<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 21:00:00" &
                       ETH_sent$date <= "2021-05-30 21:59:59")
ETH_sent.178$index<-seq(1,nrow(ETH_sent.178),by=1)
ETH_sent.178.sent<-mean(ETH_sent.178$compound,na.rm = TRUE)
ETH_sent.178.sent<-cbind(ETH_sent.178.sent,ETH_sent.178[nrow(ETH_sent.178),4])
colnames(ETH_sent.178.sent)<-paste(c("avg.sent","volume"))
ETH_sent.178.sent

ETH_sent.179<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 22:00:00" &
                       ETH_sent$date <= "2021-05-30 22:59:59")
ETH_sent.179$index<-seq(1,nrow(ETH_sent.179),by=1)
ETH_sent.179.sent<-mean(ETH_sent.179$compound,na.rm = TRUE)
ETH_sent.179.sent<-cbind(ETH_sent.179.sent,ETH_sent.179[nrow(ETH_sent.179),4])
colnames(ETH_sent.179.sent)<-paste(c("avg.sent","volume"))
ETH_sent.179.sent

ETH_sent.180<-subset(ETH_sent,ETH_sent$date >= "2021-05-30 23:00:00" &
                       ETH_sent$date <= "2021-05-30 23:59:59")
ETH_sent.180$index<-seq(1,nrow(ETH_sent.180),by=1)
ETH_sent.180.sent<-mean(ETH_sent.180$compound,na.rm = TRUE)
ETH_sent.180.sent<-cbind(ETH_sent.180.sent,ETH_sent.180[nrow(ETH_sent.180),4])
colnames(ETH_sent.180.sent)<-paste(c("avg.sent","volume"))
ETH_sent.180.sent

ETH_sent.181<-subset(ETH_sent,ETH_sent$date >= "2021-05-31 00:00:00" &
                       ETH_sent$date <= "2021-05-31 00:59:59")
ETH_sent.181$index<-seq(1,nrow(ETH_sent.181),by=1)
ETH_sent.181.sent<-mean(ETH_sent.181$compound,na.rm = TRUE)
ETH_sent.181.sent<-cbind(ETH_sent.181.sent,ETH_sent.181[nrow(ETH_sent.181),4])
colnames(ETH_sent.181.sent)<-paste(c("avg.sent","volume"))
ETH_sent.181.sent

ETH_sent.182<-subset(ETH_sent,ETH_sent$date >= "2021-05-31 01:00:00" &
                       ETH_sent$date <= "2021-05-31 01:59:59")
ETH_sent.182$index<-seq(1,nrow(ETH_sent.182),by=1)
ETH_sent.182.sent<-mean(ETH_sent.182$compound,na.rm = TRUE)
ETH_sent.182.sent<-cbind(ETH_sent.182.sent,ETH_sent.182[nrow(ETH_sent.182),4])
colnames(ETH_sent.182.sent)<-paste(c("avg.sent","volume"))
ETH_sent.182.sent

ETH_sent.183<-subset(ETH_sent,ETH_sent$date >= "2021-05-31 02:00:00" &
                       ETH_sent$date <= "2021-05-31 02:59:59")
ETH_sent.183$index<-seq(1,nrow(ETH_sent.183),by=1)
ETH_sent.183.sent<-mean(ETH_sent.183$compound,na.rm = TRUE)
ETH_sent.183.sent<-cbind(ETH_sent.183.sent,ETH_sent.183[nrow(ETH_sent.183),4])
colnames(ETH_sent.183.sent)<-paste(c("avg.sent","volume"))
ETH_sent.183.sent

ETH_sent.184<-subset(ETH_sent,ETH_sent$date >= "2021-05-31 03:00:00" &
                       ETH_sent$date <= "2021-05-31 03:59:59")
ETH_sent.184$index<-seq(1,nrow(ETH_sent.184),by=1)
ETH_sent.184.sent<-mean(ETH_sent.184$compound,na.rm = TRUE)
ETH_sent.184.sent<-cbind(ETH_sent.184.sent,ETH_sent.184[nrow(ETH_sent.184),4])
colnames(ETH_sent.184.sent)<-paste(c("avg.sent","volume"))
ETH_sent.184.sent

ETH_sent.185<-subset(ETH_sent,ETH_sent$date >= "2021-05-31 04:00:00" &
                       ETH_sent$date <= "2021-05-31 04:59:59")
ETH_sent.185$index<-seq(1,nrow(ETH_sent.185),by=1)
ETH_sent.185.sent<-mean(ETH_sent.185$compound,na.rm = TRUE)
ETH_sent.185.sent<-cbind(ETH_sent.185.sent,ETH_sent.185[nrow(ETH_sent.185),4])
colnames(ETH_sent.185.sent)<-paste(c("avg.sent","volume"))
ETH_sent.185.sent

ETH_sent.186<-subset(ETH_sent,ETH_sent$date >= "2021-05-31 05:00:00" &
                       ETH_sent$date <= "2021-05-31 05:59:59")
ETH_sent.186$index<-seq(1,nrow(ETH_sent.186),by=1)
ETH_sent.186.sent<-mean(ETH_sent.186$compound,na.rm = TRUE)
ETH_sent.186.sent<-cbind(ETH_sent.186.sent,ETH_sent.186[nrow(ETH_sent.186),4])
colnames(ETH_sent.186.sent)<-paste(c("avg.sent","volume"))
ETH_sent.186.sent

ETH_sent.187<-subset(ETH_sent,ETH_sent$date >= "2021-05-31 06:00:00" &
                       ETH_sent$date <= "2021-05-31 06:59:59")
ETH_sent.187$index<-seq(1,nrow(ETH_sent.187),by=1)
ETH_sent.187.sent<-mean(ETH_sent.187$compound,na.rm = TRUE)
ETH_sent.187.sent<-cbind(ETH_sent.187.sent,ETH_sent.187[nrow(ETH_sent.187),4])
colnames(ETH_sent.187.sent)<-paste(c("avg.sent","volume"))
ETH_sent.187.sent

ETH_sent.188<-subset(ETH_sent,ETH_sent$date >= "2021-05-31 07:00:00" &
                       ETH_sent$date <= "2021-05-31 07:59:59")
ETH_sent.188$index<-seq(1,nrow(ETH_sent.188),by=1)
ETH_sent.188.sent<-mean(ETH_sent.188$compound,na.rm = TRUE)
ETH_sent.188.sent<-cbind(ETH_sent.188.sent,ETH_sent.188[nrow(ETH_sent.188),4])
colnames(ETH_sent.188.sent)<-paste(c("avg.sent","volume"))
ETH_sent.188.sent

ETH_sent.189<-subset(ETH_sent,ETH_sent$date >= "2021-05-31 08:00:00" &
                       ETH_sent$date <= "2021-05-31 08:59:59")
ETH_sent.189$index<-seq(1,nrow(ETH_sent.189),by=1)
ETH_sent.189.sent<-mean(ETH_sent.189$compound,na.rm = TRUE)
ETH_sent.189.sent<-cbind(ETH_sent.189.sent,ETH_sent.189[nrow(ETH_sent.189),4])
colnames(ETH_sent.189.sent)<-paste(c("avg.sent","volume"))
ETH_sent.189.sent

ETH_sent.190<-subset(ETH_sent,ETH_sent$date >= "2021-05-31 09:00:00" &
                       ETH_sent$date <= "2021-05-31 09:59:59")
ETH_sent.190$index<-seq(1,nrow(ETH_sent.190),by=1)
ETH_sent.190.sent<-mean(ETH_sent.190$compound,na.rm = TRUE)
ETH_sent.190.sent<-cbind(ETH_sent.190.sent,ETH_sent.190[nrow(ETH_sent.190),4])
colnames(ETH_sent.190.sent)<-paste(c("avg.sent","volume"))
ETH_sent.190.sent

ETH_sent.191<-subset(ETH_sent,ETH_sent$date >= "2021-05-31 10:00:00" &
                       ETH_sent$date <= "2021-05-31 10:59:59")
ETH_sent.191$index<-seq(1,nrow(ETH_sent.191),by=1)
ETH_sent.191.sent<-mean(ETH_sent.191$compound,na.rm = TRUE)
ETH_sent.191.sent<-cbind(ETH_sent.191.sent,ETH_sent.191[nrow(ETH_sent.191),4])
colnames(ETH_sent.191.sent)<-paste(c("avg.sent","volume"))
ETH_sent.191.sent

ETH_sent.192<-subset(ETH_sent,ETH_sent$date >= "2021-05-31 11:00:00" &
                       ETH_sent$date <= "2021-05-31 11:59:59")
ETH_sent.192$index<-seq(1,nrow(ETH_sent.192),by=1)
ETH_sent.192.sent<-mean(ETH_sent.192$compound,na.rm = TRUE)
ETH_sent.192.sent<-cbind(ETH_sent.192.sent,ETH_sent.192[nrow(ETH_sent.192),4])
colnames(ETH_sent.192.sent)<-paste(c("avg.sent","volume"))
ETH_sent.192.sent

ETH_sent.193<-subset(ETH_sent,ETH_sent$date >= "2021-05-31 12:00:00" &
                       ETH_sent$date <= "2021-05-31 12:59:59")
ETH_sent.193$index<-seq(1,nrow(ETH_sent.193),by=1)
ETH_sent.193.sent<-mean(ETH_sent.193$compound,na.rm = TRUE)
ETH_sent.193.sent<-cbind(ETH_sent.193.sent,ETH_sent.193[nrow(ETH_sent.193),4])
colnames(ETH_sent.193.sent)<-paste(c("avg.sent","volume"))
ETH_sent.193.sent

ETH_sent.hourly<-rbind(ETH_sent.1.sent,ETH_sent.2.sent,ETH_sent.3.sent,ETH_sent.4.sent,ETH_sent.5.sent,ETH_sent.6.sent,
                       ETH_sent.7.sent,ETH_sent.8.sent,ETH_sent.9.sent,ETH_sent.10.sent,ETH_sent.11.sent,ETH_sent.12.sent,
                       ETH_sent.13.sent,ETH_sent.14.sent,ETH_sent.15.sent,ETH_sent.16.sent,ETH_sent.17.sent,ETH_sent.18.sent,
                       ETH_sent.19.sent,ETH_sent.20.sent,ETH_sent.21.sent,ETH_sent.22.sent,ETH_sent.23.sent,ETH_sent.24.sent,
                       ETH_sent.25.sent,ETH_sent.26.sent,ETH_sent.27.sent,ETH_sent.28.sent,ETH_sent.29.sent,ETH_sent.30.sent,
                       ETH_sent.31.sent,ETH_sent.32.sent,ETH_sent.33.sent,ETH_sent.34.sent,ETH_sent.35.sent,ETH_sent.36.sent,
                       ETH_sent.37.sent,ETH_sent.38.sent,ETH_sent.39.sent,ETH_sent.40.sent,ETH_sent.41.sent,ETH_sent.42.sent,
                       ETH_sent.43.sent,ETH_sent.44.sent,ETH_sent.45.sent,ETH_sent.46.sent,ETH_sent.47.sent,ETH_sent.48.sent,
                       ETH_sent.49.sent,ETH_sent.50.sent,ETH_sent.51.sent,ETH_sent.52.sent,ETH_sent.53.sent,ETH_sent.54.sent,
                       ETH_sent.55.sent,ETH_sent.56.sent,ETH_sent.57.sent,ETH_sent.58.sent,ETH_sent.59.sent,ETH_sent.60.sent,
                       ETH_sent.61.sent,ETH_sent.62.sent,ETH_sent.63.sent,ETH_sent.64.sent,ETH_sent.65.sent,ETH_sent.66.sent,
                       ETH_sent.67.sent,ETH_sent.68.sent,ETH_sent.69.sent,ETH_sent.70.sent,ETH_sent.71.sent,ETH_sent.72.sent,
                       ETH_sent.73.sent,ETH_sent.74.sent,ETH_sent.75.sent,ETH_sent.76.sent,ETH_sent.77.sent,ETH_sent.78.sent,
                       ETH_sent.79.sent,ETH_sent.80.sent,ETH_sent.81.sent,ETH_sent.82.sent,ETH_sent.83.sent,ETH_sent.84.sent,
                       ETH_sent.85.sent,ETH_sent.86.sent,ETH_sent.87.sent,ETH_sent.88.sent,ETH_sent.89.sent,ETH_sent.90.sent,
                       ETH_sent.91.sent,ETH_sent.92.sent,ETH_sent.93.sent,ETH_sent.94.sent,ETH_sent.95.sent,ETH_sent.96.sent,
                       ETH_sent.97.sent,ETH_sent.98.sent,ETH_sent.99.sent,ETH_sent.100.sent,ETH_sent.101.sent,ETH_sent.102.sent,
                       ETH_sent.103.sent,ETH_sent.104.sent,ETH_sent.105.sent,ETH_sent.106.sent,ETH_sent.107.sent,ETH_sent.108.sent,
                       ETH_sent.109.sent,ETH_sent.110.sent,ETH_sent.111.sent,ETH_sent.112.sent,ETH_sent.113.sent,ETH_sent.114.sent,
                       ETH_sent.115.sent,ETH_sent.116.sent,ETH_sent.117.sent,ETH_sent.118.sent,ETH_sent.119.sent,ETH_sent.120.sent,
                       ETH_sent.121.sent,ETH_sent.122.sent,ETH_sent.123.sent,ETH_sent.124.sent,ETH_sent.125.sent,ETH_sent.126.sent,
                       ETH_sent.127.sent,ETH_sent.128.sent,ETH_sent.129.sent,ETH_sent.130.sent,ETH_sent.131.sent,ETH_sent.132.sent,
                       ETH_sent.133.sent,ETH_sent.134.sent,ETH_sent.135.sent,ETH_sent.136.sent,ETH_sent.137.sent,ETH_sent.138.sent,
                       ETH_sent.139.sent,ETH_sent.140.sent,ETH_sent.141.sent,ETH_sent.142.sent,ETH_sent.143.sent,ETH_sent.144.sent,
                       ETH_sent.145.sent,ETH_sent.146.sent,ETH_sent.147.sent,ETH_sent.148.sent,ETH_sent.149.sent,ETH_sent.150.sent,
                       ETH_sent.151.sent,ETH_sent.152.sent,ETH_sent.153.sent,ETH_sent.154.sent,ETH_sent.155.sent,ETH_sent.156.sent,
                       ETH_sent.157.sent,ETH_sent.158.sent,ETH_sent.159.sent,ETH_sent.160.sent,ETH_sent.161.sent,ETH_sent.162.sent,
                       ETH_sent.163.sent,ETH_sent.164.sent,ETH_sent.165.sent,ETH_sent.166.sent,ETH_sent.167.sent,ETH_sent.168.sent,
                       ETH_sent.169.sent,ETH_sent.170.sent,ETH_sent.171.sent,ETH_sent.172.sent,ETH_sent.173.sent,ETH_sent.174.sent,
                       ETH_sent.175.sent,ETH_sent.176.sent,ETH_sent.177.sent,ETH_sent.178.sent,ETH_sent.179.sent,ETH_sent.180.sent,
                       ETH_sent.181.sent,ETH_sent.182.sent,ETH_sent.183.sent,ETH_sent.184.sent,ETH_sent.185.sent,ETH_sent.186.sent,
                       ETH_sent.187.sent,ETH_sent.188.sent,ETH_sent.189.sent,ETH_sent.190.sent,ETH_sent.191.sent,ETH_sent.192.sent,
                       ETH_sent.193.sent)

date<-seq(from=as.POSIXct("2021-05-23 12:00:00", tz="UTC"), 
          to=as.POSIXct("2021-05-31 12:00:00", tz="UTC"), by="hour")
date<- as.data.frame(date)

ETH_sent.hourly <- cbind(date, ETH_sent.hourly)
head(ETH_sent.hourly)

write_as_csv(ETH_sent.hourly, "ETH_sent.hourly.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(ETH_sent.hourly, "ETH_sent.hourly.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
ETH_sent.hourly<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/ETH_sent.hourly.csv",header = TRUE)


# *S6: AVERAGE SENTIMENT SCORE PER DAY -------------------------------------
ETH_sent.hourly[c(1:3,nrow(ETH_sent.hourly)),]
ETH_sent.daily<-ETH_sent.hourly[c(13:180),]

ETH_sent.daily1<-subset(ETH_sent.daily,ETH_sent.daily$date >= "2021-05-24 00:00:00" &
                          ETH_sent.daily$date <= "2021-05-24 23:00:00")
ETH_sent.daily1.sent<-mean(ETH_sent.daily1$avg.sent,na.rm = TRUE)
ETH_sent.daily1.vol<-sum(ETH_sent.daily1$volume)
ETH_sent.daily1<-cbind(ETH_sent.daily1.sent,ETH_sent.daily1.vol)
colnames(ETH_sent.daily1)<-paste(c("Daily.sent","Daily.vol"))
ETH_sent.daily1

ETH_sent.daily2<-subset(ETH_sent.daily,ETH_sent.daily$date >= "2021-05-25 00:00:00" &
                          ETH_sent.daily$date <= "2021-05-25 23:00:00")
ETH_sent.daily2.sent<-mean(ETH_sent.daily2$avg.sent,na.rm = TRUE)
ETH_sent.daily2.vol<-sum(ETH_sent.daily2$volume)
ETH_sent.daily2<-cbind(ETH_sent.daily2.sent,ETH_sent.daily2.vol)
colnames(ETH_sent.daily2)<-paste(c("Daily.sent","Daily.vol"))
ETH_sent.daily2

ETH_sent.daily3<-subset(ETH_sent.daily,ETH_sent.daily$date >= "2021-05-26 00:00:00" &
                          ETH_sent.daily$date <= "2021-05-26 23:00:00")
ETH_sent.daily3.sent<-mean(ETH_sent.daily3$avg.sent,na.rm = TRUE)
ETH_sent.daily3.vol<-sum(ETH_sent.daily3$volume)
ETH_sent.daily3<-cbind(ETH_sent.daily3.sent,ETH_sent.daily3.vol)
colnames(ETH_sent.daily3)<-paste(c("Daily.sent","Daily.vol"))
ETH_sent.daily3

ETH_sent.daily4<-subset(ETH_sent.daily,ETH_sent.daily$date >= "2021-05-27 00:00:00" &
                          ETH_sent.daily$date <= "2021-05-27 23:00:00")
ETH_sent.daily4.sent<-mean(ETH_sent.daily4$avg.sent,na.rm = TRUE)
ETH_sent.daily4.vol<-sum(ETH_sent.daily4$volume)
ETH_sent.daily4<-cbind(ETH_sent.daily4.sent,ETH_sent.daily4.vol)
colnames(ETH_sent.daily4)<-paste(c("Daily.sent","Daily.vol"))
ETH_sent.daily4

ETH_sent.daily5<-subset(ETH_sent.daily,ETH_sent.daily$date >= "2021-05-28 00:00:00" &
                          ETH_sent.daily$date <= "2021-05-28 23:00:00")
ETH_sent.daily5.sent<-mean(ETH_sent.daily5$avg.sent,na.rm = TRUE)
ETH_sent.daily5.vol<-sum(ETH_sent.daily5$volume)
ETH_sent.daily5<-cbind(ETH_sent.daily5.sent,ETH_sent.daily5.vol)
colnames(ETH_sent.daily5)<-paste(c("Daily.sent","Daily.vol"))
ETH_sent.daily5

ETH_sent.daily6<-subset(ETH_sent.daily,ETH_sent.daily$date >= "2021-05-29 00:00:00" &
                          ETH_sent.daily$date <= "2021-05-29 23:00:00")
ETH_sent.daily6.sent<-mean(ETH_sent.daily6$avg.sent,na.rm = TRUE)
ETH_sent.daily6.vol<-sum(ETH_sent.daily6$volume)
ETH_sent.daily6<-cbind(ETH_sent.daily6.sent,ETH_sent.daily6.vol)
colnames(ETH_sent.daily6)<-paste(c("Daily.sent","Daily.vol"))
ETH_sent.daily6

ETH_sent.daily7<-subset(ETH_sent.daily,ETH_sent.daily$date >= "2021-05-30 00:00:00" &
                          ETH_sent.daily$date <= "2021-05-30 23:00:00")
ETH_sent.daily7.sent<-mean(ETH_sent.daily7$avg.sent,na.rm = TRUE)
ETH_sent.daily7.vol<-sum(ETH_sent.daily7$volume)
ETH_sent.daily7<-cbind(ETH_sent.daily7.sent,ETH_sent.daily7.vol)
colnames(ETH_sent.daily7)<-paste(c("Daily.sent","Daily.vol"))
ETH_sent.daily7

ETH_sent.daily.tot<-rbind(ETH_sent.daily1,ETH_sent.daily2,ETH_sent.daily3,ETH_sent.daily4,ETH_sent.daily5,ETH_sent.daily6,ETH_sent.daily7)
  
date<-seq(from=as.POSIXct("2021-05-24 00:00:00", tz="UTC"), 
          to=as.POSIXct("2021-05-30 00:00:00", tz="UTC"), by="day")
date<- as.data.frame(date)

ETH_sent.daily.tot <- cbind(date, ETH_sent.daily.tot)

options(scipen = 1)
ETH_sent.daily.tot$Daily.sent.diff<-Delt(ETH_sent.daily.tot$Daily.sent)
ETH_sent.daily.tot$Daily.vol.diff<-Delt(ETH_sent.daily.tot$Daily.vol)
ETH_sent.daily.tot[is.na(ETH_sent.daily.tot)] <- 0

write_as_csv(ETH_sent.daily.tot, "ETH_sent.daily.tot.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(ETH_sent.daily.tot, "ETH_sent.daily.tot.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
ETH_sent.daily.tot<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/ETH_sent.daily.tot.csv",header = TRUE)

# PLOTS -------------------------------------------------------------------
##**Plot the frequency of tweets over a variety of time intervals ---------
ggplot(ETH_sent.hourly,aes(x=date))+
  geom_line(aes(y=volume))+
  theme_bw()+
  labs(x = NULL, y = NULL,
       title = "Frequency of #ETH tweets",
       subtitle = paste0(format(min(as.Date(ETH_twt$created_at)), "%d %B %Y"), " to ", format(max(as.Date(ETH_twt$created_at)),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet")

##**Plot the tweets sentiment score ---------------------------------------
ggplot(ETH_sent,aes(x=compound)) + 
  geom_histogram(bins = 15,alpha=.6) + 
  theme_bw() +
  labs(x="Score",y="Number of tweets")+
  labs(title = "Sentiment analysis of #ETH Tweets from 23-31 May 2021")+
  labs(subtitle = "Using VADER lexicon")

##**Plot the hourly tweets sentiment score + volume -----------------------------
# Value used to transform the data
coeff <- 0.0001

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(ETH_sent.hourly, aes(x=date)) +
  
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
  labs(title = "Hourly tweets sentiment scores and volume of #ETH",
       caption = "Using  VADER appproach")

# PART III: ANALYSIS FROM PART I + II -------------------------------------
# *S1: Extract necessary data ----------------------------------------------
ETH.price.hourly<-subset(ETHUSD_1hr,ETHUSD_1hr$Date >= "2021-05-23 15:00:00" &
                           ETHUSD_1hr$Date <= "2021-06-01 12:00:00")
ETH.price.hourly<-ETH.price.hourly[,c(2,7,8)]
ETH.price.hourly<-ETH.price.hourly[order(ETH.price.hourly$Date),]

ETH.price.hourly[c(1:3,nrow(ETH.price.hourly)),]

ETH_sent.hourly[c(1:3,nrow(ETH_sent.hourly)),]


# *S2: CALCULATE HOURLY CHANGES IN PRICE/SENTIMENT -------------------------------

#HOURLY LOGARITHMIC PRICE RETURNS
ETH.hourly.log.ret <- as.data.frame(ETH.price.hourly)
ETH.hourly.log.ret$ETH.log.ret <- c(diff(log(ETH.hourly.log.ret$Close)),0)
options(digits = 3)

#HOURLY LOGARITHMIC SENTIMENT
ETH_sent.diff<-ETH_sent.hourly
ETH_sent.diff$log.sent<-c(diff(log(ETH_sent.diff$avg.sent)),0)

#TWEETS VOLUME LOGARITHMIC CHANGES
ETH_vol.diff<-ETH_sent.hourly
ETH_vol.diff$vol.log<- c(diff(log(ETH_vol.diff$volume)),0)

#COMBINE THE RESULTS
ETH.log.change<-cbind(ETH.hourly.log.ret$ETH.log.ret,ETH_sent.diff$log.sent,ETH_vol.diff$vol.log)
colnames(ETH.log.change)<-paste(c("ETHUSD","ETH","Volume"))
ETH.log.change<-as.data.frame(ETH.log.change)
# *S3: SMA -----------------------------------------------------------------
ETH.sma.3<-ETH.price.hourly[,c(1:2)]
ETH.sma.3$sma5 <- rollmeanr(ETH.sma.3$Close, k =5, fill=NA)
ETH.sma.3$sma10 <- rollmeanr(ETH.sma.3$Close, k = 10, fill=NA)

#**S4: CORRELATION MATRIX: VOLUME, SENTIMENT, PRICE ------------------------
#HOURLY TABLE
hourly.table<-ETH_sent.diff[,-4]
hourly.table<-cbind(hourly.table,ETH.hourly.log.ret$Close,ETH.hourly.log.ret$ETH.log.ret)
colnames(hourly.table)<-paste(c("Date","Hourly.sent","Hourly.vol","Hourly.close","Hourly.log.ret"))

hourly.table$sent.idx<-hourly.table$Hourly.sent/hourly.table$Hourly.sent[1]
hourly.table$vol.idx<-hourly.table$Hourly.vol/hourly.table$Hourly.vol[1]
hourly.table$close.idx<-hourly.table$Hourly.close/hourly.table$Hourly.close[1]

#DAILY TABLE
ETH.price.daily<-subset(data.ETH,index(data.ETH)>="2021-05-24" &
                          index(data.ETH)<="2021-05-30")
ETH.price.daily<-ETH.price.daily[,6]
daily.table<-ETH_sent.daily.tot[,c(-4,-5)]
daily.table<-cbind(daily.table,ETH.price.daily$`ETH-USD.Adjusted`)
daily.table$Daily.log.ret<-c(0,diff(log(daily.table$`ETH-USD.Adjusted`)))
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
##**Correlation Scatterplot Between #ETH Sentiment changes and ETH Price Return--------
plot(ETH.log.change$ETHUSD, ETH.log.change$ETH, pch = 19, 
     main = "Correlation Matrix Between Hourly #ETH Sentiment changes and ETH Price return", xlab = "ETH-USD", ylab = "#ETH")
abline(lm( ETH.log.change$ETHUSD ~ ETH.log.change$ETH), col = "red")
text(x = 0.05, y = -0.5, label = "r = -0.00724 ", col = "red")

##**Correlation Scatterplot Between #ETH Volume changes and ETH Price Return --------
plot(ETH.log.change$ETHUSD, ETH.log.change$Volume, pch = 19, 
     main = "Correlation Matrix Between Hourly #ETH Volume changes and ETH Price return", xlab = "ETH-USD", ylab = "#ETH Volume")
abline(lm( ETH.log.change$ETHUSD ~ ETH.log.change$Volume), col = "red")
text(x = 0.05, y = -0.3, label = "r = 0.00774", col = "red")

##**Plot the hourly price returns + volume --------------------------------
return.volume.plot<-ETH_sent.hourly
return.volume.plot$price.ret<-ETH_sent.diff$log.sent

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
  labs(title = "Hourly Logarithmic price returns and volume of #ETH",
       subtitle = "Using VADER appproach")

##**Tweets volume - Price returns - Price of ETH --------------------------
p1 <- ggplot(ETH_sent.hourly,aes(x=date))+
  geom_bar(aes(y=volume), stat="identity", size=.1, color="black", alpha=.4)+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(x="Hours",y="Tweets Volume")+
  labs(title = "Hourly Tweets Volume of #ETH")

p2 <- ggplot(ETH.hourly.log.ret, aes(x=Date)) + 
  geom_line(aes(y=Close)) + 
  theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Hours", y = "Price ($)")+
  labs(title = "Hourly Price of ETH-USD", 
       subtitle =paste0(format(min(as.Date(BTC.hourly.log.ret$Date)), "%d %B %Y"), " to ", format(max(as.Date(BTC.hourly.log.ret$Date)),"%d %B %Y")))

p3 <- ggplot(ETH.hourly.log.ret,aes(x=Date)) + 
  geom_line(aes(y=ETH.log.ret)) +
  geom_hline(yintercept = 0,col="red") +
  theme_bw() +
  labs(x="Hours",y="Price Returns (%)")+
  labs(title = "Hourly Logarithmic Price Returns of ETH-USD",caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3), size = "last"))

##**Daily Sentiment score - Tweets volume - Close Prices of ETH -----------
ETH.price.daily<- subset(data.ETH,index(data.ETH) >= "2021-05-23" &
                           index(data.ETH) <= "2021-05-31")
ETH.price.daily<-ETH.price.daily[,6]
ETH.price.daily$ret<-diff(log(ETH.price.daily$`ETH-USD.Adjusted`))
ETH.price.daily[1,2]<-0

p1 <- ggplot(ETH_sent.daily.tot,aes(x=date))+
  geom_line(aes(y=Daily.sent), stat="identity", size=.1, color="black", alpha=.4)+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(x="Days",y="Daily Sentiment Score")+
  labs(title = "Daily Sentiment Scores of #ETH")

p2 <- ggplot(ETH_sent.daily.tot, aes(x=date)) + 
  geom_line(aes(y=Daily.vol)) + 
  theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Days", y = "Tweets Volume")+
  labs(title = "Daily Volume of #ETH Tweets")

p3 <- ggplot(ETH.price.daily,aes(x=index(ETH.price.daily))) + 
  geom_line(aes(y=ETH.USD.Adjusted)) +
  theme_bw() +
  labs(x="Days",y="Prices ($)", 
       subtitle =paste0(format(min(as.Date(BTC_sent.daily.tot$date)), "%d %B %Y"), " to ", format(max(as.Date(BTC_sent.daily.tot$date)),"%d %B %Y")))+
  labs(title = "Daily Prices of ETH-USD",caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3), size = "last"))

##**CORRELATION MATRIX: VOLUME, SENTIMENT, PRICE --------------------------
#Hourly data
hourly.table.corr.mat <- as.data.frame(cbind(hourly.table$sent.idx,hourly.table$vol.idx,hourly.table$close.idx))
colnames(hourly.table.corr.mat)<-paste(c("Sent","Vol","Close Price"))
corrplot(cor(hourly.table.corr.mat), method = "number", title = "Hourly Correlation Matrix #ETH", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

#Daily data
daily.table.corr.mat <- as.data.frame(cbind(daily.table$sent.idx,daily.table$vol.idx,daily.table$close.idx))
colnames(daily.table.corr.mat)<-paste(c("Sent","Vol","Close Price"))
corrplot(cor(daily.table.corr.mat), method = "number", title = "Daily Correlation Matrix #ETH", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

##**SMA + TRENDLINE during the period 2021-05-23 -> 2021-05-31 ------------------------
ggplot(ETH.sma.3,aes(x=Date))+
  geom_line(aes(y=Close))+
  theme_bw()+
  labs(x="Date",y="Price ($)",
       title = "ETH-USD Simple Moving Average",
       subtitle = "May 23, 2021 - May 31, 2021")+
  geom_line(aes(y=predict(lm(Close~index(ETH.sma.3)))),col="red")+ #trendline
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
                   max.lag = 50, alpha = 0.05, lambda = 2.576, plot = TRUE,
                   table = TRUE, var.names = NULL)

#Daily
daily.cc<-cc.test(daily.table_ts[, c("Daily.sent")], daily.table_ts[, c("Daily.log.ret")], 
                  max.lag =5, alpha = 0.05, lambda = 2.576, plot = TRUE,
                  table = TRUE, var.names = NULL)