#RIPPLE (XRP) 26 MAY 20H - 31 MAY 13H 2021
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
data.XRP<-getSymbols("XRP-USD",from="2018-05-15",to="2021-05-31",auto.assign = FALSE)

XRPUSD_1hr<-read_csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Hourly Data/XRPUSD_1hr_woline1.csv")
XRPUSD_1hr<-subset(XRPUSD_1hr,XRPUSD_1hr$date>="2016-05-31" &
                     XRPUSD_1hr$date<="2021-06-01")

# *S2: DAILY PRICE RETURNS 5 YEARS -------------------------------------------------------
#DAILY LOGARITHMIC TOTAL RETURNS
XRP.log.ret <- data.XRP[,6]
XRP.log.ret$XRP.log.ret <- diff(log(XRP.log.ret$`XRP-USD.Adjusted`))
options(digits = 3)
XRP.log.ret <- XRP.log.ret[,2]
XRP.log.ret[1,1]<-0
XRP.log.ret$Gross.ret<-XRP.log.ret$XRP.log.ret+1
XRP.log.ret$Gross.ret[1]<-1
XRP.log.ret$cum.ret<-cumprod(XRP.log.ret$Gross.ret)

#CUMULATED DAILY RETURN AFTER 5 YEARS
XRP.logcumret <- sum(XRP.log.ret$XRP.log.ret,na.rm=TRUE)
XRP.cumret <- exp(XRP.logcumret)-1
XRP.cumret #0.482

#Mean returns, Std-Dev, Min, Max, Skewness, Kurtosis
XRP.mean<-mean(XRP.log.ret$XRP.log.ret,na.rm=TRUE)
XRP.sd<-sd(XRP.log.ret$XRP.log.ret,na.rm=TRUE)
XRP.min<-min(XRP.log.ret$XRP.log.ret,na.rm=TRUE)
XRP.max<-max(XRP.log.ret$XRP.log.ret,na.rm=TRUE)
XRP.skew<-skewness(XRP.log.ret$XRP.log.ret,na.rm=TRUE)
XRP.kurt<-kurtosis(XRP.log.ret$XRP.log.ret,na.rm=TRUE)
XRP.summary.table<-cbind(XRP.cumret,XRP.mean,XRP.sd,XRP.min,XRP.max,XRP.skew,XRP.kurt)
rownames(XRP.summary.table)<-c("2018-2021")
XRP.summary.table

# *S3: SIMPLE MOVING AVERAGE & TREND ---------------------------------------------------

#Step 1: Create ETH.sma
XRP.sma <- data.XRP[,6]
XRP.sma[is.na(XRP.sma)] <- 0
colnames(XRP.sma)<-paste(c("Adjusted"))
#Step 2: Create 50-day and 100-day rolling average columns
XRP.sma$sma50 <- rollmeanr(XRP.sma$Adjusted, k =50)
XRP.sma$sma100 <- rollmeanr(XRP.sma$Adjusted, k = 100)
XRP.sma$no<-c(1:nrow(XRP.sma)) #This supports creation of Trendline

#**S4: DAILY EWMA VOLATILITY -----------------------------------------------------
#Step 1,2: Create squared logarithmic price return of ETH
XRP_squared <- data.XRP[,6]
XRP_squared$XRP.squared <- (diff(log(XRP_squared$`XRP-USD.Adjusted`)))^2
options(digits = 3)
XRP_squared <- XRP_squared[,2]
XRP_squared[1,1]<-0

XRP_squared2 <- as.numeric(XRP_squared) # Squared returns as a numeric vector

#Step 3: Create EWMA function
ewma <- function(XRP_squared2, lambda){
  stats::filter(XRP_squared2*(1-lambda), lambda, "recursive", init = XRP_squared2[1])
}

# Step 4: Calculate variance with lambda=0.94
XRP_var_ewma94 <- ewma(XRP_squared2, 0.94)

# Step 5: Transform back to a zoo object (time index from object ETH_squared)
XRP_var_ewma <- zoo(XRP_var_ewma94,index(XRP_squared))

#Step 6: Volatility
XRP_vola_ewma <- sqrt(XRP_var_ewma)
# *S5: ALPHA & BETA USING CAPM, MARKET MODEL, AND ROLLING WINDOW REGRESSION --------
#Knowledge from Ang 2015, Chapter 5
#CAPM
#Step 0: Construct a monthly returns of Crypto
data.XRP[c(1:3,nrow(data.XRP)),]

XRP.monthly<-to.monthly(data.XRP)
XRP.monthly[c(1:3,nrow(XRP.monthly)),]

XRP.monthly<-XRP.monthly[,6]

XRP.ret<-Delt(XRP.monthly$data.XRP.Adjusted)
names(XRP.ret)<-paste("XRP.ret")
XRP.ret<-XRP.ret[-1,]
XRP.ret[c(1:3,nrow(XRP.ret)),]

csv.XRP<-cbind(data.frame(index(XRP.ret)),data.frame(XRP.ret))
names(csv.XRP)[1]<-paste("date")
rownames(csv.XRP)<-seq(1,nrow(csv.XRP),by=1)
csv.XRP[c(1:3,nrow(csv.XRP)),]

write.csv(csv.XRP,"XRP Returns (Monthly).csv") #Now, we have the csv file for the next steps.

#Step 1: Import Portfolio Returns and Convert to a data.frame Object
XRP<-read.csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Others/XRP Returns (Monthly).csv", header = TRUE)
XRP[c(1:3,nrow(XRP)),]
XRP$date<-as.yearmon(as.character(XRP$date),"%b %Y")
XRP[c(1:3,nrow(XRP)),]
XRP.df<-data.frame(XRP)
XRP.df[c(1:3,nrow(XRP.df)),]

#Step 2: Import S&P 500 Index Data from Yahoo Finance and Calculate Monthly Market Returns
data.mkt<-getSymbols("^GSPC",from="2018-05-15", to="2021-05-31",auto.assign = FALSE)
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
rf<-subset(rf,rf$date >= "2018-05-15" &
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
combo.XRP<-cbind(market.df,data.frame(rf.monthly),XRP.df$XRP.ret)
combo.XRP[c(1:3,nrow(combo.XRP)),]
names(combo.XRP)<-paste(c("mkt.ret","rf","XRP.ret"))
combo.XRP[c(1:3,nrow(combo.XRP)),]

#Step 5: Calculate Excess Return and Excess Market Return
#Excess Return (exret) = port.ret - rf
#Excess Market Return (exmkt) = mkt.ret - rf
combo.XRP$exret<-combo.XRP$XRP.ret -combo.XRP$rf
combo.XRP$exmkt<-combo.XRP$mkt.ret-combo.XRP$rf
combo.XRP[c(1:3,nrow(combo.XRP)),]

#Step 6: Run Regression of Excess Return on Excess Market Return
#We use "lm" command to calculate OLS Regression between exret & exmkt
options(digits = 3)
CAPM<-lm(combo.XRP$exret~combo.XRP$exmkt) #lm stands for "linear model", the function that creates a simple regression model
summary(CAPM)

#Rolling Window Regression
#In this section, we run regression through a rolling window, which calculate Alphas and Betas in different periods.
#=> Thus we can see how variable these two are, over a time period.

#Step 1: Import XRP and S&P 500 Index Data 
data.XRP[c(1:3,nrow(data.XRP)),]
data.mkt[c(1:3,nrow(data.mkt)),]

#Step 2: Calculate the XRP and Market Returns
rets<-diff(log(data.XRP$`XRP-USD.Adjusted`))
rets$GSPC<-diff(log(data.mkt$GSPC.Adjusted))
names(rets)[1]<-"XRP"
rets<-rets[-1,]
rets[c(1:3,nrow(rets)),]

#Step 3: Create the Rolling Window Regression Function
require(zoo)
coeffs<-rollapply(rets,
                  width = 252,
                  FUN = function(X)
                  {
                    roll.reg=lm(XRP~GSPC,
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
##**Daily Price movement of XRP 2018-2021 ---------------------------------
ggplot(data.XRP,aes(x=index(data.XRP)))+
  geom_line(aes(y=data.XRP$`XRP-USD.Adjusted`))+
  labs(x = "Date", y = "Value ($)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + # centers headline
  labs(title = "Value of $1 Investment in Ripple 
       May 15, 2018 - May 31, 2021",
       caption = "Data retrieved from Yahoo! Finance")

##**Hourly Price movement of XRP 2016-2021 (the same with daily price movement) --------
ggplot(XRPUSD_1hr, aes(date,group=1)) + geom_line(aes(y=close)) + theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Date", y = "Price ($)")+
  labs(title = " XRP Hourly Price", 
       subtitle =paste0(format(min(as.Date(XRPUSD_1hr$date)), "%d %B %Y"), " to ", format(max(as.Date(XRPUSD_1hr$date)),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

##**Daily Log return volatility 2018-2021 ---------------------------------
XRP.log.ret<-as.data.frame(XRP.log.ret)
XRP.volatility<-ggplot(XRP.log.ret,aes(x=index(XRP.log.ret)))+
  geom_line(aes(y=XRP.log.ret))+
  labs(x = "Date", y = "Value of Investment ($)") +
  labs(title = "Volatility of XRP price
May 15, 2018 - May 31, 2021")+
  theme_bw()# white background
XRP.volatility<-XRP.volatility+geom_hline(yintercept = 0,col="red")
XRP.volatility
##**SMA + Trendline ---------------------------------------------------
ggplot(XRP.sma,aes(x=index(XRP.sma)))+
  geom_line(aes(y=Adjusted))+
  theme_bw()+
  labs(x="Date",y="Price ($)",
       title = "XRP-USD Simple Moving Average",
       subtitle = "May 15, 2018 - May 31, 2021")+
  geom_line(aes(y=predict(lm(Adjusted~no))),col="red")+ #trendline
  geom_line(aes(y=sma50), col="green",alpha=.4,size=.1)+
  geom_line(aes(y=sma100),col="red",alpha=.4,size=.1)

##**DAILY EWMA --------------------------------------------------------------
plot(XRP_var_ewma, main="XRP - Daily Variance with EWMA, lambda=0.94", xlab="Date", ylab="Variance", col="blue", type="l")
plot(XRP_vola_ewma,  main="XRP - Daily Volatility with EWMA, lambda=0.94", xlab="Date", ylab="Volatility", col="blue", type="l")

##**ALPHA-BETA --------------------------------------------------------------
p1 <- ggplot(coeffs, aes(index(coeffs), coeffs$Alpha)) + geom_line() + theme_minimal() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.text.y = element_text(angle = 90))+
  labs(x = "Date", y = "Alpha") +
  labs(title = "Ripple Alpha and Beta
Using Rolling 252-Day Windowns and
Daily Returns From 2018 to 2021")
p2 <- ggplot(coeffs, aes(index(coeffs), coeffs$Beta)) + geom_line() + theme_minimal() + 
  theme(axis.text.x = element_text())+
  geom_hline(yintercept = 0,col="red")+
  labs(x = "Date", y = "Beta")
library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

# PART II: SENTIMENT ANALYSIS USING VADER ---------------------------------
# *S1: GET TWEETS DATASET --------------------------------------------------
XRP_twt<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/Tweets data/XRP_twt.csv",header = TRUE)

# *S2: Clean Tweets ------------------------------------------------------
tweets.XRP<- XRP_twt  %>% dplyr::select(created_at, screen_name,text) #Create a table with "created_at", "screen_name", and "text" columns from BTC_twt
tweets.XRP[c(1:3,nrow(tweets.XRP)),]

tweets.XRP$stripped_text1<-gsub("https(.*)*$","",tweets.XRP$text) #Remove URLs
tweets.XRP$stripped_text1<-tolower(tweets.XRP$stripped_text1) #To lowercase
tweets.XRP$stripped_text1<-gsub("\\.\\.","",tweets.XRP$stripped_text1) #replace ..... with .
tweets.XRP$stripped_text1<-gsub("(.)\\1{2,}", "\\1",tweets.XRP$stripped_text1) #Transform "Goooooo" to "go"
tweets.XRP$stripped_text1<-gsub("([[:punct:]])\\1+", "\\1", tweets.XRP$stripped_text1) #Reduce !!!!! to !
tweets.XRP$stripped_text1<-gsub("#","", tweets.XRP$stripped_text1) #Remove hashtags
tweets.XRP$stripped_text1<-gsub("@[[:alnum:]]+","", tweets.XRP$stripped_text1) #Remove mentions
tweets.XRP$stripped_text1<-gsub("&amp","and", tweets.XRP$stripped_text1) #Replace &apm with and
tweets.XRP$stripped_text1<-gsub("<(.*)>","", tweets.XRP$stripped_text1) #Remove unicodes <U+...>
tweets.XRP$stripped_text1<-iconv(tweets.XRP$stripped_text1, "latin1", "ASCII", sub="") #remove weird letters
tweets.XRP$stripped_text1<- gsub("%%", "\'", tweets.XRP$stripped_text1) # Changing %% back to apostrophes
tweets.XRP$stripped_text1<-str_squish(tweets.XRP$stripped_text1) #Remove excessive spaces

# *S3: Run and save VADER analysis -----------------------------------------------------------
vader_xrp = vader_df(tweets.XRP$stripped_text1)
vader_xrp[c(1:3,nrow(vader_xrp)),]
write_as_csv(vader_xrp, "vader_xrp.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(vader_xrp, "vader_xrp.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

#**S3 bis: Load sentiment data ---------------------------------------------
vader_xrp<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/vader_xrp.csv",header = TRUE)
XRP_sent<-vader_xrp[,c(3:4)]
date<-tweets.XRP$created_at
XRP_sent$date<-date

# *S4: AVERAGE SENTIMENT SCORE OVER THE PERIOD -----------------------------
XRP_sent[c(1:3,nrow(XRP_sent)),]
XRP_sent.avg<-mean(XRP_sent$compound,na.rm=TRUE)
XRP_sent.avg #0.169

#**S5: AVERAGE SENTIMENT SCORE PER HOUR ----------------------------------------

XRP_sent.1<-subset(XRP_sent,XRP_sent$date >= "2021-05-26 20:00:00" &
                     XRP_sent$date <= "2021-05-26 20:59:59")
XRP_sent.1$index<-seq(1,nrow(XRP_sent.1),by=1)
XRP_sent.1.sent<-mean(XRP_sent.1$compound,na.rm = TRUE)
XRP_sent.1.sent<-cbind(XRP_sent.1.sent,XRP_sent.1[nrow(XRP_sent.1),4])
colnames(XRP_sent.1.sent)<-paste(c("avg.sent","volume"))
XRP_sent.1.sent

XRP_sent.2<-subset(XRP_sent,XRP_sent$date >= "2021-05-26 21:00:00" &
                     XRP_sent$date <= "2021-05-26 21:59:59")
XRP_sent.2$index<-seq(1,nrow(XRP_sent.2),by=1)
XRP_sent.2.sent<-mean(XRP_sent.2$compound,na.rm = TRUE)
XRP_sent.2.sent<-cbind(XRP_sent.2.sent,XRP_sent.2[nrow(XRP_sent.2),4])
colnames(XRP_sent.2.sent)<-paste(c("avg.sent","volume"))
XRP_sent.2.sent

XRP_sent.3<-subset(XRP_sent,XRP_sent$date >= "2021-05-26 22:00:00" &
                     XRP_sent$date <= "2021-05-26 22:59:59")
XRP_sent.3$index<-seq(1,nrow(XRP_sent.3),by=1)
XRP_sent.3.sent<-mean(XRP_sent.3$compound,na.rm = TRUE)
XRP_sent.3.sent<-cbind(XRP_sent.3.sent,XRP_sent.3[nrow(XRP_sent.3),4])
colnames(XRP_sent.3.sent)<-paste(c("avg.sent","volume"))
XRP_sent.3.sent

XRP_sent.4<-subset(XRP_sent,XRP_sent$date >= "2021-05-26 23:00:00" &
                     XRP_sent$date <= "2021-05-26 23:59:59")
XRP_sent.4$index<-seq(1,nrow(XRP_sent.4),by=1)
XRP_sent.4.sent<-mean(XRP_sent.4$compound,na.rm = TRUE)
XRP_sent.4.sent<-cbind(XRP_sent.4.sent,XRP_sent.4[nrow(XRP_sent.4),4])
colnames(XRP_sent.4.sent)<-paste(c("avg.sent","volume"))
XRP_sent.4.sent

XRP_sent.5<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 00:00:00" &
                     XRP_sent$date <= "2021-05-27 00:59:59")
XRP_sent.5$index<-seq(1,nrow(XRP_sent.5),by=1)
XRP_sent.5.sent<-mean(XRP_sent.5$compound,na.rm = TRUE)
XRP_sent.5.sent<-cbind(XRP_sent.5.sent,XRP_sent.5[nrow(XRP_sent.5),4])
colnames(XRP_sent.5.sent)<-paste(c("avg.sent","volume"))
XRP_sent.5.sent

XRP_sent.6<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 01:00:00" &
                     XRP_sent$date <= "2021-05-27 01:59:59")
XRP_sent.6$index<-seq(1,nrow(XRP_sent.6),by=1)
XRP_sent.6.sent<-mean(XRP_sent.6$compound,na.rm = TRUE)
XRP_sent.6.sent<-cbind(XRP_sent.6.sent,XRP_sent.6[nrow(XRP_sent.6),4])
colnames(XRP_sent.6.sent)<-paste(c("avg.sent","volume"))
XRP_sent.6.sent

XRP_sent.7<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 02:00:00" &
                     XRP_sent$date <= "2021-05-27 02:59:59")
XRP_sent.7$index<-seq(1,nrow(XRP_sent.7),by=1)
XRP_sent.7.sent<-mean(XRP_sent.7$compound,na.rm = TRUE)
XRP_sent.7.sent<-cbind(XRP_sent.7.sent,XRP_sent.7[nrow(XRP_sent.7),4])
colnames(XRP_sent.7.sent)<-paste(c("avg.sent","volume"))
XRP_sent.7.sent

XRP_sent.8<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 03:00:00" &
                     XRP_sent$date <= "2021-05-27 03:59:59")
XRP_sent.8$index<-seq(1,nrow(XRP_sent.8),by=1)
XRP_sent.8.sent<-mean(XRP_sent.8$compound,na.rm = TRUE)
XRP_sent.8.sent<-cbind(XRP_sent.8.sent,XRP_sent.8[nrow(XRP_sent.8),4])
colnames(XRP_sent.8.sent)<-paste(c("avg.sent","volume"))
XRP_sent.8.sent

XRP_sent.9<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 04:00:00" &
                     XRP_sent$date <= "2021-05-27 04:59:59")
XRP_sent.9$index<-seq(1,nrow(XRP_sent.9),by=1)
XRP_sent.9.sent<-mean(XRP_sent.9$compound,na.rm = TRUE)
XRP_sent.9.sent<-cbind(XRP_sent.9.sent,XRP_sent.9[nrow(XRP_sent.9),4])
colnames(XRP_sent.9.sent)<-paste(c("avg.sent","volume"))
XRP_sent.9.sent

XRP_sent.10<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 05:00:00" &
                     XRP_sent$date <= "2021-05-27 05:59:59")
XRP_sent.10$index<-seq(1,nrow(XRP_sent.10),by=1)
XRP_sent.10.sent<-mean(XRP_sent.10$compound,na.rm = TRUE)
XRP_sent.10.sent<-cbind(XRP_sent.10.sent,XRP_sent.10[nrow(XRP_sent.10),4])
colnames(XRP_sent.10.sent)<-paste(c("avg.sent","volume"))
XRP_sent.10.sent

XRP_sent.11<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 06:00:00" &
                      XRP_sent$date <= "2021-05-27 06:59:59")
XRP_sent.11$index<-seq(1,nrow(XRP_sent.11),by=1)
XRP_sent.11.sent<-mean(XRP_sent.11$compound,na.rm = TRUE)
XRP_sent.11.sent<-cbind(XRP_sent.11.sent,XRP_sent.11[nrow(XRP_sent.11),4])
colnames(XRP_sent.11.sent)<-paste(c("avg.sent","volume"))
XRP_sent.11.sent

XRP_sent.12<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 07:00:00" &
                      XRP_sent$date <= "2021-05-27 07:59:59")
XRP_sent.12$index<-seq(1,nrow(XRP_sent.12),by=1)
XRP_sent.12.sent<-mean(XRP_sent.12$compound,na.rm = TRUE)
XRP_sent.12.sent<-cbind(XRP_sent.12.sent,XRP_sent.12[nrow(XRP_sent.12),4])
colnames(XRP_sent.12.sent)<-paste(c("avg.sent","volume"))
XRP_sent.12.sent

XRP_sent.13<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 08:00:00" &
                      XRP_sent$date <= "2021-05-27 08:59:59")
XRP_sent.13$index<-seq(1,nrow(XRP_sent.13),by=1)
XRP_sent.13.sent<-mean(XRP_sent.13$compound,na.rm = TRUE)
XRP_sent.13.sent<-cbind(XRP_sent.13.sent,XRP_sent.13[nrow(XRP_sent.13),4])
colnames(XRP_sent.13.sent)<-paste(c("avg.sent","volume"))
XRP_sent.13.sent

XRP_sent.14<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 09:00:00" &
                      XRP_sent$date <= "2021-05-27 09:59:59")
XRP_sent.14$index<-seq(1,nrow(XRP_sent.14),by=1)
XRP_sent.14.sent<-mean(XRP_sent.14$compound,na.rm = TRUE)
XRP_sent.14.sent<-cbind(XRP_sent.14.sent,XRP_sent.14[nrow(XRP_sent.14),4])
colnames(XRP_sent.14.sent)<-paste(c("avg.sent","volume"))
XRP_sent.14.sent

XRP_sent.15<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 10:00:00" &
                      XRP_sent$date <= "2021-05-27 10:59:59")
XRP_sent.15$index<-seq(1,nrow(XRP_sent.15),by=1)
XRP_sent.15.sent<-mean(XRP_sent.15$compound,na.rm = TRUE)
XRP_sent.15.sent<-cbind(XRP_sent.15.sent,XRP_sent.15[nrow(XRP_sent.15),4])
colnames(XRP_sent.15.sent)<-paste(c("avg.sent","volume"))
XRP_sent.15.sent

XRP_sent.16<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 11:00:00" &
                      XRP_sent$date <= "2021-05-27 11:59:59")
XRP_sent.16$index<-seq(1,nrow(XRP_sent.16),by=1)
XRP_sent.16.sent<-mean(XRP_sent.16$compound,na.rm = TRUE)
XRP_sent.16.sent<-cbind(XRP_sent.16.sent,XRP_sent.16[nrow(XRP_sent.16),4])
colnames(XRP_sent.16.sent)<-paste(c("avg.sent","volume"))
XRP_sent.16.sent

XRP_sent.17<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 12:00:00" &
                      XRP_sent$date <= "2021-05-27 12:59:59")
XRP_sent.17$index<-seq(1,nrow(XRP_sent.17),by=1)
XRP_sent.17.sent<-mean(XRP_sent.17$compound,na.rm = TRUE)
XRP_sent.17.sent<-cbind(XRP_sent.17.sent,XRP_sent.17[nrow(XRP_sent.17),4])
colnames(XRP_sent.17.sent)<-paste(c("avg.sent","volume"))
XRP_sent.17.sent

XRP_sent.18<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 13:00:00" &
                      XRP_sent$date <= "2021-05-27 13:59:59")
XRP_sent.18$index<-seq(1,nrow(XRP_sent.18),by=1)
XRP_sent.18.sent<-mean(XRP_sent.18$compound,na.rm = TRUE)
XRP_sent.18.sent<-cbind(XRP_sent.18.sent,XRP_sent.18[nrow(XRP_sent.18),4])
colnames(XRP_sent.18.sent)<-paste(c("avg.sent","volume"))
XRP_sent.18.sent

XRP_sent.19<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 14:00:00" &
                      XRP_sent$date <= "2021-05-27 14:59:59")
XRP_sent.19$index<-seq(1,nrow(XRP_sent.19),by=1)
XRP_sent.19.sent<-mean(XRP_sent.19$compound,na.rm = TRUE)
XRP_sent.19.sent<-cbind(XRP_sent.19.sent,XRP_sent.19[nrow(XRP_sent.19),4])
colnames(XRP_sent.19.sent)<-paste(c("avg.sent","volume"))
XRP_sent.19.sent

XRP_sent.20<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 15:00:00" &
                      XRP_sent$date <= "2021-05-27 15:59:59")
XRP_sent.20$index<-seq(1,nrow(XRP_sent.20),by=1)
XRP_sent.20.sent<-mean(XRP_sent.20$compound,na.rm = TRUE)
XRP_sent.20.sent<-cbind(XRP_sent.20.sent,XRP_sent.20[nrow(XRP_sent.20),4])
colnames(XRP_sent.20.sent)<-paste(c("avg.sent","volume"))
XRP_sent.20.sent

XRP_sent.21<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 16:00:00" &
                      XRP_sent$date <= "2021-05-27 16:59:59")
XRP_sent.21$index<-seq(1,nrow(XRP_sent.21),by=1)
XRP_sent.21.sent<-mean(XRP_sent.21$compound,na.rm = TRUE)
XRP_sent.21.sent<-cbind(XRP_sent.21.sent,XRP_sent.21[nrow(XRP_sent.21),4])
colnames(XRP_sent.21.sent)<-paste(c("avg.sent","volume"))
XRP_sent.21.sent

XRP_sent.22<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 17:00:00" &
                      XRP_sent$date <= "2021-05-27 17:59:59")
XRP_sent.22$index<-seq(1,nrow(XRP_sent.22),by=1)
XRP_sent.22.sent<-mean(XRP_sent.22$compound,na.rm = TRUE)
XRP_sent.22.sent<-cbind(XRP_sent.22.sent,XRP_sent.22[nrow(XRP_sent.22),4])
colnames(XRP_sent.22.sent)<-paste(c("avg.sent","volume"))
XRP_sent.22.sent

XRP_sent.23<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 18:00:00" &
                      XRP_sent$date <= "2021-05-27 18:59:59")
XRP_sent.23$index<-seq(1,nrow(XRP_sent.23),by=1)
XRP_sent.23.sent<-mean(XRP_sent.23$compound,na.rm = TRUE)
XRP_sent.23.sent<-cbind(XRP_sent.23.sent,XRP_sent.23[nrow(XRP_sent.23),4])
colnames(XRP_sent.23.sent)<-paste(c("avg.sent","volume"))
XRP_sent.23.sent

XRP_sent.24<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 19:00:00" &
                      XRP_sent$date <= "2021-05-27 19:59:59")
XRP_sent.24$index<-seq(1,nrow(XRP_sent.24),by=1)
XRP_sent.24.sent<-mean(XRP_sent.24$compound,na.rm = TRUE)
XRP_sent.24.sent<-cbind(XRP_sent.24.sent,XRP_sent.24[nrow(XRP_sent.24),4])
colnames(XRP_sent.24.sent)<-paste(c("avg.sent","volume"))
XRP_sent.24.sent

XRP_sent.25<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 20:00:00" &
                      XRP_sent$date <= "2021-05-27 20:59:59")
XRP_sent.25$index<-seq(1,nrow(XRP_sent.25),by=1)
XRP_sent.25.sent<-mean(XRP_sent.25$compound,na.rm = TRUE)
XRP_sent.25.sent<-cbind(XRP_sent.25.sent,XRP_sent.25[nrow(XRP_sent.25),4])
colnames(XRP_sent.25.sent)<-paste(c("avg.sent","volume"))
XRP_sent.25.sent

XRP_sent.26<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 21:00:00" &
                      XRP_sent$date <= "2021-05-27 21:59:59")
XRP_sent.26$index<-seq(1,nrow(XRP_sent.26),by=1)
XRP_sent.26.sent<-mean(XRP_sent.26$compound,na.rm = TRUE)
XRP_sent.26.sent<-cbind(XRP_sent.26.sent,XRP_sent.26[nrow(XRP_sent.26),4])
colnames(XRP_sent.26.sent)<-paste(c("avg.sent","volume"))
XRP_sent.26.sent

XRP_sent.27<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 22:00:00" &
                      XRP_sent$date <= "2021-05-27 22:59:59")
XRP_sent.27$index<-seq(1,nrow(XRP_sent.27),by=1)
XRP_sent.27.sent<-mean(XRP_sent.27$compound,na.rm = TRUE)
XRP_sent.27.sent<-cbind(XRP_sent.27.sent,XRP_sent.27[nrow(XRP_sent.27),4])
colnames(XRP_sent.27.sent)<-paste(c("avg.sent","volume"))
XRP_sent.27.sent

XRP_sent.28<-subset(XRP_sent,XRP_sent$date >= "2021-05-27 23:00:00" &
                      XRP_sent$date <= "2021-05-27 23:59:59")
XRP_sent.28$index<-seq(1,nrow(XRP_sent.28),by=1)
XRP_sent.28.sent<-mean(XRP_sent.28$compound,na.rm = TRUE)
XRP_sent.28.sent<-cbind(XRP_sent.28.sent,XRP_sent.28[nrow(XRP_sent.28),4])
colnames(XRP_sent.28.sent)<-paste(c("avg.sent","volume"))
XRP_sent.28.sent

XRP_sent.29<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 00:00:00" &
                      XRP_sent$date <= "2021-05-28 00:59:59")
XRP_sent.29$index<-seq(1,nrow(XRP_sent.29),by=1)
XRP_sent.29.sent<-mean(XRP_sent.29$compound,na.rm = TRUE)
XRP_sent.29.sent<-cbind(XRP_sent.29.sent,XRP_sent.29[nrow(XRP_sent.29),4])
colnames(XRP_sent.29.sent)<-paste(c("avg.sent","volume"))
XRP_sent.29.sent

XRP_sent.30<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 01:00:00" &
                      XRP_sent$date <= "2021-05-28 01:59:59")
XRP_sent.30$index<-seq(1,nrow(XRP_sent.30),by=1)
XRP_sent.30.sent<-mean(XRP_sent.30$compound,na.rm = TRUE)
XRP_sent.30.sent<-cbind(XRP_sent.30.sent,XRP_sent.30[nrow(XRP_sent.30),4])
colnames(XRP_sent.30.sent)<-paste(c("avg.sent","volume"))
XRP_sent.30.sent

XRP_sent.31<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 02:00:00" &
                      XRP_sent$date <= "2021-05-28 02:59:59")
XRP_sent.31$index<-seq(1,nrow(XRP_sent.31),by=1)
XRP_sent.31.sent<-mean(XRP_sent.31$compound,na.rm = TRUE)
XRP_sent.31.sent<-cbind(XRP_sent.31.sent,XRP_sent.31[nrow(XRP_sent.31),4])
colnames(XRP_sent.31.sent)<-paste(c("avg.sent","volume"))
XRP_sent.31.sent

XRP_sent.32<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 03:00:00" &
                      XRP_sent$date <= "2021-05-28 03:59:59")
XRP_sent.32$index<-seq(1,nrow(XRP_sent.32),by=1)
XRP_sent.32.sent<-mean(XRP_sent.32$compound,na.rm = TRUE)
XRP_sent.32.sent<-cbind(XRP_sent.32.sent,XRP_sent.32[nrow(XRP_sent.32),4])
colnames(XRP_sent.32.sent)<-paste(c("avg.sent","volume"))
XRP_sent.32.sent

XRP_sent.33<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 04:00:00" &
                      XRP_sent$date <= "2021-05-28 04:59:59")
XRP_sent.33$index<-seq(1,nrow(XRP_sent.33),by=1)
XRP_sent.33.sent<-mean(XRP_sent.33$compound,na.rm = TRUE)
XRP_sent.33.sent<-cbind(XRP_sent.33.sent,XRP_sent.33[nrow(XRP_sent.33),4])
colnames(XRP_sent.33.sent)<-paste(c("avg.sent","volume"))
XRP_sent.33.sent

XRP_sent.34<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 05:00:00" &
                      XRP_sent$date <= "2021-05-28 05:59:59")
XRP_sent.34$index<-seq(1,nrow(XRP_sent.34),by=1)
XRP_sent.34.sent<-mean(XRP_sent.34$compound,na.rm = TRUE)
XRP_sent.34.sent<-cbind(XRP_sent.34.sent,XRP_sent.34[nrow(XRP_sent.34),4])
colnames(XRP_sent.34.sent)<-paste(c("avg.sent","volume"))
XRP_sent.34.sent

XRP_sent.35<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 06:00:00" &
                      XRP_sent$date <= "2021-05-28 06:59:59")
XRP_sent.35$index<-seq(1,nrow(XRP_sent.35),by=1)
XRP_sent.35.sent<-mean(XRP_sent.35$compound,na.rm = TRUE)
XRP_sent.35.sent<-cbind(XRP_sent.35.sent,XRP_sent.35[nrow(XRP_sent.35),4])
colnames(XRP_sent.35.sent)<-paste(c("avg.sent","volume"))
XRP_sent.35.sent

XRP_sent.36<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 07:00:00" &
                      XRP_sent$date <= "2021-05-28 07:59:59")
XRP_sent.36$index<-seq(1,nrow(XRP_sent.36),by=1)
XRP_sent.36.sent<-mean(XRP_sent.36$compound,na.rm = TRUE)
XRP_sent.36.sent<-cbind(XRP_sent.36.sent,XRP_sent.36[nrow(XRP_sent.36),4])
colnames(XRP_sent.36.sent)<-paste(c("avg.sent","volume"))
XRP_sent.36.sent

XRP_sent.37<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 08:00:00" &
                      XRP_sent$date <= "2021-05-28 08:59:59")
XRP_sent.37$index<-seq(1,nrow(XRP_sent.37),by=1)
XRP_sent.37.sent<-mean(XRP_sent.37$compound,na.rm = TRUE)
XRP_sent.37.sent<-cbind(XRP_sent.37.sent,XRP_sent.37[nrow(XRP_sent.37),4])
colnames(XRP_sent.37.sent)<-paste(c("avg.sent","volume"))
XRP_sent.37.sent

XRP_sent.38<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 09:00:00" &
                      XRP_sent$date <= "2021-05-28 09:59:59")
XRP_sent.38$index<-seq(1,nrow(XRP_sent.38),by=1)
XRP_sent.38.sent<-mean(XRP_sent.38$compound,na.rm = TRUE)
XRP_sent.38.sent<-cbind(XRP_sent.38.sent,XRP_sent.38[nrow(XRP_sent.38),4])
colnames(XRP_sent.38.sent)<-paste(c("avg.sent","volume"))
XRP_sent.38.sent

XRP_sent.39<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 10:00:00" &
                      XRP_sent$date <= "2021-05-28 10:59:59")
XRP_sent.39$index<-seq(1,nrow(XRP_sent.39),by=1)
XRP_sent.39.sent<-mean(XRP_sent.39$compound,na.rm = TRUE)
XRP_sent.39.sent<-cbind(XRP_sent.39.sent,XRP_sent.39[nrow(XRP_sent.39),4])
colnames(XRP_sent.39.sent)<-paste(c("avg.sent","volume"))
XRP_sent.39.sent

XRP_sent.40<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 11:00:00" &
                      XRP_sent$date <= "2021-05-28 11:59:59")
XRP_sent.40$index<-seq(1,nrow(XRP_sent.40),by=1)
XRP_sent.40.sent<-mean(XRP_sent.40$compound,na.rm = TRUE)
XRP_sent.40.sent<-cbind(XRP_sent.40.sent,XRP_sent.40[nrow(XRP_sent.40),4])
colnames(XRP_sent.40.sent)<-paste(c("avg.sent","volume"))
XRP_sent.40.sent

XRP_sent.41<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 12:00:00" &
                      XRP_sent$date <= "2021-05-28 12:59:59")
XRP_sent.41$index<-seq(1,nrow(XRP_sent.41),by=1)
XRP_sent.41.sent<-mean(XRP_sent.41$compound,na.rm = TRUE)
XRP_sent.41.sent<-cbind(XRP_sent.41.sent,XRP_sent.41[nrow(XRP_sent.41),4])
colnames(XRP_sent.41.sent)<-paste(c("avg.sent","volume"))
XRP_sent.41.sent

XRP_sent.42<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 13:00:00" &
                      XRP_sent$date <= "2021-05-28 13:59:59")
XRP_sent.42$index<-seq(1,nrow(XRP_sent.42),by=1)
XRP_sent.42.sent<-mean(XRP_sent.42$compound,na.rm = TRUE)
XRP_sent.42.sent<-cbind(XRP_sent.42.sent,XRP_sent.42[nrow(XRP_sent.42),4])
colnames(XRP_sent.42.sent)<-paste(c("avg.sent","volume"))
XRP_sent.42.sent

XRP_sent.43<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 14:00:00" &
                      XRP_sent$date <= "2021-05-28 14:59:59")
XRP_sent.43$index<-seq(1,nrow(XRP_sent.43),by=1)
XRP_sent.43.sent<-mean(XRP_sent.43$compound,na.rm = TRUE)
XRP_sent.43.sent<-cbind(XRP_sent.43.sent,XRP_sent.43[nrow(XRP_sent.43),4])
colnames(XRP_sent.43.sent)<-paste(c("avg.sent","volume"))
XRP_sent.43.sent

XRP_sent.44<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 15:00:00" &
                      XRP_sent$date <= "2021-05-28 15:59:59")
XRP_sent.44$index<-seq(1,nrow(XRP_sent.44),by=1)
XRP_sent.44.sent<-mean(XRP_sent.44$compound,na.rm = TRUE)
XRP_sent.44.sent<-cbind(XRP_sent.44.sent,XRP_sent.44[nrow(XRP_sent.44),4])
colnames(XRP_sent.44.sent)<-paste(c("avg.sent","volume"))
XRP_sent.44.sent

XRP_sent.45<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 16:00:00" &
                      XRP_sent$date <= "2021-05-28 16:59:59")
XRP_sent.45$index<-seq(1,nrow(XRP_sent.45),by=1)
XRP_sent.45.sent<-mean(XRP_sent.45$compound,na.rm = TRUE)
XRP_sent.45.sent<-cbind(XRP_sent.45.sent,XRP_sent.45[nrow(XRP_sent.45),4])
colnames(XRP_sent.45.sent)<-paste(c("avg.sent","volume"))
XRP_sent.45.sent

XRP_sent.46<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 17:00:00" &
                      XRP_sent$date <= "2021-05-28 17:59:59")
XRP_sent.46$index<-seq(1,nrow(XRP_sent.46),by=1)
XRP_sent.46.sent<-mean(XRP_sent.46$compound,na.rm = TRUE)
XRP_sent.46.sent<-cbind(XRP_sent.46.sent,XRP_sent.46[nrow(XRP_sent.46),4])
colnames(XRP_sent.46.sent)<-paste(c("avg.sent","volume"))
XRP_sent.46.sent

XRP_sent.47<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 18:00:00" &
                      XRP_sent$date <= "2021-05-28 18:59:59")
XRP_sent.47$index<-seq(1,nrow(XRP_sent.47),by=1)
XRP_sent.47.sent<-mean(XRP_sent.47$compound,na.rm = TRUE)
XRP_sent.47.sent<-cbind(XRP_sent.47.sent,XRP_sent.47[nrow(XRP_sent.47),4])
colnames(XRP_sent.47.sent)<-paste(c("avg.sent","volume"))
XRP_sent.47.sent

XRP_sent.48<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 19:00:00" &
                      XRP_sent$date <= "2021-05-28 19:59:59")
XRP_sent.48$index<-seq(1,nrow(XRP_sent.48),by=1)
XRP_sent.48.sent<-mean(XRP_sent.48$compound,na.rm = TRUE)
XRP_sent.48.sent<-cbind(XRP_sent.48.sent,XRP_sent.48[nrow(XRP_sent.48),4])
colnames(XRP_sent.48.sent)<-paste(c("avg.sent","volume"))
XRP_sent.48.sent

XRP_sent.49<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 20:00:00" &
                      XRP_sent$date <= "2021-05-28 20:59:59")
XRP_sent.49$index<-seq(1,nrow(XRP_sent.49),by=1)
XRP_sent.49.sent<-mean(XRP_sent.49$compound,na.rm = TRUE)
XRP_sent.49.sent<-cbind(XRP_sent.49.sent,XRP_sent.49[nrow(XRP_sent.49),4])
colnames(XRP_sent.49.sent)<-paste(c("avg.sent","volume"))
XRP_sent.49.sent

XRP_sent.50<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 21:00:00" &
                      XRP_sent$date <= "2021-05-28 21:59:59")
XRP_sent.50$index<-seq(1,nrow(XRP_sent.50),by=1)
XRP_sent.50.sent<-mean(XRP_sent.50$compound,na.rm = TRUE)
XRP_sent.50.sent<-cbind(XRP_sent.50.sent,XRP_sent.50[nrow(XRP_sent.50),4])
colnames(XRP_sent.50.sent)<-paste(c("avg.sent","volume"))
XRP_sent.50.sent

XRP_sent.51<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 22:00:00" &
                      XRP_sent$date <= "2021-05-28 22:59:59")
XRP_sent.51$index<-seq(1,nrow(XRP_sent.51),by=1)
XRP_sent.51.sent<-mean(XRP_sent.51$compound,na.rm = TRUE)
XRP_sent.51.sent<-cbind(XRP_sent.51.sent,XRP_sent.51[nrow(XRP_sent.51),4])
colnames(XRP_sent.51.sent)<-paste(c("avg.sent","volume"))
XRP_sent.51.sent

XRP_sent.52<-subset(XRP_sent,XRP_sent$date >= "2021-05-28 23:00:00" &
                      XRP_sent$date <= "2021-05-28 23:59:59")
XRP_sent.52$index<-seq(1,nrow(XRP_sent.52),by=1)
XRP_sent.52.sent<-mean(XRP_sent.52$compound,na.rm = TRUE)
XRP_sent.52.sent<-cbind(XRP_sent.52.sent,XRP_sent.52[nrow(XRP_sent.52),4])
colnames(XRP_sent.52.sent)<-paste(c("avg.sent","volume"))
XRP_sent.52.sent

XRP_sent.53<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 00:00:00" &
                      XRP_sent$date <= "2021-05-29 00:59:59")
XRP_sent.53$index<-seq(1,nrow(XRP_sent.53),by=1)
XRP_sent.53.sent<-mean(XRP_sent.53$compound,na.rm = TRUE)
XRP_sent.53.sent<-cbind(XRP_sent.53.sent,XRP_sent.53[nrow(XRP_sent.53),4])
colnames(XRP_sent.53.sent)<-paste(c("avg.sent","volume"))
XRP_sent.53.sent

XRP_sent.54<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 01:00:00" &
                      XRP_sent$date <= "2021-05-29 01:59:59")
XRP_sent.54$index<-seq(1,nrow(XRP_sent.54),by=1)
XRP_sent.54.sent<-mean(XRP_sent.54$compound,na.rm = TRUE)
XRP_sent.54.sent<-cbind(XRP_sent.54.sent,XRP_sent.54[nrow(XRP_sent.54),4])
colnames(XRP_sent.54.sent)<-paste(c("avg.sent","volume"))
XRP_sent.54.sent

XRP_sent.55<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 02:00:00" &
                      XRP_sent$date <= "2021-05-29 02:59:59")
XRP_sent.55$index<-seq(1,nrow(XRP_sent.55),by=1)
XRP_sent.55.sent<-mean(XRP_sent.55$compound,na.rm = TRUE)
XRP_sent.55.sent<-cbind(XRP_sent.55.sent,XRP_sent.55[nrow(XRP_sent.55),4])
colnames(XRP_sent.55.sent)<-paste(c("avg.sent","volume"))
XRP_sent.55.sent

XRP_sent.56<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 03:00:00" &
                      XRP_sent$date <= "2021-05-29 03:59:59")
XRP_sent.56$index<-seq(1,nrow(XRP_sent.56),by=1)
XRP_sent.56.sent<-mean(XRP_sent.56$compound,na.rm = TRUE)
XRP_sent.56.sent<-cbind(XRP_sent.56.sent,XRP_sent.56[nrow(XRP_sent.56),4])
colnames(XRP_sent.56.sent)<-paste(c("avg.sent","volume"))
XRP_sent.56.sent

XRP_sent.57<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 04:00:00" &
                      XRP_sent$date <= "2021-05-29 04:59:59")
XRP_sent.57$index<-seq(1,nrow(XRP_sent.57),by=1)
XRP_sent.57.sent<-mean(XRP_sent.57$compound,na.rm = TRUE)
XRP_sent.57.sent<-cbind(XRP_sent.57.sent,XRP_sent.57[nrow(XRP_sent.57),4])
colnames(XRP_sent.57.sent)<-paste(c("avg.sent","volume"))
XRP_sent.57.sent

XRP_sent.58<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 05:00:00" &
                      XRP_sent$date <= "2021-05-29 05:59:59")
XRP_sent.58$index<-seq(1,nrow(XRP_sent.58),by=1)
XRP_sent.58.sent<-mean(XRP_sent.58$compound,na.rm = TRUE)
XRP_sent.58.sent<-cbind(XRP_sent.58.sent,XRP_sent.58[nrow(XRP_sent.58),4])
colnames(XRP_sent.58.sent)<-paste(c("avg.sent","volume"))
XRP_sent.58.sent

XRP_sent.59<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 06:00:00" &
                      XRP_sent$date <= "2021-05-29 06:59:59")
XRP_sent.59$index<-seq(1,nrow(XRP_sent.59),by=1)
XRP_sent.59.sent<-mean(XRP_sent.59$compound,na.rm = TRUE)
XRP_sent.59.sent<-cbind(XRP_sent.59.sent,XRP_sent.59[nrow(XRP_sent.59),4])
colnames(XRP_sent.59.sent)<-paste(c("avg.sent","volume"))
XRP_sent.59.sent

XRP_sent.60<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 07:00:00" &
                      XRP_sent$date <= "2021-05-29 07:59:59")
XRP_sent.60$index<-seq(1,nrow(XRP_sent.60),by=1)
XRP_sent.60.sent<-mean(XRP_sent.60$compound,na.rm = TRUE)
XRP_sent.60.sent<-cbind(XRP_sent.60.sent,XRP_sent.60[nrow(XRP_sent.60),4])
colnames(XRP_sent.60.sent)<-paste(c("avg.sent","volume"))
XRP_sent.60.sent

XRP_sent.61<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 08:00:00" &
                      XRP_sent$date <= "2021-05-29 08:59:59")
XRP_sent.61$index<-seq(1,nrow(XRP_sent.61),by=1)
XRP_sent.61.sent<-mean(XRP_sent.61$compound,na.rm = TRUE)
XRP_sent.61.sent<-cbind(XRP_sent.61.sent,XRP_sent.61[nrow(XRP_sent.61),4])
colnames(XRP_sent.61.sent)<-paste(c("avg.sent","volume"))
XRP_sent.61.sent

XRP_sent.62<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 09:00:00" &
                      XRP_sent$date <= "2021-05-29 09:59:59")
XRP_sent.62$index<-seq(1,nrow(XRP_sent.62),by=1)
XRP_sent.62.sent<-mean(XRP_sent.62$compound,na.rm = TRUE)
XRP_sent.62.sent<-cbind(XRP_sent.62.sent,XRP_sent.62[nrow(XRP_sent.62),4])
colnames(XRP_sent.62.sent)<-paste(c("avg.sent","volume"))
XRP_sent.62.sent

XRP_sent.63<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 10:00:00" &
                      XRP_sent$date <= "2021-05-29 10:59:59")
XRP_sent.63$index<-seq(1,nrow(XRP_sent.63),by=1)
XRP_sent.63.sent<-mean(XRP_sent.63$compound,na.rm = TRUE)
XRP_sent.63.sent<-cbind(XRP_sent.63.sent,XRP_sent.63[nrow(XRP_sent.63),4])
colnames(XRP_sent.63.sent)<-paste(c("avg.sent","volume"))
XRP_sent.63.sent

XRP_sent.64<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 11:00:00" &
                      XRP_sent$date <= "2021-05-29 11:59:59")
XRP_sent.64$index<-seq(1,nrow(XRP_sent.64),by=1)
XRP_sent.64.sent<-mean(XRP_sent.64$compound,na.rm = TRUE)
XRP_sent.64.sent<-cbind(XRP_sent.64.sent,XRP_sent.64[nrow(XRP_sent.64),4])
colnames(XRP_sent.64.sent)<-paste(c("avg.sent","volume"))
XRP_sent.64.sent

XRP_sent.65<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 12:00:00" &
                      XRP_sent$date <= "2021-05-29 12:59:59")
XRP_sent.65$index<-seq(1,nrow(XRP_sent.65),by=1)
XRP_sent.65.sent<-mean(XRP_sent.65$compound,na.rm = TRUE)
XRP_sent.65.sent<-cbind(XRP_sent.65.sent,XRP_sent.65[nrow(XRP_sent.65),4])
colnames(XRP_sent.65.sent)<-paste(c("avg.sent","volume"))
XRP_sent.65.sent

XRP_sent.66<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 13:00:00" &
                      XRP_sent$date <= "2021-05-29 13:59:59")
XRP_sent.66$index<-seq(1,nrow(XRP_sent.66),by=1)
XRP_sent.66.sent<-mean(XRP_sent.66$compound,na.rm = TRUE)
XRP_sent.66.sent<-cbind(XRP_sent.66.sent,XRP_sent.66[nrow(XRP_sent.66),4])
colnames(XRP_sent.66.sent)<-paste(c("avg.sent","volume"))
XRP_sent.66.sent

XRP_sent.67<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 14:00:00" &
                      XRP_sent$date <= "2021-05-29 14:59:59")
XRP_sent.67$index<-seq(1,nrow(XRP_sent.67),by=1)
XRP_sent.67.sent<-mean(XRP_sent.67$compound,na.rm = TRUE)
XRP_sent.67.sent<-cbind(XRP_sent.67.sent,XRP_sent.67[nrow(XRP_sent.67),4])
colnames(XRP_sent.67.sent)<-paste(c("avg.sent","volume"))
XRP_sent.67.sent

XRP_sent.68<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 15:00:00" &
                      XRP_sent$date <= "2021-05-29 15:59:59")
XRP_sent.68$index<-seq(1,nrow(XRP_sent.68),by=1)
XRP_sent.68.sent<-mean(XRP_sent.68$compound,na.rm = TRUE)
XRP_sent.68.sent<-cbind(XRP_sent.68.sent,XRP_sent.68[nrow(XRP_sent.68),4])
colnames(XRP_sent.68.sent)<-paste(c("avg.sent","volume"))
XRP_sent.68.sent

XRP_sent.69<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 16:00:00" &
                      XRP_sent$date <= "2021-05-29 16:59:59")
XRP_sent.69$index<-seq(1,nrow(XRP_sent.69),by=1)
XRP_sent.69.sent<-mean(XRP_sent.69$compound,na.rm = TRUE)
XRP_sent.69.sent<-cbind(XRP_sent.69.sent,XRP_sent.69[nrow(XRP_sent.69),4])
colnames(XRP_sent.69.sent)<-paste(c("avg.sent","volume"))
XRP_sent.69.sent

XRP_sent.70<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 17:00:00" &
                      XRP_sent$date <= "2021-05-29 17:59:59")
XRP_sent.70$index<-seq(1,nrow(XRP_sent.70),by=1)
XRP_sent.70.sent<-mean(XRP_sent.70$compound,na.rm = TRUE)
XRP_sent.70.sent<-cbind(XRP_sent.70.sent,XRP_sent.70[nrow(XRP_sent.70),4])
colnames(XRP_sent.70.sent)<-paste(c("avg.sent","volume"))
XRP_sent.70.sent

XRP_sent.71<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 18:00:00" &
                      XRP_sent$date <= "2021-05-29 18:59:59")
XRP_sent.71$index<-seq(1,nrow(XRP_sent.71),by=1)
XRP_sent.71.sent<-mean(XRP_sent.71$compound,na.rm = TRUE)
XRP_sent.71.sent<-cbind(XRP_sent.71.sent,XRP_sent.71[nrow(XRP_sent.71),4])
colnames(XRP_sent.71.sent)<-paste(c("avg.sent","volume"))
XRP_sent.71.sent

XRP_sent.72<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 19:00:00" &
                      XRP_sent$date <= "2021-05-29 19:59:59")
XRP_sent.72$index<-seq(1,nrow(XRP_sent.72),by=1)
XRP_sent.72.sent<-mean(XRP_sent.72$compound,na.rm = TRUE)
XRP_sent.72.sent<-cbind(XRP_sent.72.sent,XRP_sent.72[nrow(XRP_sent.72),4])
colnames(XRP_sent.72.sent)<-paste(c("avg.sent","volume"))
XRP_sent.72.sent

XRP_sent.73<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 20:00:00" &
                      XRP_sent$date <= "2021-05-29 20:59:59")
XRP_sent.73$index<-seq(1,nrow(XRP_sent.73),by=1)
XRP_sent.73.sent<-mean(XRP_sent.73$compound,na.rm = TRUE)
XRP_sent.73.sent<-cbind(XRP_sent.73.sent,XRP_sent.73[nrow(XRP_sent.73),4])
colnames(XRP_sent.73.sent)<-paste(c("avg.sent","volume"))
XRP_sent.73.sent

XRP_sent.74<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 21:00:00" &
                      XRP_sent$date <= "2021-05-29 21:59:59")
XRP_sent.74$index<-seq(1,nrow(XRP_sent.74),by=1)
XRP_sent.74.sent<-mean(XRP_sent.74$compound,na.rm = TRUE)
XRP_sent.74.sent<-cbind(XRP_sent.74.sent,XRP_sent.74[nrow(XRP_sent.74),4])
colnames(XRP_sent.74.sent)<-paste(c("avg.sent","volume"))
XRP_sent.74.sent

XRP_sent.75<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 22:00:00" &
                      XRP_sent$date <= "2021-05-29 22:59:59")
XRP_sent.75$index<-seq(1,nrow(XRP_sent.75),by=1)
XRP_sent.75.sent<-mean(XRP_sent.75$compound,na.rm = TRUE)
XRP_sent.75.sent<-cbind(XRP_sent.75.sent,XRP_sent.75[nrow(XRP_sent.75),4])
colnames(XRP_sent.75.sent)<-paste(c("avg.sent","volume"))
XRP_sent.75.sent

XRP_sent.76<-subset(XRP_sent,XRP_sent$date >= "2021-05-29 23:00:00" &
                      XRP_sent$date <= "2021-05-29 23:59:59")
XRP_sent.76$index<-seq(1,nrow(XRP_sent.76),by=1)
XRP_sent.76.sent<-mean(XRP_sent.76$compound,na.rm = TRUE)
XRP_sent.76.sent<-cbind(XRP_sent.76.sent,XRP_sent.76[nrow(XRP_sent.76),4])
colnames(XRP_sent.76.sent)<-paste(c("avg.sent","volume"))
XRP_sent.76.sent

XRP_sent.77<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 00:00:00" &
                      XRP_sent$date <= "2021-05-30 00:59:59")
XRP_sent.77$index<-seq(1,nrow(XRP_sent.77),by=1)
XRP_sent.77.sent<-mean(XRP_sent.77$compound,na.rm = TRUE)
XRP_sent.77.sent<-cbind(XRP_sent.77.sent,XRP_sent.77[nrow(XRP_sent.77),4])
colnames(XRP_sent.77.sent)<-paste(c("avg.sent","volume"))
XRP_sent.77.sent

XRP_sent.78<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 01:00:00" &
                      XRP_sent$date <= "2021-05-30 01:59:59")
XRP_sent.78$index<-seq(1,nrow(XRP_sent.78),by=1)
XRP_sent.78.sent<-mean(XRP_sent.78$compound,na.rm = TRUE)
XRP_sent.78.sent<-cbind(XRP_sent.78.sent,XRP_sent.78[nrow(XRP_sent.78),4])
colnames(XRP_sent.78.sent)<-paste(c("avg.sent","volume"))
XRP_sent.78.sent

XRP_sent.79<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 02:00:00" &
                      XRP_sent$date <= "2021-05-30 02:59:59")
XRP_sent.79$index<-seq(1,nrow(XRP_sent.79),by=1)
XRP_sent.79.sent<-mean(XRP_sent.79$compound,na.rm = TRUE)
XRP_sent.79.sent<-cbind(XRP_sent.79.sent,XRP_sent.79[nrow(XRP_sent.79),4])
colnames(XRP_sent.79.sent)<-paste(c("avg.sent","volume"))
XRP_sent.79.sent

XRP_sent.80<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 03:00:00" &
                      XRP_sent$date <= "2021-05-30 03:59:59")
XRP_sent.80$index<-seq(1,nrow(XRP_sent.80),by=1)
XRP_sent.80.sent<-mean(XRP_sent.80$compound,na.rm = TRUE)
XRP_sent.80.sent<-cbind(XRP_sent.80.sent,XRP_sent.80[nrow(XRP_sent.80),4])
colnames(XRP_sent.80.sent)<-paste(c("avg.sent","volume"))
XRP_sent.80.sent

XRP_sent.81<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 04:00:00" &
                      XRP_sent$date <= "2021-05-30 04:59:59")
XRP_sent.81$index<-seq(1,nrow(XRP_sent.81),by=1)
XRP_sent.81.sent<-mean(XRP_sent.81$compound,na.rm = TRUE)
XRP_sent.81.sent<-cbind(XRP_sent.81.sent,XRP_sent.81[nrow(XRP_sent.81),4])
colnames(XRP_sent.81.sent)<-paste(c("avg.sent","volume"))
XRP_sent.81.sent

XRP_sent.82<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 05:00:00" &
                      XRP_sent$date <= "2021-05-30 05:59:59")
XRP_sent.82$index<-seq(1,nrow(XRP_sent.82),by=1)
XRP_sent.82.sent<-mean(XRP_sent.82$compound,na.rm = TRUE)
XRP_sent.82.sent<-cbind(XRP_sent.82.sent,XRP_sent.82[nrow(XRP_sent.82),4])
colnames(XRP_sent.82.sent)<-paste(c("avg.sent","volume"))
XRP_sent.82.sent

XRP_sent.83<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 06:00:00" &
                      XRP_sent$date <= "2021-05-30 06:59:59")
XRP_sent.83$index<-seq(1,nrow(XRP_sent.83),by=1)
XRP_sent.83.sent<-mean(XRP_sent.83$compound,na.rm = TRUE)
XRP_sent.83.sent<-cbind(XRP_sent.83.sent,XRP_sent.83[nrow(XRP_sent.83),4])
colnames(XRP_sent.83.sent)<-paste(c("avg.sent","volume"))
XRP_sent.83.sent

XRP_sent.84<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 07:00:00" &
                      XRP_sent$date <= "2021-05-30 07:59:59")
XRP_sent.84$index<-seq(1,nrow(XRP_sent.84),by=1)
XRP_sent.84.sent<-mean(XRP_sent.84$compound,na.rm = TRUE)
XRP_sent.84.sent<-cbind(XRP_sent.84.sent,XRP_sent.84[nrow(XRP_sent.84),4])
colnames(XRP_sent.84.sent)<-paste(c("avg.sent","volume"))
XRP_sent.84.sent

XRP_sent.85<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 08:00:00" &
                      XRP_sent$date <= "2021-05-30 08:59:59")
XRP_sent.85$index<-seq(1,nrow(XRP_sent.85),by=1)
XRP_sent.85.sent<-mean(XRP_sent.85$compound,na.rm = TRUE)
XRP_sent.85.sent<-cbind(XRP_sent.85.sent,XRP_sent.85[nrow(XRP_sent.85),4])
colnames(XRP_sent.85.sent)<-paste(c("avg.sent","volume"))
XRP_sent.85.sent

XRP_sent.86<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 09:00:00" &
                      XRP_sent$date <= "2021-05-30 09:59:59")
XRP_sent.86$index<-seq(1,nrow(XRP_sent.86),by=1)
XRP_sent.86.sent<-mean(XRP_sent.86$compound,na.rm = TRUE)
XRP_sent.86.sent<-cbind(XRP_sent.86.sent,XRP_sent.86[nrow(XRP_sent.86),4])
colnames(XRP_sent.86.sent)<-paste(c("avg.sent","volume"))
XRP_sent.86.sent

XRP_sent.87<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 10:00:00" &
                      XRP_sent$date <= "2021-05-30 10:59:59")
XRP_sent.87$index<-seq(1,nrow(XRP_sent.87),by=1)
XRP_sent.87.sent<-mean(XRP_sent.87$compound,na.rm = TRUE)
XRP_sent.87.sent<-cbind(XRP_sent.87.sent,XRP_sent.87[nrow(XRP_sent.87),4])
colnames(XRP_sent.87.sent)<-paste(c("avg.sent","volume"))
XRP_sent.87.sent

XRP_sent.88<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 11:00:00" &
                      XRP_sent$date <= "2021-05-30 11:59:59")
XRP_sent.88$index<-seq(1,nrow(XRP_sent.88),by=1)
XRP_sent.88.sent<-mean(XRP_sent.88$compound,na.rm = TRUE)
XRP_sent.88.sent<-cbind(XRP_sent.88.sent,XRP_sent.88[nrow(XRP_sent.88),4])
colnames(XRP_sent.88.sent)<-paste(c("avg.sent","volume"))
XRP_sent.88.sent

XRP_sent.89<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 12:00:00" &
                      XRP_sent$date <= "2021-05-30 12:59:59")
XRP_sent.89$index<-seq(1,nrow(XRP_sent.89),by=1)
XRP_sent.89.sent<-mean(XRP_sent.89$compound,na.rm = TRUE)
XRP_sent.89.sent<-cbind(XRP_sent.89.sent,XRP_sent.89[nrow(XRP_sent.89),4])
colnames(XRP_sent.89.sent)<-paste(c("avg.sent","volume"))
XRP_sent.89.sent

XRP_sent.90<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 13:00:00" &
                      XRP_sent$date <= "2021-05-30 13:59:59")
XRP_sent.90$index<-seq(1,nrow(XRP_sent.90),by=1)
XRP_sent.90.sent<-mean(XRP_sent.90$compound,na.rm = TRUE)
XRP_sent.90.sent<-cbind(XRP_sent.90.sent,XRP_sent.90[nrow(XRP_sent.90),4])
colnames(XRP_sent.90.sent)<-paste(c("avg.sent","volume"))
XRP_sent.90.sent

XRP_sent.91<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 14:00:00" &
                      XRP_sent$date <= "2021-05-30 14:59:59")
XRP_sent.91$index<-seq(1,nrow(XRP_sent.91),by=1)
XRP_sent.91.sent<-mean(XRP_sent.91$compound,na.rm = TRUE)
XRP_sent.91.sent<-cbind(XRP_sent.91.sent,XRP_sent.91[nrow(XRP_sent.91),4])
colnames(XRP_sent.91.sent)<-paste(c("avg.sent","volume"))
XRP_sent.91.sent

XRP_sent.92<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 15:00:00" &
                      XRP_sent$date <= "2021-05-30 15:59:59")
XRP_sent.92$index<-seq(1,nrow(XRP_sent.92),by=1)
XRP_sent.92.sent<-mean(XRP_sent.92$compound,na.rm = TRUE)
XRP_sent.92.sent<-cbind(XRP_sent.92.sent,XRP_sent.92[nrow(XRP_sent.92),4])
colnames(XRP_sent.92.sent)<-paste(c("avg.sent","volume"))
XRP_sent.92.sent

XRP_sent.93<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 16:00:00" &
                      XRP_sent$date <= "2021-05-30 16:59:59")
XRP_sent.93$index<-seq(1,nrow(XRP_sent.93),by=1)
XRP_sent.93.sent<-mean(XRP_sent.93$compound,na.rm = TRUE)
XRP_sent.93.sent<-cbind(XRP_sent.93.sent,XRP_sent.93[nrow(XRP_sent.93),4])
colnames(XRP_sent.93.sent)<-paste(c("avg.sent","volume"))
XRP_sent.93.sent

XRP_sent.94<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 17:00:00" &
                      XRP_sent$date <= "2021-05-30 17:59:59")
XRP_sent.94$index<-seq(1,nrow(XRP_sent.94),by=1)
XRP_sent.94.sent<-mean(XRP_sent.94$compound,na.rm = TRUE)
XRP_sent.94.sent<-cbind(XRP_sent.94.sent,XRP_sent.94[nrow(XRP_sent.94),4])
colnames(XRP_sent.94.sent)<-paste(c("avg.sent","volume"))
XRP_sent.94.sent

XRP_sent.95<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 18:00:00" &
                      XRP_sent$date <= "2021-05-30 18:59:59")
XRP_sent.95$index<-seq(1,nrow(XRP_sent.95),by=1)
XRP_sent.95.sent<-mean(XRP_sent.95$compound,na.rm = TRUE)
XRP_sent.95.sent<-cbind(XRP_sent.95.sent,XRP_sent.95[nrow(XRP_sent.95),4])
colnames(XRP_sent.95.sent)<-paste(c("avg.sent","volume"))
XRP_sent.95.sent

XRP_sent.96<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 19:00:00" &
                      XRP_sent$date <= "2021-05-30 19:59:59")
XRP_sent.96$index<-seq(1,nrow(XRP_sent.96),by=1)
XRP_sent.96.sent<-mean(XRP_sent.96$compound,na.rm = TRUE)
XRP_sent.96.sent<-cbind(XRP_sent.96.sent,XRP_sent.96[nrow(XRP_sent.96),4])
colnames(XRP_sent.96.sent)<-paste(c("avg.sent","volume"))
XRP_sent.96.sent

XRP_sent.97<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 20:00:00" &
                      XRP_sent$date <= "2021-05-30 20:59:59")
XRP_sent.97$index<-seq(1,nrow(XRP_sent.97),by=1)
XRP_sent.97.sent<-mean(XRP_sent.97$compound,na.rm = TRUE)
XRP_sent.97.sent<-cbind(XRP_sent.97.sent,XRP_sent.97[nrow(XRP_sent.97),4])
colnames(XRP_sent.97.sent)<-paste(c("avg.sent","volume"))
XRP_sent.97.sent

XRP_sent.98<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 21:00:00" &
                      XRP_sent$date <= "2021-05-30 21:59:59")
XRP_sent.98$index<-seq(1,nrow(XRP_sent.98),by=1)
XRP_sent.98.sent<-mean(XRP_sent.98$compound,na.rm = TRUE)
XRP_sent.98.sent<-cbind(XRP_sent.98.sent,XRP_sent.98[nrow(XRP_sent.98),4])
colnames(XRP_sent.98.sent)<-paste(c("avg.sent","volume"))
XRP_sent.98.sent

XRP_sent.99<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 22:00:00" &
                      XRP_sent$date <= "2021-05-30 22:59:59")
XRP_sent.99$index<-seq(1,nrow(XRP_sent.99),by=1)
XRP_sent.99.sent<-mean(XRP_sent.99$compound,na.rm = TRUE)
XRP_sent.99.sent<-cbind(XRP_sent.99.sent,XRP_sent.99[nrow(XRP_sent.99),4])
colnames(XRP_sent.99.sent)<-paste(c("avg.sent","volume"))
XRP_sent.99.sent

XRP_sent.100<-subset(XRP_sent,XRP_sent$date >= "2021-05-30 23:00:00" &
                      XRP_sent$date <= "2021-05-30 23:59:59")
XRP_sent.100$index<-seq(1,nrow(XRP_sent.100),by=1)
XRP_sent.100.sent<-mean(XRP_sent.100$compound,na.rm = TRUE)
XRP_sent.100.sent<-cbind(XRP_sent.100.sent,XRP_sent.100[nrow(XRP_sent.100),4])
colnames(XRP_sent.100.sent)<-paste(c("avg.sent","volume"))
XRP_sent.100.sent

XRP_sent.101<-subset(XRP_sent,XRP_sent$date >= "2021-05-31 00:00:00" &
                       XRP_sent$date <= "2021-05-31 00:59:59")
XRP_sent.101$index<-seq(1,nrow(XRP_sent.101),by=1)
XRP_sent.101.sent<-mean(XRP_sent.101$compound,na.rm = TRUE)
XRP_sent.101.sent<-cbind(XRP_sent.101.sent,XRP_sent.101[nrow(XRP_sent.101),4])
colnames(XRP_sent.101.sent)<-paste(c("avg.sent","volume"))
XRP_sent.101.sent

XRP_sent.102<-subset(XRP_sent,XRP_sent$date >= "2021-05-31 01:00:00" &
                       XRP_sent$date <= "2021-05-31 01:59:59")
XRP_sent.102$index<-seq(1,nrow(XRP_sent.102),by=1)
XRP_sent.102.sent<-mean(XRP_sent.102$compound,na.rm = TRUE)
XRP_sent.102.sent<-cbind(XRP_sent.102.sent,XRP_sent.102[nrow(XRP_sent.102),4])
colnames(XRP_sent.102.sent)<-paste(c("avg.sent","volume"))
XRP_sent.102.sent

XRP_sent.103<-subset(XRP_sent,XRP_sent$date >= "2021-05-31 02:00:00" &
                       XRP_sent$date <= "2021-05-31 02:59:59")
XRP_sent.103$index<-seq(1,nrow(XRP_sent.103),by=1)
XRP_sent.103.sent<-mean(XRP_sent.103$compound,na.rm = TRUE)
XRP_sent.103.sent<-cbind(XRP_sent.103.sent,XRP_sent.103[nrow(XRP_sent.103),4])
colnames(XRP_sent.103.sent)<-paste(c("avg.sent","volume"))
XRP_sent.103.sent

XRP_sent.104<-subset(XRP_sent,XRP_sent$date >= "2021-05-31 03:00:00" &
                       XRP_sent$date <= "2021-05-31 03:59:59")
XRP_sent.104$index<-seq(1,nrow(XRP_sent.104),by=1)
XRP_sent.104.sent<-mean(XRP_sent.104$compound,na.rm = TRUE)
XRP_sent.104.sent<-cbind(XRP_sent.104.sent,XRP_sent.104[nrow(XRP_sent.104),4])
colnames(XRP_sent.104.sent)<-paste(c("avg.sent","volume"))
XRP_sent.104.sent

XRP_sent.105<-subset(XRP_sent,XRP_sent$date >= "2021-05-31 04:00:00" &
                       XRP_sent$date <= "2021-05-31 04:59:59")
XRP_sent.105$index<-seq(1,nrow(XRP_sent.105),by=1)
XRP_sent.105.sent<-mean(XRP_sent.105$compound,na.rm = TRUE)
XRP_sent.105.sent<-cbind(XRP_sent.105.sent,XRP_sent.105[nrow(XRP_sent.105),4])
colnames(XRP_sent.105.sent)<-paste(c("avg.sent","volume"))
XRP_sent.105.sent

XRP_sent.106<-subset(XRP_sent,XRP_sent$date >= "2021-05-31 05:00:00" &
                       XRP_sent$date <= "2021-05-31 05:59:59")
XRP_sent.106$index<-seq(1,nrow(XRP_sent.106),by=1)
XRP_sent.106.sent<-mean(XRP_sent.106$compound,na.rm = TRUE)
XRP_sent.106.sent<-cbind(XRP_sent.106.sent,XRP_sent.106[nrow(XRP_sent.106),4])
colnames(XRP_sent.106.sent)<-paste(c("avg.sent","volume"))
XRP_sent.106.sent

XRP_sent.107<-subset(XRP_sent,XRP_sent$date >= "2021-05-31 06:00:00" &
                       XRP_sent$date <= "2021-05-31 06:59:59")
XRP_sent.107$index<-seq(1,nrow(XRP_sent.107),by=1)
XRP_sent.107.sent<-mean(XRP_sent.107$compound,na.rm = TRUE)
XRP_sent.107.sent<-cbind(XRP_sent.107.sent,XRP_sent.107[nrow(XRP_sent.107),4])
colnames(XRP_sent.107.sent)<-paste(c("avg.sent","volume"))
XRP_sent.107.sent

XRP_sent.108<-subset(XRP_sent,XRP_sent$date >= "2021-05-31 07:00:00" &
                       XRP_sent$date <= "2021-05-31 07:59:59")
XRP_sent.108$index<-seq(1,nrow(XRP_sent.108),by=1)
XRP_sent.108.sent<-mean(XRP_sent.108$compound,na.rm = TRUE)
XRP_sent.108.sent<-cbind(XRP_sent.108.sent,XRP_sent.108[nrow(XRP_sent.108),4])
colnames(XRP_sent.108.sent)<-paste(c("avg.sent","volume"))
XRP_sent.108.sent

XRP_sent.109<-subset(XRP_sent,XRP_sent$date >= "2021-05-31 08:00:00" &
                       XRP_sent$date <= "2021-05-31 08:59:59")
XRP_sent.109$index<-seq(1,nrow(XRP_sent.109),by=1)
XRP_sent.109.sent<-mean(XRP_sent.109$compound,na.rm = TRUE)
XRP_sent.109.sent<-cbind(XRP_sent.109.sent,XRP_sent.109[nrow(XRP_sent.109),4])
colnames(XRP_sent.109.sent)<-paste(c("avg.sent","volume"))
XRP_sent.109.sent

XRP_sent.110<-subset(XRP_sent,XRP_sent$date >= "2021-05-31 09:00:00" &
                       XRP_sent$date <= "2021-05-31 09:59:59")
XRP_sent.110$index<-seq(1,nrow(XRP_sent.110),by=1)
XRP_sent.110.sent<-mean(XRP_sent.110$compound,na.rm = TRUE)
XRP_sent.110.sent<-cbind(XRP_sent.110.sent,XRP_sent.110[nrow(XRP_sent.110),4])
colnames(XRP_sent.110.sent)<-paste(c("avg.sent","volume"))
XRP_sent.110.sent

XRP_sent.111<-subset(XRP_sent,XRP_sent$date >= "2021-05-31 10:00:00" &
                       XRP_sent$date <= "2021-05-31 10:59:59")
XRP_sent.111$index<-seq(1,nrow(XRP_sent.111),by=1)
XRP_sent.111.sent<-mean(XRP_sent.111$compound,na.rm = TRUE)
XRP_sent.111.sent<-cbind(XRP_sent.111.sent,XRP_sent.111[nrow(XRP_sent.111),4])
colnames(XRP_sent.111.sent)<-paste(c("avg.sent","volume"))
XRP_sent.111.sent

XRP_sent.112<-subset(XRP_sent,XRP_sent$date >= "2021-05-31 11:00:00" &
                       XRP_sent$date <= "2021-05-31 11:59:59")
XRP_sent.112$index<-seq(1,nrow(XRP_sent.112),by=1)
XRP_sent.112.sent<-mean(XRP_sent.112$compound,na.rm = TRUE)
XRP_sent.112.sent<-cbind(XRP_sent.112.sent,XRP_sent.112[nrow(XRP_sent.112),4])
colnames(XRP_sent.112.sent)<-paste(c("avg.sent","volume"))
XRP_sent.112.sent

XRP_sent.113<-subset(XRP_sent,XRP_sent$date >= "2021-05-31 12:00:00" &
                       XRP_sent$date <= "2021-05-31 12:59:59")
XRP_sent.113$index<-seq(1,nrow(XRP_sent.113),by=1)
XRP_sent.113.sent<-mean(XRP_sent.113$compound,na.rm = TRUE)
XRP_sent.113.sent<-cbind(XRP_sent.113.sent,XRP_sent.113[nrow(XRP_sent.113),4])
colnames(XRP_sent.113.sent)<-paste(c("avg.sent","volume"))
XRP_sent.113.sent

XRP_sent.114<-subset(XRP_sent,XRP_sent$date >= "2021-05-31 13:00:00" &
                       XRP_sent$date <= "2021-05-31 13:59:59")
XRP_sent.114$index<-seq(1,nrow(XRP_sent.114),by=1)
XRP_sent.114.sent<-mean(XRP_sent.114$compound,na.rm = TRUE)
XRP_sent.114.sent<-cbind(XRP_sent.114.sent,XRP_sent.114[nrow(XRP_sent.114),4])
colnames(XRP_sent.114.sent)<-paste(c("avg.sent","volume"))
XRP_sent.114.sent


XRP_sent.hourly<-rbind(XRP_sent.1.sent,XRP_sent.2.sent,XRP_sent.3.sent,XRP_sent.4.sent,XRP_sent.5.sent,XRP_sent.6.sent,
                       XRP_sent.7.sent,XRP_sent.8.sent,XRP_sent.9.sent,XRP_sent.10.sent,XRP_sent.11.sent,XRP_sent.12.sent,
                       XRP_sent.13.sent,XRP_sent.14.sent,XRP_sent.15.sent,XRP_sent.16.sent,XRP_sent.17.sent,XRP_sent.18.sent,
                       XRP_sent.19.sent,XRP_sent.20.sent,XRP_sent.21.sent,XRP_sent.22.sent,XRP_sent.23.sent,XRP_sent.24.sent,
                       XRP_sent.25.sent,XRP_sent.26.sent,XRP_sent.27.sent,XRP_sent.28.sent,XRP_sent.29.sent,XRP_sent.30.sent,
                       XRP_sent.31.sent,XRP_sent.32.sent,XRP_sent.33.sent,XRP_sent.34.sent,XRP_sent.35.sent,XRP_sent.36.sent,
                       XRP_sent.37.sent,XRP_sent.38.sent,XRP_sent.39.sent,XRP_sent.40.sent,XRP_sent.41.sent,XRP_sent.42.sent,
                       XRP_sent.43.sent,XRP_sent.44.sent,XRP_sent.45.sent,XRP_sent.46.sent,XRP_sent.47.sent,XRP_sent.48.sent,
                       XRP_sent.49.sent,XRP_sent.50.sent,XRP_sent.51.sent,XRP_sent.52.sent,XRP_sent.53.sent,XRP_sent.54.sent,
                       XRP_sent.55.sent,XRP_sent.56.sent,XRP_sent.57.sent,XRP_sent.58.sent,XRP_sent.59.sent,XRP_sent.60.sent,
                       XRP_sent.61.sent,XRP_sent.62.sent,XRP_sent.63.sent,XRP_sent.64.sent,XRP_sent.65.sent,XRP_sent.66.sent,
                       XRP_sent.67.sent,XRP_sent.68.sent,XRP_sent.69.sent,XRP_sent.70.sent,XRP_sent.71.sent,XRP_sent.72.sent,
                       XRP_sent.73.sent,XRP_sent.74.sent,XRP_sent.75.sent,XRP_sent.76.sent,XRP_sent.77.sent,XRP_sent.78.sent,
                       XRP_sent.79.sent,XRP_sent.80.sent,XRP_sent.81.sent,XRP_sent.82.sent,XRP_sent.83.sent,XRP_sent.84.sent,
                       XRP_sent.85.sent,XRP_sent.86.sent,XRP_sent.87.sent,XRP_sent.88.sent,XRP_sent.89.sent,XRP_sent.90.sent,
                       XRP_sent.91.sent,XRP_sent.92.sent,XRP_sent.93.sent,XRP_sent.94.sent,XRP_sent.95.sent,XRP_sent.96.sent,
                       XRP_sent.97.sent,XRP_sent.98.sent,XRP_sent.99.sent,XRP_sent.100.sent,XRP_sent.101.sent,XRP_sent.102.sent,
                       XRP_sent.103.sent,XRP_sent.104.sent,XRP_sent.105.sent,XRP_sent.106.sent,XRP_sent.107.sent,XRP_sent.108.sent,
                       XRP_sent.109.sent,XRP_sent.110.sent,XRP_sent.111.sent,XRP_sent.112.sent,XRP_sent.113.sent,XRP_sent.114.sent)

date<-seq(from=as.POSIXct("2021-05-26 20:00:00", tz="UTC"), 
          to=as.POSIXct("2021-05-31 13:00:00", tz="UTC"), by="hour")
date<- as.data.frame(date)

XRP_sent.hourly <- cbind(date, XRP_sent.hourly)
head(XRP_sent.hourly)

write_as_csv(XRP_sent.hourly, "XRP_sent.hourly.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(XRP_sent.hourly, "XRP_sent.hourly.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
XRP_sent.hourly<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/XRP_sent.hourly.csv",header = TRUE)

#**S6: AVERAGE SENTIMENT SCORE PER DAY -------------------------------------
XRP_sent.hourly[c(1:3,nrow(XRP_sent.hourly)),]
XRP_sent.daily<-XRP_sent.hourly[c(5:100),]

XRP_sent.daily1<-subset(XRP_sent.daily,XRP_sent.daily$date >= "2021-05-27 00:00:00" &
                          XRP_sent.daily$date <= "2021-05-27 23:00:00")
XRP_sent.daily1.sent<-mean(XRP_sent.daily1$avg.sent,na.rm = TRUE)
XRP_sent.daily1.vol<-sum(XRP_sent.daily1$volume)
XRP_sent.daily1<-cbind(XRP_sent.daily1.sent,XRP_sent.daily1.vol)
colnames(XRP_sent.daily1)<-paste(c("Daily.sent","Daily.vol"))
XRP_sent.daily1

XRP_sent.daily2<-subset(XRP_sent.daily,XRP_sent.daily$date >= "2021-05-28 00:00:00" &
                          XRP_sent.daily$date <= "2021-05-28 23:00:00")
XRP_sent.daily2.sent<-mean(XRP_sent.daily2$avg.sent,na.rm = TRUE)
XRP_sent.daily2.vol<-sum(XRP_sent.daily2$volume)
XRP_sent.daily2<-cbind(XRP_sent.daily2.sent,XRP_sent.daily2.vol)
colnames(XRP_sent.daily2)<-paste(c("Daily.sent","Daily.vol"))
XRP_sent.daily2

XRP_sent.daily3<-subset(XRP_sent.daily,XRP_sent.daily$date >= "2021-05-29 00:00:00" &
                          XRP_sent.daily$date <= "2021-05-29 23:00:00")
XRP_sent.daily3.sent<-mean(XRP_sent.daily3$avg.sent,na.rm = TRUE)
XRP_sent.daily3.vol<-sum(XRP_sent.daily3$volume)
XRP_sent.daily3<-cbind(XRP_sent.daily3.sent,XRP_sent.daily3.vol)
colnames(XRP_sent.daily3)<-paste(c("Daily.sent","Daily.vol"))
XRP_sent.daily3

XRP_sent.daily4<-subset(XRP_sent.daily,XRP_sent.daily$date >= "2021-05-30 00:00:00" &
                          XRP_sent.daily$date <= "2021-05-30 23:00:00")
XRP_sent.daily4.sent<-mean(XRP_sent.daily4$avg.sent,na.rm = TRUE)
XRP_sent.daily4.vol<-sum(XRP_sent.daily4$volume)
XRP_sent.daily4<-cbind(XRP_sent.daily4.sent,XRP_sent.daily4.vol)
colnames(XRP_sent.daily4)<-paste(c("Daily.sent","Daily.vol"))
XRP_sent.daily4

XRP_sent.daily.tot<-rbind(XRP_sent.daily1,XRP_sent.daily2,XRP_sent.daily3,XRP_sent.daily4)

date<-seq(from=as.POSIXct("2021-05-27 00:00:00", tz="UTC"), 
          to=as.POSIXct("2021-05-30 00:00:00", tz="UTC"), by="day")
date<- as.data.frame(date)

XRP_sent.daily.tot <- cbind(date, XRP_sent.daily.tot)

options(scipen = 1)
XRP_sent.daily.tot$Daily.sent.diff<-Delt(XRP_sent.daily.tot$Daily.sent)
XRP_sent.daily.tot$Daily.vol.diff<-Delt(XRP_sent.daily.tot$Daily.vol)
colnames(XRP_sent.daily.tot)<-paste(c("date","Daily.sent","Daily.vol","Daily.sent.diff","Daily.vol.diff"))
XRP_sent.daily.tot[is.na(XRP_sent.daily.tot)] <- 0

write_as_csv(XRP_sent.daily.tot, "XRP_sent.daily.tot.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(XRP_sent.daily.tot, "XRP_sent.daily.tot.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
XRP_sent.daily.tot<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/XRP_sent.daily.tot.csv",header = TRUE)
# PLOTS -------------------------------------------------------------------
##**Plot the frequency of tweets over a variety of time intervals ---------
ggplot(XRP_sent.hourly,aes(x=date))+
  geom_line(aes(y=volume))+
  theme_bw()+
  labs(x = NULL, y = NULL,
       title = "Frequency of #XRP tweets",
       subtitle = paste0(format(min(as.Date(XRP_twt$created_at)), "%d %B %Y"), " to ", format(max(as.Date(XRP_twt$created_at)),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet")

##**Plot the tweets sentiment score ---------------------------------------
ggplot(XRP_sent,aes(x=compound)) + 
  geom_histogram(bins = 15,alpha=.6) + 
  theme_bw() +
  labs(x="Score",y="Number of tweets")+
  labs(title = "Sentiment analysis of #XRP Tweets from 26-31 May 2021")+
  labs(subtitle = "Using VADER lexicon")

##**Plot the hourly tweets sentiment score + volume -----------------------------
# Value used to transform the data
coeff <- 0.0001

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(XRP_sent.hourly, aes(x=date)) +
  
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
  labs(title = "Hourly tweets sentiment scores and volume of #XRP",
       caption = "Using  VADER appproach")

# PART III: ANALYSIS FROM PART I + II -------------------------------------
# *S1: Extract necessary data ----------------------------------------------
XRP.price.hourly<-subset(XRPUSD_1hr,XRPUSD_1hr$date >= "2021-05-26 23:00:00" &
                           XRPUSD_1hr$date <= "2021-05-31 16:00:00")
XRP.price.hourly<-XRP.price.hourly[,c(2,7,8)]
names(XRP.price.hourly)<-paste(c("Date","Close","Volume"))
XRP.price.hourly<-XRP.price.hourly[order(XRP.price.hourly$Date),]

XRP.price.hourly[c(1:3,nrow(XRP.price.hourly)),]

XRP_sent.hourly[c(1:3,nrow(XRP_sent.hourly)),]


# *S2: CALCULATE HOURLY CHANGES IN PRICE/SENTIMENT -------------------------------

#HOURLY LOGARITHMIC PRICE RETURNS
XRP.hourly.log.ret <- as.data.frame(XRP.price.hourly)
XRP.hourly.log.ret$XRP.log.ret <- c(diff(log(XRP.hourly.log.ret$Close)),0)
options(digits = 3)

#HOURLY LOGARITHMIC SENTIMENT
XRP_sent.diff<-XRP_sent.hourly
XRP_sent.diff$log.sent<-c(diff(log(XRP_sent.diff$avg.sent)),0)

#TWEETS VOLUME LOGARITHMIC CHANGES
XRP_vol.diff<-XRP_sent.hourly
XRP_vol.diff$vol.log<- c(diff(log(XRP_vol.diff$volume)),0)

#COMBINE THE RESULTS
XRP.log.change<-cbind(XRP.hourly.log.ret$XRP.log.ret,XRP_sent.diff$log.sent,XRP_vol.diff$vol.log)
colnames(XRP.log.change)<-paste(c("XRPUSD","XRP","Volume"))
XRP.log.change<-as.data.frame(XRP.log.change)
# *S3: SMA -----------------------------------------------------------------
XRP.sma.3<-XRP.price.hourly[,c(1:2)]
XRP.sma.3$sma5 <- rollmeanr(XRP.sma.3$Close, k =5, fill=NA)
XRP.sma.3$sma10 <- rollmeanr(XRP.sma.3$Close, k = 10, fill=NA)

#**S4: CORRELATION MATRIX: VOLUME, SENTIMENT, PRICE ------------------------
#HOURLY TABLE
hourly.table<-XRP_sent.diff[,-4]
hourly.table<-cbind(hourly.table,XRP.hourly.log.ret$Close,XRP.hourly.log.ret$XRP.log.ret)
colnames(hourly.table)<-paste(c("Date","Hourly.sent","Hourly.vol","Hourly.close","Hourly.log.ret"))

hourly.table$sent.idx<-hourly.table$Hourly.sent/hourly.table$Hourly.sent[1]
hourly.table$vol.idx<-hourly.table$Hourly.vol/hourly.table$Hourly.vol[1]
hourly.table$close.idx<-hourly.table$Hourly.close/hourly.table$Hourly.close[1]

#DAILY TABLE
XRP.price.daily<-subset(data.XRP,index(data.XRP)>="2021-05-27" &
                          index(data.XRP)<="2021-05-30")
XRP.price.daily<-XRP.price.daily[,6]
daily.table<-XRP_sent.daily.tot[,c(-4,-5)]
daily.table<-cbind(daily.table,XRP.price.daily$`XRP-USD.Adjusted`)
daily.table$Daily.log.ret<-c(0,diff(log(daily.table$`XRP-USD.Adjusted`)))
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
##**Correlation Scatterplot Between #XRP Sentiment changes and XRP Price Return--------
plot(XRP.log.change$XRPUSD, XRP.log.change$XRP, pch = 19, 
     main = "Correlation Matrix Between Hourly #XRP Sentiment changes and XRP Price return", xlab = "XRP-USD", ylab = "#XRP")
abline(lm( XRP.log.change$XRPUSD ~ XRP.log.change$XRP), col = "red")
text(x = 0.05, y = -0.5, label = "r = 0.006568 ", col = "red")

##**Correlation Scatterplot Between #XRP Volume changes and XRP Price Return --------
plot(XRP.log.change$XRPUSD, XRP.log.change$Volume, pch = 19, 
     main = "Correlation Matrix Between Hourly #XRP Volume changes and XRP Price return", xlab = "XRP-USD", ylab = "#XRP Volume")
abline(lm( XRP.log.change$XRPUSD ~ XRP.log.change$Volume), col = "red")
text(x = 0.05, y = -0.3, label = "r = 0.009791", col = "red")

##**Plot the hourly price returns + volume --------------------------------
return.volume.plot<-XRP_sent.hourly
return.volume.plot$price.ret<-XRP_sent.diff$log.sent

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
  labs(title = "Hourly Logarithmic price returns and volume of #XRP",
       subtitle = "Using VADER appproach")

##**Tweets volume - Price returns - Price of XRP --------------------------
p1 <- ggplot(XRP_sent.hourly,aes(x=date))+
  geom_bar(aes(y=volume), stat="identity", size=.1, color="black", alpha=.4)+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(x="Hours",y="Tweets Volume")+
  labs(title = "Hourly Tweets Volume of #XRP")

p2 <- ggplot(XRP.hourly.log.ret, aes(x=Date)) + 
  geom_line(aes(y=Close)) + 
  theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Hours", y = "Price ($)")+
  labs(title = "Hourly Price of XRP-USD", 
       subtitle =paste0(format(min(as.Date(XRP.hourly.log.ret$Date)), "%d %B %Y"), " to ", format(max(as.Date(XRP.hourly.log.ret$Date)),"%d %B %Y")))

p3 <- ggplot(XRP.hourly.log.ret,aes(x=Date)) + 
  geom_line(aes(y=XRP.log.ret)) +
  geom_hline(yintercept = 0,col="red") +
  theme_bw() +
  labs(x="Hours",y="Price Returns (%)")+
  labs(title = "Hourly Logarithmic Price Returns of XRP-USD",caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3), size = "last"))

##**Daily Sentiment score - Tweets volume - Close Prices of XRP -----------
XRP.price.daily<- subset(data.XRP,index(data.XRP) >= "2021-05-27" &
                           index(data.XRP) <= "2021-05-31")
XRP.price.daily<-XRP.price.daily[,6]
XRP.price.daily$ret<-diff(log(XRP.price.daily$`XRP-USD.Adjusted`))
XRP.price.daily[1,2]<-0

p1 <- ggplot(XRP_sent.daily.tot,aes(x=date))+
  geom_line(aes(y=Daily.sent), stat="identity", size=.1, color="black", alpha=.4)+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(x="Days",y="Daily Sentiment Score")+
  labs(title = "Daily Sentiment Scores of #XRP")

p2 <- ggplot(XRP_sent.daily.tot, aes(x=date)) + 
  geom_line(aes(y=Daily.vol)) + 
  theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Days", y = "Tweets Volume")+
  labs(title = "Daily Volume of #XRP Tweets")

p3 <- ggplot(XRP.price.daily,aes(x=index(XRP.price.daily))) + 
  geom_line(aes(y=XRP.USD.Adjusted)) +
  theme_bw() +
  labs(x="Days",y="Prices ($)", 
       subtitle =paste0(format(min(as.Date(XRP_sent.daily.tot$date)), "%d %B %Y"), " to ", format(max(as.Date(XRP_sent.daily.tot$date)),"%d %B %Y")))+
  labs(title = "Daily Prices of XRP-USD",caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3), size = "last"))

##**CORRELATION MATRIX: VOLUME, SENTIMENT, PRICE --------------------------
#Hourly data
hourly.table.corr.mat <- as.data.frame(cbind(hourly.table$sent.idx,hourly.table$vol.idx,hourly.table$close.idx))
colnames(hourly.table.corr.mat)<-paste(c("Sent","Vol","Close Price"))
corrplot(cor(hourly.table.corr.mat), method = "number", title = "Hourly Correlation Matrix #XRP", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

#Daily data
daily.table.corr.mat <- as.data.frame(cbind(daily.table$sent.idx,daily.table$vol.idx,daily.table$close.idx))
colnames(daily.table.corr.mat)<-paste(c("Sent","Vol","Close Price"))
corrplot(cor(daily.table.corr.mat), method = "number", title = "Daily Correlation Matrix #XRP", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

##**SMA + TRENDLINE during the period 2021-05-26 -> 2021-05-31 ------------------------
ggplot(XRP.sma.3,aes(x=Date))+
  geom_line(aes(y=Close))+
  theme_bw()+
  labs(x="Date",y="Price ($)",
       title = "XRP-USD Simple Moving Average",
       subtitle = "May 26, 2021 - May 31, 2021")+
  geom_line(aes(y=predict(lm(Close~index(XRP.sma.3)))),col="red")+ #trendline
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
                  max.lag =3, alpha = 0.05, lambda = 2.576, plot = TRUE,
                  table = TRUE, var.names = NULL)