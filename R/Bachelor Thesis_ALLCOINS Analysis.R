#Combination: All coins Hourly: 2021-05-27 00:00:00 -> 2021-05-31 11:00:00
#Daily: 2021-05-27 -> 2021-05-30
# LIBRARIES ---------------------------------------------------------------
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

# S1: Get necessary data -------------------------------------------------

#DAILY + HOURLY PRICE DATA
data.BTC<-getSymbols("BTC-USD",from="2021-05-27",to="2021-05-31",auto.assign = FALSE)
BTCUSD_1hr<-read_csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Hourly Data/BTCUSD_1hr_woline1.csv")
BTCUSD_1hr<-BTCUSD_1hr[c(662:769),]

data.ETH<-getSymbols("ETH-USD",from="2021-05-27",to="2021-05-31",auto.assign = FALSE)
ETHUSD_1hr<-read_csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Hourly Data/ETHUSD_1hr_woline1.csv")
ETHUSD_1hr<-ETHUSD_1hr[c(566:673),]

data.XRP<-getSymbols("XRP-USD",from="2021-05-27",to="2021-05-31",auto.assign = FALSE)
XRPUSD_1hr<-read_csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Hourly Data/XRPUSD_1hr_woline1.csv")
XRPUSD_1hr<-XRPUSD_1hr[c(326:433),]

data.DOGE<-getSymbols("DOGE-USD",from="2021-05-27",to="2021-05-31",auto.assign = FALSE)
DOGEUSD_1hr<-read_csv("E:/Study Materials/Augsburg Hochschule/BACHELOR THESIS/R/Cryptos Hourly Data/DOGEUSD_1hr_woline1.csv")
DOGEUSD_1hr<-DOGEUSD_1hr[c(13:nrow(DOGEUSD_1hr)),]

#HOURLY SENTIMENT SCORE
BTC_sent.hourly<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/BTC_sent.hourly.csv",header = TRUE)
BTC_sent.hourly<-BTC_sent.hourly[c(4:nrow(BTC_sent.hourly)),]

ETH_sent.hourly<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/ETH_sent.hourly.csv",header = TRUE)
ETH_sent.hourly<-ETH_sent.hourly[c(85:192),]

XRP_sent.hourly<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/XRP_sent.hourly.csv",header = TRUE)
XRP_sent.hourly<-XRP_sent.hourly[c(5:112),]

DOGE_sent.hourly<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/DOGE_sent.hourly.csv",header = TRUE)
DOGE_sent.hourly<-DOGE_sent.hourly[c(1:108),]

#DAILY SENTIMENT SCORE
BTC_sent.daily.tot<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/BTC_sent.daily.tot.csv",header = TRUE)

ETH_sent.daily.tot<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/ETH_sent.daily.tot.csv",header = TRUE)
ETH_sent.daily.tot<-ETH_sent.daily.tot[c(4:nrow(ETH_sent.daily.tot)),]

XRP_sent.daily.tot<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/XRP_sent.daily.tot.csv",header = TRUE)

DOGE_sent.daily.tot<-read.csv("E:/Study Materials/Augsburg Hochschule/Bachelor Thesis/R/VADER/DOGE_sent.daily.tot.csv",header = TRUE)

# S2: Logarithmic Price Return --------------------------------------------

#Daily
BTC.log.ret<-data.BTC[,6]
BTC.log.ret<-diff(log(BTC.log.ret$`BTC-USD.Adjusted`))
BTC.log.ret[1,]<-0

ETH.log.ret<-data.ETH[,6]
ETH.log.ret<-diff(log(ETH.log.ret$`ETH-USD.Adjusted`))
ETH.log.ret[1,]<-0

XRP.log.ret<-data.XRP[,6]
XRP.log.ret<-diff(log(XRP.log.ret$`XRP-USD.Adjusted`))
XRP.log.ret[1,]<-0

DOGE.log.ret<-data.DOGE[,6]
DOGE.log.ret<-diff(log(DOGE.log.ret$`DOGE-USD.Adjusted`))
DOGE.log.ret[1,]<-0

daily.log.ret<-cbind(BTC.log.ret,ETH.log.ret,XRP.log.ret,DOGE.log.ret)
colnames(daily.log.ret)<-paste(c("BTC","ETH","XRP","DOGE"))

#Hourly
BTC.log.ret<-xts(BTCUSD_1hr[,7], order.by = as.POSIXct(BTCUSD_1hr$Date), tz="UTC")
BTC.log.ret<-diff(log(BTC.log.ret$Close))
BTC.log.ret[1,]<-0

ETH.log.ret<-xts(ETHUSD_1hr[,7], order.by = as.POSIXct(ETHUSD_1hr$Date), tz="UTC")
ETH.log.ret<-diff(log(ETH.log.ret$Close))
ETH.log.ret[1,]<-0

XRP.log.ret<-xts(XRPUSD_1hr[,7], order.by = as.POSIXct(XRPUSD_1hr$date), tz="UTC")
XRP.log.ret<-diff(log(XRP.log.ret$close))
XRP.log.ret[1,]<-0

DOGE.log.ret<-xts(DOGEUSD_1hr[,7], order.by = as.POSIXct(DOGEUSD_1hr$Date), tz="UTC")
DOGE.log.ret<-diff(log(DOGE.log.ret$Close))
DOGE.log.ret[1,]<-0

hourly.log.ret<-cbind(BTC.log.ret,ETH.log.ret,XRP.log.ret,DOGE.log.ret)
colnames(hourly.log.ret)<-paste(c("BTC","ETH","XRP","DOGE"))
hourly.log.ret

# S3: Changes in Tweet sentiment score --------------------------------------------
#Daily change
BTC.sent.change<-xts(BTC_sent.daily.tot[,2], order.by = as.POSIXct(BTC_sent.daily.tot$date))
colnames(BTC.sent.change)<-paste("BTC.sent")
BTC.sent.change<-Delt(BTC.sent.change$BTC.sent)
BTC.sent.change[1,]<-0

ETH.sent.change<-xts(ETH_sent.daily.tot[,2], order.by = as.POSIXct(ETH_sent.daily.tot$date))
colnames(ETH.sent.change)<-paste("ETH.sent")
ETH.sent.change<-Delt(ETH.sent.change$ETH.sent)
ETH.sent.change[1,]<-0

XRP.sent.change<-xts(XRP_sent.daily.tot[,2], order.by = as.POSIXct(XRP_sent.daily.tot$date))
colnames(XRP.sent.change)<-paste("XRP.sent")
XRP.sent.change<-Delt(XRP.sent.change$XRP.sent)
XRP.sent.change[1,]<-0

DOGE.sent.change<-xts(DOGE_sent.daily.tot[,2], order.by = as.POSIXct(DOGE_sent.daily.tot$date))
colnames(DOGE.sent.change)<-paste("DOGE.sent")
DOGE.sent.change<-Delt(DOGE.sent.change$DOGE.sent)
DOGE.sent.change[1,]<-0

daily.sent.change<-cbind(BTC.sent.change,ETH.sent.change,XRP.sent.change,DOGE.sent.change)
colnames(daily.sent.change)<-paste(c("BTC","ETH","XRP","DOGE"))
daily.sent.change

#Hourly change
BTC.sent.change<-xts(BTC_sent.hourly[,2], order.by = as.POSIXct(BTC_sent.hourly$date))
colnames(BTC.sent.change)<-paste("BTC.sent")
BTC.sent.change<-Delt(BTC.sent.change$BTC.sent)
BTC.sent.change[1,]<-0

ETH.sent.change<-xts(ETH_sent.hourly[,2], order.by = as.POSIXct(ETH_sent.hourly$date))
colnames(ETH.sent.change)<-paste("ETH.sent")
ETH.sent.change<-Delt(ETH.sent.change$ETH.sent)
ETH.sent.change[1,]<-0

XRP.sent.change<-xts(XRP_sent.hourly[,2], order.by = as.POSIXct(XRP_sent.hourly$date))
colnames(XRP.sent.change)<-paste("XRP.sent")
XRP.sent.change<-Delt(XRP.sent.change$XRP.sent)
XRP.sent.change[1,]<-0

DOGE.sent.change<-xts(DOGE_sent.hourly[,2], order.by = as.POSIXct(DOGE_sent.hourly$date))
colnames(DOGE.sent.change)<-paste("DOGE.sent")
DOGE.sent.change<-Delt(DOGE.sent.change$DOGE.sent)
DOGE.sent.change[1,]<-0

hourly.sent.change<-cbind(BTC.sent.change,ETH.sent.change,XRP.sent.change,DOGE.sent.change)
colnames(hourly.sent.change)<-paste(c("BTC","ETH","XRP","DOGE"))
hourly.sent.change

# S4: Changes in Tweets volume --------------------------------------------
#Daily change
BTC.vol.change<-xts(BTC_sent.daily.tot[,3], order.by = as.POSIXct(BTC_sent.daily.tot$date))
colnames(BTC.vol.change)<-paste("BTC.vol")
BTC.vol.change<-Delt(BTC.vol.change$BTC.vol)
BTC.vol.change[1,]<-0

ETH.vol.change<-xts(ETH_sent.daily.tot[,3], order.by = as.POSIXct(ETH_sent.daily.tot$date))
colnames(ETH.vol.change)<-paste("ETH.vol")
ETH.vol.change<-Delt(ETH.vol.change$ETH.vol)
ETH.vol.change[1,]<-0

XRP.vol.change<-xts(XRP_sent.daily.tot[,3], order.by = as.POSIXct(XRP_sent.daily.tot$date))
colnames(XRP.vol.change)<-paste("XRP.vol")
XRP.vol.change<-Delt(XRP.vol.change$XRP.vol)
XRP.vol.change[1,]<-0

DOGE.vol.change<-xts(DOGE_sent.daily.tot[,3], order.by = as.POSIXct(DOGE_sent.daily.tot$date))
colnames(DOGE.vol.change)<-paste("DOGE.vol")
DOGE.vol.change<-Delt(DOGE.vol.change$DOGE.vol)
DOGE.vol.change[1,]<-0

daily.vol.change<-cbind(BTC.vol.change,ETH.vol.change,XRP.vol.change,DOGE.vol.change)
colnames(daily.vol.change)<-paste(c("BTC","ETH","XRP","DOGE"))
daily.vol.change

#Hourly change
BTC.vol.change<-xts(BTC_sent.hourly[,3], order.by = as.POSIXct(BTC_sent.hourly$date))
colnames(BTC.vol.change)<-paste("BTC.vol")
BTC.vol.change<-Delt(BTC.vol.change$BTC.vol)
BTC.vol.change[1,]<-0
colnames(BTC.vol.change)<-paste("BTC.vol")

ETH.vol.change<-xts(ETH_sent.hourly[,3], order.by = as.POSIXct(ETH_sent.hourly$date))
colnames(ETH.vol.change)<-paste("ETH.vol")
ETH.vol.change<-Delt(ETH.vol.change$ETH.vol)
ETH.vol.change[1,]<-0
colnames(ETH.vol.change)<-paste("ETH.vol")

XRP.vol.change<-xts(XRP_sent.hourly[,3], order.by = as.POSIXct(XRP_sent.hourly$date))
colnames(XRP.vol.change)<-paste("XRP.vol")
XRP.vol.change<-Delt(XRP.vol.change$XRP.vol)
XRP.vol.change[1,]<-0
colnames(XRP.vol.change)<-paste("XRP.vol")

DOGE.vol.change<-xts(DOGE_sent.hourly[,3], order.by = as.POSIXct(DOGE_sent.hourly$date))
colnames(DOGE.vol.change)<-paste("DOGE.vol")
DOGE.vol.change<-Delt(DOGE.vol.change$DOGE.vol)
DOGE.vol.change[1,]<-0
colnames(DOGE.vol.change)<-paste("DOGE.vol")

hourly.vol.change<-cbind(BTC.vol.change,ETH.vol.change,XRP.vol.change,DOGE.vol.change)
colnames(hourly.vol.change)<-paste(c("BTC","ETH","XRP","DOGE"))

# PLOTS -------------------------------------------------------------------
##Hourly Cryptos prices --------------------------
p1 <- ggplot(BTCUSD_1hr,aes(x=Date))+
geom_line(aes(y=Close)) +
theme_bw()+
theme(axis.text.x = element_text())+
labs(y="BTC")+
labs(title = "Hourly BTC Price")

p2 <- ggplot(ETHUSD_1hr,aes(x=Date))+
geom_line(aes(y=Close)) +
theme_bw()+
theme(axis.text.x = element_text())+
labs(y="ETH")+
labs(title = "Hourly ETH Price",
     subtitle =paste0(format(min(as.Date(ETHUSD_1hr$Date)), "%d %B %Y"), " to ", format(max(as.Date(ETHUSD_1hr$Date)),"%d %B %Y")))

p3 <- ggplot(XRPUSD_1hr,aes(x=date))+
geom_line(aes(y=close)) +
theme_bw()+
theme(axis.text.x = element_text())+
labs(y="XRP")+
labs(title = "Hourly XRP Price")

p4 <- ggplot(DOGEUSD_1hr,aes(x=Date))+
geom_line(aes(y=Close)) +
theme_bw()+
theme(axis.text.x = element_text())+
labs(y="DOGE")+
labs(title = "Hourly DOGE Price",
     caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3),ggplotGrob(p4), size = "last"))
##Hourly Cryptos volume --------------------------
p1 <- ggplot(BTC_sent.hourly,aes(x=date))+
  geom_line(aes(y=volume,group = 1))+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(y="#BTC")+
  labs(title = "Hourly #BTC Volume",
       subtitle =paste0(format(min(as.Date(BTC_sent.hourly$date)), "%d %B %Y"), " to ", format(max(as.Date(BTC_sent.hourly$date)),"%d %B %Y")))
p1+theme(
  axis.text.x = element_blank(),
  axis.ticks = element_blank())

p2 <- ggplot(ETH_sent.hourly,aes(x=date))+
  geom_line(aes(y=volume,group = 1))+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(y="#ETH")+
  labs(title = "Hourly #ETH Volume",
       subtitle =paste0(format(min(as.Date(ETH_sent.hourly$date)), "%d %B %Y"), " to ", format(max(as.Date(ETH_sent.hourly$date)),"%d %B %Y")))
p2+theme(
  axis.text.x = element_blank(),
  axis.ticks = element_blank())

p3 <- ggplot(XRP_sent.hourly,aes(x=date))+
  geom_line(aes(y=volume,group = 1))+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(y="#XRP")+
  labs(title = "Hourly #XRP Volume",
       subtitle =paste0(format(min(as.Date(XRP_sent.hourly$date)), "%d %B %Y"), " to ", format(max(as.Date(XRP_sent.hourly$date)),"%d %B %Y")))
p3+theme(
  axis.text.x = element_blank(),
  axis.ticks = element_blank())

p4 <- ggplot(DOGE_sent.hourly,aes(x=date))+
  geom_line(aes(y=volume,group = 1))+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(y="#DOGE")+
  labs(title = "Hourly #DOGE Volume",
       subtitle =paste0(format(min(as.Date(DOGE_sent.hourly$date)), "%d %B %Y"), " to ", format(max(as.Date(DOGE_sent.hourly$date)),"%d %B %Y")))
p4+theme(
  axis.text.x = element_blank(),
  axis.ticks = element_blank())

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3),ggplotGrob(p4), size = "last"))
##Hourly Tweets volume change - Sentiment score change - Price change of BTC --------------------------
p1 <- ggplot(hourly.vol.change,aes(x=index(hourly.vol.change)))+
  geom_line(aes(y=BTC), stat="identity", color="black")+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(x="Hours",y="Tweets volume change")+
  labs(title = "Hourly Tweets volume change of #BTC")

p2 <- ggplot(hourly.sent.change, aes(x=index(hourly.sent.change))) + 
  geom_line(aes(y=BTC)) + 
  theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Hours", y = "Sentiment score change")+
  labs(title = "Hourly Sentiment score change of #BTC Tweets")

p3 <- ggplot(hourly.log.ret,aes(x=index(hourly.log.ret))) + 
  geom_line(aes(y=BTC)) +
  theme_bw() +
  labs(x="Hours",y="Prices changes", 
       subtitle =paste0(format(min(as.Date(index(hourly.log.ret))), "%d %B %Y"), " to ", format(max(as.Date(index(hourly.log.ret))),"%d %B %Y")))+
  labs(title = "Hourly Price changes of BTC-USD",caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3), size = "last"))
##Hourly Tweets volume change - Sentiment score change - Price change of ETH --------------------------
p1 <- ggplot(hourly.vol.change,aes(x=index(hourly.vol.change)))+
  geom_line(aes(y=ETH), stat="identity", color="black")+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(x="Hours",y="Tweets volume change")+
  labs(title = "Hourly Tweets volume change of #ETH")

p2 <- ggplot(hourly.sent.change, aes(x=index(hourly.sent.change))) + 
  geom_line(aes(y=ETH)) + 
  theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Hours", y = "Sentiment score change")+
  labs(title = "Hourly Sentiment score change of #ETH Tweets")

p3 <- ggplot(hourly.log.ret,aes(x=index(hourly.log.ret))) + 
  geom_line(aes(y=ETH)) +
  theme_bw() +
  labs(x="Hours",y="Prices changes", 
       subtitle =paste0(format(min(as.Date(index(hourly.log.ret))), "%d %B %Y"), " to ", format(max(as.Date(index(hourly.log.ret))),"%d %B %Y")))+
  labs(title = "Hourly Price changes of ETH-USD",caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3), size = "last"))
##Hourly Tweets volume change - Sentiment score change - Price change of XRP --------------------------
p1 <- ggplot(hourly.vol.change,aes(x=index(hourly.vol.change)))+
  geom_line(aes(y=XRP), stat="identity", color="black")+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(x="Hours",y="Tweets volume change")+
  labs(title = "Hourly Tweets volume change of #XRP")

p2 <- ggplot(hourly.sent.change, aes(x=index(hourly.sent.change))) + 
  geom_line(aes(y=XRP)) + 
  theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Hours", y = "Sentiment score change")+
  labs(title = "Hourly Sentiment score change of #XRP Tweets")

p3 <- ggplot(hourly.log.ret,aes(x=index(hourly.log.ret))) + 
  geom_line(aes(y=XRP)) +
  theme_bw() +
  labs(x="Hours",y="Prices changes", 
       subtitle =paste0(format(min(as.Date(index(hourly.log.ret))), "%d %B %Y"), " to ", format(max(as.Date(index(hourly.log.ret))),"%d %B %Y")))+
  labs(title = "Hourly Price changes of XRP-USD",caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3), size = "last"))
##Hourly Tweets volume change - Sentiment score change - Price change of DOGE --------------------------
p1 <- ggplot(hourly.vol.change,aes(x=index(hourly.vol.change)))+
  geom_line(aes(y=DOGE), stat="identity", color="black")+
  theme_bw()+
  theme(axis.text.x = element_text())+
  labs(x="Hours",y="Tweets volume change")+
  labs(title = "Hourly Tweets volume change of #DOGE")

p2 <- ggplot(hourly.sent.change, aes(x=index(hourly.sent.change))) + 
  geom_line(aes(y=DOGE)) + 
  theme_minimal()+
  theme(axis.text.x = element_text())+
  labs(x = "Hours", y = "Sentiment score change")+
  labs(title = "Hourly Sentiment score change of #DOGE Tweets")

p3 <- ggplot(hourly.log.ret,aes(x=index(hourly.log.ret))) + 
  geom_line(aes(y=DOGE)) +
  theme_bw() +
  labs(x="Hours",y="Prices changes", 
       subtitle =paste0(format(min(as.Date(index(hourly.log.ret))), "%d %B %Y"), " to ", format(max(as.Date(index(hourly.log.ret))),"%d %B %Y")))+
  labs(title = "Hourly Price changes of DOGE-USD",caption = "Data collected from Twitter's REST API via rtweet and www.cryptodatadownload.com")

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3), size = "last"))
##Hourly/Daily Tweets volume change - Sentiment score change - Price change Correlation matrix of BTC --------------------------
#Hourly
hourly.table.corr.mat <- as.data.frame(cbind(hourly.vol.change$BTC,hourly.sent.change$BTC,hourly.log.ret$BTC))
colnames(hourly.table.corr.mat)<-paste(c("vol.change","sent.change","log.ret"))
hourly.table.corr.mat[1,3]<-hourly.table.corr.mat[4,3]
hourly.table.corr.mat[2,3]<-hourly.table.corr.mat[5,3]
hourly.table.corr.mat[3,3]<-hourly.table.corr.mat[6,3]
hourly.table.corr.mat[c(4:108),3]<-hourly.table.corr.mat[c(7:nrow(hourly.table.corr.mat)),3]
hourly.table.corr.mat<-hourly.table.corr.mat[-c(109:111),]
corrplot(cor(hourly.table.corr.mat), method = "number", title = "Hourly Correlation change Matrix #BTC", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

#Daily
BTC.log.ret<-data.BTC[,6]
BTC.log.ret<-diff(log(BTC.log.ret$`BTC-USD.Adjusted`))
BTC.log.ret[1,]<-0
BTC.log.ret<-BTC.log.ret[-5,]
BTC.log.ret

BTC.sent.change<-xts(BTC_sent.daily.tot[,2], order.by = as.POSIXct(BTC_sent.daily.tot$date))
colnames(BTC.sent.change)<-paste("BTC.sent")
BTC.sent.change<-Delt(BTC.sent.change$BTC.sent)
BTC.sent.change[1,]<-0
BTC.sent.change

BTC.vol.change<-xts(BTC_sent.daily.tot[,3], order.by = as.POSIXct(BTC_sent.daily.tot$date))
colnames(BTC.vol.change)<-paste("BTC.vol")
BTC.vol.change<-Delt(BTC.vol.change$BTC.vol)
BTC.vol.change[1,]<-0
BTC.vol.change

daily.table.corr.mat <- as.data.frame(cbind(BTC.vol.change$Delt.1.arithmetic,BTC.sent.change$Delt.1.arithmetic,BTC.log.ret$`BTC-USD.Adjusted`))
daily.table.corr.mat<-na.locf(daily.table.corr.mat)
daily.table.corr.mat<-daily.table.corr.mat[c(-2,-4,-6),]
colnames(daily.table.corr.mat)<-paste(c("vol.change","sent.change","log.ret"))
corrplot(cor(daily.table.corr.mat), method = "number", title = "Daily Correlation change Matrix #BTC", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

##Hourly/Daily Tweets volume change - Sentiment score change - Price change Correlation matrix of ETH --------------------------
#Hourly
hourly.table.corr.mat <- as.data.frame(cbind(hourly.vol.change$ETH,hourly.sent.change$ETH,hourly.log.ret$ETH))
colnames(hourly.table.corr.mat)<-paste(c("vol.change","sent.change","log.ret"))
hourly.table.corr.mat[1,3]<-hourly.table.corr.mat[4,3]
hourly.table.corr.mat[2,3]<-hourly.table.corr.mat[5,3]
hourly.table.corr.mat[3,3]<-hourly.table.corr.mat[6,3]
hourly.table.corr.mat[c(4:108),3]<-hourly.table.corr.mat[c(7:nrow(hourly.table.corr.mat)),3]
hourly.table.corr.mat<-hourly.table.corr.mat[-c(109:111),]
corrplot(cor(hourly.table.corr.mat), method = "number", title = "Hourly Correlation Change Matrix #ETH", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

#Daily
ETH.log.ret<-data.ETH[,6]
ETH.log.ret<-diff(log(ETH.log.ret$`ETH-USD.Adjusted`))
ETH.log.ret[1,]<-0
ETH.log.ret<-ETH.log.ret[-5,]
ETH.log.ret

ETH.sent.change<-xts(ETH_sent.daily.tot[,2], order.by = as.POSIXct(ETH_sent.daily.tot$date))
colnames(ETH.sent.change)<-paste("ETH.sent")
ETH.sent.change<-Delt(ETH.sent.change$ETH.sent)
ETH.sent.change[1,]<-0
ETH.sent.change

ETH.vol.change<-xts(ETH_sent.daily.tot[,3], order.by = as.POSIXct(ETH_sent.daily.tot$date))
colnames(ETH.vol.change)<-paste("ETH.vol")
ETH.vol.change<-Delt(ETH.vol.change$ETH.vol)
ETH.vol.change[1,]<-0
ETH.vol.change

daily.table.corr.mat <- as.data.frame(cbind(ETH.vol.change$Delt.1.arithmetic,ETH.sent.change$Delt.1.arithmetic,ETH.log.ret$`ETH-USD.Adjusted`))
daily.table.corr.mat<-na.locf(daily.table.corr.mat)
daily.table.corr.mat<-daily.table.corr.mat[c(-2,-4,-6),]
colnames(daily.table.corr.mat)<-paste(c("vol.change","sent.change","log.ret"))
corrplot(cor(daily.table.corr.mat), method = "number", title = "Daily Correlation change Matrix #ETH", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")
##Hourly/Daily Tweets volume change - Sentiment score change - Price change Correlation matrix of XRP --------------------------
#Hourly
hourly.table.corr.mat <- as.data.frame(cbind(hourly.vol.change$XRP,hourly.sent.change$XRP,hourly.log.ret$XRP))
colnames(hourly.table.corr.mat)<-paste(c("vol.change","sent.change","log.ret"))
hourly.table.corr.mat[1,3]<-hourly.table.corr.mat[4,3]
hourly.table.corr.mat[2,3]<-hourly.table.corr.mat[5,3]
hourly.table.corr.mat[3,3]<-hourly.table.corr.mat[6,3]
hourly.table.corr.mat[c(4:108),3]<-hourly.table.corr.mat[c(7:nrow(hourly.table.corr.mat)),3]
hourly.table.corr.mat<-hourly.table.corr.mat[-c(109:111),]
corrplot(cor(hourly.table.corr.mat), method = "number", title = "Hourly Correlation Change Matrix #XRP", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

#Daily
XRP.log.ret<-data.XRP[,6]
XRP.log.ret<-diff(log(XRP.log.ret$`XRP-USD.Adjusted`))
XRP.log.ret[1,]<-0
XRP.log.ret<-XRP.log.ret[-5,]
XRP.log.ret

XRP.sent.change<-xts(XRP_sent.daily.tot[,2], order.by = as.POSIXct(XRP_sent.daily.tot$date))
colnames(XRP.sent.change)<-paste("XRP.sent")
XRP.sent.change<-Delt(XRP.sent.change$XRP.sent)
XRP.sent.change[1,]<-0
XRP.sent.change

XRP.vol.change<-xts(XRP_sent.daily.tot[,3], order.by = as.POSIXct(XRP_sent.daily.tot$date))
colnames(XRP.vol.change)<-paste("XRP.vol")
XRP.vol.change<-Delt(XRP.vol.change$XRP.vol)
XRP.vol.change[1,]<-0
XRP.vol.change

daily.table.corr.mat <- as.data.frame(cbind(XRP.vol.change$Delt.1.arithmetic,XRP.sent.change$Delt.1.arithmetic,XRP.log.ret$`XRP-USD.Adjusted`))
daily.table.corr.mat<-na.locf(daily.table.corr.mat)
daily.table.corr.mat<-daily.table.corr.mat[c(-2,-4,-6),]
colnames(daily.table.corr.mat)<-paste(c("vol.change","sent.change","log.ret"))
corrplot(cor(daily.table.corr.mat), method = "number", title = "Daily Correlation change Matrix #XRP", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")
##Hourly/Daily Tweets volume change - Sentiment score change - Price change Correlation matrix of DOGE --------------------------
#Hourly
hourly.table.corr.mat <- as.data.frame(cbind(hourly.vol.change$DOGE,hourly.sent.change$DOGE,hourly.log.ret$DOGE))
colnames(hourly.table.corr.mat)<-paste(c("vol.change","sent.change","log.ret"))
hourly.table.corr.mat[1,3]<-hourly.table.corr.mat[4,3]
hourly.table.corr.mat[2,3]<-hourly.table.corr.mat[5,3]
hourly.table.corr.mat[3,3]<-hourly.table.corr.mat[6,3]
hourly.table.corr.mat[c(4:108),3]<-hourly.table.corr.mat[c(7:nrow(hourly.table.corr.mat)),3]
hourly.table.corr.mat<-hourly.table.corr.mat[-c(109:111),]
corrplot(cor(hourly.table.corr.mat), method = "number", title = "Hourly Correlation Change Matrix #DOGE", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

#Daily
DOGE.log.ret<-data.DOGE[,6]
DOGE.log.ret<-diff(log(DOGE.log.ret$`DOGE-USD.Adjusted`))
DOGE.log.ret[1,]<-0
DOGE.log.ret<-DOGE.log.ret[-5,]
DOGE.log.ret

DOGE.sent.change<-xts(DOGE_sent.daily.tot[,2], order.by = as.POSIXct(DOGE_sent.daily.tot$date))
colnames(DOGE.sent.change)<-paste("DOGE.sent")
DOGE.sent.change<-Delt(DOGE.sent.change$DOGE.sent)
DOGE.sent.change[1,]<-0
DOGE.sent.change

DOGE.vol.change<-xts(DOGE_sent.daily.tot[,3], order.by = as.POSIXct(DOGE_sent.daily.tot$date))
colnames(DOGE.vol.change)<-paste("DOGE.vol")
DOGE.vol.change<-Delt(DOGE.vol.change$DOGE.vol)
DOGE.vol.change[1,]<-0
DOGE.vol.change

daily.table.corr.mat <- as.data.frame(cbind(DOGE.vol.change$Delt.1.arithmetic,DOGE.sent.change$Delt.1.arithmetic,DOGE.log.ret$`DOGE-USD.Adjusted`))
daily.table.corr.mat<-na.locf(daily.table.corr.mat)
daily.table.corr.mat<-daily.table.corr.mat[c(-2,-4,-6),]
colnames(daily.table.corr.mat)<-paste(c("vol.change","sent.change","log.ret"))
corrplot(cor(daily.table.corr.mat), method = "number", title = "Daily Correlation change Matrix #DOGE", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")
##Hourly/Daily Tweets volume - Sentiment score - Price Correlation matrix of BTC --------------------------
#Hourly
BTC.price<-xts(BTCUSD_1hr[,7], order.by = as.POSIXct(BTCUSD_1hr$Date), tz="UTC")
BTC_sent.hourly[c(1:3,nrow(BTC_sent.hourly)),]


hourly.table.corr.mat <- as.data.frame(cbind(BTC_sent.hourly$volume,BTC_sent.hourly$avg.sent,BTC.price$Close))
colnames(hourly.table.corr.mat)<-paste(c("vol","sent","price"))

corrplot(cor(hourly.table.corr.mat), method = "number", title = "Hourly Correlation Matrix #BTC", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")
#Daily
BTC.price<-data.BTC[-5,c(5:6)]
BTC.price
BTC_sent.daily.tot

hourly.table.corr.mat <- as.data.frame(cbind(BTC_sent.daily.tot$Daily.vol,BTC_sent.daily.tot$Daily.sent,BTC.price$`BTC-USD.Adjusted`))
colnames(hourly.table.corr.mat)<-paste(c("vol","sent","price"))

corrplot(cor(hourly.table.corr.mat), method = "number", title = "Daily Correlation Matrix #BTC", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

##Hourly/Daily Tweets volume - Sentiment score - Price Correlation matrix of ETH --------------------------
ETH.price<-xts(ETHUSD_1hr[,7], order.by = as.POSIXct(ETHUSD_1hr$Date), tz="UTC")
ETH_sent.hourly[c(1:3,nrow(ETH_sent.hourly)),]


hourly.table.corr.mat <- as.data.frame(cbind(ETH_sent.hourly$volume,ETH_sent.hourly$avg.sent,ETH.price$Close))
colnames(hourly.table.corr.mat)<-paste(c("vol","sent","price"))

corrplot(cor(hourly.table.corr.mat), method = "number", title = "Hourly Correlation Matrix #ETH", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

#Daily
ETH.price<-data.ETH[-5,c(5:6)]
ETH.price
ETH_sent.daily.tot

hourly.table.corr.mat <- as.data.frame(cbind(ETH_sent.daily.tot$Daily.vol,ETH_sent.daily.tot$Daily.sent,ETH.price$`ETH-USD.Adjusted`))
colnames(hourly.table.corr.mat)<-paste(c("vol","sent","price"))

corrplot(cor(hourly.table.corr.mat), method = "number", title = "Daily Correlation Matrix #ETH", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")
##Hourly/Daily Tweets volume - Sentiment score - Price Correlation matrix of XRP --------------------------
XRP.price<-xts(XRPUSD_1hr[,7], order.by = as.POSIXct(XRPUSD_1hr$date), tz="UTC")
XRP_sent.hourly[c(1:3,nrow(XRP_sent.hourly)),]


hourly.table.corr.mat <- as.data.frame(cbind(XRP_sent.hourly$volume,XRP_sent.hourly$avg.sent,XRP.price$close))
colnames(hourly.table.corr.mat)<-paste(c("vol","sent","price"))

corrplot(cor(hourly.table.corr.mat), method = "number", title = "Hourly Correlation Matrix #XRP", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

#Daily
XRP.price<-data.XRP[-5,c(5:6)]
XRP.price
XRP_sent.daily.tot

hourly.table.corr.mat <- as.data.frame(cbind(XRP_sent.daily.tot$Daily.vol,XRP_sent.daily.tot$Daily.sent,XRP.price$`XRP-USD.Adjusted`))
colnames(hourly.table.corr.mat)<-paste(c("vol","sent","price"))

corrplot(cor(hourly.table.corr.mat), method = "number", title = "Daily Correlation Matrix #XRP", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")
##Hourly/Daily Tweets volume - Sentiment score - Price Correlation matrix of DOGE --------------------------
DOGE.price<-xts(DOGEUSD_1hr[,7], order.by = as.POSIXct(DOGEUSD_1hr$Date), tz="UTC")
DOGE_sent.hourly[c(1:3,nrow(DOGE_sent.hourly)),]


hourly.table.corr.mat <- as.data.frame(cbind(DOGE_sent.hourly$volume,DOGE_sent.hourly$avg.sent,DOGE.price$Close))
colnames(hourly.table.corr.mat)<-paste(c("vol","sent","price"))

corrplot(cor(hourly.table.corr.mat), method = "number", title = "Hourly Correlation Matrix #DOGE", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")

#Daily
DOGE.price<-data.DOGE[-5,c(5:6)]
DOGE.price
DOGE_sent.daily.tot

hourly.table.corr.mat <- as.data.frame(cbind(DOGE_sent.daily.tot$Daily.vol,DOGE_sent.daily.tot$Daily.sent,DOGE.price$`DOGE-USD.Adjusted`))
colnames(hourly.table.corr.mat)<-paste(c("vol","sent","price"))

corrplot(cor(hourly.table.corr.mat), method = "number", title = "Daily Correlation Matrix #DOGE", 
         tl.col = "black", mar=c(0, 0, 1, 5), cl.pos = "b")
##LAG ANALYSIS + CROSS-CORRELATION ANALYSIS of BTC -------------------------------
#Step 1: Get daily & Hourly sentiment & crypto price data
hourly.table<-BTC_sent.hourly
hourly.table<-cbind(hourly.table,BTC.price$Close,BTC.log.ret$Close)
colnames(hourly.table)<-paste(c("Date","Hourly.sent","Hourly.vol","Hourly.close","Hourly.log.ret"))

hourly.table[c(1:3,nrow(hourly.table)),]

#Step 2: convert data to "Time series" format

#Hourly
hourly.table_ts <- ts(data = hourly.table[, c(2,3,5)],
                      start = c(1),
                      end = c(nrow(hourly.table)),
                      frequency=1)

#Step 3: Lag analysis
par(mfrow = c(1, 2))

par(mfrow = c(1, 2))

#Hourly
#Hourly Sentiment Score
# acf Sentiment time series
hourly.table_ts[, c("Hourly.sent")] %>% 
  acf(lag.max = 180, 
      main = "ACF - BTC Hourly Sentiment")

# pacf R time series
hourly.table_ts[, c("Hourly.sent")] %>%
  pacf(lag.max = 180,
       main = "PACF - BTC Hourly Sentiment")

#Hourly Crypto Log ret
# acf Sentiment time series
hourly.table_ts[, c("Hourly.log.ret")] %>% 
  acf(lag.max = 180, 
      main = "ACF - BTC Hourly Log Return")

# pacf R time series
hourly.table_ts[, c("Hourly.log.ret")] %>%
  pacf(lag.max = 180,
       main = "PACF - BTC Hourly Log Return")

#Step 4: Cross-correlation analysis
#Hourly
par(mfrow=c(1,1))
ccf(hourly.table_ts[, c("Hourly.sent")], hourly.table_ts[, c("Hourly.log.ret")], 
    lag.max = 180,
    main = "BTC Cross-Correlation Plot 
    Between Hourly Sentiment Score & Crypto Logarithmic Return",
    ylab = "CCF")

hourly.ccfvalues = ccf(hourly.table_ts[, c("Hourly.sent")], hourly.table_ts[, c("Hourly.log.ret")])
hourly.ccfvalues
#Hourly sentiment score affects significantly positively on later hourly crypto price

#Step 5: Significance test
#Hourly
hourly.cc<-cc.test(hourly.table_ts[, c("Hourly.sent")], hourly.table_ts[, c("Hourly.log.ret")], 
                   max.lag = 107, alpha = 0.05, lambda = 2.576, plot = TRUE,
                   table = TRUE, var.names = NULL)

##LAG ANALYSIS + CROSS-CORRELATION ANALYSIS of ETH -------------------------------
#Step 1: Get daily & Hourly sentiment & crypto price data
hourly.table<-ETH_sent.hourly
hourly.table<-cbind(hourly.table,ETH.price$Close,ETH.log.ret$Close)
colnames(hourly.table)<-paste(c("Date","Hourly.sent","Hourly.vol","Hourly.close","Hourly.log.ret"))

hourly.table[c(1:3,nrow(hourly.table)),]

#Step 2: convert data to "Time series" format

#Hourly
hourly.table_ts <- ts(data = hourly.table[, c(2,3,5)],
                      start = c(1),
                      end = c(nrow(hourly.table)),
                      frequency=1)

#Step 3: Lag analysis
par(mfrow = c(1, 2))

par(mfrow = c(1, 2))

#Hourly
#Hourly Sentiment Score
# acf Sentiment time series
hourly.table_ts[, c("Hourly.sent")] %>% 
  acf(lag.max = 180, 
      main = "ACF - ETH Hourly Sentiment")

# pacf R time series
hourly.table_ts[, c("Hourly.sent")] %>%
  pacf(lag.max = 180,
       main = "PACF - ETH Hourly Sentiment")

#Hourly Crypto Log ret
# acf Sentiment time series
hourly.table_ts[, c("Hourly.log.ret")] %>% 
  acf(lag.max = 180, 
      main = "ACF - ETH Hourly Log Return")

# pacf R time series
hourly.table_ts[, c("Hourly.log.ret")] %>%
  pacf(lag.max = 180,
       main = "PACF - ETH Hourly Log Return")

#Step 4: Cross-correlation analysis
#Hourly
par(mfrow=c(1,1))
ccf(hourly.table_ts[, c("Hourly.sent")], hourly.table_ts[, c("Hourly.log.ret")], 
    lag.max = 180,
    main = "ETH Cross-Correlation Plot 
    Between Hourly Sentiment Score & Crypto Logarithmic Return",
    ylab = "CCF")

hourly.ccfvalues = ccf(hourly.table_ts[, c("Hourly.sent")], hourly.table_ts[, c("Hourly.log.ret")])
hourly.ccfvalues
#Hourly sentiment score affects significantly positively on later hourly crypto price

#Step 5: Significance test
#Hourly
hourly.cc<-cc.test(hourly.table_ts[, c("Hourly.sent")], hourly.table_ts[, c("Hourly.log.ret")], 
                   max.lag = 107, alpha = 0.05, lambda = 2.576, plot = TRUE,
                   table = TRUE, var.names = NULL)
##LAG ANALYSIS + CROSS-CORRELATION ANALYSIS of XRP -------------------------------
#Step 1: Get daily & Hourly sentiment & crypto price data
hourly.table<-XRP_sent.hourly
hourly.table<-cbind(hourly.table,XRP.price$close,XRP.log.ret$close)
colnames(hourly.table)<-paste(c("Date","Hourly.sent","Hourly.vol","Hourly.close","Hourly.log.ret"))

hourly.table[c(1:3,nrow(hourly.table)),]

#Step 2: convert data to "Time series" format

#Hourly
hourly.table_ts <- ts(data = hourly.table[, c(2,3,5)],
                      start = c(1),
                      end = c(nrow(hourly.table)),
                      frequency=1)

#Step 3: Lag analysis
par(mfrow = c(1, 2))

par(mfrow = c(1, 2))

#Hourly
#Hourly Sentiment Score
# acf Sentiment time series
hourly.table_ts[, c("Hourly.sent")] %>% 
  acf(lag.max = 180, 
      main = "ACF - XRP Hourly Sentiment")

# pacf R time series
hourly.table_ts[, c("Hourly.sent")] %>%
  pacf(lag.max = 180,
       main = "PACF - XRP Hourly Sentiment")

#Hourly Crypto Log ret
# acf Sentiment time series
hourly.table_ts[, c("Hourly.log.ret")] %>% 
  acf(lag.max = 180, 
      main = "ACF - XRP Hourly Log Return")

# pacf R time series
hourly.table_ts[, c("Hourly.log.ret")] %>%
  pacf(lag.max = 180,
       main = "PACF - XRP Hourly Log Return")

#Step 4: Cross-correlation analysis
#Hourly
par(mfrow=c(1,1))
ccf(hourly.table_ts[, c("Hourly.sent")], hourly.table_ts[, c("Hourly.log.ret")], 
    lag.max = 180,
    main = "XRP Cross-Correlation Plot 
    Between Hourly Sentiment Score & Crypto Logarithmic Return",
    ylab = "CCF")

hourly.ccfvalues = ccf(hourly.table_ts[, c("Hourly.sent")], hourly.table_ts[, c("Hourly.log.ret")])
hourly.ccfvalues
#Hourly sentiment score affects significantly positively on later hourly crypto price

#Step 5: Significance test
#Hourly
hourly.cc<-cc.test(hourly.table_ts[, c("Hourly.sent")], hourly.table_ts[, c("Hourly.log.ret")], 
                   max.lag = 107, alpha = 0.05, lambda = 2.576, plot = TRUE,
                   table = TRUE, var.names = NULL)
##LAG ANALYSIS + CROSS-CORRELATION ANALYSIS of DOGE -------------------------------
#Step 1: Get daily & Hourly sentiment & crypto price data
hourly.table<-DOGE_sent.hourly
hourly.table<-cbind(hourly.table,DOGE.price$Close,DOGE.log.ret$Close)
colnames(hourly.table)<-paste(c("Date","Hourly.sent","Hourly.vol","Hourly.close","Hourly.log.ret"))

hourly.table[c(1:3,nrow(hourly.table)),]

#Step 2: convert data to "Time series" format

#Hourly
hourly.table_ts <- ts(data = hourly.table[, c(2,3,5)],
                      start = c(1),
                      end = c(nrow(hourly.table)),
                      frequency=1)

#Step 3: Lag analysis
par(mfrow = c(1, 2))

par(mfrow = c(1, 2))

#Hourly
#Hourly Sentiment Score
# acf Sentiment time series
hourly.table_ts[, c("Hourly.sent")] %>% 
  acf(lag.max = 180, 
      main = "ACF - DOGE Hourly Sentiment")

# pacf R time series
hourly.table_ts[, c("Hourly.sent")] %>%
  pacf(lag.max = 180,
       main = "PACF - DOGE Hourly Sentiment")

#Hourly Crypto Log ret
# acf Sentiment time series
hourly.table_ts[, c("Hourly.log.ret")] %>% 
  acf(lag.max = 180, 
      main = "ACF - DOGE Hourly Log Return")

# pacf R time series
hourly.table_ts[, c("Hourly.log.ret")] %>%
  pacf(lag.max = 180,
       main = "PACF - DOGE Hourly Log Return")

#Step 4: Cross-correlation analysis
#Hourly
par(mfrow=c(1,1))
ccf(hourly.table_ts[, c("Hourly.sent")], hourly.table_ts[, c("Hourly.log.ret")], 
    lag.max = 180,
    main = "DOGE Cross-Correlation Plot 
    Between Hourly Sentiment Score & Crypto Logarithmic Return",
    ylab = "CCF")

hourly.ccfvalues = ccf(hourly.table_ts[, c("Hourly.sent")], hourly.table_ts[, c("Hourly.log.ret")])
hourly.ccfvalues
#Hourly sentiment score affects significantly positively on later hourly crypto price

#Step 5: Significance test
#Hourly
hourly.cc<-cc.test(hourly.table_ts[, c("Hourly.sent")], hourly.table_ts[, c("Hourly.log.ret")], 
                   max.lag = 107, alpha = 0.05, lambda = 2.576, plot = TRUE,
                   table = TRUE, var.names = NULL)
