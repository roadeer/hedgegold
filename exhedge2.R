# this is a new version of the code for external hedge of gold
# against exchange rate change
# weights: Oz
# source: FastMarkets (daily)
# Unit: US Dollar and convered into local currencies using daily exchange rate

# Receiving help from: Jonas Klamka; Dr. Guanping Lu
rm(list=ls())
#setwd("C:/Users/lugp/ll Dropbox/Guanping Lu/External Hedge2/R-code")
setwd("D:/R-code")
library(readxl)

gold <- read_excel("allgold.xlsx", 
                      col_types = c("date", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric"))


gold <- na.omit(gold)

library(xts)
dates <- gold$Date
gold <- xts(x=gold[,2:7], order.by=dates)

# load the exchange rate pairs
library(quantmod)
EURUSD <- new.env()
GBPUSD <- new.env()
USDJPY <- new.env()
USDCNY <- new.env()
USDINR <- new.env()
AUDUSD <- new.env()
CADUSD <- new.env()
getSymbols("EURUSD=X", env = EURUSD, from = as.Date("1999-01-04"), to = as.Date("2021-05-17"),src='yahoo')
getSymbols("GBPUSD=X", env = GBPUSD, from = as.Date("1999-01-04"), to = as.Date("2021-05-17"),src='yahoo')
getSymbols("AUDUSD=X", env = AUDUSD, from = as.Date("1999-01-04"), to = as.Date("2021-05-17"),src='yahoo')
getSymbols("CADUSD=X", env = CADUSD, from = as.Date("1999-01-04"), to = as.Date("2021-05-17"),src='yahoo')
getSymbols("JPY=X", env = USDJPY, from = as.Date("1999-01-04"), to = as.Date("2021-05-17"),src='yahoo')
getSymbols("CNY=X", env = USDCNY, from = as.Date("1999-01-04"), to = as.Date("2021-05-17"),src='yahoo')
getSymbols("INR=X", env = USDINR, from = as.Date("1999-01-04"), to = as.Date("2021-05-17"),src='yahoo')






# create currency pairs 
# EUR
eurusd <- EURUSD$`EURUSD=X`$`EURUSD=X.Close`

# GBP
gbpusd <- GBPUSD$`GBPUSD=X`$`GBPUSD=X.Close`

# AUD
audusd <- AUDUSD$`AUDUSD=X`$`AUDUSD=X.Close`

# CAD
cadusd <- CADUSD$`CADUSD=X`$`CADUSD=X.Close`

# JPY
usdjpy <- USDJPY$`JPY=X`$`JPY=X.Close`
jpyusd <- 1/usdjpy

# CNY
usdcny <- USDCNY$`CNY=X`$`CNY=X.Close`
cnyusd <- 1/usdcny

# INR
usdinr <- USDINR$`INR=X`$`INR=X.Close`
inrusd <- 1/usdinr

# combine the data together
EXP <- merge(eurusd, gbpusd, join='right')
EXP <- merge(EXP, audusd, join='right')
EXP <- merge(EXP, cadusd, join='right')
EXP <- merge(EXP, jpyusd, join='right')
EXP <- merge(EXP, cnyusd, join='right')
EXP <- merge(EXP, inrusd, join='right')


EXP <- na.omit(EXP)
colnames(EXP) <- c("EUR","GBP","AUD","CAD","JPY","CNY","INR")

DF <- merge(EXP, gold, join='left')
DF <- na.omit(DF)
colnames(DF) <- c("EUR","GBP","AUD","CAD","JPY","CNY","INR","G_USD","G_EUR","G_JPY","G_GBP","G_CNY","G_INR")
#DF[160,3] <- DF[159,3]
#DF[1320,6] <- DF[1319,6]
DF[643,1] <- DF[642,1]
DF[600,1] <- DF[599,1]
DF[600,5] <- DF[599,5]
DF[1320,6] <- DF[1319,6]
DF[160,3] <- DF[159,3]
DF[643,5] <- DF[642,5]
write.table(DF,"DF1.csv",row.names=FALSE,col.names=TRUE,sep=",")
# create differenced logged return
# log for gold price
lg_usd <- log(gold$USD)
lg_eur <- log(gold$EUR)
lg_gbp <- log(gold$GBP)
lg_jpy <- log(gold$JPY)
lg_cny <- log(gold$CNY)
lg_inr <- log(gold$INR)

# log for exchange rate
leur <- log(DF$EUR)
lgbp <- log(DF$GBP)
laud <- log(DF$AUD)
lcad <- log(DF$CAD)
ljpy <- log(DF$JPY)
lcny <- log(DF$CNY)
linr <- log(DF$INR)

# create difference logged return
# diff for gold price
dg_usd <- diff(lg_usd)
dg_eur <- diff(lg_eur)
dg_gbp <- diff(lg_gbp)
dg_jpy <- diff(lg_jpy)
dg_cny <- diff(lg_cny)
dg_inr <- diff(lg_inr)

# diff for exchange rate
deur <- diff(leur)
dgbp <- diff(lgbp)
daud <- diff(laud)
dcad <- diff(lcad)
djpy <- diff(ljpy)
dcny <- diff(lcny)
dinr <- diff(linr)

# merge the diff-log data frame
exdata <- merge(dg_usd, dg_eur, join='right')
exdata <- merge(exdata, dg_gbp, join='right')
exdata <- merge(exdata, dg_jpy, join='right')
exdata <- merge(exdata, dg_cny, join='right')
exdata <- merge(exdata, dg_inr, join='right')

exdata <- merge(exdata, deur, join='right')
exdata <- merge(exdata, dgbp, join='right')
exdata <- merge(exdata, daud, join='right')
exdata <- merge(exdata, dcad, join='right')
exdata <- merge(exdata, djpy, join='right')
exdata <- merge(exdata, dcny, join='right')
exdata <- merge(exdata, dinr, join='right')

colnames(exdata) <- c("GUSD","GEUR","GGBP","GJPY","GCNY","GINR","eur","gbp","aud","cad","jpy","cny","inr")
exdata <- na.omit(exdata)

# abnormal flutuation for AUD 2006-12-25
# for CNY flutuation 2011-07-18

write.table(exdata,"exdata1.csv",row.names=FALSE,col.names=TRUE,sep=",")

save(exdata,file="exdata.Rda")

load("exdata.Rda")

# adl mean model
library(fBasics)
basicStats(exdata)
library(tseries)

# test the lag order
library(tsDyn)

lags.select(exdata[,1], lag.max = 40, include = c("both"), 
            fitMeasure = c("LL"), sameSample = TRUE)

par(mfrow=c(2,4))

### with own currency
ccf_UE <- ccf(drop(na.omit(exdata$GEUR)), drop(na.omit(exdata$eur))) 
ccf_UE <- ccf(drop(na.omit(exdata$GGBP)), drop(na.omit(exdata$gbp))) 
ccf_UE <- ccf(drop(na.omit(exdata$GJPY)), drop(na.omit(exdata$jpy))) 
ccf_UE <- ccf(drop(na.omit(exdata$GCNY)), drop(na.omit(exdata$cny))) 
ccf_UE <- ccf(drop(na.omit(exdata$GINR)), drop(na.omit(exdata$inr))) 

## with gold priced in usd
ccf_UE <- ccf(drop(na.omit(exdata$GUSD)), drop(na.omit(exdata$eur))) 
ccf_UE <- ccf(drop(na.omit(exdata$GUSD)), drop(na.omit(exdata$gbp))) 
ccf_UE <- ccf(drop(na.omit(exdata$GUSD)), drop(na.omit(exdata$aud))) 
ccf_UE <- ccf(drop(na.omit(exdata$GUSD)), drop(na.omit(exdata$cad))) 
ccf_UE <- ccf(drop(na.omit(exdata$GUSD)), drop(na.omit(exdata$jpy))) 
ccf_UE <- ccf(drop(na.omit(exdata$GUSD)), drop(na.omit(exdata$cny))) 
ccf_UE <- ccf(drop(na.omit(exdata$GUSD)), drop(na.omit(exdata$inr))) 


# plot the trend
#################################
# start from here #
load("data.DF")
library(zoo)

par(mfrow=c(2,4))


plot(as.zoo(DF$G_USD), las=1, col="orange", xlab="time", ylab="price")
legend("topright", inset=0,
       legend=c("Gold in USD"), 
       col=c("orange"),
       lty=1,              
       cex=1) 


plot(as.zoo(DF$G_USD), las=1, col="orange", xaxt="n", xlab="time", ylab="price")
par(new=TRUE)    
plot(as.zoo(DF$EUR), col="blue", bty='n', yaxt="n",  xlab="", ylab="")
axis(4, las=0.5, col.ticks="blue")
legend("topright", inset=0,
       legend=c("Gold in USD","EUR/USD"), 
       col=c("orange","blue"),
       lty=1,              
       cex=1) 

####
plot(as.zoo(DF$G_USD), las=1, col="orange", xaxt="n", xlab="time", ylab="price")
par(new=TRUE)    
plot(as.zoo(DF$GBP), col="darkgreen", bty='n', yaxt="n",  xlab="", ylab="")
axis(4, las=0.5, col.ticks="darkgreen")
legend("topright", inset=0,
       legend=c("Gold in USD","GBP/USD"), 
       col=c("orange","darkgreen"),
       lty=1,              
       cex=1) 

###
plot(as.zoo(DF$G_USD), las=1, col="orange", xaxt="n", xlab="time", ylab="price")
par(new=TRUE)    
plot(as.zoo(DF$AUD), col="blue4", bty='n', yaxt="n",  xlab="", ylab="")
axis(4, las=0.5, col.ticks="blue4")
legend("topright", inset=0,
       legend=c("Gold in USD","AUD/USD"), 
       col=c("orange","blue4"),
       lty=1,              
       cex=1) 


###
plot(as.zoo(DF$G_USD), las=1, col="orange", xaxt="n", xlab="time", ylab="price")
par(new=TRUE)    
plot(as.zoo(DF$CAD), col="cornflowerblue", bty='n', yaxt="n",  xlab="", ylab="")
axis(4, las=0.5, col.ticks="cornflowerblue")
legend("topright", inset=0,
       legend=c("Gold in USD","CAD/USD"), 
       col=c("orange","cornflowerblue"),
       lty=1,              
       cex=1) 

###
plot(as.zoo(DF$G_USD), las=1, col="orange", xaxt="n", xlab="time", ylab="price")
par(new=TRUE)    
plot(as.zoo(DF$JPY), col="cadetblue", bty='n', yaxt="n",  xlab="", ylab="")
axis(4, las=0.5, col.ticks="cadetblue")
legend("topright", inset=0,
       legend=c("Gold in USD","JPY/USD"), 
       col=c("orange","cadetblue"),
       lty=1,              
       cex=1) 

###
plot(as.zoo(DF$G_USD), las=1, col="orange", xaxt="n", xlab="time", ylab="price")
par(new=TRUE)    
plot(as.zoo(DF$CNY), col="red", bty='n', yaxt="n",  xlab="", ylab="")
axis(4, las=0.5, col.ticks="red")
legend("bottomright", inset=0,
       legend=c("Gold in USD","CNY/USD"), 
       col=c("orange","red"),
       lty=1,              
       cex=1) 


###
plot(as.zoo(DF$G_USD), las=1, col="orange", xaxt="n", xlab="year", ylab="price")
par(new=TRUE)    
plot(as.zoo(DF$INR), col="chartreuse4", bty='n', yaxt="n",  xlab="", ylab="")
axis(4, las=0.5, col.ticks="chartreuse4", ylab="exchange rate")
legend("topright", inset=0,
       legend=c("Gold in USD","INR/USD"), 
       col=c("orange","chartreuse4"),
       lty=1,              
       cex=1) 

################################################################################################################################

#start fitting the mean model
library(dynamac)
library(fGarch)
library(rugarch)

load("data.ex")

# EUR
adl_eur <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2) + exdata$eur + lshift(exdata$eur,1) + lshift(exdata$eur,2))
summary(adl_eur) 

err_eur <- (exdata$GUSD - (-0.0439815)*lshift(exdata$GUSD,1) - (-0.0431315)*lshift(exdata$GUSD,2)  - (0.4272690 )*exdata$eur 
           - (0.4151569)*lshift(exdata$eur, 1)
           - (0.2358517)*lshift(exdata$eur, 2))

plot(as.numeric(err_eur), type='l', col="blue")

spec_eur <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(0,0),
                                      include.mean=FALSE),
                      distribution.model = "sstd")
rugarch_eur <- ugarchfit(spec_eur, err_eur)

rugarch_eur

plot(rugarch_eur, which="all")

# GBP
adl_gbp <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
              + exdata$gbp + lshift(exdata$gbp,1) +lshift(exdata$gbp,2) )
summary(adl_gbp) 
adl_gbp$residuals

err_gbp <- exdata$GUSD 
- (-0.0258180)*lshift(exdata$GUSD,1)
- (-0.0341509 )*lshift(exdata$GUSD,2) 
- (0.3802617 )*exdata$gbp
- (0.3619394)*lshift(exdata$gbp, 1)

plot(as.numeric(adl_gbp$residuals), type='l', col="darkgreen")

spec_gbp <- ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0),
                                       include.mean=FALSE),
                       distribution.model = "sstd")
rugarch_gbp <- ugarchfit(spec_gbp, err_gbp)
rugarch_gbp

plot(rugarch_gbp, which="all")

# AUD
adl_aud <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
              +exdata$aud+lshift(exdata$aud,1)+lshift(exdata$aud,2))
summary(adl_aud)


# CAD
adl_cad <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
              +exdata$cad+lshift(exdata$cad,1)+lshift(exdata$cad,2))
summary(adl_cad)


# JPY
adl_jpy <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
              +exdata$jpy+lshift(exdata$jpy,1)+lshift(exdata$jpy,2))
summary(adl_jpy)


# CNY
adl_cny <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
              + exdata$cny + lshift(exdata$cny,1)+lshift(exdata$cny,2))

summary(adl_cny)

# INR
adl_inr <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
              + exdata$inr + lshift(exdata$inr,1)+lshift(exdata$inr,2))
summary(adl_inr)


# draw the distribution

par(mfrow=c(2,4))


hist(exdata$GUSD, breaks=101, col="orange", main="Gold in USD", xlab="percentage change", ylab="occurrence")
hist(exdata$eur, breaks=101, col="blue", main="EUR/USD", xlab="percentage change", ylab="occurrence")
hist(exdata$gbp, breaks=101, col="darkgreen", main="GBP/USD", xlab="percentage change", ylab="occurrence")
hist(exdata$aud, breaks=101, col="blue4", main="AUD/USD", xlab="percentage change", ylab="occurrence")



hist(exdata$cad, breaks=101, col="cornflowerblue", main="CAD/USD", xlab="percentage change", ylab="occurrence")
hist(exdata$jpy, breaks=101, col="cadetblue", main="JPY/USD", xlab="percentage change", ylab="occurrence")
hist(exdata$cny, breaks=101, col="red", main="CNY/USD", xlab="percentage change", ylab="occurrence")
hist(exdata$inr, breaks=101, col="chartreuse4", main="INR/USD", xlab="percentage change", ylab="occurrence")



# cross correlation function
par(mfrow=c(2,4))

ccf_gold_eur <- ccf(drop(exdata$GUSD), drop(exdata$GUSD), lag.max = 8, main="auto-correlation gold") 
ccf_gold_eur <- ccf(drop(exdata$GUSD), drop(exdata$eur), lag.max = 8,main="cross-correlation gold&eur") 
ccf_gold_gbp <- ccf(drop(exdata$GUSD), drop(exdata$gbp),lag.max = 8, main="cross-correlation gold&gbp") 
ccf_gold_aud <- ccf(drop(exdata$GUSD), drop(exdata$aud), lag.max = 8,main="cross-correlation gold&aud")

ccf_gold_cad <- ccf(drop(exdata$GUSD), drop(exdata$cad), lag.max = 8, main="cross-correlation gold&cad") 
ccf_gold_jpy <- ccf(drop(exdata$GUSD), drop(exdata$jpy), lag.max = 8, main="cross-correlation gold&jpy") 
ccf_gold_cny <- ccf(drop(exdata$GUSD), drop(exdata$cny), lag.max = 8, main="cross-correlation gold&cny") 
ccf_gold_inr <- ccf(drop(exdata$GUSD), drop(exdata$inr), lag.max = 8, main="cross-correlation gold&inr") 


#########
library(stargazer)

exdata <- exdata[,c(1,7,8,9,10,11,12,13)]

library(stats)
stargazer(stats(exdata))
stargazer(basicStats(exdata))

out<-basicStats(data)

stargazer(data)
 
data <-  data.frame(date=index(exdata), coredata(exdata))

sapply(exdata,skewness)
sapply(exdata,kurtosis)


##
# stargazer
stargazer(adl_eur, adl_gbp, adl_aud, adl_cad, adl_jpy, adl_cny, adl_inr)

# inverse estimation

# redo the garch part using residual
par(mfrow=c(4,2))

plot(as.numeric(exdata$GUSD),        xlab="entries",ylab="", main="gold", type='l', col="orange")
plot(as.numeric(adl_eur$residuals),  xlab="entries",ylab="", main="residual eur", type='l', col="blue")
plot(as.numeric(adl_gbp$residuals),  xlab="entries",ylab="", main="residual gbp",type='l', col="darkgreen")
plot(as.numeric(adl_aud$residuals),  xlab="entries",ylab="", main="residual aud",type='l', col="blue4")
plot(as.numeric(adl_cad$residuals),  xlab="entries",ylab="", main="residual cad",type='l', col="cornflowerblue")

plot(as.numeric(adl_jpy$residuals), type='l',  xlab="entries",ylab="", main="residual jpy", col="cadetblue")
plot(as.numeric(adl_cny$residuals), type='l',  xlab="entries",ylab="", main="residual cny",col="red")
plot(as.numeric(adl_inr$residuals), type='l',  xlab="entries",ylab="", main="residual inr",col="chartreuse4")

# fit the garch model




res <- merge(adl_eur$residuals,adl_gbp$residuals,adl_aud$residuals,adl_cad$residuals,
             adl_jpy$residuals,adl_cny$residuals,adl_inr$residuals)

colnames(res) <- c("eur","gbp","aud","cad","jpy","cny","inr")

garchfit <- merge(adl_eur$residuals,adl_gbp$residuals,adl_aud$residuals,adl_cad$residuals,
             adl_jpy$residuals,adl_cny$residuals,adl_inr$residuals)


spec <- ugarchspec(variance.model=list(model="iGARCH",garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(0,0),
                                   include.mean=FALSE),
                   distribution.model = "ged")


rugarch_eur <- ugarchfit(spec, adl_eur$residuals)
rugarch_eur

rugarch_gbp <- ugarchfit(spec, adl_gbp$residuals)
rugarch_gbp 

rugarch_aud <- ugarchfit(spec, adl_aud$residuals)
rugarch_aud 

rugarch_cad <- ugarchfit(spec, adl_cad$residuals)
rugarch_cad

rugarch_jpy <- ugarchfit(spec, adl_jpy$residuals)
rugarch_jpy

rugarch_cny <- ugarchfit(spec, adl_cny$residuals)
rugarch_cny

rugarch_inr <- ugarchfit(spec, adl_inr$residuals)
rugarch_inr

# quantile distributed regression

##############################################################################
# Symmtric case

# 5%, q=0.05
data005 <- exdata

quantile(exdata$eur, probs=seq(0, 1, 0.05), na.rm=FALSE)
quantile(exdata$gbp, probs=seq(0, 1, 0.05), na.rm=FALSE)
quantile(exdata$aud, probs=seq(0, 1, 0.05), na.rm=FALSE)
quantile(exdata$cad, probs=seq(0, 1, 0.05), na.rm=FALSE)

quantile(exdata$jpy, probs=seq(0, 1, 0.05), na.rm=FALSE)
quantile(exdata$cny, probs=seq(0, 1, 0.05), na.rm=FALSE)
quantile(exdata$inr, probs=seq(0, 1, 0.05), na.rm=FALSE)

q005 <-  c(-0.0056856686,-5.744481e-03, -0.0055829574, -4.415096e-03,-9.960570e-05, -3.648931e-04 , -1.514724e-04, 
          0.0054198181, 5.735503e-03,  0.0053331293,  4.554219e-03, 1.015173e-04,  3.283070e-04,   1.371203e-04)

for(i in 1:7){
  data005[,i+1] =ifelse(q005[i] < (data005[,i+1])  & (data005[,i+1]) < q005[i+7] , 0 ,data005[,i])
  }

# 2.5%, q=0.025
data0025 <- exdata

quantile(exdata$eur, probs=seq(0, 1, 0.025), na.rm=FALSE)
quantile(exdata$gbp, probs=seq(0, 1, 0.025), na.rm=FALSE)
quantile(exdata$aud, probs=seq(0, 1, 0.025), na.rm=FALSE)
quantile(exdata$cad, probs=seq(0, 1, 0.025), na.rm=FALSE)

quantile(exdata$jpy, probs=seq(0, 1, 0.025), na.rm=FALSE)
quantile(exdata$cny, probs=seq(0, 1, 0.025), na.rm=FALSE)
quantile(exdata$inr, probs=seq(0, 1, 0.025), na.rm=FALSE)

q0025 <- c(-0.0074477103, -7.174608e-03, -7.586412e-03, -6.199855e-03, -1.251268e-04, -5.170425e-04, -2.013179e-04,
            0.0067447796,  6.974719e-03,  6.910927e-03,  5.744776e-03,  1.298031e-04,  4.790441e-04, 1.832770e-04 )

for(i in 1:7){
  data0025[,i+1] =ifelse(q0025[i] < (data0025[,i+1])  & (data0025[,i]) < q0025[i+7] , 0 ,data0025[,i+1])
}

# 1%, q=0.01
data001 <- exdata

quantile(exdata$eur, probs=seq(0, 1, 0.01), na.rm=FALSE)
quantile(exdata$gbp, probs=seq(0, 1, 0.01), na.rm=FALSE)
quantile(exdata$aud, probs=seq(0, 1, 0.01), na.rm=FALSE)
quantile(exdata$cad, probs=seq(0, 1, 0.01), na.rm=FALSE)

quantile(exdata$jpy, probs=seq(0, 1, 0.01), na.rm=FALSE)
quantile(exdata$cny, probs=seq(0, 1, 0.01), na.rm=FALSE)
quantile(exdata$inr, probs=seq(0, 1, 0.01), na.rm=FALSE)


q001 <- c(-0.0098654441, -1.013214e-02, -1.038567e-02, -8.439304e-03, -1.828041e-04, -7.837858e-04, -2.945343e-04,
           0.0096481304,  9.048319e-03,  9.152417e-03,  7.494048e-03,  1.871665e-04,  7.382041e-04, 2.561384e-04 
)

for(i in 1:7){
  data001[,i+1] =ifelse(q001[i] < (data001[,i+1])  & (data001[,i]) < q001[i+7] , 0 ,data001[,i+1])
}

# quantile regression
# EUR
qdl_eur005 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2) 
              + exdata$eur + lshift(exdata$eur,1) + lshift(exdata$eur,2)
              + data005$eur)
summary(qdl_eur005) 

qdl_eur0025 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2) 
                 + exdata$eur + lshift(exdata$eur,1) + lshift(exdata$eur,2)
                 + data0025$eur)
summary(qdl_eur0025) 

qdl_eur001 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2) 
                  + exdata$eur + lshift(exdata$eur,1) + lshift(exdata$eur,2)
                  + data001$eur)
summary(qdl_eur001) 


# GBP
qdl_gbp005 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
              + exdata$gbp + lshift(exdata$gbp,1) +lshift(exdata$gbp,2)
              + data005$gbp)
summary(qdl_gbp005) 

qdl_gbp0025 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
                 + exdata$gbp + lshift(exdata$gbp,1) +lshift(exdata$gbp,2)
                 + data0025$gbp)
summary(qdl_gbp0025) 

qdl_gbp001 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
                  + exdata$gbp + lshift(exdata$gbp,1) +lshift(exdata$gbp,2)
                  + data001$gbp)
summary(qdl_gbp001) 

# AUD
qdl_aud005 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
              +exdata$aud+lshift(exdata$aud,1)+lshift(exdata$aud,2)
              +data005$aud)
summary(qdl_aud005)

qdl_aud0025 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
                 +exdata$aud+lshift(exdata$aud,1)+lshift(exdata$aud,2)
                 +data0025$aud)
summary(qdl_aud0025)

qdl_aud001 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
                 +exdata$aud+lshift(exdata$aud,1)+lshift(exdata$aud,2)
                 +data001$aud)
summary(qdl_aud001)

# CAD
qdl_cad005 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
              +exdata$cad+lshift(exdata$cad,1)+lshift(exdata$cad,2)
              +data005$cad)
summary(qdl_cad005)

qdl_cad0025 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
                 +exdata$cad+lshift(exdata$cad,1)+lshift(exdata$cad,2)
                 +data0025$cad)
summary(qdl_cad0025)

qdl_cad001 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
                  +exdata$cad+lshift(exdata$cad,1)+lshift(exdata$cad,2)
                  +data001$cad)
summary(qdl_cad001)


# JPY
qdl_jpy005 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
              +exdata$jpy+lshift(exdata$jpy,1)+lshift(exdata$jpy,2)
              +data005$jpy)
summary(qdl_jpy005)


qdl_jpy0025 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
                 +exdata$jpy+lshift(exdata$jpy,1)+lshift(exdata$jpy,2)
                 +data0025$jpy)
summary(qdl_jpy0025)


qdl_jpy001 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
                  +exdata$jpy+lshift(exdata$jpy,1)+lshift(exdata$jpy,2)
                  +data001$jpy)
summary(qdl_jpy001)


# CNY
qdl_cny005 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
              + exdata$cny + lshift(exdata$cny,1)+lshift(exdata$cny,2)
              +data005$cny)
summary(qdl_cny005)

qdl_cny0025 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
                 + exdata$cny + lshift(exdata$cny,1)+lshift(exdata$cny,2)
                 +data0025$cny)
summary(qdl_cny0025)

qdl_cny001 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
                  + exdata$cny + lshift(exdata$cny,1)+lshift(exdata$cny,2)
                  +data001$cny)
summary(qdl_cny001)

# INR
qdl_inr005 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
              + exdata$inr + lshift(exdata$inr,1)+lshift(exdata$inr,2)
              + data005$inr)
summary(qdl_inr005)


qdl_inr0025 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
                 + exdata$inr + lshift(exdata$inr,1)+lshift(exdata$inr,2)
                 + data0025$inr)
summary(qdl_inr0025)

qdl_inr001 <- lm(exdata$GUSD ~ lshift(exdata$GUSD,1) + lshift(exdata$GUSD,2)
                  + exdata$inr + lshift(exdata$inr,1)+lshift(exdata$inr,2)
                  + data001$inr)
summary(qdl_inr001)

############
# generate the latex tables

stargazer(qdl_eur005, qdl_eur0025, qdl_eur001)
stargazer(qdl_gbp005, qdl_gbp0025, qdl_gbp001)
stargazer(qdl_aud005, qdl_aud0025, qdl_aud001)
stargazer(qdl_cad005, qdl_cad0025, qdl_cad001)

stargazer(qdl_jpy005, qdl_jpy0025, qdl_jpy001)
stargazer(qdl_cny005, qdl_cny0025, qdl_cny001)
stargazer(qdl_inr005, qdl_inr0025, qdl_inr001)

##############################################################################
# Asymmtric case

pdata005 <- exdata

ndata005 <- exdata

pq005 <- c( 0.0054198181, 5.735503e-03,  0.0053331293,  4.554219e-03, 1.015173e-04,  3.283070e-04,   1.371203e-04)

nq005 <- c(-0.0056856686,-5.744481e-03, -0.0055829574, -4.415096e-03,-9.960570e-05, -3.648931e-04 , -1.514724e-04)

for(i in 1:7){
  pdata005[,i] =ifelse( (pdata005[,i]) < pq005[i] , 0 ,pdata005[,i])
}



#######################
# density plot for intuitivly explain the extreme quantile

d <- density(exdata$GUSD)
plot(d, main="Density Plot of EUR/USD")
abline(v=0.05, col="red")
abline(v=-0.05, col="red")
polygon(d, col="red", border="blue")

qqnorm(exdata$eur, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)

