#----------------------------Библиотеки------------------------------
install.packages ('Quandl')
install.packages("ggplot2")
install.packages("CombMSC")
install.packages("caret")
install.packages("tseries")
install.packages("forecast")
install.packages("Metrics")
install.packages("car")
install.packages("forecastHybrid")
install.packages("urca")
library(Metrics)
library(Quandl)
library(fBasics)
library(ggplot2)
library(CombMSC)
library(caret)
library(tseries)
library(forecast)
library(car)
library(forecastHybrid)
library(urca)
#----------------------------Входни данни-----------------------------
USDEUR <-(Quandl("CURRFX/USDEUR",start_date="2016-09-01", api_key="znExgNzFuVxpS9TU7_R4")[,1:2])
USDGBP <-(Quandl("CURRFX/USDGBP",start_date="2016-09-01", api_key="znExgNzFuVxpS9TU7_R4")[,1:2])
USDCHF <-(Quandl("CURRFX/USDCHF",start_date="2016-09-01", api_key="znExgNzFuVxpS9TU7_R4")[,1:2])
USDJPY <-(Quandl("CURRFX/USDJPY",start_date="2016-09-01", api_key="znExgNzFuVxpS9TU7_R4")[,1:2])
USDCAD <-(Quandl("CURRFX/USDCAD",start_date="2016-09-01", api_key="znExgNzFuVxpS9TU7_R4")[,1:2])

par(mfrow=c(2,3))
plot(USDEUR)
plot(USDGBP)
plot(USDCHF)
plot(USDJPY)
plot(USDCAD)
MyMerge       <- function(x, y){
  df            <- merge(x, y, by= "Date")
  rownames(df)  <- df$Row.names
  df$Row.names  <- NULL
  return(df)
}

dat <- Reduce(MyMerge, list(USDEUR,USDGBP,USDCHF,USDJPY,USDCAD  ))
colnames (dat)<-c("DATE","USDEUR","USDGBP","USDCHF", "USDJPY","USDCAD")
rm(USDEUR,USDGBP,USDCAD,USDCHF,USDJPY)
dat<-tail(dat,201)
head(dat)

par(mfrow=c(2,3))
plot(density(dat$USDEUR))
plot(density(dat$USDGBP))
plot(density(dat$USDCHF))
plot(density(dat$USDJPY))
plot(density(dat$USDCAD))
#------------------Логаритмична възвръщаемост------------------------------
dat$USDEUR=c(0,diff(log(dat$USDEUR), lag=1))
dat$USDGBP=c(0,diff(log(dat$USDGBP), lag=1))
dat$USDCHF=c(0,diff(log(dat$USDCHF), lag=1))
dat$USDJPY=c(0,diff(log(dat$USDJPY), lag=1))
dat$USDCAD=c(0,diff(log(dat$USDCAD), lag=1))
dat=dat[-1,]
head(dat)

par(mfrow=c(2,3))
plot.ts(dat$USDEUR)
plot.ts(dat$USDGBP)
plot.ts(dat$USDCHF)
plot.ts(dat$USDJPY)
plot.ts(dat$USDCAD)
par(mfrow=c(2,3))
qqPlot(dat$USDEUR)
qqPlot(dat$USDGBP)
qqPlot(dat$USDCHF)
qqPlot(dat$USDJPY)
qqPlot(dat$USDCAD)
pairs(~dat$USDEUR+dat$USDGBP+dat$USDCHF+dat$USDJPY+dat$USDCAD)
#------------------outliars------
dat$USDEUR=tsclean(dat$USDEUR)
dat$USDGBP=tsclean(dat$USDGBP)
dat$USDCHF=tsclean(dat$USDCHF)
dat$USDJPY=tsclean(dat$USDJPY)
dat$USDCAD=tsclean(dat$USDCAD)

plot.ts(dat$USDEUR)
plot.ts(dat$USDGBP)
plot.ts(dat$USDCHF)
plot.ts(dat$USDJPY)
plot.ts(dat$USDCAD)
#-----------------stationary‚-----------------------
adf.test(dat$USDEUR)
adf.test(dat$USDGBP)
adf.test(dat$USDCHF)
adf.test(dat$USDJPY)
adf.test(dat$USDCAD)

par(mfrow=c(2,3))
acf(dat$USDEUR)
acf(dat$USDGBP)
acf(dat$USDCHF)
acf(dat$USDJPY)
acf(dat$USDCAD)

par(mfrow=c(2,3))
pacf(dat$USDEUR)
pacf(dat$USDGBP)
pacf(dat$USDCHF)
pacf(dat$USDJPY)
pacf(dat$USDCAD)

#razpredelenie
shapiro.test(dat$USDEUR)
shapiro.test(dat$USDGBP)
shapiro.test(dat$USDCHF)
shapiro.test(dat$USDJPY)
shapiro.test(dat$USDCAD)
sk<-c(skewness(dat$USDEUR),skewness(dat$USDGBP),skewness(dat$USDCHF),skewness(dat$USDJPY),skewness(dat$USDCAD))
sk
ku<-c(kurtosis(dat$USDEUR),kurtosis(dat$USDGBP),kurtosis(dat$USDCHF),kurtosis(dat$USDJPY),kurtosis(dat$USDCAD))
ku

par(mfrow=c(2,3))
plot(density(dat$USDEUR))
plot(density(dat$USDGBP))
plot(density(dat$USDCHF))
plot(density(dat$USDJPY))
plot(density(dat$USDCAD))

#-----------------split---------------
datTS<-as.ts(dat[,2:6])
train1<-splitTrainTest(datTS[,1], numTrain = length(dat[,1]) - 20)
train2<-splitTrainTest(datTS[,2], numTrain = length(dat[,2]) - 20)
train3<-splitTrainTest(datTS[,3], numTrain = length(dat[,3]) - 20)
train4<-splitTrainTest(datTS[,4], numTrain = length(dat[,4]) - 20)
train5<-splitTrainTest(datTS[,5], numTrain = length(dat[,5]) - 20)
#-----------------Прогноза НН-------------------------------
par(mfrow=c(2,3))
nnt1 <- nnetar(train1$train,4)
plot(forecast(nnt1))
points(1:length(train1$train),fitted(nnt1),type="l",col="green")
nntt1 <- nnetar(train1$test, model=nnt1)


nnt2 <- nnetar(train2$train,4)
plot(forecast(nnt2))
points(1:length(train2$train),fitted(nnt2),type="l",col="green")
nntt2 <- nnetar(train2$test, model=nnt2)

nnt3 <- nnetar(train3$train,4)
plot(forecast(nnt3))
points(1:length(train3$train),fitted(nnt3),type="l",col="green")
nntt3 <- nnetar(train3$test, model=nnt3)

nnt4 <- nnetar(train4$train,4)
plot(forecast(nnt4))
points(1:length(train4$train),fitted(nnt4),type="l",col="green")
nntt4 <- nnetar(train4$test, model=nnt4)

nnt5 <- nnetar(train5$train,4)
plot(forecast(nnt5))
points(1:length(train5$train),fitted(nnt5),type="l",col="green")
nntt5 <- nnetar(train5$test, model=nnt5)

#-------------accuracy----------------------
eur=c(accuracy(nnt1)[2],accuracy(nntt1)[2])
gbp=c(accuracy(nnt2)[2],accuracy(nntt2)[2])
chf=c(accuracy(nnt3)[2],accuracy(nntt3)[2])
jpy=c(accuracy(nnt4)[2],accuracy(nntt4)[2])
cad=c(accuracy(nnt5)[2],accuracy(nntt5)[2])
acc=cbind(eur,gbp,chf,jpy,cad)
row.names(acc)<-c("train","test")
acc
rm(eur,gbp,chf,jpy,cad)
#-----------------РџСЂРѕРіРЅРѕР·Р°-------------------------------------
pr1=forecast (nntt1,h=1)
pr2=forecast (nntt2,h=1)
pr3=forecast (nntt3,h=1)
pr4=forecast (nntt4,h=1)
pr5=forecast (nntt5,h=1)
nc<-c(pr1$mean[1],pr2$mean[1],pr3$mean[1],pr4$mean[1],pr5$mean[1])
rm(pr1,pr2,pr3,pr4,pr5)

eur=tail(dat[2],1)-nc[1]
gbp=(tail(dat[3],1)-nc[2])
chf=(tail(dat[4],1)-nc[3])
jpy=(tail(dat[5],1)-nc[4])
cad=(tail(dat[6],1)-nc[5])
bs<-cbind(eur[1],gbp[1],chf[1],jpy[1],cad[1])
rm(eur,gbp,chf,jpy,cad,nc)
bs
#----------------Algorithm--------------
if(((bs[1]-(max(bs)))>=0)&&((max(bs))>0.00015)) {
  a=("bue usd/eur")
} else  if(((bs[1]-(min(bs)))<=0)&&((min(bs))<(-0.00015))) {
  a=("sell usd/eur")
} else a=("hold usd/eur")

if(((bs[2]-(max(bs)))>=0)&&((max(bs))>0.00015)) {
  b=("bue usd/gbp")
} else  if(((bs[2]-(min(bs)))<=0)&&((min(bs))<(-0.00015))) {
  b=("sell usd/gbp")
} else b=("hold usd/gbp")

if(((bs[3]-(max(bs)))>=0)&&((max(bs))>0.00015)) {
  c=("bue usd/chf")
} else  if(((bs[3]-(min(bs)))<=0)&&((min(bs))<(-0.00015))) {
  c=("sell usd/chf")
} else c=("hold usd/chf")

if(((bs[4]-(max(bs)))>=0)&&((max(bs))>0.00015)) {
  d=("bue usd/jpy")
} else  if(((bs[4]-(min(bs)))<=0)&&((min(bs))<(-0.00015))) {
  d=("sell usd/jpy")
} else d=("hold usd/jpy")

if(((bs[5]-(max(bs)))>=0)&&((max(bs))>0.00015)) {
  e=("bue usd/cad")
} else  if(((bs[5]-(min(bs)))<=0)&&((min(bs))<(-0.00015))) {
  e=("sell usd/cad")
} else e=("hold usd/cad")
 results=c(a,b,c,d,e)
 rm(a,b,c,d,e)
 results
 