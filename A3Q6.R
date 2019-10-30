#Small project on Time series Analysis of bitcoin with respect to S&P500 (SP500), the London bullion market price
#for gold in US dollars (GOLDPMGBD228NLBM), the US/Euro exchange rate (DEXUSEU), and the
#West Texas Intermediate spot price of oil (DCOILWTICO).
head(Mergedatafile)#merged the file, tried alteryx for a change.
names(Mergedatafile)[2]<-"gold"
names(Mergedatafile)[3]<-"bitcoin"
names(Mergedatafile)[4]<-"oil"
names(Mergedatafile)[5]<-"exchange"
head(Mergedatafile)
#(4)
library(ggplot2)
Mergedatafile$Date <- as.Date(Mergedatafile$Date)
plot(Mergedatafile)
#(5)#Naive model without making the model stationary 
model1 <- lm(gold~bitcoin+oil+exchange+SP500+Date, data=Mergedatafile)
model2 <- lm(bitcoin~gold+oil+exchange+SP500+Date, data=Mergedatafile)
model3 <- lm(exchange~bitcoin+oil+gold+SP500+Date, data=Mergedatafile)
model4 <- lm(SP500~bitcoin+oil+exchange+gold+Date, data=Mergedatafile)
summary(model1)
summary(model2)
summary(model3)
summary(model4)
#(6)#defining a function to get the outputs of KPSS in a better way.
library(tseries)
rep.kpss <- function(series){
  for(diffs in 0:5){
    suppressWarnings(
      pval <- kpss.test(series)$p.value
    )
    if(pval>=0.05){return(c(diffs,0,pval))}
    suppressWarnings(
      pval <- kpss.test(series,null="Trend")$p.value
    )
    if(pval>=0.05){return(c(diffs,1,pval))}
    series <- diff(series)
  }
}

kpss.test(Mergedatafile$oil)
kpss.test(Mergedatafile$SP500)
rep.kpss(Mergedatafile$gold) #1 level stationary 
rep.kpss(Mergedatafile$bitcoin) # 1 level stationary
rep.kpss(Mergedatafile$exchange) # 1 level stationary 
rep.kpss(Mergedatafile$SP500)# 1 level stationary
rep.kpss(Mergedatafile$oil) #1 level stationary
#(7)
model2 <- lm(diff(bitcoin)~diff(SP500)+diff(gold)+diff(exchange)+diff(oil), data = Mergedatafile)
summary(model2)
#Not a good model, we don't have much relationship between the variables

#(8) Removed all the data before 2017 where the bitcoin price starts to spike.
Newdata <- Mergedatafile [date>="2017-01-01"]

Newdata <- Mergedatafile %>%
  select(Date, oil, bitcoin, SP500, gold, exchange) %>%
  filter(Date >= as.Date("2017-01-01"))

ts.plot(Newdata$bitcoin, gpars = list(xlab = "day" , ylab = "bitcoin"))
ggplot(Newdata, aes(x=Date,bitcoin)) + geom_line()

#(9) finding acf and pcf
acf(diff(Newdata$bitcoin))
pacf(diff(Newdata$bitcoin))

#(10)
outp1 <- matrix(0L,7^2,3)
row <- 1
for(p in 0:6){
  for(q in 0:6){
    aic <- AIC(arima(log(Newdata$bitcoin),c(p,1,q)))
    outp1[row,] <- c(p,q,aic)
    row <- row + 1
  }
}
order(outp[,3])
outp1[38,]


#(11)
model3 <- stats::arima(log(Newdata$bitcoin),c(5,1,2),seasonal=list(order=c(1,0,1),period=12))
Future <- 30
future <- forecast(model3,h=Future)
plot(future)

#(12)
TSA::periodogram(diff(Newdata$bitcoin))
#There are seasonal peaks in the periodogram as there are periodic changes in the peak.

#(13)
n <- nrow(Newdata)
model416 <- lm(diff(bitcoin)~as.factor(weekdays(as.Date(Newdata$Date)))[2:n],data=Newdata)
TSA::periodogram(residuals(model416))
#The periodogram is exactly the same. There is no seasonality here.

#14
mdata <- cbind(diff(Newdata$bitcoin),diff(Newdata$SP500),diff(Newdata$oil),diff(Newdata$gold),diff(Newdata$exchange))
mdata <- data.table(mdata)
names(mdata) <- c('price','sp500','oil','gold','euro')
mdata <- mdata[complete.cases(mdata)]
AIC(vars::VAR(mdata,p=1))
AIC(vars::VAR(mdata,p=2))
AIC(vars::VAR(mdata,p=3))
AIC(vars::VAR(mdata,p=4))
model5<- vars::VAR(mdata,p=1)
summary(model5)
#sp500 granger causes oil, oil and euro granger cause gold.

#(15)Forecasting the next 30 days of the prices using the VAR model. 
model7 <- Arima(Newdata$bitcoin,c(9,1,2))
fcst <- predict(model5,n.ahead=38)$fcst$bitcoin[,1]
last.value <- tail(Newdata[complete.cases(Newdata),fcst$bitcoin],1)
fcst[1] <- fcst[1]+last.value
varfcst <- c(Newdata[complete.cases(Newdata),"bitcoin"],cumsum(fcst))
arimafcst <- c(Newdata$bitcoin,forecast(model7,h=30))
dt <- data.table(var=varfcst,arima=arimafcst)
n <- nrow(dt)
dt$date <- as.Date('2017-01-01')+c(0:(n-1))
ggplot(melt(dt,id=3,measure=1:2)[date>=as.Date('2019-06-01')],aes(x=date,y=value))+geom_line(aes(color=variable))


