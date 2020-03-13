stock <- fread('stockdata.csv')

head(stock)
summary(stock)
stock <- stock[date>="2000-01-01"]
head(stock)
summary(stock)
n <- nrow(stock)
stock$date <- as.Date(stock$date)

model_naive <- lm(mmm~sp500+djia+date,data=stock)
summary(model_naive)
# Every 1 point increase from SP500 will lead to a 
#    0.016 point increase in MMM share price controlling for the DJIA

models <- list()
models[[1]] <- lm(mmm~sp500+djia,data=stock)
models[[2]] <- lm(mmm~sp500+djia+date,data=stock)
models[[3]] <- lm(diff(mmm)~sp500[2:n]+djia[2:n],data=stock)
models[[4]] <- lm(diff(mmm)~sp500[2:n]+djia[2:n]+date[2:n],data=stock)
models[[5]] <- lm(mmm[2:n]~diff(sp500)+diff(djia),data=stock)
models[[6]] <- lm(mmm[2:n]~diff(sp500)+diff(djia)+date[2:n],data=stock)
models[[7]] <- lm(diff(mmm)~diff(sp500)+diff(djia),data=stock)
models[[8]] <- lm(diff(mmm)~diff(sp500)+diff(djia)+date[2:n],data=stock)
dt <- lapply(models,glance)
dt <- rbindlist(dt)
dt

rep.kpss(stock$mmm)
rep.kpss(stock$sp500)
rep.kpss(stock$djia)

modela <- lm(diff(mmm)~diff(sp500)+diff(djia)+date[2:n],data=stock)
summary(modela)
# Every 1 point increase from SP500 will lead to a 
#    0.034 point DECREASE in MMM share price when the DJIA is flat.

tidynw <- function(model,...)tidyg(model,sandwich::NeweyWest(model),...)
tidynw(modela)

modelb <- lm(diff(log(mmm))~diff(log(sp500))+diff(log(djia)),data=stock)
glance(modelb)
tidynw(modelb)

# Ho: b1+b2=1
#     th1=b1+b2-1 or th1-b2+1=b1
#     gr(mmm)=b0+(th1-b2+1)gr(sp500)+b2 gr(dj)+e
#     gr(mmm)-gr(sp500)=b0+th1 gr(sp500)+b2(gr(dj)-gr(sp500))+e
modelb_v2 <- lm(I(diff(log(mmm))-diff(log(sp500)))~diff(log(sp500))+
                  I(diff(log(djia))-diff(log(sp500))),data=stock)
glance(modelb_v2)
tidynw(modelb_v2)
# Test is insignificant at even the 20% level
#   so if both market indices go up by 1%, 
#   MMM will probably go up by about 1% as well
