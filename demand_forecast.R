library(lubridate)
require(bsts)
library(dplyr)
library(ggplot2)
library(forecast)
require(xts)

# Get data
setwd('C:\\Users\\dmoxley\\Documents\\GitHub\\forecasting')
df <- read.csv('demand.csv') 

typ <- df[df$store==1 & df$item==1,c('date','sales')]
names(typ) <- c('date','demand')

### Creat sample data
demand <- c(0,1,0,0,0,0,5,0,0,0,0,0,2,0,0,0,0,0,4,0,0,0,0,0,0,3,0,0,1,0,0,0,0,0,1,2)
int <- sample(dat, replace = TRUE, size=1826) 


##    Create TS   ##
# set date
date <- seq.Date(as.Date(min(typ$date)),as.Date(max(typ$date)),by="day")
int <- as.data.frame(cbind(dates,int))
names(int) <- c('date','demand')

# create TS
ts_int <- xts(int$demand, order.by=as.Date(int$date))
ts_typ <- xts(typ$demand, order.by=as.Date(typ$date))


# Visualize
plot(demand
     ,xlab='Months'
     ,main='Engineered Part Demand'
     ,col='navy'
     ,type='p')

typ <- c(3,0,1,2)
typ <- sample(typ, replace = TRUE, size=35) 
plot(typ
     ,xlab='Months'
     ,main='Engineered Part Demand'
     ,col='navy'
     ,type='p')


### Fit the ARIMA model
arima <- arima(log(ts_int)
               ,order=c(0, 1, 1)
               ,seasonal=list(order=c(0,1,1), period=12))
arima <- auto.arima(log(ts_typ))

length(union(exp(fitted(arima)), exp(predict(arima, n.ahead = 90)$pred)))
### Actual versus predicted
d1 <- data.frame(union(exp(fitted(arima)), exp(predict(arima, n.ahead = 90)$pred))
                 , df_1$demand
                 , as.Date(df_1$date))

names(d1) <- c("fitted", "actual", "date")


### MAPE (mean absolute percentage error)
mape <- d1 %>% summarise(MAPE=mean(abs(actual-fitted)/actual))
mape

### Plot actual versus predicted
ggplot(data=d1, aes(x=date)) +
  geom_line(aes(y=actual, colour = "Actual"), size=.5) +
  geom_line(aes(y=fitted, colour = "Fitted"), size=.5, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("") + xlab("") +
  geom_vline(xintercept=as.numeric(as.Date("1959-12-01")), linetype=2) +
  ggtitle(paste0("ARIMA -- Holdout MAPE = ", round(100*MAPE,2), "%")) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0))


library(lubridate)
install.packages('bsts')
library(dplyr)
library(ggplot2)

### Load the data
data("AirPassengers")
Y <- window(AirPassengers, start=c(1949, 1), end=c(1959,12))
y <- log10(Y)


### Run the bsts model
ss <- AddLocalLinearTrend(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 12)
bsts.model <- bsts(y, state.specification = ss, niter = 500, ping=0, seed=2016)

### Get a suggested number of burn-ins
burn <- SuggestBurn(0.1, bsts.model)

### Predict
p <- predict.bsts(bsts.model, horizon = 12, burn = burn, quantiles = c(.025, .975))

### Actual versus predicted
d2 <- data.frame(
  # fitted values and predictions
  c(10^as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),])+y),  
    10^as.numeric(p$mean)),
  # actual data and dates 
  as.numeric(AirPassengers),
  as.Date(time(AirPassengers)))
names(d2) <- c("Fitted", "Actual", "Date")

### MAPE (mean absolute percentage error)
MAPE <- filter(d2, year(Date)>1959) %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))

### 95% forecast credible interval
posterior.interval <- cbind.data.frame(
  10^as.numeric(p$interval[1,]),
  10^as.numeric(p$interval[2,]), 
  subset(d2, year(Date)>1959)$Date)
names(posterior.interval) <- c("LL", "UL", "Date")

### Join intervals to the forecast
d3 <- left_join(d2, posterior.interval, by="Date")

### Plot actual versus predicted with credible intervals for the holdout period
ggplot(data=d3, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=as.numeric(as.Date("1959-12-01")), linetype=2) + 
  geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
  ggtitle(paste0("BSTS -- Holdout MAPE = ", round(100*MAPE,2), "%")) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))