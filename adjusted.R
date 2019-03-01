library(xts)
library(quantmod)
library(PerformanceAnalytics)
library(readxl)

#-----------------------load the data

# data includes daily high price, low price, mid price, open price, close price
data = read_xlsx("C:/Users/hongy/Desktop/2019spring/practicum/week5/usz14/usz14-1.xlsx")
#sapply(seq(as.Date("2011-01-01"),to=as.Date("2011-01-07"), by=1),getDayOfWeek)
#data <- dayOfWeek(data)
#weekdays(as.Date(data$Dates))

high<-data$PX_HIGH
low<-data$PX_LOW

price<-c()
price = data.frame(high,low)
price<-na.omit(price)

#n: from which line we start predict
n = 44
#time period is how many days you would like to use to estimate
time_period = 30
half_time_period = time_period/2

h<-c()
l<-c()
beta<-c()
gama<-c()
vol<-c()
alpha<-c()
spread<-c()
temp<-c()
k1<- 4*log(2)
k2<- sqrt(8/pi)


for(i in (n+1):nrow(data))
{
  #calculate beta
  sum_beta<-0
  for(j in 1:time_period) {
    h[j]<-high[i-(time_period-(j-1))]
    l[j]<-low[i-(time_period-(j-1))]

    sum_beta<-sum_beta+(log(h[j]/l[j]))^2
  }
  beta[i-n]<-sum_beta/half_time_period
  
  #calculate gama
  two_day_max <- function(j) {
    return(max(h[j], h[j+1]))
  }
  two_day_min <- function(j) {
    return(min(l[j], l[j+1]))
  }
  
  gamma1 = 0
  for(j in 1:half_time_period) {
    gamma1 = gamma1 + (log(two_day_max(2*j-1) / two_day_min(2*j-1)))^2
  }
  gamma1 = gamma1 / half_time_period
  gama[i-n] <- gamma1
  
  # gama1<-(log(h_max/l_min))^2
  # gama[i-n]<-gama1
  #calculate vol
  vol[i-n]<-(sqrt(beta[i-n]/2)-sqrt(beta[i-n]))/(k2*(3-2*sqrt(2)))+sqrt(gama[i-n]/(k2^2*(3-2*sqrt(2))))
  
  #calculate alpha
  temp[i-n]<-(vol[i-n]^2)*(k2^2-k1)+(beta[i-n]/2)
  alpha[i-n]<-(-k2*vol[i-n])+sqrt(temp[i-n])
  
  #calculate spread
  spread[i-n]<-(2*(exp(alpha[i-n])-1))/(1+exp(alpha[i-n]))
}

results<-c()
results = data.frame(date=data[(n+1):nrow(data),1],
                     vol,
                     spread)
plot(results$Dates,results$vol,type = 'l',ylim=c(0.003,0.011))
title('Volatility',sub = time_period)
plot(results$Dates,results$spread,type = 'l',ylim=c(-0.0095,0.002))
title('Spread', sub = time_period)



write.csv( results, "C:/Users/hongy/Desktop/2019spring/practicum/week5/usz14_unadjusted/usz14-1.csv" )

