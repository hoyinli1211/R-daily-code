date_time <- as.character(c('2018-01-22 18:18:00','2018-01-22 18:18:05','2018-01-22 18:18:19','2018-01-22 18:18:00','2018-01-22 18:30:12','2018-01-22 18:30:59','2018-01-22 18:18:11'))
account <- as.character(c('a0001','a0001','a0001','b0001','b0001','b0001','c0001'))
amount <- c(1000,200,300,10000,400,300,10000)
df.sample <- data.frame(date_time, account, amount)

library(data.table)
library(lubridate)
library(zoo)

v.interval.time <- seq(10,30,10)
v.interval.time.name <- paste0("interval.",v.interval.time)
v.interval.count <- seq(1,5,1)
v.interval.count.name <- paste0("count.",v.interval.time)
v.interval.amount <- seq(0,5000,1000)
v.interval.amount.name <- paste0("amount.",v.interval.time)

df.result <- expand.grid(amount=v.interval.amount, count=v.interval.count, time=v.interval.time)

df <- df.sample
setDT(df)
df[, date.time := as.POSIXct(date_time, format="%Y-%m-%d %H:%M:%S")]
df[, time.diff := difftime(date.time,min(date.time), units='mins')+0.0001, by=account]

list.result <- list()

for (i in 1:length(v.interval.time)) {
  
  #initial value
  df.temp <- data.table()
  df.temp <- setDT(df)
  #interval.time
  v.v.1 <- paste0("interval:=ceiling(time.diff/dminutes(",v.interval.time[i],"))")
  v.str.1 <- parse(text=v.v.1)
  print(v.str.1)
  df.temp <- df.temp[, eval(v.str.1), by=account]
  
  #agg.count
  df.temp[,count:=seq_len(.N),by=.(account, interval)]
  #agg.amount
  df.temp[,amount:=cumsum(amount),by=.(account, interval)]
  list.result[[i]] <- as.data.frame(df.temp)

}

names(list.result) <- v.interval.time.name
