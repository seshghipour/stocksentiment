require(tidyquant)
require(dplyr)

## define name vectors and time period ##

tickers <- c('TSLA','GME','MRNA','DWAC')
names <- c('Tesla','Gamestop','Moderna','DWAC')

ndays = 365
today <- Sys.Date()
after <- today - ndays

## loop through function and bind to matrix ##

list<-list()
for(i in 1:length(tickers)){
  temp <- tq_get(tickers[i],get = "stock.prices", from = after)
  temp$name <- names[i]
  list[[length(list)+1]] <- temp
}

table <- do.call(
  rbind,list
)

## write out data ##

setwd("")
write.csv(table,file='.csv',row.names = F)