require(sentimentr)
require(httr)
require(dplyr)
require(tidytext)

## define name vectors and time period ##

names <- c('Tesla','Gamestop','Moderna','DWAC')
tickers <- c('TSLA','GME','MRNA','DWAC')
ndays=1

## loop through our function and bind to matrix ##

list<-list()
for(i in names){
  get_stocks(stock=i,ndays=ndays)
  mx[,"stock"] = i
  list[[length(list)+1]] <- mx
}

table <- do.call(
  rbind,list
  )

## sentiment calculation ##

table <- table %>% 
  mutate(
  sentiment=round(sentiment_by(get_sentences(body))),2
  )

## munge data and unnest tokens ##

table$body <- gsub(pattern="[[:punct:]]",replacement="",x=table$body)
table$body <- gsub(pattern="[0-9]+",replacement="",x=table$body)

tokens <- table %>% 
  select(created_utc,stock,body) %>%
  mutate(date=as.Date(created_utc)) %>%
  unnest_tokens(word,body) %>%
  anti_join(stop_words) %>%
  filter(
    # excuse other people's language!
    !word %in% c("shit","fuck","fucking","retard","ass","dumbass","stupid")
    & nchar(word) > 2
    )

## write out data ##

setwd("")
write.csv(x=table,file='.csv',row.names = F)
write.csv(x=tokens,file='.csv',row.names = F)