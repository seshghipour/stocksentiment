get_stocks <- function(stock,ndays){
  
  ## define data objects and create first request ##
  
  # define lists
  list <- list()
  temp_list <- list()

  # define dates
  today <- Sys.Date()
  after <- as.numeric(as.POSIXct(today - ndays))
  
  # create initial request url
  request_url <- paste0('https://api.pushshift.io/reddit/comment/search/?&sort=asc&size=100',
                        '&subreddit=',
                        'wallstreetbets',
                        '&after=',
                        after,
                        '&q=',
                        stock)
  
  # parse response
  response <- GET(
    request_url
  )
  temp_list <- content(
    response,
    as="parsed"
  )
  list[[length(list)+1]] <- temp_list
  
  # check if pagination needs to occur
  if(length(list[[1]]$data) != 0){
    
    after <- list[[1]]$data[[length(list[[1]]$data)]]$created_utc
    
    # paginate request with "created_utc" field
    while(length(temp_list$data) != 0){
      
      # rate limit!
      Sys.sleep(1)
      
      after <- temp_list$data[[length(temp_list$data)]]$created_utc
      
      request_url <- paste0('https://api.pushshift.io/reddit/comment/search/?&sort=asc&size=100',
                            '&subreddit=',
                            'wallstreetbets',
                            '&after=',
                            after,
                            '&q=',
                            stock)
      
      response <- GET(
        request_url
      )
      temp_list <- content(
        response,
        as="parsed"
      )
      list[[length(list)+1]] <- temp_list
      
    }
  }
  
  ## convert list into usable matrix ##
  
  # define matrix attributes
  colnames <- c('created_utc','permalink','body','link_id')
  rowcount <- length(list[[length(list)]]$data) + (length(list) - 1) * 100
  colcount <- length(colnames)
  mx <- matrix(nrow=rowcount,ncol=colcount)
  colnames(mx) <- colnames
  
  # loop through list for all comments
  counter <- 1
  for(i in 1:length(list)){
    for(j in 1:length(list[[i]]$data)){
      if(length(list[[i]]$data) != 0){
        mx[counter,1] <- list[[i]]$data[[j]]$created_utc
        mx[counter,2] <- list[[i]]$data[[j]]$permalink
        mx[counter,3] <- list[[i]]$data[[j]]$body
        mx[counter,4] <- list[[i]]$data[[j]]$link_id
        counter <- counter + 1
      }
    }
  }
  
  ## munge data ##
  
  mx <- as_tibble(mx)
  mx <<- mx %>% 
    filter(
      !is.na(created_utc)
      ) %>%
    mutate(
      created_utc=as.POSIXct(as.numeric(created_utc),origin="1970-01-01")
      )
 
}