
#============================================
#   Description: A function for collecting
#   all financial data of a ticker from
#   https://www.vndirect.com.vn/
#============================================

rm(list = ls())

library(httr)
library(stringr)
library(magrittr)
library(rvest)
library(tidyverse)
library(lubridate)



#-----------------------
# Some Suport Functions
#-----------------------

getLastPage <- function (url, symbol, from, to){
  
  fd <- list(
    searchMarketStatisticsView.symbol = symbol,
    strFromDate = dateChar(from),
    strToDate = dateChar(to)
  )
  
  resp <- POST(url,body = fd,encode = "form")
  resp %>% read_html() %>% html_nodes(xpath = '//*[@id="tab-1"]/div[1]') %>%
    html_text(.,trim=TRUE) %>%
    str_split("[^0-9]+") %>% unlist() -> df
  
  lastPage <- ifelse(is.na(df[3]), 1, as.numeric(df[3]))
  return(lastPage)
}



dateChar <- function(dateTime){
  dateTime %<>%  
    as.Date() %>% 
    format(., format = "%d/%m/%Y") %>% 
    return()
}


myexport <- function(...) {
  arg.list <- list(...)
  names <- all.names(match.call())[-1]
  for (i in seq_along(names)) assign(names[i], arg.list[[i]], .GlobalEnv)
} 


#--------------------------------------------------------
# Write function for getting financial data of a ticker
#--------------------------------------------------------

get_symbols_VND <- function(symbol, from, to) {
  url <- "https://www.vndirect.com.vn/portal/thong-ke-thi-truong-chung-khoan/lich-su-gia.shtml"
  lastPage <- getLastPage(url, symbol, from, to)
  cname <- c("date",
             "change1",
             "change2",
             "open",
             "high",
             "low",
             "close",
             "average",
             "adjusted",
             "volume_Match",
             "volume_Reconcile")
  symbolData <- matrix(nrow = 0, 
                       ncol = 11, 
                       byrow = TRUE, 
                       dimnames = list(c(), cname))
  for (page in 1:lastPage) {
    fd <- list(
      searchMarketStatisticsView.symbol= symbol,
      strFromDate = dateChar(from),
      strToDate = dateChar(to),
      pagingInfo.indexPage = page
    )
    
    resp <- POST(url,body = fd,encode = "form")
    tmp <- resp %>% 
      read_html() %>% 
      html_nodes(xpath = '//*[@id="tab-1"]/div[2]/ul') %>%
      html_children() %>%  
      html_text()
    
    noDays <- length(tmp)
    
    for(i in 2:noDays){
      row <- str_replace_all(tmp[i],"\t","") %>% 
        str_replace_all("\n"," ") %>%
        gsub(" +"," ", .) %>% 
        str_trim() %>%
        str_split(" ") %>% 
        unlist() %>% 
        str_split(" ") %>% 
        unlist() %>% 
        as.vector()
      symbolData <- rbind(symbolData, row)
    }
  }
  
  symbolData <- data.frame(symbolData, row.names = symbolData[, 1])
  symbolData <- symbolData %>% 
    mutate(date = ymd(date)) %>% 
    mutate_if(is.factor, as.numeric)
  myexport(symbolData)
  assign(symbol, symbolData, envir = .GlobalEnv)
  rm(list = "symbolData", envir = .GlobalEnv)
  my_df <- symbolData %<>% 
    mutate(symbol = str_to_upper(symbol), volume = volume_Match + volume_Reconcile) %>% 
    select(symbol, date, open, high, low, close, volume, adjusted, everything()) %>% 
    arrange(date) %>% 
    as.tibble()
  return(my_df)
  
}



#------------------------
#      Test Function
#------------------------

# Collect data: 
get_symbols_VND("vnm", "2017-12-01", today()) -> vnm
vnm %>% head()

# Plot open price: 
vnm %>% ggplot(aes(date, open)) + geom_line()

#------------------------------------------------------------
# Write function for getting financial data of many tickers
#-------------------------------------------------------------


multi_get_symbols_VND <- function(symbol_list, from, to) {
  my_df <- data.frame()
  for (i in symbol_list) {
    df1 <- get_symbols_VND(i, from = from, to = to)
    my_df <- rbind(my_df, df1)
  }
  return(my_df)
}

#------------------------
#      Test Function
#------------------------

my_symbols <- c("vcb", "ssi", "vnm", "fpt")
FANG <- multi_get_symbols_VND(my_symbols, "2015-01-01", "2017-12-31")


FANG %>%
  group_by(symbol) %>%
  ggplot(aes(date, adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "Daily Stock Prices",
       x = NULL, y = "Adjusted Prices") +
  facet_wrap(~ symbol, ncol = 2, scales = "free")
