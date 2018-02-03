library(tidyverse)
library(lubridate)



thePath <- here::here()

transaction <- read_csv(here::here("transaction.csv"))
transaction$transaction_date <- dmy(transaction$transaction_date)

cash_transaction <- transaction

transaction <- transaction %>% 
  filter(instrument_type != "Forex" & instrument_type != "Dividend" & 
           instrument_type != "Fees" & instrument_type != "Equity")

######################################################################
## Main functions
######################################################################

## functions used in the open / close positions report
######################################################################

# Function to calculate the average price of an instrument in case of several purchases
avgPurchasePrice <- function(qty, transac_Price){
  tot_value <- vector(mode="numeric", length = length(qty))
  tot_qty <- vector(mode = "numeric", length = length(qty))
  ap = 0
  q = 0
  avpr=vector()
  for(i in 1:length(qty)){
    if(qty[i]>0){
      tot_value[i] <- qty[i] * transac_Price[i] + ap * q 
      tot_qty[i] <- qty[i] + q 
      avpr[i] <- tot_value[i] / tot_qty[i]
      ap <- avpr[i]
      q <- tot_qty[i]
    } else {
      tot_qty[i] <- qty[i] + q 
      avpr[i] <- avpr[i-1]
      ap <- avpr[i]         
      q <- tot_qty[i]                                                              
      tot_value <- avpr[i] * tot_qty[i]
    }}
  avpr
}

# Function that create a df with all transactions and their average prices 
# Then it returns the date of the first transaction (initial transaction) and 
#last average price (in case of multiple purchases)
make_avgPurchasePrice_df <- function(tickerss){
  df <- transaction %>% 
    select(ticker, transaction_date, quantity, price, commission, is_short) %>% 
    arrange(transaction_date) %>% 
    filter(ticker == tickerss & is_short != 1) %>% 
    mutate(average_price = avgPurchasePrice(quantity, price), 
           cum_sum = cumsum(quantity)) %>% 
    select(-ticker)
  
  #df <- finish_openposition_df(df)
  return(df)
}

make_avgPurchasePrice_short_df <- function(tickerss){
  df <- transaction %>% 
    select(ticker, transaction_date, quantity, price, commission, is_short) %>% 
    arrange(transaction_date) %>% 
    filter(ticker == tickerss & is_short == 1) %>% 
    mutate(average_price = avgPurchasePrice(-quantity, price), 
           cum_sum = cumsum(quantity)) %>% 
    select(-ticker)
  
  #df <- finish_openposition_df(df)
  return(df)
}


## This function take the average_price df and transform it in just 2 value
## first purchase date, second avg purchase price since that date
finish_openposition_df <- function(df){ 
  yo <- df %>% filter(cum_sum == 0)
  yo <- df %>% filter(transaction_date > last(yo$transaction_date))
  df <- tibble(first_purchase = if_else(nrow(yo) == 0, ymd(first(df$transaction_date)), ymd(first(yo$transaction_date))), 
               avg_purchase_price = if_else(nrow(yo) == 0, last(df$average_price), last(yo$average_price)))
  return(df)
}

## This furnction take the list of lists from the safely function on current_pr
# and transform it into a proper df excluding the error message
transform_current_pr <- function(df){
  temp <- df$current_pr %>% map_dfr("result")
  
  if(nrow(temp)!=0){
    is_ok <- df$current_pr %>% transpose()
    is_ok <- is_ok$error %>% map_lgl(is_null)
    temp <- bind_cols(df[is_ok, ] %>% select(ticker), temp)
  } 
  
  else{
    temp <- tibble(ticker = df$ticker, Date = NA, last_price = NA)
  }
  
  df <- left_join(df %>% select(-current_pr), temp, by = "ticker")
  return(df)
}

## This furnction take the list of lists from the safely function on find_higher/lower_price
# and transform it into a proper df excluding the error message
transform_high_low_price <- function(df){
  temp <- df$high_low %>% map("result") %>% unlist() %>% as_tibble() 
  
  if(nrow(temp) != 0){
    temp <- temp %>% select(high_low = value)
    is_ok <- df$high_low %>% transpose()
    is_ok <- is_ok$error %>% map_lgl(is_null)
    temp <- bind_cols(df[is_ok, ] %>% select(ticker), temp)
  } 
  
  else{
    temp <- tibble(ticker = df$ticker, high_low = NA)
  }
  
  df <- left_join(df %>% select(-high_low), temp, by = "ticker") 
  return(df)
}

# Function to find the highest closing price since purchase date
find_higher_price <- function(tickerss, date) {
  df <- read_csv(paste0(thePath, "/financial_data/", tickerss, ".csv"))
  df$Index <- ymd(df$Index) 
  df <- df %>% filter(Index >= date)
  yo <- max(df$Adjusted, na.rm = TRUE)
  return(yo)
}

# Function to find the lowest closing price since purchase date
find_lower_price <- function(tickerss, date) {
  df <- read_csv(paste0(thePath, "/financial_data/", tickerss, ".csv"))
  df$Index <- ymd(df$Index) 
  df <- df %>% filter(Index >= date)
  yo <- min(df$Adjusted, na.rm = TRUE)
  return(yo)
}

# Function to get the last recorded price of an instrument.  
# It will fetch from a stored .csv file
get_last_price <- function(ticker){
  df <- read_csv(paste0(thePath, "/financial_data/", ticker, ".csv"))
  df$Index <- ymd(df$Index)
  df <- df %>% as_tibble() %>% 
    select(Index, Adjusted) %>% arrange(Index) %>% 
    rename(Date = Index, last_price = Adjusted) %>% tail(., 1)
  return(df)
}

## Functions used in the cash report
######################################################################

# This function will return a df with the amount of foregin currency exchanged
adding_forex <- function(df){
  forex <- df %>% filter(instrument_type == "Forex") %>% 
    select(ticker, quantity, portfolio) %>% 
    group_by(portfolio, ticker) %>% 
    summarize(currency = unique(ticker), 
              instrument_type = "Forex", 
              amount = sum(quantity), 
              commission = 0) %>% 
    select(-ticker)
  return(forex)
}


######################################################################
## Main DATAFRAMES
######################################################################

########## OPEN POSITION ##########
######################################################################
open_position <- transaction %>% arrange(transaction_date) %>% 
  group_by(ticker, is_short) %>% 
  summarize(currency = unique(currency), instrument_type = unique(instrument_type), 
            position= sum(quantity)) %>%  
  ungroup() %>% 
  filter(position != 0) 

#for short position
## TODO: differentiate for short put where we are looking for higher price for the underlying instrument
open_pos_short <- open_position %>% filter(is_short == 1) %>% 
  mutate(avg_price = map(ticker, make_avgPurchasePrice_short_df), 
         avg_price = map(avg_price, finish_openposition_df)) %>% 
  unnest() %>% 
  mutate(current_pr = map(ticker, safely(get_last_price))) %>% 
  transform_current_pr() %>% 
  mutate(high_low = map2(ticker, first_purchase, safely(find_lower_price))) %>% 
  transform_high_low_price()

#for long position
## TODO: differentiate for long put where we are looking for lower price for the undelying instrument
open_pos_long <- open_position %>% filter(is_short != 1) %>% 
  mutate(avg_price = map(ticker, make_avgPurchasePrice_df), 
         avg_price = map(avg_price, finish_openposition_df)) %>% 
  unnest() %>% 
  mutate(current_pr = map(ticker, safely(get_last_price))) %>% 
  transform_current_pr() %>% 
  mutate(high_low = map2(ticker, first_purchase, safely(find_higher_price))) %>% 
  transform_high_low_price()

open_position <- bind_rows(open_pos_long, open_pos_short) %>% 
  mutate(profit_percent = round(((last_price / avg_purchase_price) - 1)* 100, 2), 
         profit_base = round((last_price - avg_purchase_price) * position, 2), 
         percent_from_high = round(((last_price / high_low) - 1) * 100, 2), 
         avg_purchase_price = round(avg_purchase_price, 2))


########## closed POSITION ##########

closed_position <- transaction %>% arrange(transaction_date) %>% 
  group_by(ticker, is_short) %>% 
  summarize(currency = unique(currency), instrument_type = unique(instrument_type), 
            position= sum(quantity)) %>%  
  ungroup() 

closed_pos_long <- closed_position %>% filter(is_short == 0) %>% 
  mutate(avg_price = map(ticker, make_avgPurchasePrice_df)) %>% 
  unnest() %>% filter(quantity < 0)

closed_pos_short <- closed_position %>% filter(is_short == 1) %>% 
  mutate(avg_price = map(ticker, make_avgPurchasePrice_short_df)) %>% 
  unnest() %>% filter(quantity > 0)

closed_position <- bind_rows(closed_pos_long, closed_pos_short) %>% 
  select(ticker, is_short, currency, instrument_type, 
         date = transaction_date, quantity, average_price, price) %>% 
  mutate(profit = -quantity * (price - average_price), 
         profit_percent = ((price / average_price) - 1) * -100) %>% 
  arrange(desc(date)) %>% 
  mutate(price = round(price, 2), 
         average_price = round(average_price, 2), 
         profit = round(profit, 2), profit_percent = round(profit_percent, 2))


########## cash situations ##########
######################################################################

cash <- cash_transaction %>% 
  select(ticker, quantity, is_short, price, commission, instrument_type, currency, portfolio) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% 
  mutate(amount = case_when(instrument_type == "Equity" ~ quantity * price, 
                            instrument_type == "ETF" ~ -(quantity * price), 
                            instrument_type == "Stock" ~ -(quantity * price),
                            instrument_type == "Dividend" ~ (quantity * price),
                            instrument_type == "Forex" ~ -(quantity * price), 
                            instrument_type == "Fees" ~ -(quantity * price),
                            instrument_type == "Options" ~ -(quantity * price)), 
         transaction_date = cash_transaction$transaction_date) 


