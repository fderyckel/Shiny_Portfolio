library(tidyverse)

thePath <- here::here()
portfolio <- read_csv(here::here("transaction.csv"))

######################################################################
## Alpha Vantage for Stocks from American Exchange  
######################################################################
av <- portfolio %>% 
  select(tickerb = saved_as, ticker = alpha_ticker) %>% na.omit() %>% distinct()


for (i in 1:nrow(av)){
  print(av$ticker[i])
  data = quantmod::getSymbols(Symbols = av$ticker[i],
                              src = "av",
                              api.key = "PUT YOUR OWN KEY HERE ;-) ",
                              output.size = "full", 
                              auto.assign = FALSE,
                              index.class="POSIXct")
  colnames(data) = c("Open", "High", "Low", "Adjusted", "Volume")
  zoo::write.zoo(data, paste0(thePath, "/financial_data/", av$tickerb[i], ".csv"), 
                 sep = ",", row.names = FALSE)
}


##### Doing the same for options - need to use the undelying instrument #### 
options <- portfolio %>% filter(instrument_type == "Options") %>% select(ticker) %>% 
  as_vector() %>% 
  str_replace_all("\ .*", "") %>% as_tibble() %>% select(ticker = value) %>% unique()

for (i in 1:nrow(options)){
  print(options$ticker[i])
  data = quantmod::getSymbols(Symbols = options$ticker[i],
                              src = "av",
                              api.key = "PUT YOUR OWN KEY HERE ;-) ",
                              output.size = "full", 
                              auto.assign = FALSE,
                              index.class="POSIXct")
  colnames(data) = c("Open", "High", "Low", "Adjusted", "Volume")
  zoo::write.zoo(data, paste0(thePath, "/financial_data/", options$ticker[i], ".csv"), 
                 sep = ",", row.names = FALSE)
}
