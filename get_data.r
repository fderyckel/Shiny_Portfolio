library(tidyverse)

thePath <- here::here()

portfolio <- map_df(list.files(paste0(thePath, "/transactions"), pattern = "\\.csv", full.names=TRUE), 
                      read_csv, col_names=TRUE) %>% select(1:15)

######################################################################
## Alpha Vantage for Stocks from American Exchange  
######################################################################
av <- portfolio %>% 
  select(tickerb = saved_as, ticker = alpha_ticker) %>% na.omit() %>% distinct()

#av <- av[64:67,]

for (i in 1:nrow(av)){
  print(av$ticker[i])
  data = quantmod::getSymbols(Symbols = av$ticker[i],
                              src = "av",
                              api.key = "W7SHG93NFG5YWE2K",
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
                              api.key = "W7SHG93NFG5YWE2K",
                              output.size = "full", 
                              auto.assign = FALSE,
                              index.class="POSIXct")
  colnames(data) = c("Open", "High", "Low", "Adjusted", "Volume")
  zoo::write.zoo(data, paste0(thePath, "/financial_data/", options$ticker[i], ".csv"), 
                 sep = ",", row.names = FALSE)
}
