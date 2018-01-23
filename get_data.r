library(tidyverse)

portfolio <- read_csv("~/Google Drive/Software/R projects/shinyapps/Portfolio/transaction.csv") 

startDate = "2004-01-01"
thePath = "~/Google Drive/Software/R projects/shinyapps/Portfolio/"

######################################################################
## Alpha Vantage for Stocks from American Exchange  
######################################################################
av <- portfolio %>% 
  select(tickerb = saved_as, ticker = alpha_ticker) %>% na.omit() %>% distinct()


for (i in 1:nrow(av)){
  print(av$ticker[i])
  data = quantmod::getSymbols(Symbols = av$ticker[i],
                              src = "av",
                              api.key = "PUT YOUR OWN HERE :-) . ",
                              output.size = "full", 
                              from = startDate,
                              auto.assign = FALSE,
                              index.class="POSIXct")
  colnames(data) = c("Open", "High", "Low", "Adjusted", "Volume")
  zoo::write.zoo(data, paste0(thePath, "financial_data/", av$tickerb[i], ".csv"), 
                 sep = ",", row.names = FALSE)
}


##### Doing the same for options - need to use the undelying instrument #### 
options <- portfolio %>% filter(instrument_type == "Options") %>% select(ticker) %>% 
  as_vector() %>% 
  str_replace_all("\ .*", "") %>% as_tibble() %>% select(ticker = value)

for (i in 1:nrow(options)){
  print(options$ticker[i])
  data = quantmod::getSymbols(Symbols = options$ticker[i],
                              src = "av",
                              api.key = "W7SHG93NFG5YWE2K",
                              output.size = "full", 
                              from = startDate,
                              auto.assign = FALSE,
                              index.class="POSIXct")
  colnames(data) = c("Open", "High", "Low", "Adjusted", "Volume")
  zoo::write.zoo(data, paste0(thePath, "financial_data/", options$ticker[i], ".csv"), 
                 sep = ",", row.names = FALSE)
}










