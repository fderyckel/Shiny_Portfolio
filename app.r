library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)


thePath <- here::here()
######################################################################
## Main functions and themes
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
    select(ticker, purchase_date, quantity, price, commission, is_short) %>% 
    arrange(purchase_date) %>% 
    filter(ticker == tickerss & is_short != 1) %>% 
    mutate(average_price = avgPurchasePrice(quantity, price), 
           cum_sum = cumsum(quantity)) %>% 
    select(-ticker)
  
  df <- finish_openposition_df(df)
  return(df)
}

make_avgPurchasePrice_short_df <- function(tickerss){
  df <- transaction %>% 
    select(ticker, purchase_date, quantity, price, commission, is_short) %>% 
    arrange(purchase_date) %>% 
    filter(ticker == tickerss & is_short == 1) %>% 
    mutate(average_price = avgPurchasePrice(-quantity, price), 
           cum_sum = cumsum(quantity)) %>% 
    select(-ticker)
  
  df <- finish_openposition_df(df)
  return(df)
}


## This function take the average_price df and transform it in just 2 value
## first purchase price and avg price
finish_openposition_df <- function(df){ 
  yo <- df %>% filter(cum_sum == 0)
  yo <- df %>% filter(purchase_date > last(yo$purchase_date))
  df <- tibble(first_purchase = if_else(nrow(yo) == 0, ymd(first(df$purchase_date)), ymd(first(yo$purchase_date))), 
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


######################################################################
# Create the themes that are used in the main chart
theme_chart_top <- theme(panel.background = element_rect(fill = "grey25", color = "black"), 
                         legend.position = c(0.265, 0.925), 
                         #legend.position = "top", 
                         legend.direction = "horizontal", 
                         legend.title = element_blank(), 
                         axis.title.x = element_blank(), 
                         axis.text.x = element_text(angle = 60, hjust = 1))

theme_chart_bottom <- theme(panel.background = element_rect(fill = "grey85", color = "black"), 
                            legend.position = c(0.045, 0.8), 
                            legend.direction = "vertical", 
                            legend.title = element_blank(), 
                            axis.title.x = element_blank(), 
                            axis.text.x = element_blank())




######################################################################
## Building the UI
######################################################################
header <- dashboardHeader(
  title = "Portfolio Management"
  )

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Transactions", tabName = "summary", icon = icon("dashboard"), selected = TRUE), 
    menuItem("Instruments", tabName = "finan_instru", icon = icon("area-chart")),  
    menuItem("Alerts", tabName = "alerts", icon = icon("calendar")), 
    menuItem("Cash Position", tabName = "cash", icon = icon("money"))))

body <- dashboardBody(
  tabItems(
    #For the summary page
    tabItem(tabName = "summary", 
            fluidRow(
              valueBox(10*2, "Daily % Change"), 
              valueBox(30*1.4, "% Change YTD"), 
              valueBox(12*3.1, "% Change since Inceptin")), 
            
            fluidRow(
              column(width = 9, 
                     tabBox(title = "Summary", width = NULL,  selected = "Open positions", 
                            tabPanel(title = "Open positions", width = NULL, status = "primary", 
                                     dataTableOutput('tbl_open_position')), 
                            tabPanel(title = "Close positions", width = NULL, status = "sucess"), 
                            tabPanel(title = "Transactions", status = "warning")
                            )), 
              
              column(width = 3, 
                     box(title = "Transactions", width = NULL, 
                         solidHeader = TRUE, status = "warning", 
                         dateInput("transaction_date", "Date of transaction", 
                                   format = "yyyy-mm-dd", weekstart = 1), 
                         selectInput("currency", "Currency", c("EUR", "USD", "CAD")), 
                         selectInput("type_transaction", "Type of transaction", c("ETF", "Stock", "Options", "Dividend", 
                                                                                  "Fees", "Forex", "Equity")), 
                         textInput("id_finan_instru", "Ticker"), 
                         numericInput("quantity", "Quantity", 0), 
                         numericInput("price", "Price", 0), 
                         numericInput("commission", "Commission fee", 14.95), 
                         selectInput("portfolio_id", "Choose your portfolio", c("BestBroker", "IbBrokers", "Others")), 
                         actionButton("submit", "Submit", class = "btn-primary")
                         ))
            )), 
    
    #For the financial instrument page 
    tabItem(tabName = "finan_instru", 
            fluidRow(
              column(width = 9, 
                     
                     #The main grah of prices
                     box(title = "Charting", width = NULL, status = "primary", 
                         plotlyOutput("instrument_chart"), height = 600), 
                     box(title = "box2", width = NULL)), 
              
              #The input boxe where to choose dates, financial instrument, etc. 
              column(width = 3, 
                     box(title = "Input", width = NULL, status = "warning", solidHeader = TRUE, 
                         uiOutput("pick_portfolio"), 
                         uiOutput("pick_instrument"), 
                         dateRangeInput("date_range", 
                                        label = "Date Range", 
                                        start = Sys.Date() - 352, end = Sys.Date())))
           ))
    ))

ui <- dashboardPage(header, sidebar, body)




######################################################################
## Building the server side
######################################################################
server <- function(input, output) { 
  
  transaction <- read_csv(here::here("transaction.csv"))
  transaction$purchase_date <- dmy(transaction$purchase_date)
  
  # First tab "summary", make the summary of open position
  output$tbl_open_position <- renderDataTable({
    # Open Position table
    transaction <- transaction %>% 
      filter(instrument_type != "Forex" & instrument_type != "Dividend" & 
               instrument_type != "Fees" & instrument_type != "Equity")
    open_position <- transaction %>% arrange(purchase_date) %>% 
      group_by(ticker, is_short) %>% 
      summarize(currency = unique(currency), instrument_type = unique(instrument_type), 
                position= sum(quantity)) %>%  
      ungroup() %>% 
      filter(position != 0) 
    
    #for short position
    ## TODO: differentiate for short put where we are looking for higher price for the underlying instrument
    df1 <- open_position %>% filter(is_short == 1) %>% 
      mutate(avg_price = map(ticker, make_avgPurchasePrice_short_df)) %>% 
      unnest() %>% 
      mutate(current_pr = map(ticker, safely(get_last_price))) %>% 
      transform_current_pr() %>% 
      mutate(high_low = map2(ticker, first_purchase, safely(find_lower_price))) %>% 
      transform_high_low_price()
    
    
    #for long position
    ## TODO: differentiate for long put where we are looking for lower price for the undelying instrument
    df2 <- open_position %>% filter(is_short != 1) %>% 
      mutate(avg_price = map(ticker, make_avgPurchasePrice_df)) %>% 
      unnest() %>% 
      mutate(current_pr = map(ticker, safely(get_last_price))) %>% 
      transform_current_pr() %>% 
      mutate(high_low = map2(ticker, first_purchase, safely(find_higher_price))) %>% 
      transform_high_low_price()
    
    open_position <- bind_rows(df1, df2) %>% 
      mutate(profit_percent = round(((last_price / avg_purchase_price) - 1)* 100, 2), 
             profit_base = round((last_price - avg_purchase_price) * position, 2), 
             percent_from_high = round(((last_price / high_low) - 1) * 100, 2), 
             avg_purchase_price = round(avg_purchase_price, 2))
    
    
    open_position <- open_position %>% 
      select(Ticker = ticker, Currency = currency, `Instr. \ Type` = instrument_type, `is \ short` = is_short, 
             Position = position, `Inital \ position \ date` = first_purchase,  `Average Price` = avg_purchase_price,  
             `Last \ price \ date` = Date, `Current \ Price` = last_price, `Profit \ Percent` = profit_percent, 
             `Profit \ Base` = profit_base, `Percent \ from High` = percent_from_high)
    
    open_position
  })
  
  # Second tab "financial Instrument", UI Pick the financial instrument to graph
  output$pick_portfolio <- renderUI({
    open_position <- transaction %>% 
      filter(instrument_type == "Options" | instrument_type == "ETF" | instrument_type == "Stock") %>% 
      group_by(ticker, portfolio) %>% 
      summarize(position= sum(quantity)) %>%  
      filter(position != 0)
    
    selectInput("portfolio_to_choose", "Portfolio", as.list(unique(open_position$portfolio)))
  })
  
  output$pick_instrument <- renderUI({
    open_position <- transaction %>% 
      filter(instrument_type == "Options" | instrument_type == "ETF" | instrument_type == "Stock") %>% 
      filter(portfolio == input$portfolio_to_choose) %>% 
      group_by(ticker, portfolio) %>% 
      summarize(position= sum(quantity)) %>%  
      filter(position != 0) %>% 
      mutate(tickerb = str_replace(ticker, "\ .*", ""))
    
    selectInput("instru_to_graph", "Pick your instrument", as.list(unique(open_position$tickerb)))
  })
  
  # Second tab "financial Instrument", UI the main chart
  output$instrument_chart <- renderPlotly({
    df_etf <- read_csv(paste0(thePath, "/financial_data/", input$instru_to_graph, ".csv")) %>% na.omit()
    df_etf$Index <- ymd(df_etf$Index)
    df_etf <- df_etf %>% filter(Adjusted != 0)
    ## Create MACD
    df2 <- TTR::MACD(df_etf$Adjusted) %>% as_tibble()
    colnames(df2) <- c("MACD_line", "MACD_signal")
    df_etf <- bind_cols(df_etf, df2)
    ## Create RSI and MA, then filter for dates
    df_etf <- df_etf %>% mutate(rsi14 = TTR::RSI(df_etf$Adjusted, n= 14), 
                                rsi3 = TTR::RSI(df_etf$Adjusted, n = 3), 
                                sma50 = round(TTR::SMA(df_etf$Adjusted, n = 50), 2), 
                                sma200 = round(TTR::SMA(df_etf$Adjusted, n = 200), 2), 
                                ema9 = round(TTR::EMA(df_etf$Adjusted, n=9), 2)) %>% 
      select(-Open, -High, -Low) %>% 
      filter(Index >= input$date_range[1] & Index <= input$date_range[2])
    
    ## Create first plot with adjusted price
    p1 <- plot_ly(data = df_etf, name = "Adjusted Price", 
                  x = ~Index, y = ~Adjusted, type = "scatter", mode = "lines", 
                  line = list(color = "black", width = 4)) %>% 
      add_trace(y = ~ema9, name = "EMA9", line = list(color = "rgb(230,39,28)", width = 2)) %>% 
      add_trace(y = ~sma50, name = "SMA50", line = list(color = "rgb(79,174,66)", width = 2, dash = "dot")) %>% 
      add_trace(y = ~sma200, name = "SMA200", line = list(color = "rgb(151,81, 165)", width = 2, dash = "dot")) %>% 
      layout(xaxis = list(title = "Date"), 
             yaxis = list(title = "Adjusted Prices"))
    ## Create second plot
    p2 <- plot_ly(data = df_etf, name = "MACD_line", 
                  x = ~Index, y = ~MACD_line, type = "scatter", mode = "lines", 
                  line = list(color = "rgb(156,188,219)")) %>% 
      add_trace(y = ~MACD_signal, name = "MACD Signal", line = list(color = "rgb(135,88,169)"))
    ## Create third plot
    p3 <- plot_ly(data = df_etf, name = "RSI14", 
                  x = ~Index, y = ~rsi14, type = "scatter", mode = "lines", 
                  line = list(color = "rgb(232,89,3")) %>% 
      add_trace(y = ~rsi3, name="RSI3", line = list(color = "rgb(255,175,103)"))
    
    ## Create a subplot of the above plots
    plotly::subplot(p1, p2, p3, nrows = 3, shareX=TRUE, heights = c(0.6, 0.2, 0.2)) %>% 
      layout(xaxis = list(title = "Date"), 
             plot_bgcolor = "rgb(210,210,210)", 
             showlegend = FALSE, height = 530)
  })
  }


######################################################################
## End of dashboard
######################################################################

shinyApp(ui, server)
