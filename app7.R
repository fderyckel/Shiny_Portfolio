library(shinydashboard)
library(tidyverse)
library(lubridate)
library(RollingWindow)
library(plotly)

thePath <- here::here()

source('global.R', local = TRUE)


######################################################################
## Building the UI
######################################################################
header <- dashboardHeader(
  title = "Portfolio Management"
  )

sidebar <- dashboardSidebar(
  sidebarUserPanel('François de Ryckel', 
                   image = "https://avatars3.githubusercontent.com/u/7240992?s=460&v=4"), 
  sidebarMenu(
    menuItem("Transactions", tabName = "summary", icon = icon("dashboard"), selected = TRUE), 
    menuItem("Instruments", tabName = "finan_instru", icon = icon("area-chart")),  
    menuItem("Cash Position", tabName = "cash", icon = icon("money")), 
    menuItem("Alerts", tabName = "alerts", icon = icon("calendar"))), 
  
  dateRangeInput("date_range", 
                 label = "Date Range", 
                 start = Sys.Date() - 63, end = Sys.Date()), 
  uiOutput("pick_portfolio"), 
  uiOutput("pick_instrument"))

body <- dashboardBody(
  tabItems(
    #For the summary page
    tabItem(tabName = "summary", 
            fluidRow(
              valueBoxOutput("realized_profit"),  
              valueBox(30*1.4, "% Change YTD"), 
              valueBox(12*3.1, "% Change since Inceptin")), 
            
            fluidRow(
              column(width = 12, 
                     tabBox(title = "Summary", width = NULL,  selected = "Open positions", 
                            tabPanel(title = "Open positions", status = "primary", 
                                     dataTableOutput('tbl_open_position'), width = 9), 
                            tabPanel(title = "Closed positions", width = NULL, status = "sucess", 
                                     dataTableOutput('tbl_closed_position')), 
                            tabPanel(title = "Transactions", status = "warning", 
                                     dataTableOutput('tbl_transactions')), 
                            tabPanel(title = "Returns, Volatility and Weights", width = NULL, 
                                     dataTableOutput('tbl_financial_instru'))))
            )), 
    
    #For the financial instrument page 
    tabItem(tabName = "finan_instru", 
            fluidRow(
              column(width = 12, 
                     tabBox(title = "Charts", width = NULL, selected = "Charting", 
                            
                            #The main grah of prices
                            tabPanel(title = "Charting", width = NULL, status = "primary", 
                                    plotlyOutput("instrument_chart", height = "600px")), 
                            tabPanel(title = "Std from mean", width = NULL, plotlyOutput('instrument_chart_std'))), 
                     # Add the related transactions under the graph
                     box(title = "transactions", width = NULL, status = "primary", 
                         dataTableOutput("tbl_related_transactions")))
           )), 
    
    #For the cash position page 
    tabItem(tabName = "cash", 
            fluidRow(
              column(width = 12, 
                     
                     #The main grah of prices
                     box(title = "Cash Positions", width = NULL, status = "primary", 
                         dataTableOutput("cash_positions")), 
                     box(title = "box2_cash", width = NULL))))
  
    ))

ui <- dashboardPage(header, sidebar, body)




######################################################################
## Building the server side
######################################################################
server <- function(input, output) { 
  ##########
  # Value box
  ##########
  output$realized_profit <- renderValueBox({
    valueBox(
      as.integer(ytd_realized_profit %>% na.omit() %>% select(realized_profit) %>% tail(.,1)), 
      "YTD Realized Profit", color = "purple"
    )
  })
  
  
  ##########
  # First tab "summary", make the summary of open positions
  ##########
  output$tbl_open_position <- renderDataTable({
    # Open Positions table
    open_position <- open_position %>% 
      select(Ticker = ticker, Currency = currency, `Instr. \ Type` = instrument_type, `is \ short` = is_short, 
             Position = position, `Inital \ position \ date` = first_purchase,  `Average Price` = avg_purchase_price,  
             `Last \ price \ date` = Date, `Current \ Price` = last_price, `Profit \ Percent` = profit_percent, 
             `Profit \ Base` = profit_base, `Percent \ from High` = percent_from_high)
    
    open_position
  },options = list(filter = 'top', scrollX = TRUE, pageLength = 10))

  # Closed Positions table
  output$tbl_closed_position <- renderDataTable({
    closed_position <- closed_position %>% 
      select(Ticker = ticker, Currency = currency, `Instr. \ Type` = instrument_type, `is \ short` = is_short, 
             Quantity = quantity, Date = date,  `Average Price` = average_price,  
             Price = price, `Profit` = profit, `Profit \ Percent` = profit_percent)
    
    closed_position
  },options = list(filter = 'top', scrollX = TRUE, pageLength = 10))
  
  # All transactions table
  output$tbl_transactions <- renderDataTable({
    all_transactions %>% filter(Date >= input$date_range[1] & 
                                Date <= input$date_range[2] & 
                                portfolio == input$portfolio_to_choose) %>% 
      arrange(Type, Currency, Ticker) %>% 
      select(-portfolio)
  },options = list(filter = 'top', scrollX = TRUE, pageLength = 10))
    
  
  # Returns and volatility table
  output$tbl_financial_instru <- renderDataTable({
    
  })
  
  
  ##########
  # Second tab "financial Instrument", UI Pick the financial instrument to graph
  ##########
  output$pick_portfolio <- renderUI({
    open_position <- transaction %>% 
      filter(instrument_type == "Options" | instrument_type == "ETF" | instrument_type == "Stock")
    
    selectInput("portfolio_to_choose", "Portfolio", as.list(unique(open_position$portfolio)))
  })
  
  output$pick_instrument <- renderUI({
    df <- transaction %>% 
      filter(instrument_type == "Options" | instrument_type == "ETF" | instrument_type == "Stock") %>% 
      filter(portfolio == input$portfolio_to_choose) %>% 
      group_by(ticker, portfolio) %>% 
      #summarize(position= sum(quantity)) %>%  
      filter(transaction_date >= today() - 200) %>% 
      mutate(tickerb = str_replace(ticker, "\ .*", "")) %>% 
      arrange(tickerb)
    
    selectInput("instru_to_graph", "Pick your instrument", as.list(unique(df$tickerb)))
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
      filter(Index >= input$date_range[1] & Index <= input$date_range[2])
    
    ## Create first plot with adjusted price
    p1 <- plot_ly(data = df_etf, 
                  x = ~Index, type = "candlestick", 
                  open = ~Open, close = ~Adjusted, high = ~High, low = ~Low) %>% 
      add_lines(x = ~Index, y = ~ema9, name = "EMA9", line = list(color = "rgb(230,39,28)", width = 2), inherit = F) %>% 
      add_lines(x = ~Index, y = ~sma50, name = "SMA50", line = list(color = "rgb(79,174,66)", width = 2, dash = "dot"), inherit = F) %>% 
      add_lines(x = ~Index, y = ~sma200, name = "SMA200", line = list(color = "rgb(151,81, 165)", width = 2, dash = "dot"), inherit = F) %>% 
      layout(xaxis = list(title = "Date", rangeslider = list(visible = F)), 
             yaxis = list(title = "Adjusted Prices", type = "log"), 
             showlegend = FALSE)
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
      layout(xaxis = list(title = "Date"), title = paste0("Chart of ", input$instru_to_graph), 
             plot_bgcolor = "rgb(210,210,210)", height = 580, 
             showlegend = FALSE)
  })
  
  
  output$instrument_chart_std <- renderPlotly({
    df_etf <- read_csv(paste0(thePath, "/financial_data/",input$instru_to_graph , ".csv"))
    df_etf$Index <- ymd(df_etf$Index)
    ## Create RSI and MA, then filter for dates
    df_etf <- df_etf %>% mutate(EMA9 = TTR::EMA(df_etf$Adjusted, n= 9), 
                                SMA50 = TTR::SMA(df_etf$Adjusted, n = 50), 
                                SMA200 = TTR::SMA(df_etf$Adjusted, n = 200))
    
    df_etf <- df_etf %>% 
      mutate(EMA9 = Adjusted / EMA9 - 1, 
             roll_sd_ema9 = RollingStd(EMA9, window = 54, na_method = "ignore"), 
             roll_mean_ema9 = RollingMean(EMA9, window = 54, na_method = "ignore"), 
             diff_sd_ema9 = round((EMA9 - roll_mean_ema9) / roll_sd_ema9, 2), 
             SMA50 = Adjusted / SMA50 - 1, 
             roll_sd_sma50 = RollingStd(SMA50, window = 300, na_method = "ignore"), 
             roll_mean_sma50 = RollingMean(SMA50, window = 300, na_method = "ignore"), 
             diff_sd_sma50 = round((SMA50 - roll_mean_sma50) / roll_sd_sma50, 2),  
             SMA200 = Adjusted / SMA200 - 1,                      
             roll_sd_sma200 = RollingStd(SMA200, window = 1200, na_method = "ignore"), 
             roll_mean_sma200 = RollingMean(SMA200, window = 1200, na_method = "ignore"), 
             diff_sd_sma200 = round((SMA200 - roll_mean_sma200) / roll_sd_sma200, 2))
    
    df_etfb <- df_etf %>% filter(Index >= input$date_range[1]) %>% 
      select(Index, diff_sd_ema9, diff_sd_sma50, diff_sd_sma200) %>% 
      mutate(ema9 = as.numeric(diff_sd_ema9), 
             sma50 = as.numeric(diff_sd_sma50), 
             sma200 = as.numeric(diff_sd_sma200))
    
    #  %>%   gather(key = "variable", value = "value", -Index) 
    plot_ly(df_etfb, x = ~Index) %>% 
      add_trace(y = ~ema9, name = "EMA9", mode = 'lines', line = list(color = "rgb(230,39,28)", width = 1)) %>% 
      add_trace(y = ~sma50, name = "SMA50", mode = 'lines', line = list(color = "rgb(79,174,66)", width = 2)) %>% 
      add_trace(y = ~sma200, name = "SMA200", mode = 'lines', line = list(color = "rgb(151,81, 165)", width = 2)) %>% 
      layout(xaxis = list(title = "Date"), 
             title = "How many std away is the MA from its latest mean", 
             showlegend = FALSE)
  })
  
  
  # Second tab about "instrument" - show related transactions to the instrument
  output$tbl_related_transactions <- renderDataTable({
    df <- transaction %>% select(ticker, quantity, transaction_date, price, instrument_type, is_short) %>% 
      mutate(ticker_short = str_replace_all(ticker, "\ .*", "")) %>% 
      filter(ticker_short == input$instru_to_graph) %>% arrange(desc(transaction_date))
    
    df2 <- open_position %>% 
      mutate(ticker_short = str_replace_all(ticker, "\ .*", ""), 
             Open = TRUE) %>% 
      filter(ticker_short == input$instru_to_graph) %>% 
      select(Ticker = ticker, Currency = currency, Instr_Type = instrument_type, is_short, Position = position, 
             Date = first_purchase, Avg_Price = avg_purchase_price, price = last_price, Profit = profit_base, 
             Open)
    
    
    df3 <- closed_position %>% 
      mutate(ticker_short = str_replace_all(ticker, "\ .*", ""), 
             Open = FALSE) %>% 
      filter(ticker_short == input$instru_to_graph) %>%  
      select(Ticker = ticker, Currency = currency, Instr_Type = instrument_type, is_short, Position = quantity, 
             Date = date, Avg_Price = average_price, price = price, Profit = profit, Open)
    
    # Include the dividends
    df4 <- cash_transaction %>% 
      filter(instrument_type == "Dividend" & ticker == input$instru_to_graph) %>% 
      mutate(is_short = NA, profit = price, Open = NA, average_price = NA) %>% 
      select(Ticker = ticker, Currency = currency, Instr_Type = instrument_type, is_short, Position = quantity, 
             Date = transaction_date, Avg_Price = average_price, price = price, Profit = profit, Open)
    
    df <- bind_rows(df2, df3, df4)
    
  })
  
  
  ##########
  # Third tab "Cash", UI Pick the portfolio and the dates
  ##########
  output$cash_positions <- renderDataTable({
    ## Create the opening positions
    opening_A <- cash %>% filter(transaction_date < input$date_range[1] & 
                                   portfolio == input$portfolio_to_choose) %>% 
      group_by(currency, instrument_type) %>% 
      summarize(amount = sum(amount), 
                commission = sum(commission)) 
    opening_B <- cash %>% 
      filter(transaction_date < input$date_range[1] & 
               portfolio == input$portfolio_to_choose) %>% 
      adding_forex(.)
    opening <- bind_rows(opening_A, opening_B) %>% 
      group_by(currency) %>% 
      summarize(total = sum(amount) - sum(commission)) %>% 
      spread(key = currency, value = total) %>% 
      mutate(instrument_type = "Opening Cash")
    
    ## Create the closing positions
    closing_A <- cash %>% filter(transaction_date >= input$date_range[1] & 
                                   transaction_date <= input$date_range[2] & 
                                   portfolio == input$portfolio_to_choose) %>% 
      group_by(currency, instrument_type) %>% 
      summarize(amount = sum(amount), 
                commission = sum(commission)) 
    closing_B <- cash %>% filter(transaction_date >= input$date_range[1] & 
                                   transaction_date <= input$date_range[2] & 
                                   portfolio == input$portfolio_to_choose) %>% 
      adding_forex(.)
    closing <- bind_rows(closing_A, closing_B) %>% ungroup() %>% 
      select(-portfolio)
    
    temp_com <- closing %>% group_by(currency) %>% 
      summarize(amount = -sum(commission)) %>% 
      mutate(instrument_type = "Commission") 
    
    temp <- bind_rows(closing, temp_com) %>% select(-commission) %>% 
      spread(key = currency, value = amount)
    
    temp1 <- bind_rows(opening, temp) %>% select(instrument_type, everything())
    
    temp2 <- temp1[,-1] %>%
      replace(is.na(.), 0) %>%
      summarise_all(funs(sum)) %>% mutate(instrument_type = "Closing cash")
    temp3 <- bind_rows(temp1, temp2)
    temp3
  })
  
  }



######################################################################
## End of dashboard
######################################################################

shinyApp(ui, server)