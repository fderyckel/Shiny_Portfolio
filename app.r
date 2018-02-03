library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)


thePath <- here::here()

source('global.R', local = TRUE)

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
    menuItem("Cash Position", tabName = "cash", icon = icon("money")), 
    menuItem("Alerts", tabName = "alerts", icon = icon("calendar"))))

body <- dashboardBody(
  tabItems(
    #For the summary page
    tabItem(tabName = "summary", 
            fluidRow(
              valueBox(10*2, "Daily % Change"), 
              valueBox(30*1.4, "% Change YTD"), 
              valueBox(12*3.1, "% Change since Inceptin")), 
            
            fluidRow(
              column(width = 12, 
                     tabBox(title = "Summary", width = NULL,  selected = "Open positions", 
                            tabPanel(title = "Open positions", status = "primary", 
                                     dataTableOutput('tbl_open_position'), width = 9), 
                            tabPanel(title = "Closed positions", width = NULL, status = "sucess", 
                                     dataTableOutput('tbl_closed_position')), 
                            tabPanel(title = "Transactions", status = "warning")
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
           )), 
    
    #For the cash position page 
    tabItem(tabName = "cash", 
            fluidRow(
              column(width = 9, 
                     
                     #The main grah of prices
                     box(title = "Cash Positions", width = NULL, status = "primary", 
                         dataTableOutput("cash_positions")), 
                     box(title = "box2_cash", width = NULL)), 
              
              #The input boxe where to choose dates, financial instrument, etc. 
              column(width = 3, 
                     box(title = "Input", width = NULL, status = "warning", solidHeader = TRUE, 
                         uiOutput("pick_portfolio_cash"), 
                         dateRangeInput("date_range_cash", 
                                        label = "Date Range", 
                                        start = Sys.Date() - 30, end = Sys.Date())))
            ))
    
    ))

ui <- dashboardPage(header, sidebar, body)




######################################################################
## Building the server side
######################################################################
server <- function(input, output) { 
  
  ##########
  # First tab "summary", make the summary of open positions
  ##########
  output$tbl_open_position <- renderDataTable({
    # Open Position table
    open_position <- open_position %>% 
      select(Ticker = ticker, Currency = currency, `Instr. \ Type` = instrument_type, `is \ short` = is_short, 
             Position = position, `Inital \ position \ date` = first_purchase,  `Average Price` = avg_purchase_price,  
             `Last \ price \ date` = Date, `Current \ Price` = last_price, `Profit \ Percent` = profit_percent, 
             `Profit \ Base` = profit_base, `Percent \ from High` = percent_from_high)
    
    open_position
  },options = list(filter = 'top', scrollX = TRUE, pageLength = 10))

  
  output$tbl_closed_position <- renderDataTable({
    closed_position <- closed_position %>% 
      select(Ticker = ticker, Currency = currency, `Instr. \ Type` = instrument_type, `is \ short` = is_short, 
             Quantity = quantity, Date = date,  `Average Price` = average_price,  
             Price = price, `Profit` = profit, `Profit \ Percent` = profit_percent)
    
    closed_position
  })
    
  # Second tab "financial Instrument", UI Pick the financial instrument to graph
  output$pick_portfolio <- renderUI({
    open_position <- transaction %>% 
      filter(instrument_type == "Options" | instrument_type == "ETF" | instrument_type == "Stock")
    
    selectInput("portfolio_to_choose", "Portfolio", as.list(unique(open_position$portfolio)))
  })
  
  output$pick_instrument <- renderUI({
    open_position <- transaction %>% 
      filter(instrument_type == "Options" | instrument_type == "ETF" | instrument_type == "Stock") %>% 
      filter(portfolio == input$portfolio_to_choose) %>% 
      group_by(ticker, portfolio) %>% 
      #summarize(position= sum(quantity)) %>%  
      filter(transaction_date >= today() - 200) %>% 
      mutate(tickerb = str_replace(ticker, "\ .*", "")) %>% 
      arrange(tickerb)
    
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
  
  # Third tab about "cash Position" 
  output$pick_portfolio_cash <- renderUI({
    selectInput("portfolio_to_choose_cash", "Portfolio", as.list(unique(cash$portfolio)))
  })
  
  output$cash_positions <- renderDataTable({
    ## Create the opening positions
    opening_A <- cash %>% filter(transaction_date < input$date_range_cash[1] & 
                                   portfolio == input$portfolio_to_choose_cash) %>% 
      group_by(currency, instrument_type) %>% 
      summarize(amount = sum(amount), 
                commission = sum(commission)) 
    opening_B <- cash %>% 
      filter(transaction_date < input$date_range_cash[1] & 
               portfolio == input$portfolio_to_choose_cash) %>% 
      adding_forex(.)
    opening <- bind_rows(opening_A, opening_B) %>% 
      group_by(currency) %>% 
      summarize(total = sum(amount) - sum(commission)) %>% 
      spread(key = currency, value = total) %>% 
      mutate(instrument_type = "Opening Cash")
    
    ## Create the closing positions
    closing_A <- cash %>% filter(transaction_date >= input$date_range_cash[1] & 
                                   transaction_date < input$date_range_cash[2] & 
                                   portfolio == input$portfolio_to_choose_cash) %>% 
      group_by(currency, instrument_type) %>% 
      summarize(amount = sum(amount), 
                commission = sum(commission)) 
    closing_B <- cash %>% filter(transaction_date >= input$date_range_cash[1] & 
                                   transaction_date < input$date_range_cash[2] & 
                                   portfolio == input$portfolio_to_choose_cash) %>% 
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
