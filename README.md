# Shiny_Portfolio
Portfolio Management

This R program use Shiny Dashboard to manage a portfolio of financial instruments.  
At the moment, the program is based on a "transaction.csv" file where all the transactions takes place.  Go check the [transaction.csv](../master/transaction.csv) file to see how it is organized.  At the moment, it is the base from which all computations are made.  
The price of all the financial instruments are saved as "hard" data as .csv in a separate folder.  

At the moment, you can have 
* have multiple currency (although it has only limited functionality at the moment)
* have multiple brokers (in the sense that you prortfolio is comprise of sub-portfolio / account)
* various financial operations such as: equity, stocks, etf, dividends, fees and forex
* have short positions
