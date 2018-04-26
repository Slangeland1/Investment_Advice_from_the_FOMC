## :::::::::::::::::::: File Info:
## Date: 2018-03-06
## Author: Stephanie Langeland  
## File Name: `09_h15t10y_intraday_20180306_v1.R`
## Version: 1
## Previous Versions/Files: None
## Dependencies: None
## Purpose: Create intraday data set with new variables for H15T10Y.
## Input File(s): 
    ## /clean_corpus_2018-03-04/FOMC_text.rds
    ## H15T10Y Index_intraday_20180301.xlsx
## Output File(s): 
    ## h15t10y_intraday.rds
## Data Output: None
## Required by: Master's Thesis
## Status: Complete
## Machine: 2018 MacBook Pro
## R version: R version 3.4.3 (2017-11-30) -- "Kite-Eating Tree"



## :::::::::::::::::::: Set up:
rm(list = ls(all = TRUE)) ## cleans the work space
options(scipen = 999)

## :::::::::::::::::::: Create a base data set with the `release_date`, 
## :::::::::::::::::::: `release_time`, and `type` variables from the 
## :::::::::::::::::::: `FOMC_text.rds`:
fomc <- readRDS("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/clean_corpus_2018-03-04/FOMC_text.rds")

str(fomc)



## :::::::::::::::::::: H15T10Y:
library(readxl)

import_file <- "/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/H15T10Y Index_intraday_20180301.xlsx"

## ::::::::::: Trade data tab:
trade <- read_xlsx(
  path = import_file,
  sheet = "trade"
)

str(trade)

View(trade)




## ::::::::::: Ask and Bid data tab:
ask_bid <- read_xlsx(
  path = import_file,
  sheet = "ask_bid"
)

str(ask_bid)

View(ask_bid)





## ::::::::::: Merge the trade, ask, and bid data:
h15T10y_1 <- merge(
  ask_bid,
  trade,
  all = T
)

str(h15T10y_1)

View(h15T10y_1)


h15T10y_1$Date <- as.character(h15T10y_1$DateTime)

ncol(h15T10y_1)

h15T10y_1[ , 24] <- str_extract(h15T10y_1$Date,
                                " .*$") 

colnames(h15T10y_1)[24] <- "h15T10y_time"

h15T10y_1$h15T10y_time <- str_replace_all(h15T10y_1$h15T10y_time,
                                          "[:space:]",
                                          "")

h15T10y_1$h15T10y_time <- times(h15T10y_1$h15T10y_time)


h15T10y_1$Date <- str_replace_all(h15T10y_1$Date,
                                  " .*$",
                                  "")

h15T10y_1$Date <- str_replace_all(h15T10y_1$Date,
                                  "[:space:]",
                                  "")

h15T10y_1$Date <- as.Date(h15T10y_1$Date,
                       format = "%Y-%m-%d")




## ::::::::::: All of the varaibles are the same - and there's only one observation per day - totally useless:
length(unique(h15T10y_1$Date))

summary(h15T10y_1$H15T10Y_BID_LOW_PRICE == h15T10y_1$H15T10Y_BID_HIGH_PRICE)

summary(h15T10y_1$H15T10Y_BID_LOW_PRICE == h15T10y_1$H15T10Y_BID_CLOSE_PRICE)

summary(h15T10y_1$H15T10Y_BID_OPEN_PRICE == h15T10y_1$H15T10Y_BID_CLOSE_PRICE)

summary(h15T10y_1$H15T10Y_ASK_LOW_PRICE == h15T10y_1$H15T10Y_ASK_HIGH_PRICE)

summary(h15T10y_1$H15T10Y_ASK_CLOSE_PRICE == h15T10y_1$H15T10Y_ASK_HIGH_PRICE)

summary(h15T10y_1$H15T10Y_ASK_CLOSE_PRICE == h15T10y_1$H15T10Y_ASK_OPEN_PRICE)

summary(h15T10y_1$H15T10Y_TRADE_LOW_PRICE == h15T10y_1$H15T10Y_TRADE_HIGH_PRICE)

summary(h15T10y_1$H15T10Y_TRADE_CLOSE_PRICE == h15T10y_1$H15T10Y_TRADE_OPEN_PRICE)

summary(h15T10y_1$H15T10Y_TRADE_CLOSE_PRICE == h15T10y_1$H15T10Y_BID_LOW_PRICE)

summary(h15T10y_1$H15T10Y_TRADE_CLOSE_PRICE == h15T10y_1$H15T10Y_ASK_HIGH_PRICE)

