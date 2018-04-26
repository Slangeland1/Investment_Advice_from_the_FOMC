## :::::::::::::::::::: File Info:
## Date: 2018-04-18
## Author: Stephanie Langeland  
## File Name: `03_open_close_data_20180418_v1.R`
## Version: 3
## Previous Versions/Files: 03_open_close_data_20180416_v1.R
## Dependencies: None
## Purpose: Create open and close data set with new variables.
## Input File(s): 
    ## open close data.xlsx
    ## TNX_open close_yahoo finance_20180301.csv
    ## /clean_corpus_2018-04-18/FOMC_text.rds
## Output File(s): 
    ## open_close_data.rds
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
fomc <- readRDS("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/clean_corpus_2018-04-18/FOMC_text.rds")

str(fomc)



## :::::::::::::::::::: Open vs. Close (1995 - 2018) Changes:
## ::::::::::: Load BICLB10Y, H15T10Y, SPX, CT10Govt, USSOC, VIX, CLAComdty, 
## ::::::::::: & MOODCBAA Data Sets:
library(gdata)

openclose_path1 <- "/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/open close data.xlsx"

openclose1 <- read.xls(
  openclose_path1,
  sheet = "all_data"
)

str(openclose1)

View(openclose1)

openclose1$Date <- as.Date(openclose1$Date)



## ::::::::::: 2.2) Load TNX Data Set:
tnx_openclose_path <- "/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/TNX_open close_yahoo finance_20180301.csv"

tnx_openclose <- read.csv(tnx_openclose_path)

str(tnx_openclose)

View(tnx_openclose)

tnx_openclose$Date <- as.Date(tnx_openclose$Date)



## ::::::::::: 2.3) Merge Data Sets:
oc_data <- merge(
  x = openclose1,
  y = tnx_openclose,
  by = "Date",
  all = T
)

str(oc_data)

summary(oc_data)

View(oc_data)


## ::::: FOMC data set - reformat to merge:

fomc <- readRDS("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/clean_corpus_2018-03-04/FOMC_text.rds")

str(fomc)

stmts <- subset(
  fomc, 
  subset = c(
    type == "statement"
  )
)

colnames(stmts)

colnames(stmts)[1] <- "stmt_text"

stmts <- stmts[ , c(1, 3:4)]


mins <- subset(
  fomc, 
  subset = c(
    type == "minutes"
  )
)

colnames(mins)

colnames(mins)[1] <- "min_text"


mins <- mins[ ,c(1, 3:4)]

fomc_final <- merge(
  x = stmts, 
  y = mins,
  all = T
)


oc_data <- merge(
  x = oc_data,
  y = fomc_final,
  by.x = "Date",
  by.y = "release_date",
  all = T
)

str(oc_data)

colnames(oc_data)[1] <- "release_date"

## Remove variables that are only 0s or NAs:
oc_data <- oc_data[ , -c(4, ## "H15T10Y_VOLUME",
                         22, ## "VIX_ASK_PRICE", 
                         26, ## "VIX_BID_PRICE",
                         40 ##"TNX_VOLUME"
                         )]




## ::::::::::: 2.4) Calculate the change in the response variables:
sort(colnames(oc_data))


## ::::: CLAComdty:
oc_data$CLAComdty_change <- oc_data$CLAComdty_LAST_PRICE - oc_data$CLAComdty_OPEN_PRICE

oc_data$CLAComdty_change_cat <- ifelse(
  oc_data$CLAComdty_change > 0, 
  1, 
  NA
)

oc_data$CLAComdty_change_cat <- ifelse(
  oc_data$CLAComdty_change < 0,
  -1,
  oc_data$CLAComdty_change_cat
)

oc_data$CLAComdty_change_cat <- ifelse(
  oc_data$CLAComdty_change == 0,
  0,
  oc_data$CLAComdty_change_cat
)


oc_data$CLAComdty_close_perc_open <- oc_data$CLAComdty_LAST_PRICE / oc_data$CLAComdty_OPEN_PRICE



## ::::: H15T10Y:
oc_data$H15T10Y_change <- oc_data$H15T10Y_LAST_PRICE - oc_data$H15T10Y_OPEN_PRICE

oc_data$H15T10Y_change_cat <- ifelse(
  oc_data$H15T10Y_change > 0 , 
  1, 
  NA
)

oc_data$H15T10Y_change_cat <- ifelse(
  oc_data$H15T10Y_change < 0,
  -1,
  oc_data$H15T10Y_change_cat
)

oc_data$H15T10Y_change_cat <- ifelse(
  oc_data$H15T10Y_change == 0,
  0,
  oc_data$H15T10Y_change_cat
)

oc_data$H15T10Y_close_perc_open <- oc_data$H15T10Y_LAST_PRICE / oc_data$H15T10Y_OPEN_PRICE





## ::::: SPX:
oc_data$SPX_change <- oc_data$SPX_LAST_PRICE - oc_data$SPX_OPEN_PRICE

oc_data$SPX_change_cat <- ifelse(
  oc_data$SPX_change > 0,
  1,
  NA
)

oc_data$SPX_change_cat <- ifelse(
  oc_data$SPX_change < 0,
  -1,
  oc_data$SPX_change_cat
)

oc_data$SPX_change_cat <- ifelse(
  oc_data$SPX_change == 0,
  0,
  oc_data$SPX_change_cat
)


oc_data$SPX_close_perc_open <- oc_data$SPX_LAST_PRICE / oc_data$SPX_OPEN_PRICE



## ::::: TNX:
oc_data$TNX_change <- oc_data$TNX_CLOSE_PRICE - oc_data$TNX_OPEN_PRICE

oc_data$TNX_change_cat <- ifelse(
  oc_data$TNX_change > 0,
  1,
  NA
)

oc_data$TNX_change_cat <- ifelse(
  oc_data$TNX_change < 0,
  -1,
  oc_data$TNX_change_cat
)

oc_data$TNX_change_cat <- ifelse(
  oc_data$TNX_change == 0,
  0,
  oc_data$TNX_change_cat
)

oc_data$TNX_close_perc_open <- oc_data$TNX_CLOSE_PRICE / oc_data$TNX_OPEN_PRICE



## ::::: USSOC:
oc_data$USSOC_change <- oc_data$USSOC_LAST_PRICE - oc_data$USSOC_OPEN_PRICE

oc_data$USSOC_change_cat <- ifelse(
  oc_data$USSOC_change > 0, 
  1, 
  NA
)

oc_data$USSOC_change_cat <- ifelse(
  oc_data$USSOC_change < 0, 
  -1, 
  oc_data$USSOC_change_cat
)

oc_data$USSOC_change_cat <- ifelse(
  oc_data$USSOC_change == 0, 
  0, 
  oc_data$USSOC_change_cat
)


oc_data$USSOC_close_perc_open <- oc_data$USSOC_LAST_PRICE / oc_data$USSOC_OPEN_PRICE



## ::::: VIX:
oc_data$VIX_change <- oc_data$VIX_LAST_PRICE - oc_data$VIX_OPEN_PRICE

oc_data$VIX_change_cat <- ifelse(
  oc_data$VIX_change > 0,
  1,
  NA
)

oc_data$VIX_change_cat <- ifelse(
  oc_data$VIX_change < 0,
  -1,
  oc_data$VIX_change_cat
)

oc_data$VIX_change_cat <- ifelse(
  oc_data$VIX_change == 0,
  0,
  oc_data$VIX_change_cat
)

oc_data$VIX_close_perc_open <- oc_data$VIX_LAST_PRICE / oc_data$VIX_OPEN_PRICE

library(VGAM)

oc_data$VIX_close_perc_open <- reciprocal(oc_data$VIX_close_perc_open) ## take the reciprocal of VIX so that higher is "better"


## :::::::::::::::::::: Rearrange columns in oc_data:
column_order <- c(
  "meeting_date",
  "release_date",
  "min_text",
  "stmt_text",
  "BICLB10Y_LAST_PRICE",
  "CLAComdty_ASK_PRICE",
  "CLAComdty_BID_PRICE",
  "CLAComdty_change",
  "CLAComdty_change_cat",
  "CLAComdty_HIGH_PRICE",
  "CLAComdty_LAST_PRICE",
  "CLAComdty_LOW_PRICE",
  "CLAComdty_OPEN_PRICE",
  "CLAComdty_VOLUME",
  "H15T10Y_change",
  "H15T10Y_change_cat",
  "H15T10Y_HIGH_PRICE",
  "H15T10Y_LAST_PRICE",
  "H15T10Y_LOW_PRICE",
  "H15T10Y_OPEN_PRICE",
  "MOODCBAA_LAST_PRICE",
  "SPX_ASK_PRICE",
  "SPX_BID_PRICE",
  "SPX_change",
  "SPX_change_cat",
  "SPX_HIGH_PRICE",
  "SPX_LAST_PRICE",
  "SPX_LOW_PRICE",
  "SPX_OPEN_PRICE",
  "SPX_VOLUME",
  "TNX_ADJ_CLOSE_PRICE",
  "TNX_change",
  "TNX_change_cat",
  "TNX_CLOSE_PRICE",
  "TNX_HIGH_PRICE",
  "TNX_LOW_PRICE",
  "TNX_OPEN_PRICE",
  "USSOC_ASK_PRICE",
  "USSOC_BID_PRICE",
  "USSOC_change",
  "USSOC_change_cat",
  "USSOC_HIGH_PRICE",
  "USSOC_LAST_PRICE",
  "USSOC_LOW_PRICE",
  "USSOC_OPEN_PRICE",
  "VIX_change",
  "VIX_change_cat",
  "VIX_HIGH_PRICE",
  "VIX_LAST_PRICE",
  "VIX_LOW_PRICE",
  "VIX_OPEN_PRICE",
  "CLAComdty_close_perc_open",
  "H15T10Y_close_perc_open",
  "SPX_close_perc_open",
  "TNX_close_perc_open",
  "USSOC_close_perc_open",
  "VIX_close_perc_open"
)

oc_data_final <- oc_data[ , column_order]



## :::::::::::::::::::: Save the data set:
setwd("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files")

saveRDS(oc_data_final,
        file = "open_close_data.rds")
