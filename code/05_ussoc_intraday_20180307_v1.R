## :::::::::::::::::::: File Info:
## Date: 2018-03-06
## Author: Stephanie Langeland  
## File Name: `05_ussoc_intraday_20180307_v1.R`
## Version: 2
## Previous Versions/Files: `ussoc_intraday_20180306_v1.R`
## Dependencies: None
## Purpose: Create intraday data set with new variables for USSOC.
## Input File(s): 
    ## /clean_corpus_2018-03-04/FOMC_text.rds
    ## USSOC_intraday_bloomberg_20180301.xlsx
## Output File(s): 
    ## ussoc_intraday.rds
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



## :::::::::::::::::::: USSOC:
library(readxl)


## ::::::::::: Trade data tab:
trade <- read_xlsx(
  path = "/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/USSOC_intraday_bloomberg_20180301.xlsx",
  sheet = "trade"
)

str(trade)

View(trade)



## ::::::::::: Bid data tab:
bid <- read_xlsx(
  path = "/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/USSOC_intraday_bloomberg_20180301.xlsx",
  sheet = "bid"
)

str(bid)

View(bid)



## ::::::::::: Ask data tab:
ask <- read_xlsx(
  path = "/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/USSOC_intraday_bloomberg_20180301.xlsx",
  sheet = "ask"
)

str(ask)

View(ask)



## ::::::::::: Merge the trade, ask, and bid data:
data1 <- merge(
  ask, 
  bid, 
  by = "DateTime",
  all = T
)

ussoc1 <- merge(
  data1, 
  trade, 
  by = "DateTime",
  all = T
)

str(ussoc1)

View(ussoc1)

ussoc1$Date <- as.character(ussoc1$DateTime)

ncol(ussoc1)

ussoc1[ , 24] <- str_extract(ussoc1$Date,
                             " .*$") 

colnames(ussoc1)[24] <- "ussoc_time"

ussoc1$ussoc_time <- str_replace_all(ussoc1$ussoc_time,
                                     "[:space:]",
                                      "")

ussoc1$ussoc_time <- times(ussoc1$ussoc_time)


ussoc1$Date <- str_replace_all(ussoc1$Date,
                               " .*$",
                               "")

ussoc1$Date <- str_replace_all(ussoc1$Date,
                               "[:space:]",
                               "")

ussoc1$Date <- as.Date(ussoc1$Date,
                       format = "%Y-%m-%d")




## ::::::::::: Calculate the standard deviation (change in the response variables) for
## ::::::::::: the whole day:
library(dplyr)

USSOC_ASK_OPEN_PRICE_sd_day <- ussoc1 %>%
  group_by(Date) %>%
  summarize(USSOC_ASK_OPEN_PRICE_sd_day = sd(USSOC_ASK_OPEN_PRICE,
                                             na.rm = T))

USSOC_ASK_HIGH_PRICE_sd_day <- ussoc1 %>%
  group_by(Date) %>%
  summarize(USSOC_ASK_HIGH_PRICE_sd_day = sd(USSOC_ASK_HIGH_PRICE,
                                             na.rm = T))

USSOC_ASK_CLOSE_PRICE_sd_day <- ussoc1 %>%
  group_by(Date) %>%
  summarize(USSOC_ASK_CLOSE_PRICE_sd_day = sd(USSOC_ASK_CLOSE_PRICE,
                                              na.rm = T))

USSOC_ASK_LOW_PRICE_sd_day <- ussoc1 %>%
  group_by(Date) %>%
  summarize(USSOC_ASK_LOW_PRICE_sd_day = sd(USSOC_ASK_LOW_PRICE,
                                            na.rm = T))

USSOC_BID_OPEN_PRICE_sd_day <- ussoc1 %>%
  group_by(Date) %>%
  summarize(USSOC_BID_OPEN_PRICE_sd_day = sd(USSOC_BID_OPEN_PRICE,
                                             na.rm = T))

USSOC_BID_HIGH_PRICE_sd_day <- ussoc1 %>%
  group_by(Date) %>%
  summarize(USSOC_BID_HIGH_PRICE_sd_day = sd(USSOC_BID_HIGH_PRICE,
                                             na.rm = T))

USSOC_BID_CLOSE_PRICE_sd_day <- ussoc1 %>%
  group_by(Date) %>%
  summarize(USSOC_BID_CLOSE_PRICE_sd_day = sd(USSOC_BID_CLOSE_PRICE,
                                              na.rm = T))

USSOC_BID_LOW_PRICE_sd_day <- ussoc1 %>%
  group_by(Date) %>%
  summarize(USSOC_BID_LOW_PRICE_sd_day = sd(USSOC_BID_LOW_PRICE,
                                            na.rm = T))

USSOC_TRADE_OPEN_PRICE_sd_day <- ussoc1 %>%
  group_by(Date) %>%
  summarize(USSOC_TRADE_OPEN_PRICE_sd_day = sd(USSOC_TRADE_OPEN_PRICE,
                                               na.rm = T))

USSOC_TRADE_HIGH_PRICE_sd_day <- ussoc1 %>%
  group_by(Date) %>%
  summarize(USSOC_TRADE_HIGH_PRICE_sd_day = sd(USSOC_TRADE_HIGH_PRICE,
                                               na.rm = T))

USSOC_TRADE_CLOSE_PRICE_sd_day <- ussoc1 %>%
  group_by(Date) %>%
  summarize(USSOC_TRADE_CLOSE_PRICE_sd_day = sd(USSOC_TRADE_CLOSE_PRICE,
                                                na.rm = T))

USSOC_TRADE_LOW_PRICE_sd_day <- ussoc1 %>%
  group_by(Date) %>%
  summarize(USSOC_TRADE_LOW_PRICE_sd_day = sd(USSOC_TRADE_LOW_PRICE,
                                              na.rm = T))




## ::::::::::: Calculate the standard deviation (change in the response variables) from
## ::::::::::: 2 PM - closing:
USSOC_ASK_OPEN_PRICE_sd_postrelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time >= "14:00:00") %>%
  summarize(USSOC_ASK_OPEN_PRICE_sd_postrelease = sd(USSOC_ASK_OPEN_PRICE,
                                                     na.rm = T))

USSOC_ASK_HIGH_PRICE_sd_postrelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time >= "14:00:00") %>%
  summarize(USSOC_ASK_HIGH_PRICE_sd_postrelease = sd(USSOC_ASK_HIGH_PRICE,
                                                     na.rm = T))

USSOC_ASK_CLOSE_PRICE_sd_postrelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time >= "14:00:00") %>%
  summarize(USSOC_ASK_CLOSE_PRICE_sd_postrelease = sd(USSOC_ASK_CLOSE_PRICE,
                                                      na.rm = T))

USSOC_ASK_LOW_PRICE_sd_postrelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time >= "14:00:00") %>%
  summarize(USSOC_ASK_LOW_PRICE_sd_postrelease = sd(USSOC_ASK_LOW_PRICE,
                                                    na.rm = T))

USSOC_BID_OPEN_PRICE_sd_postrelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time >= "14:00:00") %>%
  summarize(USSOC_BID_OPEN_PRICE_sd_postrelease = sd(USSOC_BID_OPEN_PRICE,
                                                     na.rm = T))

USSOC_BID_HIGH_PRICE_sd_postrelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time >= "14:00:00") %>%
  summarize(USSOC_BID_HIGH_PRICE_sd_postrelease = sd(USSOC_BID_HIGH_PRICE,
                                                     na.rm = T))

USSOC_BID_CLOSE_PRICE_sd_postrelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time >= "14:00:00") %>%
  summarize(USSOC_BID_CLOSE_PRICE_sd_postrelease = sd(USSOC_BID_CLOSE_PRICE,
                                                      na.rm = T))

USSOC_BID_LOW_PRICE_sd_postrelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time >= "14:00:00") %>%
  summarize(USSOC_BID_LOW_PRICE_sd_postrelease = sd(USSOC_BID_LOW_PRICE,
                                                    na.rm = T))

USSOC_TRADE_OPEN_PRICE_sd_postrelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time >= "14:00:00") %>%
  summarize(USSOC_TRADE_OPEN_PRICE_sd_postrelease = sd(USSOC_TRADE_OPEN_PRICE,
                                                       na.rm = T))

USSOC_TRADE_HIGH_PRICE_sd_postrelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time >= "14:00:00") %>%
  summarize(USSOC_TRADE_HIGH_PRICE_sd_postrelease = sd(USSOC_TRADE_HIGH_PRICE,
                                                       na.rm = T))

USSOC_TRADE_CLOSE_PRICE_sd_postrelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time >= "14:00:00") %>%
  summarize(USSOC_TRADE_CLOSE_PRICE_sd_postrelease = sd(USSOC_TRADE_CLOSE_PRICE,
                                                        na.rm = T))

USSOC_TRADE_LOW_PRICE_sd_postrelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time >= "14:00:00") %>%
  summarize(USSOC_TRADE_LOW_PRICE_sd_postrelease = sd(USSOC_TRADE_LOW_PRICE,
                                                      na.rm = T))




## ::::::::::: Calculate the standard deviation (change in the response variables) from
## ::::::::::: opening - 2 PM:
USSOC_ASK_OPEN_PRICE_sd_prerelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time < "14:00:00") %>%
  summarize(USSOC_ASK_OPEN_PRICE_sd_prerelease = sd(USSOC_ASK_OPEN_PRICE,
                                                    na.rm = T))

USSOC_ASK_HIGH_PRICE_sd_prerelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time < "14:00:00") %>%
  summarize(USSOC_ASK_HIGH_PRICE_sd_prerelease = sd(USSOC_ASK_HIGH_PRICE,
                                                    na.rm = T))

USSOC_ASK_CLOSE_PRICE_sd_prerelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time < "14:00:00") %>%
  summarize(USSOC_ASK_CLOSE_PRICE_sd_prerelease = sd(USSOC_ASK_CLOSE_PRICE,
                                                     na.rm = T))

USSOC_ASK_LOW_PRICE_sd_prerelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time < "14:00:00") %>%
  summarize(USSOC_ASK_LOW_PRICE_sd_prerelease = sd(USSOC_ASK_LOW_PRICE,
                                                   na.rm = T))

USSOC_BID_OPEN_PRICE_sd_prerelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time < "14:00:00") %>%
  summarize(USSOC_BID_OPEN_PRICE_sd_prerelease = sd(USSOC_BID_OPEN_PRICE,
                                                    na.rm = T))

USSOC_BID_HIGH_PRICE_sd_prerelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time < "14:00:00") %>%
  summarize(USSOC_BID_HIGH_PRICE_sd_prerelease = sd(USSOC_BID_HIGH_PRICE,
                                                    na.rm = T))

USSOC_BID_CLOSE_PRICE_sd_prerelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time < "14:00:00") %>%
  summarize(USSOC_BID_CLOSE_PRICE_sd_prerelease = sd(USSOC_BID_CLOSE_PRICE,
                                                     na.rm = T))

USSOC_BID_LOW_PRICE_sd_prerelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time < "14:00:00") %>%
  summarize(USSOC_BID_LOW_PRICE_sd_prerelease = sd(USSOC_BID_LOW_PRICE,
                                                   na.rm = T))

USSOC_TRADE_OPEN_PRICE_sd_prerelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time < "14:00:00") %>%
  summarize(USSOC_TRADE_OPEN_PRICE_sd_prerelease = sd(USSOC_TRADE_OPEN_PRICE,
                                                      na.rm = T))

USSOC_TRADE_HIGH_PRICE_sd_prerelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time < "14:00:00") %>%
  summarize(USSOC_TRADE_HIGH_PRICE_sd_prerelease = sd(USSOC_TRADE_HIGH_PRICE,
                                                      na.rm = T))

USSOC_TRADE_CLOSE_PRICE_sd_prerelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time < "14:00:00") %>%
  summarize(USSOC_TRADE_CLOSE_PRICE_sd_prerelease = sd(USSOC_TRADE_CLOSE_PRICE,
                                                       na.rm = T))

USSOC_TRADE_LOW_PRICE_sd_prerelease <- ussoc1 %>%
  group_by(Date) %>%
  filter(ussoc_time < "14:00:00") %>%
  summarize(USSOC_TRADE_LOW_PRICE_sd_prerelease = sd(USSOC_TRADE_LOW_PRICE,
                                                     na.rm = T))




## ::::::::::: Merge all data sets:
ussoc2 <- cbind(
  USSOC_ASK_CLOSE_PRICE_sd_day,
  USSOC_ASK_OPEN_PRICE_sd_day[ , 2],
  USSOC_BID_LOW_PRICE_sd_day[ , 2],
  USSOC_TRADE_HIGH_PRICE_sd_day[ , 2],
  USSOC_ASK_HIGH_PRICE_sd_day[ , 2],
  USSOC_BID_CLOSE_PRICE_sd_day[ , 2],
  USSOC_BID_OPEN_PRICE_sd_day[ , 2],
  USSOC_TRADE_LOW_PRICE_sd_day[ , 2],
  USSOC_ASK_LOW_PRICE_sd_day[ , 2],
  USSOC_BID_HIGH_PRICE_sd_day[ , 2],
  USSOC_TRADE_CLOSE_PRICE_sd_day[ , 2],
  USSOC_TRADE_OPEN_PRICE_sd_day[ , 2]
)

ussoc3 <- cbind(
  USSOC_ASK_CLOSE_PRICE_sd_postrelease,
  USSOC_ASK_HIGH_PRICE_sd_postrelease[ , 2],
  USSOC_ASK_LOW_PRICE_sd_postrelease[ , 2],
  USSOC_ASK_OPEN_PRICE_sd_postrelease[ , 2],
  USSOC_BID_CLOSE_PRICE_sd_postrelease[ , 2],
  USSOC_BID_HIGH_PRICE_sd_postrelease[ , 2],
  USSOC_BID_LOW_PRICE_sd_postrelease[ , 2],
  USSOC_BID_OPEN_PRICE_sd_postrelease[ , 2],
  USSOC_TRADE_CLOSE_PRICE_sd_postrelease[ , 2],
  USSOC_TRADE_HIGH_PRICE_sd_postrelease[ , 2],
  USSOC_TRADE_LOW_PRICE_sd_postrelease[ , 2],
  USSOC_TRADE_OPEN_PRICE_sd_postrelease[ , 2]
)

ussoc4 <- cbind(
  USSOC_ASK_CLOSE_PRICE_sd_prerelease,
  USSOC_ASK_HIGH_PRICE_sd_prerelease[ , 2],
  USSOC_ASK_LOW_PRICE_sd_prerelease[ , 2],
  USSOC_ASK_OPEN_PRICE_sd_prerelease[ , 2],
  USSOC_BID_CLOSE_PRICE_sd_prerelease[ , 2],
  USSOC_BID_HIGH_PRICE_sd_prerelease[ , 2],
  USSOC_BID_LOW_PRICE_sd_prerelease[ , 2],
  USSOC_BID_OPEN_PRICE_sd_prerelease[ , 2],
  USSOC_TRADE_CLOSE_PRICE_sd_prerelease[ , 2],
  USSOC_TRADE_HIGH_PRICE_sd_prerelease[ , 2],
  USSOC_TRADE_LOW_PRICE_sd_prerelease[ , 2],
  USSOC_TRADE_OPEN_PRICE_sd_prerelease[ , 2]
)

ussoc5 <- merge(
  ussoc2,
  ussoc3,
  all = T
)

ussoc6 <- merge(
  ussoc5,
  ussoc4,
  all = T
)




## ::::::::::: Calculate the directional change: value at closing - value at 2pm:
ussoc_sub <- ussoc1 %>%
  filter(
    ussoc_time >= "14:00:00"
  )

values_at_2pm <- ussoc_sub %>% 
  group_by(Date) %>% 
  slice(which.min(ussoc_time))

colnames(values_at_2pm)[2] <- "USSOC_ASK_OPEN_PRICE_2pm"
colnames(values_at_2pm)[3] <- "USSOC_ASK_HIGH_PRICE_2pm"
colnames(values_at_2pm)[4] <- "USSOC_ASK_CLOSE_PRICE_2pm"
colnames(values_at_2pm)[5] <- "USSOC_ASK_LOW_PRICE_2pm"
colnames(values_at_2pm)[9] <- "USSOC_BID_OPEN_PRICE_2pm"
colnames(values_at_2pm)[10] <- "USSOC_BID_HIGH_PRICE_2pm"
colnames(values_at_2pm)[11] <- "USSOC_BID_CLOSE_PRICE_2pm"
colnames(values_at_2pm)[12] <- "USSOC_BID_LOW_PRICE_2pm"
colnames(values_at_2pm)[16] <- "USSOC_TRADE_OPEN_PRICE_2pm"
colnames(values_at_2pm)[17] <- "USSOC_TRADE_HIGH_PRICE_2pm"
colnames(values_at_2pm)[18] <- "USSOC_TRADE_CLOSE_PRICE_2pm"
colnames(values_at_2pm)[19] <- "USSOC_TRADE_LOW_PRICE_2pm"
colnames(values_at_2pm)[24] <- "ussoc_time_2pm"

values_at_2pm <- values_at_2pm[ , c(23:24, 2:5, 9:12, 16:19)]



values_at_closing <- ussoc_sub %>% 
  group_by(Date) %>% 
  slice(which.max(ussoc_time))

colnames(values_at_closing)[2] <- "USSOC_ASK_OPEN_PRICE_closing"
colnames(values_at_closing)[3] <- "USSOC_ASK_HIGH_PRICE_closing"
colnames(values_at_closing)[4] <- "USSOC_ASK_CLOSE_PRICE_closing"
colnames(values_at_closing)[5] <- "USSOC_ASK_LOW_PRICE_closing"
colnames(values_at_closing)[9] <- "USSOC_BID_OPEN_PRICE_closing"
colnames(values_at_closing)[10] <- "USSOC_BID_HIGH_PRICE_closing"
colnames(values_at_closing)[11] <- "USSOC_BID_CLOSE_PRICE_closing"
colnames(values_at_closing)[12] <- "USSOC_BID_LOW_PRICE_closing"
colnames(values_at_closing)[16] <- "USSOC_TRADE_OPEN_PRICE_closing"
colnames(values_at_closing)[17] <- "USSOC_TRADE_HIGH_PRICE_closing"
colnames(values_at_closing)[18] <- "USSOC_TRADE_CLOSE_PRICE_closing"
colnames(values_at_closing)[19] <- "USSOC_TRADE_LOW_PRICE_closing"
colnames(values_at_closing)[24] <- "ussoc_time_closing"


values_at_closing <- values_at_closing[ , c(23:24, 2:5, 9:12, 16:19)]


tmp <- merge(
  x = values_at_2pm,
  y = values_at_closing,
  by = "Date"
)


tmp$USSOC_TRADE_LOW_PRICE_postrelease_chg  <- tmp$USSOC_TRADE_LOW_PRICE_closing - 
  tmp$USSOC_TRADE_LOW_PRICE_2pm

tmp$USSOC_TRADE_CLOSE_PRICE_postrelease_chg <- tmp$USSOC_TRADE_CLOSE_PRICE_closing - 
  tmp$USSOC_TRADE_CLOSE_PRICE_2pm

tmp$USSOC_TRADE_HIGH_PRICE_postrelease_chg <- tmp$USSOC_TRADE_HIGH_PRICE_closing - 
  tmp$USSOC_TRADE_HIGH_PRICE_2pm

tmp$USSOC_TRADE_OPEN_PRICE_postrelease_chg <- tmp$USSOC_TRADE_OPEN_PRICE_closing - 
  tmp$USSOC_TRADE_OPEN_PRICE_2pm

tmp$USSOC_ASK_LOW_PRICE_postrelease_chg <- tmp$USSOC_ASK_LOW_PRICE_closing - 
  tmp$USSOC_ASK_LOW_PRICE_2pm

tmp$USSOC_ASK_HIGH_PRICE_postrelease_chg <- tmp$USSOC_ASK_HIGH_PRICE_closing -
  tmp$USSOC_ASK_HIGH_PRICE_2pm

tmp$USSOC_ASK_CLOSE_PRICE_postrelease_chg <- tmp$USSOC_ASK_CLOSE_PRICE_closing - 
  tmp$USSOC_ASK_CLOSE_PRICE_2pm

tmp$USSOC_ASK_OPEN_PRICE_postrelease_chg <- tmp$USSOC_ASK_OPEN_PRICE_closing -
  tmp$USSOC_ASK_OPEN_PRICE_2pm

tmp$USSOC_BID_CLOSE_PRICE_postrelease_chg <- tmp$USSOC_BID_CLOSE_PRICE_closing - 
  tmp$USSOC_BID_CLOSE_PRICE_2pm

tmp$USSOC_BID_OPEN_PRICE_postrelease_chg <- tmp$USSOC_BID_OPEN_PRICE_closing - 
  tmp$USSOC_BID_OPEN_PRICE_2pm

tmp$USSOC_BID_HIGH_PRICE_postrelease_chg <- tmp$USSOC_BID_HIGH_PRICE_closing -
  tmp$USSOC_BID_HIGH_PRICE_2pm

tmp$USSOC_BID_LOW_PRICE_postrelease_chg <- tmp$USSOC_BID_LOW_PRICE_closing - 
  tmp$USSOC_BID_LOW_PRICE_2pm


ussoc7 <- merge(
  ussoc6,
  tmp,
  all = T
)

ussoc_fomc <- merge(
  x = fomc[ , 2:5],
  y = ussoc7,
  by.x = "release_date",
  by.y = "Date",
  all = T
)




## ::::::::::: Make the "_postrelease_chg" variables into categorical varibales == -1, 0, 1:
ussoc_fomc$USSOC_TRADE_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_TRADE_LOW_PRICE_postrelease_chg < 0,
  -1, 
  NA
)

ussoc_fomc$USSOC_TRADE_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_TRADE_LOW_PRICE_postrelease_chg > 0,
  1, 
  ussoc_fomc$USSOC_TRADE_LOW_PRICE_postrelease_chg_cat
)

ussoc_fomc$USSOC_TRADE_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_TRADE_LOW_PRICE_postrelease_chg == 0,
  0, 
  ussoc_fomc$USSOC_TRADE_LOW_PRICE_postrelease_chg_cat
)


ussoc_fomc$USSOC_TRADE_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_TRADE_CLOSE_PRICE_postrelease_chg < 0, 
  -1,
  NA
)

ussoc_fomc$USSOC_TRADE_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_TRADE_CLOSE_PRICE_postrelease_chg > 0, 
  1,
  ussoc_fomc$USSOC_TRADE_CLOSE_PRICE_postrelease_chg_cat
)

ussoc_fomc$USSOC_TRADE_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_TRADE_CLOSE_PRICE_postrelease_chg == 0, 
  0,
  ussoc_fomc$USSOC_TRADE_CLOSE_PRICE_postrelease_chg_cat
)


ussoc_fomc$USSOC_TRADE_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_TRADE_OPEN_PRICE_postrelease_chg < 0, 
  -1,
  NA
)

ussoc_fomc$USSOC_TRADE_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_TRADE_OPEN_PRICE_postrelease_chg > 0, 
  1,
  ussoc_fomc$USSOC_TRADE_OPEN_PRICE_postrelease_chg_cat
)

ussoc_fomc$USSOC_TRADE_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_TRADE_OPEN_PRICE_postrelease_chg == 0, 
  0,
  ussoc_fomc$USSOC_TRADE_OPEN_PRICE_postrelease_chg_cat
)


ussoc_fomc$USSOC_ASK_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_ASK_HIGH_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ussoc_fomc$USSOC_ASK_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_ASK_HIGH_PRICE_postrelease_chg > 0,
  1,
  ussoc_fomc$USSOC_ASK_HIGH_PRICE_postrelease_chg_cat
)

ussoc_fomc$USSOC_ASK_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_ASK_HIGH_PRICE_postrelease_chg == 0,
  0,
  ussoc_fomc$USSOC_ASK_HIGH_PRICE_postrelease_chg_cat
)


ussoc_fomc$USSOC_ASK_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_ASK_OPEN_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ussoc_fomc$USSOC_ASK_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_ASK_OPEN_PRICE_postrelease_chg > 0,
  1,
  ussoc_fomc$USSOC_ASK_OPEN_PRICE_postrelease_chg_cat
)

ussoc_fomc$USSOC_ASK_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_ASK_OPEN_PRICE_postrelease_chg == 0,
  0,
  ussoc_fomc$USSOC_ASK_OPEN_PRICE_postrelease_chg_cat
)


ussoc_fomc$USSOC_BID_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_BID_OPEN_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ussoc_fomc$USSOC_BID_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_BID_OPEN_PRICE_postrelease_chg > 0,
  1,
  ussoc_fomc$USSOC_BID_OPEN_PRICE_postrelease_chg_cat
)

ussoc_fomc$USSOC_BID_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_BID_OPEN_PRICE_postrelease_chg == 0,
  0,
  ussoc_fomc$USSOC_BID_OPEN_PRICE_postrelease_chg_cat
)


ussoc_fomc$USSOC_BID_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_BID_LOW_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ussoc_fomc$USSOC_BID_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_BID_LOW_PRICE_postrelease_chg > 0,
  1,
  ussoc_fomc$USSOC_BID_LOW_PRICE_postrelease_chg_cat
)

ussoc_fomc$USSOC_BID_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_BID_LOW_PRICE_postrelease_chg == 0,
  0,
  ussoc_fomc$USSOC_BID_LOW_PRICE_postrelease_chg_cat
)


ussoc_fomc$USSOC_TRADE_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_TRADE_HIGH_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ussoc_fomc$USSOC_TRADE_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_TRADE_HIGH_PRICE_postrelease_chg > 0,
  1,
  ussoc_fomc$USSOC_TRADE_HIGH_PRICE_postrelease_chg_cat
)

ussoc_fomc$USSOC_TRADE_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_TRADE_HIGH_PRICE_postrelease_chg == 0,
  0,
  ussoc_fomc$USSOC_TRADE_HIGH_PRICE_postrelease_chg_cat
)


ussoc_fomc$USSOC_ASK_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_ASK_LOW_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ussoc_fomc$USSOC_ASK_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_ASK_LOW_PRICE_postrelease_chg > 0,
  1,
  ussoc_fomc$USSOC_ASK_LOW_PRICE_postrelease_chg_cat
)

ussoc_fomc$USSOC_ASK_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_ASK_LOW_PRICE_postrelease_chg == 0,
  0,
  ussoc_fomc$USSOC_ASK_LOW_PRICE_postrelease_chg_cat
)


ussoc_fomc$USSOC_ASK_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_ASK_CLOSE_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ussoc_fomc$USSOC_ASK_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_ASK_CLOSE_PRICE_postrelease_chg > 0,
  1,
  ussoc_fomc$USSOC_ASK_CLOSE_PRICE_postrelease_chg_cat
)

ussoc_fomc$USSOC_ASK_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_ASK_CLOSE_PRICE_postrelease_chg == 0,
  0,
  ussoc_fomc$USSOC_ASK_CLOSE_PRICE_postrelease_chg_cat
)


ussoc_fomc$USSOC_BID_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_BID_CLOSE_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ussoc_fomc$USSOC_BID_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_BID_CLOSE_PRICE_postrelease_chg > 0,
  1,
  ussoc_fomc$USSOC_BID_CLOSE_PRICE_postrelease_chg_cat
)

ussoc_fomc$USSOC_BID_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_BID_CLOSE_PRICE_postrelease_chg == 0,
  0,
  ussoc_fomc$USSOC_BID_CLOSE_PRICE_postrelease_chg_cat
)


ussoc_fomc$USSOC_BID_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_BID_HIGH_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ussoc_fomc$USSOC_BID_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_BID_HIGH_PRICE_postrelease_chg > 0,
  1,
  ussoc_fomc$USSOC_BID_HIGH_PRICE_postrelease_chg_cat
)

ussoc_fomc$USSOC_BID_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ussoc_fomc$USSOC_BID_HIGH_PRICE_postrelease_chg == 0,
  0,
  ussoc_fomc$USSOC_BID_HIGH_PRICE_postrelease_chg_cat
)




## ::::::::::: Merge & Save Data Set:
setwd("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files")

saveRDS(ussoc_fomc,
        file = "ussoc_intraday.rds")
