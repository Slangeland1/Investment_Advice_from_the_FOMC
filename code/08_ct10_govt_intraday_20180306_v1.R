## :::::::::::::::::::: File Info:
## Date: 2018-03-06
## Author: Stephanie Langeland  
## File Name: `08_ct10_govt_intraday_20180306_v1.R`
## Version: 1
## Previous Versions/Files: None
## Dependencies: None
## Purpose: Create intraday data set with new variables for CT10 Govt.
## Input File(s): 
    ## /clean_corpus_2018-03-04/FOMC_text.rds
    ## CT10 Govt_intraday_20180301.csv
## Output File(s): 
    ## ct10govt_intraday.rds
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




## :::::::::::::::::::: CT10 Govt:
ct10 <- read.csv("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/CT10 Govt_intraday_20180301.csv")

str(ct10)

View(ct10)

ct10$DateTime <- as.character(ct10$DateTime)

ncol(ct10)

library(stringr)

ct10[ , 23] <- str_extract(ct10$DateTime,
                         " .*$") 

colnames(ct10)[23] <- "Time"

ct10$Time <- str_replace_all(ct10$Time,
                            "[:space:]",
                            "")

library(chron)

ct10$Time <- times(paste0(ct10$Time, ":00"))


ct10$Date <- str_replace_all(ct10$Date,
                             " .*$",
                             "")

ct10$Date <- str_replace_all(ct10$Date,
                             "[:space:]",
                             "")

ct10$Date <- as.Date(ct10$Date,
                     format = "%Y-%m-%d")




## ::::::::::: Calculate the standard deviation (change in the response variables) for
## ::::::::::: the whole day:
library(dplyr)

CT10Govt_TRADE_LOW_PRICE_sd_day <- ct10 %>%
  group_by(Date) %>%
  summarize(CT10Govt_TRADE_LOW_PRICE_sd_day = sd(CT10Govt_TRADE_LOW_PRICE,
                                            na.rm = T))

CT10Govt_TRADE_HIGH_PRICE_sd_day <- ct10 %>%
  group_by(Date) %>%
  summarize(CT10Govt_TRADE_HIGH_PRICE_sd_day = sd(CT10Govt_TRADE_HIGH_PRICE,
                                                 na.rm = T))

CT10Govt_TRADE_CLOSE_PRICE_sd_day <- ct10 %>%
  group_by(Date) %>%
  summarize(CT10Govt_TRADE_CLOSE_PRICE_sd_day = sd(CT10Govt_TRADE_CLOSE_PRICE,
                  na.rm = T))

CT10Govt_TRADE_OPEN_PRICE_sd_day <- ct10 %>%
  group_by(Date) %>%
  summarize(CT10Govt_TRADE_OPEN_PRICE_sd_day = sd(CT10Govt_TRADE_OPEN_PRICE,
                  na.rm = T))

CT10Govt_BID_LOW_PRICE_sd_day <- ct10 %>%
  group_by(Date) %>%
  summarize(CT10Govt_BID_LOW_PRICE_sd_day = sd(CT10Govt_BID_LOW_PRICE,
                  na.rm = T))

CT10Govt_BID_HIGH_PRICE_sd_day <- ct10 %>%
  group_by(Date) %>%
  summarize(CT10Govt_BID_HIGH_PRICE_sd_day = sd(CT10Govt_BID_HIGH_PRICE,
                  na.rm = T))

CT10Govt_BID_CLOSE_PRICE_sd_day <- ct10 %>%
  group_by(Date) %>%
  summarize(CT10Govt_BID_CLOSE_PRICE_sd_day = sd(CT10Govt_BID_CLOSE_PRICE,
                  na.rm = T))

CT10Govt_BID_OPEN_PRICE_sd_day <- ct10 %>%
  group_by(Date) %>%
  summarize(CT10Govt_BID_OPEN_PRICE_sd_day = sd(CT10Govt_BID_OPEN_PRICE,
                  na.rm = T))

CT10Govt_ASK_LOW_PRICE_sd_day <- ct10 %>%
  group_by(Date) %>%
  summarize(CT10Govt_ASK_LOW_PRICE_sd_day = sd(CT10Govt_ASK_LOW_PRICE,
                  na.rm = T))

CT10Govt_ASK_HIGH_PRICE_sd_day <- ct10 %>%
  group_by(Date) %>%
  summarize(CT10Govt_ASK_HIGH_PRICE_sd_day = sd(CT10Govt_ASK_HIGH_PRICE,
                  na.rm = T))

CT10Govt_ASK_CLOSE_PRICE_sd_day <- ct10 %>%
  group_by(Date) %>%
  summarize(CT10Govt_ASK_CLOSE_PRICE_sd_day = sd(CT10Govt_ASK_CLOSE_PRICE,
                  na.rm = T))

CT10Govt_ASK_OPEN_PRICE_sd_day <- ct10 %>%
  group_by(Date) %>%
  summarize(CT10Govt_ASK_OPEN_PRICE_sd_day = sd(CT10Govt_ASK_OPEN_PRICE,
                  na.rm = T))




## ::::::::::: Calculate the standard deviation (change in the response variables) from
## ::::::::::: 2 PM - closing:
CT10Govt_TRADE_LOW_PRICE_sd_postrelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CT10Govt_TRADE_LOW_PRICE_sd_postrelease = sd(CT10Govt_TRADE_LOW_PRICE,
                                                    na.rm = T))

CT10Govt_TRADE_HIGH_PRICE_sd_postrelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CT10Govt_TRADE_HIGH_PRICE_sd_postrelease = sd(CT10Govt_TRADE_HIGH_PRICE,
                  na.rm = T))

CT10Govt_TRADE_CLOSE_PRICE_sd_postrelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CT10Govt_TRADE_CLOSE_PRICE_sd_postrelease = sd(CT10Govt_TRADE_CLOSE_PRICE,
                  na.rm = T))

CT10Govt_TRADE_OPEN_PRICE_sd_postrelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CT10Govt_TRADE_OPEN_PRICE_sd_postrelease = sd(CT10Govt_TRADE_OPEN_PRICE,
                  na.rm = T))

CT10Govt_BID_LOW_PRICE_sd_postrelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CT10Govt_BID_LOW_PRICE_sd_postrelease = sd(CT10Govt_BID_LOW_PRICE,
                  na.rm = T))

CT10Govt_BID_HIGH_PRICE_sd_postrelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CT10Govt_BID_HIGH_PRICE_sd_postrelease = sd(CT10Govt_BID_HIGH_PRICE,
                  na.rm = T))

CT10Govt_BID_CLOSE_PRICE_sd_postrelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CT10Govt_BID_CLOSE_PRICE_sd_postrelease = sd(CT10Govt_BID_CLOSE_PRICE,
                  na.rm = T))

CT10Govt_BID_OPEN_PRICE_sd_postrelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CT10Govt_BID_OPEN_PRICE_sd_postrelease = sd(CT10Govt_BID_OPEN_PRICE,
                  na.rm = T))

CT10Govt_ASK_LOW_PRICE_sd_postrelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CT10Govt_ASK_LOW_PRICE_sd_postrelease = sd(CT10Govt_ASK_LOW_PRICE,
                  na.rm = T))

CT10Govt_ASK_HIGH_PRICE_sd_postrelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CT10Govt_ASK_HIGH_PRICE_sd_postrelease = sd(CT10Govt_ASK_HIGH_PRICE,
                  na.rm = T))

CT10Govt_ASK_CLOSE_PRICE_sd_postrelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CT10Govt_ASK_CLOSE_PRICE_sd_postrelease = sd(CT10Govt_ASK_CLOSE_PRICE,
                  na.rm = T))

CT10Govt_ASK_OPEN_PRICE_sd_postrelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CT10Govt_ASK_OPEN_PRICE_sd_postrelease = sd(CT10Govt_ASK_OPEN_PRICE,
                  na.rm = T))




## ::::::::::: Calculate the standard deviation (change in the response variables) from
## ::::::::::: opening - 2 PM:
CT10Govt_TRADE_LOW_PRICE_sd_prerelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CT10Govt_TRADE_LOW_PRICE_sd_prerelease = sd(CT10Govt_TRADE_LOW_PRICE,
                                                   na.rm = T))

CT10Govt_TRADE_HIGH_PRICE_sd_prerelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CT10Govt_TRADE_HIGH_PRICE_sd_prerelease = sd(CT10Govt_TRADE_HIGH_PRICE,
                  na.rm = T))

CT10Govt_TRADE_CLOSE_PRICE_sd_prerelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CT10Govt_TRADE_CLOSE_PRICE_sd_prerelease = sd(CT10Govt_TRADE_CLOSE_PRICE,
                  na.rm = T))

CT10Govt_TRADE_OPEN_PRICE_sd_prerelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CT10Govt_TRADE_OPEN_PRICE_sd_prerelease = sd(CT10Govt_TRADE_OPEN_PRICE,
                  na.rm = T))

CT10Govt_BID_LOW_PRICE_sd_prerelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CT10Govt_BID_LOW_PRICE_sd_prerelease = sd(CT10Govt_BID_LOW_PRICE,
                  na.rm = T))

CT10Govt_BID_HIGH_PRICE_sd_prerelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CT10Govt_BID_HIGH_PRICE_sd_prerelease = sd(CT10Govt_BID_HIGH_PRICE,
                  na.rm = T))

CT10Govt_BID_CLOSE_PRICE_sd_prerelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CT10Govt_BID_CLOSE_PRICE_sd_prerelease = sd(CT10Govt_BID_CLOSE_PRICE,
                  na.rm = T))

CT10Govt_BID_OPEN_PRICE_sd_prerelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CT10Govt_BID_OPEN_PRICE_sd_prerelease = sd(CT10Govt_BID_OPEN_PRICE,
                  na.rm = T))

CT10Govt_ASK_LOW_PRICE_sd_prerelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CT10Govt_ASK_LOW_PRICE_sd_prerelease = sd(CT10Govt_ASK_LOW_PRICE,
                  na.rm = T))

CT10Govt_ASK_HIGH_PRICE_sd_prerelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CT10Govt_ASK_HIGH_PRICE_sd_prerelease = sd(CT10Govt_ASK_HIGH_PRICE,
                  na.rm = T))

CT10Govt_ASK_CLOSE_PRICE_sd_prerelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CT10Govt_ASK_CLOSE_PRICE_sd_prerelease = sd(CT10Govt_ASK_CLOSE_PRICE,
                  na.rm = T))

CT10Govt_ASK_OPEN_PRICE_sd_prerelease <- ct10 %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CT10Govt_ASK_OPEN_PRICE_sd_prerelease = sd(CT10Govt_ASK_OPEN_PRICE,
                  na.rm = T))




## ::::::::::: Merge all data sets:

ct10_1 <- cbind(
  CT10Govt_ASK_CLOSE_PRICE_sd_day,
  CT10Govt_ASK_HIGH_PRICE_sd_day[ , 2],
  CT10Govt_BID_OPEN_PRICE_sd_day[ , 2],
  CT10Govt_TRADE_LOW_PRICE_sd_day[ , 2],
  CT10Govt_ASK_LOW_PRICE_sd_day[ , 2],
  CT10Govt_BID_CLOSE_PRICE_sd_day[ , 2],
  CT10Govt_TRADE_CLOSE_PRICE_sd_day[ , 2],
  CT10Govt_TRADE_OPEN_PRICE_sd_day[ , 2],
  CT10Govt_ASK_OPEN_PRICE_sd_day[ , 2],
  CT10Govt_BID_HIGH_PRICE_sd_day[ , 2],
  CT10Govt_TRADE_HIGH_PRICE_sd_day[ , 2],
  CT10Govt_BID_LOW_PRICE_sd_day[ , 2],
  CT10Govt_BID_CLOSE_PRICE_sd_postrelease[ , 2],
  CT10Govt_BID_HIGH_PRICE_sd_postrelease[ , 2],
  CT10Govt_BID_LOW_PRICE_sd_postrelease[ , 2],
  CT10Govt_BID_OPEN_PRICE_sd_postrelease[ , 2],
  CT10Govt_TRADE_CLOSE_PRICE_sd_postrelease[ , 2],
  CT10Govt_TRADE_HIGH_PRICE_sd_postrelease[ , 2],
  CT10Govt_TRADE_LOW_PRICE_sd_postrelease[ , 2],
  CT10Govt_TRADE_OPEN_PRICE_sd_postrelease[ , 2],
  CT10Govt_ASK_CLOSE_PRICE_sd_postrelease[ , 2],
  CT10Govt_ASK_HIGH_PRICE_sd_postrelease[ , 2],
  CT10Govt_ASK_LOW_PRICE_sd_postrelease[ , 2],
  CT10Govt_ASK_OPEN_PRICE_sd_postrelease[ , 2]
)

ct10_2 <- cbind(
  CT10Govt_ASK_CLOSE_PRICE_sd_prerelease,
  CT10Govt_ASK_LOW_PRICE_sd_prerelease[ , 2],
  CT10Govt_BID_CLOSE_PRICE_sd_prerelease[ , 2],
  CT10Govt_BID_LOW_PRICE_sd_prerelease[ , 2],
  CT10Govt_TRADE_CLOSE_PRICE_sd_prerelease[ , 2],
  CT10Govt_TRADE_LOW_PRICE_sd_prerelease[ , 2],
  CT10Govt_ASK_HIGH_PRICE_sd_prerelease[ , 2],
  CT10Govt_ASK_OPEN_PRICE_sd_prerelease[ , 2],
  CT10Govt_BID_HIGH_PRICE_sd_prerelease[ , 2],
  CT10Govt_BID_OPEN_PRICE_sd_prerelease[ , 2],
  CT10Govt_TRADE_HIGH_PRICE_sd_prerelease[ , 2],
  CT10Govt_TRADE_OPEN_PRICE_sd_prerelease[ , 2]
)

ct10_3 <- merge(
  ct10_1,
  ct10_2,
  by = "Date",
  all = T
)




## ::::::::::: Calculate the directional change: value at closing - value at 2pm:
ct10_sub <- ct10 %>%
  filter(
    Time >= "14:00:00"
  )

values_at_2pm <- ct10_sub %>% 
  group_by(Date) %>% 
  slice(which.min(Time))

colnames(values_at_2pm)[5] <- "CT10Govt_TRADE_LOW_PRICE_2pm"
colnames(values_at_2pm)[6] <- "CT10Govt_TRADE_HIGH_PRICE_2pm"
colnames(values_at_2pm)[7] <- "CT10Govt_TRADE_CLOSE_PRICE_2pm"
colnames(values_at_2pm)[8] <- "CT10Govt_TRADE_OPEN_PRICE_2pm"
colnames(values_at_2pm)[12] <- "CT10Govt_BID_LOW_PRICE_2pm"
colnames(values_at_2pm)[13] <- "CT10Govt_BID_HIGH_PRICE_2pm"
colnames(values_at_2pm)[14] <- "CT10Govt_BID_CLOSE_PRICE_2pm"
colnames(values_at_2pm)[15] <- "CT10Govt_BID_OPEN_PRICE_2pm"
colnames(values_at_2pm)[19] <- "CT10Govt_ASK_LOW_PRICE_2pm"
colnames(values_at_2pm)[20] <- "CT10Govt_ASK_HIGH_PRICE_2pm"
colnames(values_at_2pm)[21] <- "CT10Govt_ASK_CLOSE_PRICE_2pm"
colnames(values_at_2pm)[22] <- "CT10Govt_ASK_OPEN_PRICE_2pm"
colnames(values_at_2pm)[23] <- "ct10_time_2pm"

values_at_2pm <- values_at_2pm[ , c(24, 23, 5:8, 12:15, 19:22)]


values_at_closing <- ct10_sub %>% 
  group_by(Date) %>% 
  slice(which.max(Time))

colnames(values_at_closing)[5] <- "CT10Govt_TRADE_LOW_PRICE_closing"
colnames(values_at_closing)[6] <- "CT10Govt_TRADE_HIGH_PRICE_closing"
colnames(values_at_closing)[7] <- "CT10Govt_TRADE_CLOSE_PRICE_closing"
colnames(values_at_closing)[8] <- "CT10Govt_TRADE_OPEN_PRICE_closing"
colnames(values_at_closing)[12] <- "CT10Govt_BID_LOW_PRICE_closing"
colnames(values_at_closing)[13] <- "CT10Govt_BID_HIGH_PRICE_closing"
colnames(values_at_closing)[14] <- "CT10Govt_BID_CLOSE_PRICE_closing"
colnames(values_at_closing)[15] <- "CT10Govt_BID_OPEN_PRICE_closing"
colnames(values_at_closing)[19] <- "CT10Govt_ASK_LOW_PRICE_closing"
colnames(values_at_closing)[20] <- "CT10Govt_ASK_HIGH_PRICE_closing"
colnames(values_at_closing)[21] <- "CT10Govt_ASK_CLOSE_PRICE_closing"
colnames(values_at_closing)[22] <- "CT10Govt_ASK_OPEN_PRICE_closing"
colnames(values_at_closing)[23] <- "ct10_time_closing"

values_at_closing <- values_at_closing[ , c(24, 23, 5:8, 12:15, 19:22)]



tmp <- merge(
  x = values_at_2pm,
  y = values_at_closing,
  by = "Date"
)


tmp$CT10Govt_ASK_CLOSE_PRICE_postrelease_chg <- tmp$CT10Govt_ASK_CLOSE_PRICE_closing - tmp$CT10Govt_ASK_CLOSE_PRICE_2pm
tmp$CT10Govt_ASK_HIGH_PRICE_postrelease_chg <- tmp$CT10Govt_ASK_HIGH_PRICE_closing - tmp$CT10Govt_ASK_HIGH_PRICE_2pm
tmp$CT10Govt_ASK_LOW_PRICE_postrelease_chg <- tmp$CT10Govt_ASK_LOW_PRICE_closing - tmp$CT10Govt_ASK_LOW_PRICE_2pm
tmp$CT10Govt_ASK_OPEN_PRICE_postrelease_chg <- tmp$CT10Govt_ASK_OPEN_PRICE_closing - tmp$CT10Govt_ASK_OPEN_PRICE_2pm
tmp$CT10Govt_BID_CLOSE_PRICE_postrelease_chg <- tmp$CT10Govt_BID_CLOSE_PRICE_closing - tmp$CT10Govt_BID_CLOSE_PRICE_2pm
tmp$CT10Govt_BID_HIGH_PRICE_postrelease_chg <- tmp$CT10Govt_BID_HIGH_PRICE_closing - tmp$CT10Govt_BID_HIGH_PRICE_2pm
tmp$CT10Govt_BID_LOW_PRICE_postrelease_chg <- tmp$CT10Govt_BID_LOW_PRICE_closing - tmp$CT10Govt_BID_LOW_PRICE_2pm
tmp$CT10Govt_BID_OPEN_PRICE_postrelease_chg <- tmp$CT10Govt_BID_OPEN_PRICE_closing - tmp$CT10Govt_BID_OPEN_PRICE_2pm
tmp$CT10Govt_TRADE_CLOSE_PRICE_postrelease_chg <- tmp$CT10Govt_TRADE_CLOSE_PRICE_closing - tmp$CT10Govt_TRADE_CLOSE_PRICE_2pm
tmp$CT10Govt_TRADE_HIGH_PRICE_postrelease_chg <- tmp$CT10Govt_TRADE_HIGH_PRICE_closing - tmp$CT10Govt_TRADE_HIGH_PRICE_2pm
tmp$CT10Govt_TRADE_LOW_PRICE_postrelease_chg <- tmp$CT10Govt_TRADE_LOW_PRICE_closing - tmp$CT10Govt_TRADE_LOW_PRICE_2pm
tmp$CT10Govt_TRADE_OPEN_PRICE_postrelease_chg <- tmp$CT10Govt_TRADE_OPEN_PRICE_closing - tmp$CT10Govt_TRADE_OPEN_PRICE_2pm

ct10_4 <- merge(
  ct10_3,
  tmp,
  by = "Date",
  all = T
)

ct10_fomc <- merge(
  x = fomc[, 2:5], 
  y = ct10_4,
  by.x = "release_date",
  by.y = "Date",
  all = T
)




## ::::::::::: Make the "_postrelease_chg" variables into categorical varibales == -1, 0, 1:
ct10_fomc$CT10Govt_ASK_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_ASK_CLOSE_PRICE_postrelease_chg < 0,
  -1, 
  NA
)

ct10_fomc$CT10Govt_ASK_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_ASK_CLOSE_PRICE_postrelease_chg > 0,
  1, 
  ct10_fomc$CT10Govt_ASK_CLOSE_PRICE_postrelease_chg_cat
)

ct10_fomc$CT10Govt_ASK_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_ASK_CLOSE_PRICE_postrelease_chg == 0,
  0, 
  ct10_fomc$CT10Govt_ASK_CLOSE_PRICE_postrelease_chg_cat
)


ct10_fomc$CT10Govt_ASK_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_ASK_LOW_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ct10_fomc$CT10Govt_ASK_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_ASK_LOW_PRICE_postrelease_chg > 0,
  1,
  ct10_fomc$CT10Govt_ASK_LOW_PRICE_postrelease_chg_cat
)

ct10_fomc$CT10Govt_ASK_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_ASK_LOW_PRICE_postrelease_chg == 0,
  0,
  ct10_fomc$CT10Govt_ASK_LOW_PRICE_postrelease_chg_cat
)


ct10_fomc$CT10Govt_BID_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_BID_CLOSE_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ct10_fomc$CT10Govt_BID_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_BID_CLOSE_PRICE_postrelease_chg > 0,
  1,
  ct10_fomc$CT10Govt_BID_CLOSE_PRICE_postrelease_chg_cat
)

ct10_fomc$CT10Govt_BID_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_BID_CLOSE_PRICE_postrelease_chg == 0,
  0,
  ct10_fomc$CT10Govt_BID_CLOSE_PRICE_postrelease_chg_cat
)


ct10_fomc$CT10Govt_BID_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_BID_LOW_PRICE_postrelease_chg < 0, 
  -1,
  NA
)

ct10_fomc$CT10Govt_BID_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_BID_LOW_PRICE_postrelease_chg > 0, 
  1,
  ct10_fomc$CT10Govt_BID_LOW_PRICE_postrelease_chg_cat
)

ct10_fomc$CT10Govt_BID_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_BID_LOW_PRICE_postrelease_chg == 0, 
  0,
  ct10_fomc$CT10Govt_BID_LOW_PRICE_postrelease_chg_cat
)


ct10_fomc$CT10Govt_TRADE_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_TRADE_CLOSE_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ct10_fomc$CT10Govt_TRADE_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_TRADE_CLOSE_PRICE_postrelease_chg > 0,
  1,
  ct10_fomc$CT10Govt_TRADE_CLOSE_PRICE_postrelease_chg_cat
)

ct10_fomc$CT10Govt_TRADE_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_TRADE_CLOSE_PRICE_postrelease_chg == 0,
  0,
  ct10_fomc$CT10Govt_TRADE_CLOSE_PRICE_postrelease_chg_cat
)


ct10_fomc$CT10Govt_TRADE_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_TRADE_LOW_PRICE_postrelease_chg < 0,
  -1, 
  NA
)

ct10_fomc$CT10Govt_TRADE_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_TRADE_LOW_PRICE_postrelease_chg > 0,
  1, 
  ct10_fomc$CT10Govt_TRADE_LOW_PRICE_postrelease_chg_cat
)

ct10_fomc$CT10Govt_TRADE_LOW_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_TRADE_LOW_PRICE_postrelease_chg == 0,
  0, 
  ct10_fomc$CT10Govt_TRADE_LOW_PRICE_postrelease_chg_cat
)


ct10_fomc$CT10Govt_ASK_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_ASK_HIGH_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ct10_fomc$CT10Govt_ASK_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_ASK_HIGH_PRICE_postrelease_chg > 0,
  1,
  ct10_fomc$CT10Govt_ASK_HIGH_PRICE_postrelease_chg_cat
)

ct10_fomc$CT10Govt_ASK_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_ASK_HIGH_PRICE_postrelease_chg == 0,
  0,
  ct10_fomc$CT10Govt_ASK_HIGH_PRICE_postrelease_chg_cat
)


ct10_fomc$CT10Govt_ASK_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_ASK_OPEN_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ct10_fomc$CT10Govt_ASK_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_ASK_OPEN_PRICE_postrelease_chg > 0,
  1,
  ct10_fomc$CT10Govt_ASK_OPEN_PRICE_postrelease_chg_cat
)

ct10_fomc$CT10Govt_ASK_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_ASK_OPEN_PRICE_postrelease_chg == 0,
  0,
  ct10_fomc$CT10Govt_ASK_OPEN_PRICE_postrelease_chg_cat
)


ct10_fomc$CT10Govt_BID_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_BID_HIGH_PRICE_postrelease_chg < 0, 
  -1,
  NA
)

ct10_fomc$CT10Govt_BID_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_BID_HIGH_PRICE_postrelease_chg > 0, 
  1,
  ct10_fomc$CT10Govt_BID_HIGH_PRICE_postrelease_chg_cat
)

ct10_fomc$CT10Govt_BID_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_BID_HIGH_PRICE_postrelease_chg == 0, 
  0,
  ct10_fomc$CT10Govt_BID_HIGH_PRICE_postrelease_chg_cat
)


ct10_fomc$CT10Govt_BID_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_BID_OPEN_PRICE_postrelease_chg < 0, 
  -1,
  NA
)

ct10_fomc$CT10Govt_BID_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_BID_OPEN_PRICE_postrelease_chg > 0, 
  1,
  ct10_fomc$CT10Govt_BID_OPEN_PRICE_postrelease_chg_cat
)

ct10_fomc$CT10Govt_BID_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_BID_OPEN_PRICE_postrelease_chg == 0, 
  0,
  ct10_fomc$CT10Govt_BID_OPEN_PRICE_postrelease_chg_cat
)


ct10_fomc$CT10Govt_TRADE_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_TRADE_HIGH_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ct10_fomc$CT10Govt_TRADE_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_TRADE_HIGH_PRICE_postrelease_chg > 0,
  1,
  ct10_fomc$CT10Govt_TRADE_HIGH_PRICE_postrelease_chg_cat
)

ct10_fomc$CT10Govt_TRADE_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_TRADE_HIGH_PRICE_postrelease_chg == 0,
  0,
  ct10_fomc$CT10Govt_TRADE_HIGH_PRICE_postrelease_chg_cat
)


ct10_fomc$CT10Govt_TRADE_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_TRADE_OPEN_PRICE_postrelease_chg < 0,
  -1,
  NA
)

ct10_fomc$CT10Govt_TRADE_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_TRADE_OPEN_PRICE_postrelease_chg > 0,
  1,
  ct10_fomc$CT10Govt_TRADE_OPEN_PRICE_postrelease_chg_cat
)

ct10_fomc$CT10Govt_TRADE_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  ct10_fomc$CT10Govt_TRADE_OPEN_PRICE_postrelease_chg == 0,
  0,
  ct10_fomc$CT10Govt_TRADE_OPEN_PRICE_postrelease_chg_cat
)




## ::::::::::: Save Data Set:
setwd("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files")

saveRDS(ct10_fomc,
        file= "ct10govt_intraday.rds")
  
