## :::::::::::::::::::: File Info:
## Date: 2018-03-06
## Author: Stephanie Langeland  
## File Name: `07_cla_comdty_intraday_20180306_v1.R`
## Version: 1
## Previous Versions/Files: None
## Dependencies: None
## Purpose: Create intraday data set with new variables for CLA Comdty.
## Input File(s): 
    ## /clean_corpus_2018-03-04/FOMC_text.rds
    ## cla comdty_intraday_20180301.csv
## Output File(s): 
    ## cla_intraday.rds
## Data Output: None
## Required by: Master's Thesis
## Status: Complete
## Machine: 2018 MacBook Pro
## R version: R version 3.4.3 (2017-11-30) -- "Kite-Eating Tree"



## :::::::::::::::::::: Set up:
rm(list = ls(all = TRUE)) ## cleans the work space
options(scipen = 99)

## :::::::::::::::::::: Create a base data set with the `release_date`, 
## :::::::::::::::::::: `release_time`, and `type` variables from the 
## :::::::::::::::::::: `FOMC_text.rds`:
fomc <- readRDS("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/clean_corpus_2018-03-04/FOMC_text.rds")

str(fomc)


## :::::::::::::::::::: CLA Comdty:
cla <- read.csv("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/cla comdty_intraday_20180301.csv")

str(cla)

cla$Date <- as.character(cla$Date)

library(stringr)

ncol(cla)

cla[ , 9] <- str_extract(cla$Date,
                         " .*$") 

colnames(cla)[9] <- "Time"

cla$Time <- str_replace_all(cla$Time,
                            "[:space:]",
                            "")

library(chron)

cla$Time <- times(paste0(cla$Time, ":00"))


cla$Date <- str_replace_all(cla$Date,
                            " .*$",
                            "")

cla$Date <- str_replace_all(cla$Date,
                            "[:space:]",
                            "")

cla$Date <- as.Date(cla$Date,
                    format = "%Y-%m-%d")




## ::::::::::: Calculate the standard deviation (change in the response variables) for
## ::::::::::: the whole day:
library(dplyr)

CLAComdty_TRADE_VALUE_sd_day <- cla %>%
  group_by(Date) %>%
  summarize(CLAComdty_TRADE_VALUE_sd_day = sd(CLAComdty_TRADE_VALUE,
                                              na.rm = T))

CLAComdty_TRADE_LOW_PRICE_sd_day <- cla %>%
  group_by(Date) %>%
  summarize(CLAComdty_TRADE_LOW_PRICE_sd_day = sd(CLAComdty_TRADE_LOW_PRICE,
                                                  na.rm = T))

CLAComdty_TRADE_HIGH_PRICE_sd_day <- cla %>%
  group_by(Date) %>%
  summarize(CLAComdty_TRADE_HIGH_PRICE_sd_day = sd(CLAComdty_TRADE_HIGH_PRICE,
                                                   na.rm = T))

CLAComdty_TRADE_CLOSE_PRICE_sd_day <- cla %>%
  group_by(Date) %>%
  summarize(CLAComdty_TRADE_CLOSE_PRICE_sd_day = sd(CLAComdty_TRADE_CLOSE_PRICE,
                                                    na.rm = T))

CLAComdty_TRADE_OPEN_PRICE_sd_day <- cla %>%
  group_by(Date) %>%
  summarize(CLAComdty_TRADE_OPEN_PRICE_sd_day = sd(CLAComdty_TRADE_OPEN_PRICE,
                                                   na.rm = T))




## ::::::::::: Calculate the standard deviation (change in the response variables) from
## ::::::::::: 2 PM - closing:
CLAComdty_TRADE_VALUE_sd_postrelease <- cla %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CLAComdty_TRADE_VALUE_sd_postrelease = sd(CLAComdty_TRADE_VALUE,
                                                      na.rm = T))

CLAComdty_TRADE_LOW_PRICE_sd_postrelease <- cla %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CLAComdty_TRADE_LOW_PRICE_sd_postrelease = sd(CLAComdty_TRADE_LOW_PRICE,
                                                          na.rm = T))

CLAComdty_TRADE_HIGH_PRICE_sd_postrelease <- cla %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CLAComdty_TRADE_HIGH_PRICE_sd_postrelease = sd(CLAComdty_TRADE_HIGH_PRICE,
                                                           na.rm = T))

CLAComdty_TRADE_CLOSE_PRICE_sd_postrelease <- cla %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CLAComdty_TRADE_CLOSE_PRICE_sd_postrelease = sd(CLAComdty_TRADE_CLOSE_PRICE,
                                                            na.rm = T))

CLAComdty_TRADE_OPEN_PRICE_sd_postrelease <- cla %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(CLAComdty_TRADE_OPEN_PRICE_sd_postrelease = sd(CLAComdty_TRADE_OPEN_PRICE,
                                                           na.rm = T))




## ::::::::::: Calculate the standard deviation (change in the response variables) from
## ::::::::::: opening - 2 PM:
CLAComdty_TRADE_VALUE_sd_prerelease <- cla %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CLAComdty_TRADE_VALUE_sd_prerelease = sd(CLAComdty_TRADE_VALUE,
                                                     na.rm = T))

CLAComdty_TRADE_LOW_PRICE_sd_prerelease <- cla %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CLAComdty_TRADE_LOW_PRICE_sd_prerelease = sd(CLAComdty_TRADE_LOW_PRICE,
                                                         na.rm = T))

CLAComdty_TRADE_HIGH_PRICE_sd_prerelease <- cla %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CLAComdty_TRADE_HIGH_PRICE_sd_prerelease = sd(CLAComdty_TRADE_HIGH_PRICE,
                                                          na.rm = T))

CLAComdty_TRADE_CLOSE_PRICE_sd_prerelease <- cla %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CLAComdty_TRADE_CLOSE_PRICE_sd_prerelease = sd(CLAComdty_TRADE_CLOSE_PRICE,
                                                           na.rm = T))

CLAComdty_TRADE_OPEN_PRICE_sd_prerelease <- cla %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(CLAComdty_TRADE_OPEN_PRICE_sd_prerelease = sd(CLAComdty_TRADE_OPEN_PRICE,
                                                          na.rm = T))




## ::::::::::: Merge all data sets:
cla_intraday1 <- cbind(
  CLAComdty_TRADE_VALUE_sd_day,
  CLAComdty_TRADE_LOW_PRICE_sd_day[ , 2],
  CLAComdty_TRADE_HIGH_PRICE_sd_day[ , 2],
  CLAComdty_TRADE_CLOSE_PRICE_sd_day[ , 2],
  CLAComdty_TRADE_OPEN_PRICE_sd_day[ , 2]
)

cla_intraday2 <- cbind(
  CLAComdty_TRADE_VALUE_sd_postrelease,
  CLAComdty_TRADE_LOW_PRICE_sd_postrelease[ , 2],
  CLAComdty_TRADE_HIGH_PRICE_sd_postrelease[ , 2],
  CLAComdty_TRADE_CLOSE_PRICE_sd_postrelease[ , 2],
  CLAComdty_TRADE_OPEN_PRICE_sd_postrelease[ , 2]
)

cla_intraday3 <- cbind(
  CLAComdty_TRADE_VALUE_sd_prerelease,
  CLAComdty_TRADE_LOW_PRICE_sd_prerelease[ , 2],
  CLAComdty_TRADE_HIGH_PRICE_sd_prerelease[ , 2],
  CLAComdty_TRADE_CLOSE_PRICE_sd_prerelease[ , 2],
  CLAComdty_TRADE_OPEN_PRICE_sd_prerelease[ , 2]
)

cla_intraday4 <- merge(
  cla_intraday1,
  cla_intraday2,
  all = T
)

cla_intraday5 <- merge(
  cla_intraday4,
  cla_intraday3,
  all = T
)




## ::::::::::: Calculate the directional change: value at closing - value at 2pm:
cla_sub <- cla %>%
  filter(
    Time >= "14:00:00"
  )

values_at_2pm <- cla_sub %>% 
  group_by(Date) %>% 
  slice(which.min(Time))


colnames(values_at_2pm)[2] <- "CLAComdty_TRADE_VALUE_2pm"
colnames(values_at_2pm)[5] <- "CLAComdty_TRADE_LOW_PRICE_2pm"
colnames(values_at_2pm)[6] <- "CLAComdty_TRADE_HIGH_PRICE_2pm"
colnames(values_at_2pm)[7] <- "CLAComdty_TRADE_CLOSE_PRICE_2pm"
colnames(values_at_2pm)[8] <- "CLAComdty_TRADE_OPEN_PRICE_2pm"
colnames(values_at_2pm)[9] <- "cla_time_2pm"

values_at_2pm <- values_at_2pm[ , c(1, 9, 2, 5:8)]



values_at_closing <- cla_sub %>% 
  group_by(Date) %>% 
  slice(which.max(Time))

colnames(values_at_closing)[2] <- "CLAComdty_TRADE_VALUE_closing"
colnames(values_at_closing)[5] <- "CLAComdty_TRADE_LOW_PRICE_closing"
colnames(values_at_closing)[6] <- "CLAComdty_TRADE_HIGH_PRICE_closing"
colnames(values_at_closing)[7] <- "CLAComdty_TRADE_CLOSE_PRICE_closing"
colnames(values_at_closing)[8] <- "CLAComdty_TRADE_OPEN_PRICE_closing"
colnames(values_at_closing)[9] <- "cla_time_closing"

values_at_closing <- values_at_closing[ , c(1, 9, 2, 5:8)]


tmp <- merge(
  x = values_at_2pm,
  y = values_at_closing,
  by = "Date"
)

tmp$CLAComdty_TRADE_VALUE_postrelease_chg <- tmp$CLAComdty_TRADE_VALUE_closing - 
  tmp$CLAComdty_TRADE_VALUE_2pm

tmp$CLAComdty_TRADE_LOW_PRICE_postrelease_chg <- tmp$CLAComdty_TRADE_LOW_PRICE_closing - 
  tmp$CLAComdty_TRADE_LOW_PRICE_2pm

tmp$CLAComdty_TRADE_HIGH_PRICE_postrelease_chg <- tmp$CLAComdty_TRADE_HIGH_PRICE_closing - 
  tmp$CLAComdty_TRADE_HIGH_PRICE_2pm

tmp$CLAComdty_TRADE_CLOSE_PRICE_postrelease_chg <- tmp$CLAComdty_TRADE_CLOSE_PRICE_closing -
  tmp$CLAComdty_TRADE_CLOSE_PRICE_2pm

tmp$CLAComdty_TRADE_OPEN_PRICE_postrelease_chg <- tmp$CLAComdty_TRADE_OPEN_PRICE_closing -
  tmp$CLAComdty_TRADE_OPEN_PRICE_2pm


cla_changes <- merge(
  cla_intraday5,
  tmp,
  all = T
)

cla_fomc <- merge(
  x = fomc[ , 2:5],
  y = cla_changes,
  by.x = "release_date",
  by.y = "Date",
  all = T
)



## ::::::::::: Make the "_postrelease_chg" variables into categorical varibales == -1, 0, 1:
cla_fomc$CLAComdty_TRADE_VALUE_postrelease_chg_cat <- ifelse(
  cla_fomc$CLAComdty_TRADE_VALUE_postrelease_chg > 0,
  1,
  NA
)

cla_fomc$CLAComdty_TRADE_VALUE_postrelease_chg_cat <- ifelse(
  cla_fomc$CLAComdty_TRADE_VALUE_postrelease_chg < 0,
  -1,
  cla_fomc$CLAComdty_TRADE_VALUE_postrelease_chg_cat
)

cla_fomc$CLAComdty_TRADE_VALUE_postrelease_chg_cat <- ifelse(
  cla_fomc$CLAComdty_TRADE_VALUE_postrelease_chg == 0,
  0,
  cla_fomc$CLAComdty_TRADE_VALUE_postrelease_chg_cat
)


cla_fomc$CLAComdty_TRADE_LOW_PRICE_postrelease_chg_cat <- ifelse(
  cla_fomc$CLAComdty_TRADE_LOW_PRICE_postrelease_chg < 0,
  -1,
  NA
)

cla_fomc$CLAComdty_TRADE_LOW_PRICE_postrelease_chg_cat <- ifelse(
  cla_fomc$CLAComdty_TRADE_LOW_PRICE_postrelease_chg > 0,
  1,
  cla_fomc$CLAComdty_TRADE_LOW_PRICE_postrelease_chg_cat
)

cla_fomc$CLAComdty_TRADE_LOW_PRICE_postrelease_chg_cat <- ifelse(
  cla_fomc$CLAComdty_TRADE_LOW_PRICE_postrelease_chg == 0,
  0,
  cla_fomc$CLAComdty_TRADE_LOW_PRICE_postrelease_chg_cat
)


cla_fomc$CLAComdty_TRADE_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  cla_fomc$CLAComdty_TRADE_HIGH_PRICE_postrelease_chg < 0,
  -1,
  NA
)

cla_fomc$CLAComdty_TRADE_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  cla_fomc$CLAComdty_TRADE_HIGH_PRICE_postrelease_chg > 0,
  1,
  cla_fomc$CLAComdty_TRADE_HIGH_PRICE_postrelease_chg_cat
)

cla_fomc$CLAComdty_TRADE_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  cla_fomc$CLAComdty_TRADE_HIGH_PRICE_postrelease_chg == 0,
  0,
  cla_fomc$CLAComdty_TRADE_HIGH_PRICE_postrelease_chg_cat
)


cla_fomc$CLAComdty_TRADE_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  cla_fomc$CLAComdty_TRADE_CLOSE_PRICE_postrelease_chg < 0,
  -1,
  NA
)

cla_fomc$CLAComdty_TRADE_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  cla_fomc$CLAComdty_TRADE_CLOSE_PRICE_postrelease_chg > 0,
  1,
  cla_fomc$CLAComdty_TRADE_CLOSE_PRICE_postrelease_chg_cat
)

cla_fomc$CLAComdty_TRADE_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  cla_fomc$CLAComdty_TRADE_CLOSE_PRICE_postrelease_chg == 0,
  0,
  cla_fomc$CLAComdty_TRADE_CLOSE_PRICE_postrelease_chg_cat
)


cla_fomc$CLAComdty_TRADE_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  cla_fomc$CLAComdty_TRADE_OPEN_PRICE_postrelease_chg < 0, 
  -1,
  NA
)

cla_fomc$CLAComdty_TRADE_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  cla_fomc$CLAComdty_TRADE_OPEN_PRICE_postrelease_chg > 0, 
  1,
  cla_fomc$CLAComdty_TRADE_OPEN_PRICE_postrelease_chg_cat
)

cla_fomc$CLAComdty_TRADE_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  cla_fomc$CLAComdty_TRADE_OPEN_PRICE_postrelease_chg == 0, 
  0,
  cla_fomc$CLAComdty_TRADE_OPEN_PRICE_postrelease_chg_cat
)




## ::::::::::: Save Data Set:
setwd("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files")

saveRDS(cla_fomc,
        file = "cla_intraday.rds")
