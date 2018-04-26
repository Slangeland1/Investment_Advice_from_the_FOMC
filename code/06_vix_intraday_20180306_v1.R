## :::::::::::::::::::: File Info:
## Date: 2018-03-06
## Author: Stephanie Langeland  
## File Name: `06_vix_intraday_20180306_v1.R`
## Version: 1
## Previous Versions/Files: None
## Dependencies: None
## Purpose: Create intraday data set with new variables for VIX.
## Input File(s): 
    ## /clean_corpus_2018-03-04/FOMC_text.rds
    ## vix index_intraday_20180301.csv
## Output File(s): 
    ## vix_intraday.rds
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

## :::::::::::::::::::: VIX:
vix <- read.csv("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/vix index_intraday_20180301.csv")

str(vix)

vix$DateTime <- as.character(vix$DateTime)

library(stringr)

ncol(vix)

vix[ , 9] <- str_extract(vix$Date,
                         " .*$") 

colnames(vix)[9] <- "Time"

vix$Time <- str_replace_all(vix$Time,
                            "[:space:]",
                            "")

library(chron)

vix$Time <- times(paste0(vix$Time, ":00"))


vix$Date <- str_replace_all(vix$Date,
                            " .*$",
                            "")

vix$Date <- str_replace_all(vix$Date,
                            "[:space:]",
                            "")

vix$Date <- as.Date(vix$Date,
                    format = "%Y-%m-%d")




## ::::::::::: Calculate the standard deviation (change in the response variables) for
## ::::::::::: the whole day:
library(dplyr)

VIX_TRADE_LOW_PRICE_sd_day <- vix %>%
  group_by(Date) %>%
  summarize(VIX_TRADE_LOW_PRICE_sd_day = sd(VIX_TRADE_LOW_PRICE,
                                            na.rm = T))

VIX_TRADE_HIGH_PRICE_sd_day <- vix %>%
  group_by(Date) %>%
  summarize(VIX_TRADE_HIGH_PRICE_sd_day = sd(VIX_TRADE_HIGH_PRICE,
                                             na.rm = T))

VIX_TRADE_CLOSE_PRICE_sd_day <- vix %>%
  group_by(Date) %>%
  summarize(VIX_TRADE_CLOSE_PRICE_sd_day = sd(VIX_TRADE_CLOSE_PRICE,
                                              na.rm = T))

VIX_TRADE_OPEN_PRICE_sd_day <- vix %>%
  group_by(Date) %>%
  summarize(VIX_TRADE_OPEN_PRICE_sd_day = sd(VIX_TRADE_OPEN_PRICE,
                                             na.rm = T))




## ::::::::::: Calculate the standard deviation (change in the response variables) from
## ::::::::::: 2 PM - closing:
VIX_TRADE_LOW_PRICE_sd_postrelease <- vix %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(VIX_TRADE_LOW_PRICE_sd_postrelease = sd(VIX_TRADE_LOW_PRICE,
                                                    na.rm = T))

VIX_TRADE_HIGH_PRICE_sd_postrelease <- vix %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(VIX_TRADE_HIGH_PRICE_sd_postrelease = sd(VIX_TRADE_HIGH_PRICE,
                                                     na.rm = T))

VIX_TRADE_CLOSE_PRICE_sd_postrelease <- vix %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(VIX_TRADE_CLOSE_PRICE_sd_postrelease = sd(VIX_TRADE_CLOSE_PRICE,
                                                      na.rm = T))

VIX_TRADE_OPEN_PRICE_sd_postrelease <- vix %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(VIX_TRADE_OPEN_PRICE_sd_postrelease = sd(VIX_TRADE_OPEN_PRICE,
                                                     na.rm = T))




## ::::::::::: Calculate the standard deviation (change in the response variables) from
## ::::::::::: opening - 2 PM:
VIX_TRADE_LOW_PRICE_sd_prerelease <- vix %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(VIX_TRADE_LOW_PRICE_sd_prerelease = sd(VIX_TRADE_LOW_PRICE,
                                                   na.rm = T))

VIX_TRADE_HIGH_PRICE_sd_prerelease <- vix %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(VIX_TRADE_HIGH_PRICE_sd_prerelease = sd(VIX_TRADE_HIGH_PRICE,
                                                    na.rm = T))

VIX_TRADE_CLOSE_PRICE_sd_prerelease <- vix %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(VIX_TRADE_CLOSE_PRICE_sd_prerelease = sd(VIX_TRADE_CLOSE_PRICE,
                                                     na.rm = T))

VIX_TRADE_OPEN_PRICE_sd_prerelease <- vix %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(VIX_TRADE_OPEN_PRICE_sd_prerelease = sd(VIX_TRADE_OPEN_PRICE,
                                                    na.rm = T))




## ::::::::::: Merge all data sets:

vix1 <- cbind(
  VIX_TRADE_LOW_PRICE_sd_day,
  VIX_TRADE_HIGH_PRICE_sd_day[ , 2],
  VIX_TRADE_CLOSE_PRICE_sd_day[ , 2],
  VIX_TRADE_OPEN_PRICE_sd_day[ , 2]
)

vix2 <- cbind(
  VIX_TRADE_LOW_PRICE_sd_postrelease,
  VIX_TRADE_HIGH_PRICE_sd_postrelease[ , 2],
  VIX_TRADE_CLOSE_PRICE_sd_postrelease[ , 2],
  VIX_TRADE_OPEN_PRICE_sd_postrelease[ , 2]
)

vix3 <- cbind(
  VIX_TRADE_LOW_PRICE_sd_prerelease,
  VIX_TRADE_HIGH_PRICE_sd_prerelease[ , 2],
  VIX_TRADE_CLOSE_PRICE_sd_prerelease[ , 2],
  VIX_TRADE_OPEN_PRICE_sd_prerelease[ , 2]
)

vix4 <- merge(
  vix1, 
  vix3, 
  all = T
)

vix5 <- merge(
  vix4,
  vix2, 
  all = T
)




## ::::::::::: Calculate the directional change: value at closing - value at 2pm:
vix_sub <- vix %>%
  filter(
    Time >= "14:00:00"
  )

values_at_2pm <- vix_sub %>% 
  group_by(Date) %>% 
  slice(which.min(Time))


colnames(values_at_2pm)[5] <- "VIX_TRADE_LOW_PRICE_2pm"
colnames(values_at_2pm)[6] <- "VIX_TRADE_HIGH_PRICE_2pm"
colnames(values_at_2pm)[7] <- "VIX_TRADE_CLOSE_PRICE_2pm"
colnames(values_at_2pm)[8] <- "VIX_TRADE_OPEN_PRICE_2pm"
colnames(values_at_2pm)[9] <- "vix_time_2pm"


values_at_2pm <- values_at_2pm[ , c(10, 9, 5:8)]



values_at_closing <- vix_sub %>% 
  group_by(Date) %>% 
  slice(which.max(Time))

colnames(values_at_closing)[5] <- "VIX_TRADE_LOW_PRICE_closing"
colnames(values_at_closing)[6] <- "VIX_TRADE_HIGH_PRICE_closing"
colnames(values_at_closing)[7] <- "VIX_TRADE_CLOSE_PRICE_closing"
colnames(values_at_closing)[8] <- "VIX_TRADE_OPEN_PRICE_closing"
colnames(values_at_closing)[9] <- "vix_time_closing"


values_at_closing <- values_at_closing[ , c(10, 9, 5:8)]


tmp <- merge(
  x = values_at_2pm,
  y = values_at_closing,
  by = "Date"
)


tmp$VIX_TRADE_LOW_PRICE_postrelease_chg <- tmp$VIX_TRADE_LOW_PRICE_closing - 
  tmp$VIX_TRADE_LOW_PRICE_2pm

tmp$VIX_TRADE_HIGH_PRICE_postrelease_chg <- tmp$VIX_TRADE_HIGH_PRICE_closing - 
  tmp$VIX_TRADE_HIGH_PRICE_2pm

tmp$VIX_TRADE_CLOSE_PRICE_postrelease_chg <- tmp$VIX_TRADE_CLOSE_PRICE_closing - 
  tmp$VIX_TRADE_CLOSE_PRICE_2pm

tmp$VIX_TRADE_OPEN_PRICE_postrelease_chg <- tmp$VIX_TRADE_OPEN_PRICE_closing - 
  tmp$VIX_TRADE_OPEN_PRICE_2pm

vix6 <- merge(
  vix5,
  tmp,
  all = T
)

vix_fomc <- merge(
  x = fomc[ , 2:5],
  y = vix6,
  by.x = "release_date",
  by.y = "Date",
  all = T
)




## ::::::::::: Make the "_postrelease_chg" variables into categorical varibales == -1, 0, 1:
vix_fomc$VIX_TRADE_LOW_PRICE_postrelease_chg_cat <- ifelse(
  vix_fomc$VIX_TRADE_LOW_PRICE_postrelease_chg > 0, 
  1, 
  NA
)

vix_fomc$VIX_TRADE_LOW_PRICE_postrelease_chg_cat <- ifelse(
  vix_fomc$VIX_TRADE_LOW_PRICE_postrelease_chg < 0, 
  -1, 
  vix_fomc$VIX_TRADE_LOW_PRICE_postrelease_chg_cat
)

vix_fomc$VIX_TRADE_LOW_PRICE_postrelease_chg_cat <- ifelse(
  vix_fomc$VIX_TRADE_LOW_PRICE_postrelease_chg == 0, 
  0, 
  vix_fomc$VIX_TRADE_LOW_PRICE_postrelease_chg_cat
)


vix_fomc$VIX_TRADE_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  vix_fomc$VIX_TRADE_CLOSE_PRICE_postrelease_chg < 0, 
  -1, 
  NA
)

vix_fomc$VIX_TRADE_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  vix_fomc$VIX_TRADE_CLOSE_PRICE_postrelease_chg > 0, 
  1, 
  vix_fomc$VIX_TRADE_CLOSE_PRICE_postrelease_chg_cat 
)

vix_fomc$VIX_TRADE_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  vix_fomc$VIX_TRADE_CLOSE_PRICE_postrelease_chg == 0, 
  0, 
  vix_fomc$VIX_TRADE_CLOSE_PRICE_postrelease_chg_cat 
)


vix_fomc$VIX_TRADE_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  vix_fomc$VIX_TRADE_HIGH_PRICE_postrelease_chg > 0, 
  1,
  NA
)

vix_fomc$VIX_TRADE_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  vix_fomc$VIX_TRADE_HIGH_PRICE_postrelease_chg < 0, 
  -1,
  vix_fomc$VIX_TRADE_HIGH_PRICE_postrelease_chg_cat
)

vix_fomc$VIX_TRADE_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  vix_fomc$VIX_TRADE_HIGH_PRICE_postrelease_chg == 0, 
  0,
  vix_fomc$VIX_TRADE_HIGH_PRICE_postrelease_chg_cat
)


vix_fomc$VIX_TRADE_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  vix_fomc$VIX_TRADE_OPEN_PRICE_postrelease_chg < 0,
  -1,
  NA
)

vix_fomc$VIX_TRADE_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  vix_fomc$VIX_TRADE_OPEN_PRICE_postrelease_chg > 0,
  1,
  vix_fomc$VIX_TRADE_OPEN_PRICE_postrelease_chg_cat
)

vix_fomc$VIX_TRADE_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  vix_fomc$VIX_TRADE_OPEN_PRICE_postrelease_chg == 0,
  0,
  vix_fomc$VIX_TRADE_OPEN_PRICE_postrelease_chg_cat
)



## ::::::::::: Save Data Set:
setwd("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files")

saveRDS(vix_fomc,
        file = "vix_intraday.rds")

