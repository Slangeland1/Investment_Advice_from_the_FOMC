## :::::::::::::::::::: File Info:
## Date: 2018-03-06
## Author: Stephanie Langeland  
## File Name: `04_spx_intraday_20180306_v1.R`
## Version: 1
## Previous Versions/Files: None
## Dependencies: None
## Purpose: Create intraday data set with new variables for SPX.
## Input File(s): 
    ## /clean_corpus_2018-03-04/FOMC_text.rds
    ## spx index_intraday_20180301.csv
## Output File(s):  `spx_intraday.rds`
## Data Output: None
## Required by: Master's Thesis
## Status: Complete
## Machine: 2018 MacBook Pro
## R version: R version 3.4.3 (2017-11-30) -- "Kite-Eating Tree"



## :::::::::::::::::::: Set up:
rm(list = ls(all = TRUE)) ## cleans the work space

## :::::::::::::::::::: Create a base data set with the `release_date`, 
## :::::::::::::::::::: `release_time`, and `type` variables from the 
## :::::::::::::::::::: `FOMC_text.rds`:
fomc <- readRDS("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/clean_corpus_2018-03-04/FOMC_text.rds")

str(fomc)


## :::::::::::::::::::: SPX:
spx <- read.csv("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/spx index_intraday_20180301.csv")

str(spx)

spx$DateTime <- as.character(spx$Date)

library(stringr)

spx[ , 16] <- str_extract(spx$DateTime,
                          "^.* ")

colnames(spx)[16] <- "Date"

spx$Date <- str_replace_all(spx$Date,
                            "[:space:]",
                            "")

spx$Date <- as.Date(spx$Date,
                    format = "%Y-%m-%d")

spx[ , 17] <- str_extract(spx$DateTime,
                          " .*$")

colnames(spx)[17] <- "Time"

spx$Time <- str_replace_all(spx$Time,
                            "[:space:]",
                            "")

library(chron)

spx$Time <- times(paste0(spx$Time, ":00"))



## ::::::::::: Calculate the standard deviation (change in the response variables) for
## ::::::::::: the whole day:
library(dplyr)

SPX_BID_CLOSE_PRICE_sd_day <- spx %>%
  group_by(Date) %>%
  summarize(SPX_BID_CLOSE_PRICE_sd_day = sd(SPX_BID_CLOSE_PRICE,
                                            na.rm = T))

SPX_BID_HIGH_PRICE_sd_day <- spx %>%
  group_by(Date) %>%
  summarize(SPX_BID_HIGH_PRICE_sd_day = sd(SPX_BID_HIGH_PRICE,
                                           na.rm = T))

SPX_BID_LOW_PRICE_sd_day <- spx %>%
  group_by(Date) %>%
  summarize(SPX_BID_LOW_PRICE_sd_day = sd(SPX_BID_LOW_PRICE,
                                          na.rm = T))

SPX_BID_OPEN_PRICE_sd_day <- spx %>%
  group_by(Date) %>%
  summarize(SPX_BID_OPEN_PRICE_sd_day = sd(SPX_BID_OPEN_PRICE,
                                           na.rm = T))

SPX_ASK_CLOSE_PRICE_sd_day <- spx %>%
  group_by(Date) %>%
  summarize(SPX_ASK_CLOSE_PRICE_sd_day = sd(SPX_ASK_CLOSE_PRICE,
                                            na.rm = T))

SPX_ASK_HIGH_PRICE_sd_day <- spx %>%
  group_by(Date) %>%
  summarize(SPX_ASK_HIGH_PRICE_sd_day = sd(SPX_ASK_HIGH_PRICE,
                                           na.rm = T))

SPX_ASK_LOW_PRICE_sd_day <- spx %>%
  group_by(Date) %>%
  summarize(SPX_ASK_LOW_PRICE_sd_day = sd(SPX_ASK_LOW_PRICE,
                                          na.rm = T))

SPX_ASK_OPEN_PRICE_sd_day <- spx %>%
  group_by(Date) %>%
  summarize(SPX_ASK_OPEN_PRICE_sd_day = sd(SPX_ASK_OPEN_PRICE,
                                           na.rm = T))



## ::::::::::: Calculate the standard deviation (change in the response variables) from
## ::::::::::: 2 PM - closing:
SPX_BID_CLOSE_PRICE_sd_postrelease <- spx %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(SPX_BID_CLOSE_PRICE_sd_postrelease = sd(SPX_BID_CLOSE_PRICE,
                                                    na.rm = T))

SPX_BID_HIGH_PRICE_sd_postrelease <- spx %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(SPX_BID_HIGH_PRICE_sd_postrelease = sd(SPX_BID_HIGH_PRICE,
                                                   na.rm = T))

SPX_BID_LOW_PRICE_sd_postrelease <- spx %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(SPX_BID_LOW_PRICE_sd_postrelease = sd(SPX_BID_LOW_PRICE,
                                                  na.rm = T))

SPX_BID_OPEN_PRICE_sd_postrelease <- spx %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(SPX_BID_OPEN_PRICE_sd_postrelease = sd(SPX_BID_OPEN_PRICE,
                                                   na.rm = T))

SPX_ASK_CLOSE_PRICE_sd_postrelease <- spx %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(SPX_ASK_CLOSE_PRICE_sd_postrelease = sd(SPX_ASK_CLOSE_PRICE,
                                                    na.rm = T))

SPX_ASK_HIGH_PRICE_sd_postrelease <- spx %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(SPX_ASK_HIGH_PRICE_sd_postrelease = sd(SPX_ASK_HIGH_PRICE,
                                                   na.rm = T))

SPX_ASK_LOW_PRICE_sd_postrelease <- spx %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(SPX_ASK_LOW_PRICE_sd_postrelease = sd(SPX_ASK_LOW_PRICE,
                                                  na.rm = T))

SPX_ASK_OPEN_PRICE_sd_postrelease <- spx %>%
  group_by(Date) %>%
  filter(Time >= "14:00:00") %>%
  summarize(SPX_ASK_OPEN_PRICE_sd_postrelease = sd(SPX_ASK_OPEN_PRICE,
                                                   na.rm = T))



## ::::::::::: Calculate the standard deviation (change in the response variables) from
## ::::::::::: opening - 2 PM:
SPX_BID_CLOSE_PRICE_sd_prerelease <- spx %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(SPX_BID_CLOSE_PRICE_sd_prerelease = sd(SPX_BID_CLOSE_PRICE,
                                                   na.rm = T))

SPX_BID_HIGH_PRICE_sd_prerelease <- spx %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(SPX_BID_HIGH_PRICE_sd_prerelease = sd(SPX_BID_HIGH_PRICE,
                                                  na.rm = T))

SPX_BID_LOW_PRICE_sd_prerelease <- spx %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(SPX_BID_LOW_PRICE_sd_prerelease = sd(SPX_BID_LOW_PRICE,
                                                 na.rm = T))

SPX_BID_OPEN_PRICE_sd_prerelease <- spx %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(SPX_BID_OPEN_PRICE_sd_prerelease = sd(SPX_BID_OPEN_PRICE,
                                                  na.rm = T))

SPX_ASK_CLOSE_PRICE_sd_prerelease <- spx %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(SPX_ASK_CLOSE_PRICE_sd_prerelease = sd(SPX_ASK_CLOSE_PRICE,
                                                   na.rm = T))

SPX_ASK_HIGH_PRICE_sd_prerelease <- spx %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(SPX_ASK_HIGH_PRICE_sd_prerelease = sd(SPX_ASK_HIGH_PRICE,
                                                  na.rm = T))

SPX_ASK_LOW_PRICE_sd_prerelease <- spx %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(SPX_ASK_LOW_PRICE_sd_prerelease = sd(SPX_ASK_LOW_PRICE,
                                                 na.rm = T))

SPX_ASK_OPEN_PRICE_sd_prerelease <- spx %>%
  group_by(Date) %>%
  filter(Time < "14:00:00") %>%
  summarize(SPX_ASK_OPEN_PRICE_sd_prerelease = sd(SPX_ASK_OPEN_PRICE,
                                                  na.rm = T))



## ::::::::::: Merge all data sets:
spx_changes <- cbind(
  SPX_BID_CLOSE_PRICE_sd_day,
  SPX_BID_HIGH_PRICE_sd_day[ , 2],
  SPX_BID_LOW_PRICE_sd_day[ , 2],
  SPX_BID_OPEN_PRICE_sd_day[ , 2],
  SPX_ASK_CLOSE_PRICE_sd_day[ , 2],
  SPX_ASK_HIGH_PRICE_sd_day[ , 2],
  SPX_ASK_LOW_PRICE_sd_day[ , 2],
  SPX_ASK_OPEN_PRICE_sd_day[ , 2],
  SPX_BID_CLOSE_PRICE_sd_prerelease[ , 2],
  SPX_BID_HIGH_PRICE_sd_prerelease[ , 2],
  SPX_BID_LOW_PRICE_sd_prerelease[ , 2],
  SPX_BID_OPEN_PRICE_sd_prerelease[ , 2],
  SPX_ASK_CLOSE_PRICE_sd_prerelease[ , 2],
  SPX_ASK_HIGH_PRICE_sd_prerelease[ , 2],
  SPX_ASK_LOW_PRICE_sd_prerelease[ , 2],
  SPX_ASK_OPEN_PRICE_sd_prerelease[ , 2],
  SPX_BID_CLOSE_PRICE_sd_postrelease[ , 2],
  SPX_BID_HIGH_PRICE_sd_postrelease[ , 2],
  SPX_BID_LOW_PRICE_sd_postrelease[ , 2],
  SPX_BID_OPEN_PRICE_sd_postrelease[ , 2],
  SPX_ASK_CLOSE_PRICE_sd_postrelease[ , 2],
  SPX_ASK_HIGH_PRICE_sd_postrelease[ , 2],
  SPX_ASK_LOW_PRICE_sd_postrelease[ , 2],
  SPX_ASK_OPEN_PRICE_sd_postrelease[ , 2]
)



## ::::::::::: Calculate the directional change: value at closing - value at 2pm:
spx_sub <- spx %>%
  filter(
    Time >= "14:00:00"
  )

values_at_2pm <- spx_sub %>% 
  group_by(Date) %>% 
  slice(which.min(Time))

colnames(values_at_2pm)[5] <- "SPX_BID_CLOSE_PRICE_2pm"
colnames(values_at_2pm)[6] <- "SPX_BID_HIGH_PRICE_2pm"
colnames(values_at_2pm)[7] <- "SPX_BID_LOW_PRICE_2pm"
colnames(values_at_2pm)[8] <- "SPX_BID_OPEN_PRICE_2pm"
colnames(values_at_2pm)[12] <- "SPX_ASK_CLOSE_PRICE_2pm"
colnames(values_at_2pm)[13] <- "SPX_ASK_HIGH_PRICE_2pm"
colnames(values_at_2pm)[14] <- "SPX_ASK_LOW_PRICE_2pm"
colnames(values_at_2pm)[15] <- "SPX_ASK_OPEN_PRICE_2pm"
colnames(values_at_2pm)[17] <- "spx_time_2pm"


values_at_2pm <- values_at_2pm[ , c(16:17, 5:8, 12:15)]



values_at_closing <- spx_sub %>% 
  group_by(Date) %>% 
  slice(which.max(Time))

colnames(values_at_closing)[5] <- "SPX_BID_CLOSE_PRICE_closing"
colnames(values_at_closing)[6] <- "SPX_BID_HIGH_PRICE_closing"
colnames(values_at_closing)[7] <- "SPX_BID_LOW_PRICE_closing"
colnames(values_at_closing)[8] <- "SPX_BID_OPEN_PRICE_closing"
colnames(values_at_closing)[12] <- "SPX_ASK_CLOSE_PRICE_closing"
colnames(values_at_closing)[13] <- "SPX_ASK_HIGH_PRICE_closing"
colnames(values_at_closing)[14] <- "SPX_ASK_LOW_PRICE_closing"
colnames(values_at_closing)[15] <- "SPX_ASK_OPEN_PRICE_closing"
colnames(values_at_closing)[17] <- "spx_time_closing"


values_at_closing <- values_at_closing[ , c(16:17, 5:8, 12:15)]


tmp <- merge(
  x = values_at_2pm,
  y = values_at_closing,
  by = "Date"
  )

tmp$SPX_BID_CLOSE_PRICE_postrelease_chg <- tmp$SPX_BID_CLOSE_PRICE_closing - tmp$SPX_BID_CLOSE_PRICE_2pm
tmp$SPX_BID_HIGH_PRICE_postrelease_chg <- tmp$SPX_BID_HIGH_PRICE_closing - tmp$SPX_BID_HIGH_PRICE_2pm
tmp$SPX_BID_LOW_PRICE_postrelease_chg <- tmp$SPX_BID_LOW_PRICE_closing - tmp$SPX_BID_LOW_PRICE_2pm
tmp$SPX_BID_OPEN_PRICE_postrelease_chg <- tmp$SPX_BID_OPEN_PRICE_closing - tmp$SPX_BID_OPEN_PRICE_2pm
tmp$SPX_ASK_CLOSE_PRICE_postrelease_chg <- tmp$SPX_ASK_CLOSE_PRICE_closing - tmp$SPX_ASK_CLOSE_PRICE_2pm
tmp$SPX_ASK_HIGH_PRICE_postrelease_chg <- tmp$SPX_ASK_HIGH_PRICE_closing - tmp$SPX_ASK_HIGH_PRICE_2pm
tmp$SPX_ASK_LOW_PRICE_postrelease_chg <- tmp$SPX_ASK_LOW_PRICE_closing - tmp$SPX_ASK_LOW_PRICE_2pm
tmp$SPX_ASK_OPEN_PRICE_postrelease_chg <- tmp$SPX_ASK_OPEN_PRICE_closing - tmp$SPX_ASK_OPEN_PRICE_2pm

spx_changes <- merge(
  spx_changes,
  tmp
)

spx_fomc <- merge(
  x = fomc[ , 2:5],
  y = spx_changes,
  by.x = "release_date",
  by.y = "Date",
  all = T
)



## ::::::::::: Make the "_postrelease_chg" variables into categorical varibales == -1, 0, 1:
spx_fomc$SPX_ASK_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_ASK_CLOSE_PRICE_postrelease_chg > 0, 
  1, 
  NA
)

spx_fomc$SPX_ASK_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_ASK_CLOSE_PRICE_postrelease_chg < 0, 
  -1, 
  spx_fomc$SPX_ASK_CLOSE_PRICE_postrelease_chg_cat
)

spx_fomc$SPX_ASK_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_ASK_CLOSE_PRICE_postrelease_chg == 0, 
  0, 
  spx_fomc$SPX_ASK_CLOSE_PRICE_postrelease_chg_cat
)


spx_fomc$SPX_ASK_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_ASK_HIGH_PRICE_postrelease_chg < 0,
  -1,
  NA
)

spx_fomc$SPX_ASK_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_ASK_HIGH_PRICE_postrelease_chg > 0,
  1,
  spx_fomc$SPX_ASK_HIGH_PRICE_postrelease_chg_cat
)

spx_fomc$SPX_ASK_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_ASK_HIGH_PRICE_postrelease_chg == 0,
  0,
  spx_fomc$SPX_ASK_HIGH_PRICE_postrelease_chg_cat
)


spx_fomc$SPX_ASK_LOW_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_ASK_LOW_PRICE_postrelease_chg < 0, 
  -1,
  NA
)

spx_fomc$SPX_ASK_LOW_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_ASK_LOW_PRICE_postrelease_chg > 0, 
  1,
  spx_fomc$SPX_ASK_LOW_PRICE_postrelease_chg_cat
)

spx_fomc$SPX_ASK_LOW_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_ASK_LOW_PRICE_postrelease_chg == 0, 
  0,
  spx_fomc$SPX_ASK_LOW_PRICE_postrelease_chg_cat
)


spx_fomc$SPX_ASK_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_ASK_OPEN_PRICE_postrelease_chg < 0,
  -1,
  NA
)

spx_fomc$SPX_ASK_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_ASK_OPEN_PRICE_postrelease_chg > 0,
  1,
  spx_fomc$SPX_ASK_OPEN_PRICE_postrelease_chg_cat
)

spx_fomc$SPX_ASK_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_ASK_OPEN_PRICE_postrelease_chg == 0,
  0,
  spx_fomc$SPX_ASK_OPEN_PRICE_postrelease_chg_cat
)


spx_fomc$SPX_BID_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_BID_CLOSE_PRICE_postrelease_chg < 0,
  -1,
  NA
)

spx_fomc$SPX_BID_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_BID_CLOSE_PRICE_postrelease_chg > 0,
  1,
  spx_fomc$SPX_BID_CLOSE_PRICE_postrelease_chg_cat
)

spx_fomc$SPX_BID_CLOSE_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_BID_CLOSE_PRICE_postrelease_chg == 0,
  0,
  spx_fomc$SPX_BID_CLOSE_PRICE_postrelease_chg_cat
)


spx_fomc$SPX_BID_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_BID_HIGH_PRICE_postrelease_chg < 0,
  -1,
  NA
)

spx_fomc$SPX_BID_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_BID_HIGH_PRICE_postrelease_chg > 0,
  1,
  spx_fomc$SPX_BID_HIGH_PRICE_postrelease_chg_cat
)

spx_fomc$SPX_BID_HIGH_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_BID_HIGH_PRICE_postrelease_chg == 0,
  0,
  spx_fomc$SPX_BID_HIGH_PRICE_postrelease_chg_cat
)


spx_fomc$SPX_BID_LOW_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_BID_LOW_PRICE_postrelease_chg < 0, 
  -1, 
  NA
)

spx_fomc$SPX_BID_LOW_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_BID_LOW_PRICE_postrelease_chg > 0, 
  1, 
  spx_fomc$SPX_BID_LOW_PRICE_postrelease_chg_cat
)

spx_fomc$SPX_BID_LOW_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_BID_LOW_PRICE_postrelease_chg == 0, 
  0, 
  spx_fomc$SPX_BID_LOW_PRICE_postrelease_chg_cat
)


spx_fomc$SPX_BID_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_BID_OPEN_PRICE_postrelease_chg < 0,
  -1,
  NA
)

spx_fomc$SPX_BID_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_BID_OPEN_PRICE_postrelease_chg > 0,
  1,
  spx_fomc$SPX_BID_OPEN_PRICE_postrelease_chg_cat
)

spx_fomc$SPX_BID_OPEN_PRICE_postrelease_chg_cat <- ifelse(
  spx_fomc$SPX_BID_OPEN_PRICE_postrelease_chg == 0,
  0,
  spx_fomc$SPX_BID_OPEN_PRICE_postrelease_chg_cat
)



## ::::::::::: What is the average standard deviations on non-fomc dates:
ifelse(
  is.na(spx_fomc$type),
  mean(spx_fomc$SPX_BID_OPEN_PRICE_sd_day,
       na.rm = T),
  NA
) ## 7.704667


ifelse(
  is.na(spx_fomc$type),
  mean(spx_fomc$SPX_BID_CLOSE_PRICE_sd_day,
       na.rm = T),
  NA
) ## 7.8186


ifelse(
  is.na(spx_fomc$type),
  mean(spx_fomc$SPX_ASK_OPEN_PRICE_sd_day,
       na.rm = T),
  NA
) ## 6.378311


ifelse(
  is.na(spx_fomc$type),
  mean(spx_fomc$SPX_ASK_CLOSE_PRICE_sd_day,
       na.rm = T),
  NA
) ## 6.458956




## ::::::::::: What is the average standard deviations on fomc dates:
ifelse(
  !is.na(spx_fomc$type),
  mean(spx_fomc$SPX_BID_OPEN_PRICE_sd_day,
       na.rm = T),
  NA
) ## 7.704667


ifelse(
  !is.na(spx_fomc$type),
  mean(spx_fomc$SPX_BID_CLOSE_PRICE_sd_day,
       na.rm = T),
  NA
) ## 7.8186


ifelse(
  !is.na(spx_fomc$type),
  mean(spx_fomc$SPX_ASK_OPEN_PRICE_sd_day,
       na.rm = T),
  NA
) ## 6.378311


ifelse(
  !is.na(spx_fomc$type),
  mean(spx_fomc$SPX_ASK_CLOSE_PRICE_sd_day,
       na.rm = T),
  NA
) ## 6.458956


## ::::::::::: conclusion: same standard deviations on non-fomc and fomc dates


## ::::::::::: Save Data Set:
setwd("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files")

saveRDS(spx_fomc,
        file = "spx_intraday.rds")




