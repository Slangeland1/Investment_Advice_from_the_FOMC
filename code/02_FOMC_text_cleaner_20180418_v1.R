## :::::::::::::::::::: File Info:
## Date: 2018-04-18
## Author: Stephanie Langeland  
## File Name: `02_FOMC_text_cleaner_20180418_v1.R`
## Version: 7
## Previous Versions/Files: `02_FOMC_text_cleaner_20180304_v1.R`
## Dependencies: None
## Purpose: This file outlines how FOMC meeting materials are cleaned and later saved in 
## an RDS file and individual text files.
## Input File(s): `/corpus_20180304` folder
## Output File(s): `clean_corpus_2018-04-18` folder
## Data Output: None
## Required by: Master's Thesis
## Status: Complete
## Machine: 2018 MacBook Pro
## R version: R version 3.4.3 (2017-11-30) -- "Kite-Eating Tree"





## :::::::::::::::::::: Set up:
rm(list = ls(all = TRUE)) ## cleans the work space

raw_corpus_path <- "/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/corpus_20180304"





## :::::::::::::::::::: Clean the raw corpus:
files <- dir(raw_corpus_path,
             full.names = T) ## get files

raw_text <- c()

for (f in files) {
  raw_text <- c(raw_text,
                paste(
                  readLines(f),
                  collapse = "\n"
                )) ## collapse files into list 
}

str(raw_text)

head(raw_text)




## ::::::::::: Remove the google analytics object function:
library(stringr)

raw_text <- str_replace_all(raw_text,
                            "[(]function[(]i,s,o,g,r,a,m[)]",
                            "STARTHERE")

raw_text <- str_replace_all(raw_text,
                            "'pageview'[)]",
                            "ENDHERE")

raw_text <- sub("STARTHERE.*ENDHERE", "", raw_text) ## remove google analytics object function




## ::::::::::: Remove the html code:
library(rvest)

strip_html <- function(s) {
  html_text(read_html(s))
}

clean_text <- c()

for (i in 1:length(raw_text)) {
  clean_text[[i]] <- strip_html(raw_text[[i]])
} ## removes most of the html code




## ::::::::::: Remove breaks:
clean_text <- str_replace_all(clean_text,
                              "</p>|\n|\t|<p>",
                              " ") 




## ::::::::::: Remove irrelevant headers and footers:
clean_text <- str_replace_all(clean_text,
                              "^.*Minutes of the Federal Open Market Committee",
                              "") ## remove this header and everything before it - minutes

clean_text <- str_replace_all(clean_text,
                              "The meeting adjourned.*$",
                              "") ## remove this footer and everything thereafter - minutes

clean_text <- str_replace_all(clean_text, 
                              "^.*Please enable JavaScript if it is disabled in your browser or access the information through the links provided below.",
                              "") ## removes this header and everything before it - statements 

clean_text <- str_replace_all(clean_text, 
                              "[:digit:][:digit:][:digit:][:digit:] Monetary policy|Last update:|Home \\| News and events.*$",
                              "") ## removes this footer and everything after it - statements

clean_text <- str_replace_all(clean_text,
                              "Voting for the FOMC monetary policy action were.*$",
                              "") ## remove voting info - statements

View(clean_text)





## :::::::::::::::::::: Convert to data frame:
clean_text_df <- as.data.frame(clean_text)

View(clean_text_df)

str(clean_text_df)

colnames(clean_text_df)[1] <- "text"

clean_text_df$text <- as.character(clean_text_df$text)




## ::::::::::: Create release date column:
clean_text_df$type <- files

clean_text_df$release_date <- str_extract(clean_text_df$type,
                                          "[:digit:]{8,}\\..*$")

clean_text_df$release_date <- str_replace_all(clean_text_df$release_date,
                                              ".htm",
                                              "")

clean_text_df$release_date <- as.Date(clean_text_df$release_date,
                                      "%Y%m%d")




## ::::::::::: Create meeting/statement type column:
clean_text_df$type <- str_extract_all(clean_text_df$type,
                                      "minutes|statement")

clean_text_df_backup <- clean_text_df




## ::::::::::: Create release time column (timezone: EDT):
clean_text_df$tmp <- str_extract(clean_text_df$text,
                                 "^.*For release at.*Share|For immediate release")

clean_text_df$tmp_hour <- ifelse(
  str_detect(clean_text_df$tmp,
             "immediate") == TRUE |
    clean_text_df$type == "minutes",
  "at 2:00 p.m. EST",
  str_extract(clean_text_df$tmp,
              "at .*E[SD]T")
) ## minutes and "immediate release" statements are released at 2:00 PM

clean_text_df$tmp_min <- str_extract(clean_text_df$tmp_hour,
                                     ":.* ") ## extract minutes

clean_text_df$tmp_AMPM <- str_extract(clean_text_df$tmp_hour,
                                      "[ap].m.") ## extract am or pm 

clean_text_df$tmp_hour <- str_extract(clean_text_df$tmp_hour,
                                      " .*:") ## remove everything but the hour

clean_text_df$tmp_hour <- str_replace_all(clean_text_df$tmp_hour,
                                          "[:space:]|:",
                                          "")  ## remove everything but the hour

clean_text_df$tmp_hour <- as.numeric(clean_text_df$tmp_hour)


clean_text_df$tmp_min <- str_extract(clean_text_df$tmp_min,
                                     "[:digit:]{2,}") ## remove everything but the minutes

clean_text_df$tmp_min <- str_replace_all(clean_text_df$tmp_min,
                                         "[:space:]",
                                         "")  ## remove everything but the hour

clean_text_df$release_time <- ifelse(
  clean_text_df$tmp_AMPM == "p.m.",
  clean_text_df$tmp_hour + 12,
  clean_text_df$tmp_hour
) ## convert to military time

clean_text_df$release_time <- paste(
  clean_text_df$release_time,
  ":",
  clean_text_df$tmp_min,
  sep = ""
) ## final military time


## ::::::::::: Fix release_date for the mintues:
clean_text_df$release_date2 <- clean_text_df$release_date ## release_date will later become meeting_date

## :::: 1995 - Nov 2004: the minutes were released 2 days after their subsequent meeting:
for (i in 1:79) {
  thing <- clean_text_df$release_date[i + 1] ## the cell below the release date is the next meeting's release date
  new_mins_date <- thing + 2 ## plus 2 days 
  clean_text_df$release_date2[i] <- new_mins_date
}

## The Fed only started showing the release date on its website in 2004, to
## this code is the correct a few minutes release date anomolies:
july96_mins <- which(clean_text_df$release_date == "1996-07-02")
clean_text_df[july96_mins, c("release_date2", "release_date")] <- "1996-07-03"

feb97_mins <- which(clean_text_df$release_date == "1997-02-04")
clean_text_df[feb97_mins, c("release_date2", "release_date")] <- "1997-02-05"

july97_mins <- which(clean_text_df$release_date == "1997-07-01")
clean_text_df[july97_mins, c("release_date2", "release_date")] <- "1997-07-02"

feb98_mins <- which(clean_text_df$release_date == "1998-02-03")
clean_text_df[feb98_mins, c("release_date2", "release_date")] <- "1998-02-04"

jun98_mins <- which(clean_text_df$release_date == "1998-06-30")
clean_text_df[jun98_mins, c("release_date2", "release_date")] <- "1998-07-01"

feb99_mins <- which(clean_text_df$release_date == "1999-02-02")
clean_text_df[feb99_mins, c("release_date2", "release_date")] <- "1999-02-03"

jun99_mins <- which(clean_text_df$release_date == "1999-06-29")
clean_text_df[jun99_mins, c("release_date2", "release_date")] <- "1999-06-30"

july04_mins <- which(clean_text_df$release_date2 == "2004-07-02")
clean_text_df[july04_mins, c("release_date2", "release_date")] <- "2004-07-01"

nov04_mins <- which(clean_text_df$release_date2 == "2004-11-12")
clean_text_df[nov04_mins, c("release_date2", "release_date")] <- "2004-11-11"




## :::: Dec 2004 - Present: the minutes were released 3 weeks after their meeting:
for (i in 80:185) {
  new_mins_date2 <- clean_text_df$release_date[i] + 21
  clean_text_df$release_date2[i] <- new_mins_date2
}

## According to the Fed's website, correct a few minutes release date anomolies:
jan07_mins <- which(clean_text_df$release_date2 == "2007-01-02")
clean_text_df[jan07_mins, c("release_date2")] <- "2007-01-03"

nov07_mins <- which(clean_text_df$release_date2 == "2007-11-21")
clean_text_df[nov07_mins, c("release_date2")] <- "2007-11-20"

jan08_mins <- which(clean_text_df$release_date2 == "2008-01-01")
clean_text_df[jan08_mins, c("release_date2")] <- "2008-01-02"

jun07_mins <- which(clean_text_df$release_date == "2007-06-18")
clean_text_df[jun07_mins, c("release_date2", "release_date")] <- "2007-06-28"

nov10_mins <- which(clean_text_df$release_date2 == "2010-11-24")
clean_text_df[nov10_mins, c("release_date2")] <- "2010-11-23"

jul11_mins <- which(clean_text_df$release_date2 == "2011-07-13")
clean_text_df[jul11_mins, c("release_date2")] <- "2011-07-12"

nov11_mins <- which(clean_text_df$release_date2 == "2011-11-23")
clean_text_df[nov11_mins, c("release_date2")] <- "2011-11-22"

dec12_mins <- which(clean_text_df$release_date2 == "2013-01-02")
clean_text_df[dec12_mins, c("release_date2")] <- "2013-01-03"



## ::::::::::: Remove document headers e.g. dates, title, etc.:
start <- Sys.time()

clean_text_df$text2 <- ifelse(
  clean_text_df$type == "statement",
  gsub(pattern = "^.*FRB.*For immediate release|.*For release at.*Share|For immediate release|.*Federal Reserve issues FOMC statement.*Share|.*FOMC [Ss]tatement.*Share",
       replacement = "",
       x = clean_text_df$text,
       perl = TRUE
  ),
  clean_text_df$text
)  ## this took 1.95 hours to execute

end <- Sys.time()

end - start

colnames(clean_text_df)[3] <- "meeting_date"

colnames(clean_text_df)[9] <- "release_date"

text_df <- clean_text_df[ , c("text2", "type", "meeting_date", "release_date", "release_time")]

colnames(text_df)[1] <- "orig_text"



## ::::::::::: Remove the remaining footers in the statements:
text_df$orig_text <- str_replace(text_df$orig_text,
                                 "Last Update.*$",
                                 "")

## ::::::::::: Fix the type coluimn - it shouldn't be a list:
typeof(text_df$type)
text_df$type <- unlist(text_df$type)
str(text_df)


## ::::::::::: Fix the release_time coluimn - it should be of "times" class:
library(chron)

text_df$release_time <- times(
  paste0(
    text_df$release_time,
    ":00"
  )
)


## ::::::::::: In `text_df`, the minutes release_date for the 1995-05-23 meeting 
## ::::::::::: is 07/08/95, which is 2 days after the subsequent meeting that 
## ::::::::::: took place on Thursday 07/06/95.  However, 07/08/95 is a Saturday so
## ::::::::::: I am changing the "release_date" for this meeting to the Monday after
## ::::::::::: 07/08/95 which is 07/10/95 because the market would have reacted on that date.
change_date <- which(text_df$release_date == "1995-07-08")

text_df$release_date[change_date] <- "1995-07-10"



## ::::::::::: In `text_df`, there was an unscheduled conference call and a
## ::::::::::: statements released at its conclusion on Sunday 05/09/10 at 
## ::::::::::: 9:15 PM.  So I am changing that release_date to Monday 05/10/10 
## ::::::::::: because the market would have reacted on that date.
change_date2 <- which(text_df$release_date == "2010-05-09")

text_df$release_date[change_date2] <- "2010-05-10"


## ::::::::::: In `text_df`, there is a minutes release_date for Thursday 1996-07-04
## ::::::::::: but the market is closed on 4th of July so I am changing that
## ::::::::::: release_date to Friday 1996-07-05 because the market would have 
## ::::::::::: reacted on that date.
change_date3 <- which(text_df$release_date == "1996-07-04")

text_df$release_date[change_date3] <- "1996-07-05"


## ::::::::::: Fix a one off error release date:
july96_mins_release <- which(text_df$release_date == "1996-07-03")
text_df[july96_mins_release, c("release_date")] <- "1996-08-22"




## ::::::::::: Save output:
output_path <- "/Users/StephanieLangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files"

output_corpus <- paste(output_path,
                       "/",
                       "clean_corpus_",
                       Sys.Date(),
                       "/",
                       sep = "")


dir.create(output_corpus)


setwd(output_corpus)


saveRDS(text_df,
        file = "FOMC_text.rds") ## original text before pre-processing for NLP


for (i in 1:nrow(text_df)) {
  file_names <- paste(
    text_df$type[[i]], 
    "_", 
    text_df$release_date[[i]], 
    #"_",
    #text_df$release_time[[i]],
    ".txt", 
    sep = ""
  )
  
  write(
    text_df$orig_text[i],
    file_names
  )
}
