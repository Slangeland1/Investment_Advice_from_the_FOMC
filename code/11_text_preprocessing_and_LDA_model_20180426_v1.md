---
title: "Text Pre-Processing, LDA Model, and Posterior Prediction Model"
author: Stephanie Langeland
always_allow_html: yes
output:
  html_document:
    keep_md: true
    toc: true
    number_sections: true
---



# Import files & set up the corpus


```r
##:::::::::::::::::::::::::::::::::::::::::: Import Data:
library(tm)

path <- "/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/open_close_data.rds"

raw_corpus <- readRDS(path)

library(dplyr)
```

Extract statements:


```r
stmt_sub <- raw_corpus %>%
  filter(!is.na(stmt_text))
```

```
## Warning: package 'bindrcpp' was built under R version 3.4.4
```

Extract minutes:


```r
min_sub <- raw_corpus %>%
  filter(!is.na(min_text))
```

Stack minutes and statements, with their corresponding meta data (response 
variables), into a data frame:


```r
texts_orig <- data.frame(
  doc_id = c(paste0(stmt_sub$release_date, "_s"),
             paste0(min_sub$release_date, "_m")),
  text = c(stmt_sub$stmt_text,
           min_sub$min_text),
  release_date = c(stmt_sub$release_date,
                   min_sub$release_date),
  stringsAsFactors = F)
```

Merge the statements and minutes text with the meta data (response variables):


```r
texts <- merge(
  x = texts_orig,
  y = raw_corpus[ , c(2, ## release date
                      5:ncol(raw_corpus))], ## response variables
  by = "release_date"
  )

colnames(texts)
```

```
##  [1] "release_date"              "doc_id"                   
##  [3] "text"                      "BICLB10Y_LAST_PRICE"      
##  [5] "CLAComdty_ASK_PRICE"       "CLAComdty_BID_PRICE"      
##  [7] "CLAComdty_change"          "CLAComdty_change_cat"     
##  [9] "CLAComdty_HIGH_PRICE"      "CLAComdty_LAST_PRICE"     
## [11] "CLAComdty_LOW_PRICE"       "CLAComdty_OPEN_PRICE"     
## [13] "CLAComdty_VOLUME"          "H15T10Y_change"           
## [15] "H15T10Y_change_cat"        "H15T10Y_HIGH_PRICE"       
## [17] "H15T10Y_LAST_PRICE"        "H15T10Y_LOW_PRICE"        
## [19] "H15T10Y_OPEN_PRICE"        "MOODCBAA_LAST_PRICE"      
## [21] "SPX_ASK_PRICE"             "SPX_BID_PRICE"            
## [23] "SPX_change"                "SPX_change_cat"           
## [25] "SPX_HIGH_PRICE"            "SPX_LAST_PRICE"           
## [27] "SPX_LOW_PRICE"             "SPX_OPEN_PRICE"           
## [29] "SPX_VOLUME"                "TNX_ADJ_CLOSE_PRICE"      
## [31] "TNX_change"                "TNX_change_cat"           
## [33] "TNX_CLOSE_PRICE"           "TNX_HIGH_PRICE"           
## [35] "TNX_LOW_PRICE"             "TNX_OPEN_PRICE"           
## [37] "USSOC_ASK_PRICE"           "USSOC_BID_PRICE"          
## [39] "USSOC_change"              "USSOC_change_cat"         
## [41] "USSOC_HIGH_PRICE"          "USSOC_LAST_PRICE"         
## [43] "USSOC_LOW_PRICE"           "USSOC_OPEN_PRICE"         
## [45] "VIX_change"                "VIX_change_cat"           
## [47] "VIX_HIGH_PRICE"            "VIX_LAST_PRICE"           
## [49] "VIX_LOW_PRICE"             "VIX_OPEN_PRICE"           
## [51] "CLAComdty_close_perc_open" "H15T10Y_close_perc_open"  
## [53] "SPX_close_perc_open"       "TNX_close_perc_open"      
## [55] "USSOC_close_perc_open"     "VIX_close_perc_open"
```

```r
#str(texts)

#View(texts)
```

Combine documents that were released on the same day:


```r
##:::::::::::::::::::: On which dates were multiple docs released on same day:
dup_dates_location <- which(duplicated(texts$release_date) == TRUE)

duplicated_date <- texts[dup_dates_location, "release_date"] 
duplicated_date ## only one date when multiple (two) docs were released
```

```
## [1] "1999-06-30"
```

```r
dup_dates2 <- which(texts$release_date == duplicated_date) 
dup_dates2 ## rows 44 and 45 are the same date
```

```
## [1] 44 45
```

```r
##:::::::::::::::::::: Combine documents that were released on the same 
##:::::::::::::::::::: day - combine rows 44 and 45 into row 44:
texts$text[ dup_dates2[1] ] <- paste( 
  texts[ dup_dates2[1], c("text")], ## row 44
  texts[ dup_dates2[2], c("text")] ## row 45
                                      )



##:::::::::::::::::::: Update the doc_id in row 44:
texts$doc_id[ dup_dates2[1] ] <- paste(
  duplicated_date, 
  "_ms",
  sep = ""
)



##:::::::::::::::::::: Delete the duplicate row (row 45):
texts <- texts[-dup_dates2[2], ]



###::::::::::::::::::::  Verify that there are no more duplicate dates:
summary(duplicated(texts$release_date))
```

```
##    Mode   FALSE 
## logical     352
```

Use the `stringr` package to do this now because the `removePunctuation` function isn't 
catching everything later on and remove this special character:


```r
library(stringr)

texts$text <- str_replace_all(
  texts$text,
  "[:punct:]",
  " "
) 

texts$text <- str_replace_all(
  texts$text,
  "�",
  ""
) 
```

Fix spelling errors:


```r
texts$text <- str_replace_all(
  texts$text,
  "accomodative",
  "accommodative"
) 
```

No real difference between "economy" vs. "economic" for a machine:


```r
## use "economic" instead because it is said more times than "economy":
text_sub <- unlist(
  texts$text
)

text_sub <- tolower(text_sub)

sum(
  str_count(text_sub,
            "economy")
)
```

```
## [1] 1662
```

```r
sum(
  str_count(text_sub,
            "economic")
)
```

```
## [1] 6962
```

```r
texts$text <- str_replace_all(
  texts$text,
  "economy",
  "economic"
) 
```

No real difference between "accrue" vs. "accrual" for a machine:


```r
texts$text <- str_replace_all(
  texts$text,
  "accrued",
  "accrue"
) 

texts$text <- str_replace_all(
  texts$text,
  "accruing",
  "accrue"
) 

texts$text <- str_replace_all(
  texts$text,
  "accrual",
  "accrue"
) 

texts$text <- str_replace_all(
  texts$text,
  "accruals",
  "accrue"
) 
```

Create a corpus data frame:


```r
df_source <- DataframeSource(texts) ## this is the source of the corpus data frame

df_corpus <- VCorpus(df_source) ## create a volatile corpus

#View(df_corpus)

print(df_corpus) ## contains the correct number of documents
```

```
## <<VCorpus>>
## Metadata:  corpus specific: 0, document level (indexed): 54
## Content:  documents: 352
```

```r
#inspect(df_corpus)

head(meta(df_corpus)) ## response variables are saved as meta data
```

```
##   release_date BICLB10Y_LAST_PRICE CLAComdty_ASK_PRICE CLAComdty_BID_PRICE
## 1   1995-02-01               128.2                  NA                  NA
## 2   1995-03-30               149.9                  NA                  NA
## 3   1995-05-25               162.3                  NA                  NA
## 4   1995-07-06               183.7                  NA                  NA
## 5   1995-07-10               179.9                  NA                  NA
## 6   1995-08-24               172.7                  NA                  NA
##   CLAComdty_change CLAComdty_change_cat CLAComdty_HIGH_PRICE
## 1               NA                   NA                   NA
## 2               NA                   NA                   NA
## 3               NA                   NA                   NA
## 4               NA                   NA                   NA
## 5               NA                   NA                   NA
## 6               NA                   NA                   NA
##   CLAComdty_LAST_PRICE CLAComdty_LOW_PRICE CLAComdty_OPEN_PRICE
## 1                   NA                  NA                   NA
## 2                   NA                  NA                   NA
## 3                   NA                  NA                   NA
## 4                   NA                  NA                   NA
## 5                   NA                  NA                   NA
## 6                   NA                  NA                   NA
##   CLAComdty_VOLUME H15T10Y_change H15T10Y_change_cat H15T10Y_HIGH_PRICE
## 1               NA          -0.22                 -1               7.88
## 2               NA          -0.70                 -1               7.88
## 3               NA          -0.30                 -1               6.69
## 4               NA          -0.14                 -1               6.19
## 5               NA          -0.15                 -1               6.19
## 6               NA           0.31                  1               6.19
##   H15T10Y_LAST_PRICE H15T10Y_LOW_PRICE H15T10Y_OPEN_PRICE
## 1               7.66              7.88               7.88
## 2               7.18              7.88               7.88
## 3               6.39              6.69               6.69
## 4               6.05              6.19               6.19
## 5               6.04              6.19               6.19
## 6               6.50              6.19               6.19
##   MOODCBAA_LAST_PRICE SPX_ASK_PRICE SPX_BID_PRICE SPX_change
## 1                8.94            NA            NA      -0.02
## 2                8.66            NA            NA      -0.95
## 3                8.00            NA            NA       0.22
## 4                7.87            NA            NA       6.73
## 5                7.84            NA            NA       0.82
## 6                8.19            NA            NA       0.32
##   SPX_change_cat SPX_HIGH_PRICE SPX_LAST_PRICE SPX_LOW_PRICE
## 1             -1         472.75         470.40        469.29
## 2             -1         504.66         502.22        501.00
## 3              1         529.04         528.59        524.89
## 4              1         553.99         553.99        546.59
## 5              1         558.48         557.19        555.77
## 6              1         558.63         557.46        555.20
##   SPX_OPEN_PRICE SPX_VOLUME TNX_ADJ_CLOSE_PRICE TNX_change TNX_change_cat
## 1         470.42  233471600               7.638      0.090              1
## 2         503.17  276731488               7.159      0.088              1
## 3         528.37  262472000               6.380      0.013              1
## 4         547.26  275352384               6.043     -0.126             -1
## 5         556.37  288479008               6.022     -0.009             -1
## 6         557.14  236858592               6.487     -0.086             -1
##   TNX_CLOSE_PRICE TNX_HIGH_PRICE TNX_LOW_PRICE TNX_OPEN_PRICE
## 1           7.638          7.638         7.548          7.548
## 2           7.159          7.185         7.071          7.071
## 3           6.380          6.414         6.299          6.367
## 4           6.043          6.228         6.043          6.169
## 5           6.022          6.039         6.014          6.031
## 6           6.487          6.573         6.482          6.573
##   USSOC_ASK_PRICE USSOC_BID_PRICE USSOC_change USSOC_change_cat
## 1              NA              NA           NA               NA
## 2              NA              NA           NA               NA
## 3              NA              NA           NA               NA
## 4              NA              NA           NA               NA
## 5              NA              NA           NA               NA
## 6              NA              NA           NA               NA
##   USSOC_HIGH_PRICE USSOC_LAST_PRICE USSOC_LOW_PRICE USSOC_OPEN_PRICE
## 1               NA               NA              NA               NA
## 2               NA               NA              NA               NA
## 3               NA               NA              NA               NA
## 4               NA               NA              NA               NA
## 5               NA               NA              NA               NA
## 6               NA               NA              NA               NA
##   VIX_change VIX_change_cat VIX_HIGH_PRICE VIX_LAST_PRICE VIX_LOW_PRICE
## 1      -0.01             -1          11.95          11.73         11.53
## 2       0.03              1          13.03          12.62         12.30
## 3      -0.13             -1          11.99          11.63         11.31
## 4      -0.83             -1          12.39          11.52         11.43
## 5       1.00              1          12.87          12.19         10.93
## 6      -0.37             -1          13.52          12.94         12.86
##   VIX_OPEN_PRICE CLAComdty_close_perc_open H15T10Y_close_perc_open
## 1          11.74                        NA               0.9720812
## 2          12.59                        NA               0.9111675
## 3          11.76                        NA               0.9551570
## 4          12.35                        NA               0.9773829
## 5          11.19                        NA               0.9757674
## 6          13.31                        NA               1.0500808
##   SPX_close_perc_open TNX_close_perc_open USSOC_close_perc_open
## 1           0.9999575           1.0119237                    NA
## 2           0.9981120           1.0124452                    NA
## 3           1.0004164           1.0020418                    NA
## 4           1.0122976           0.9795753                    NA
## 5           1.0014738           0.9985077                    NA
## 6           1.0005744           0.9869162                    NA
##   VIX_close_perc_open
## 1           1.0008525
## 2           0.9976228
## 3           1.0111780
## 4           1.0720486
## 5           0.9179655
## 6           1.0285935
```

# Pre-process the corpus `df_corpus`

Convert all words to lowercase:


```r
df_corpus <- tm_map(
  df_corpus, 
  content_transformer(tolower)
)
```

Remove white space:


```r
df_corpus <- tm_map(
  df_corpus, 
  stripWhitespace
) 
```

Remove numbers:


```r
df_corpus <- tm_map(
  df_corpus, 
  removeNumbers
)
```

Remove stop words:


```r
df_corpus <- tm_map(
  df_corpus, 
  removeWords, 
  stopwords("english")
)
```

Remove Fed employees' names and titles, FRB cities, months, days of the week,
country names and abbreviations, punctuation; stem words to the root:


```r
delete_words <- read.csv(
  "/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/delete_words.csv"
)

str(delete_words)
```

```
## 'data.frame':	736 obs. of  1 variable:
##  $ delete_words: Factor w/ 735 levels "about","AD","Adolph",..: 2 3 4 5 6 7 8 9 10 11 ...
```

```r
delete_words$delete_words <- as.character(delete_words$delete_words)

delete_words$delete_words <- tolower(delete_words$delete_words) ## convert to lower case

delete_words <- unlist(delete_words$delete_words)

df_corpus <- tm_map(
  df_corpus, 
  removeWords, 
  delete_words
)



##:::::::::::::::::::: Delete names of U.S. States: 
df_corpus <- tm_map(
  df_corpus, 
  removeWords, 
  tolower(state.abb) ## state abbreviations
)

df_corpus <- tm_map(
  df_corpus, 
  removeWords, 
  tolower(state.name) ## full names of states
)

df_corpus <- tm_map(
  df_corpus, 
  removeWords, 
  tolower(state.region) ## U.S. regions
)



##:::::::::::::::::::: Remove punctuation:
df_corpus <- tm_map(
  df_corpus, 
  removePunctuation
)
```


```r
##:::::::::::::::::::: Delete names of U.S. cities: 
library(maps)

cities <- unlist(
  tolower(
    us.cities$name
  )
)



##:::::::::::::::::::: Stem words:
df_corpus <- tm_map(
  df_corpus, 
  stemDocument
)
```

# Create a document-term matrix (`dtm`)


```r
dtm <- DocumentTermMatrix(
  df_corpus
  )

#dim(dtm)
str(dtm)
```

```
## List of 6
##  $ i       : int [1:176522] 1 1 1 1 1 1 1 1 1 1 ...
##  $ j       : int [1:176522] 48 51 72 105 212 327 412 430 598 628 ...
##  $ v       : num [1:176522] 2 1 1 1 2 2 1 1 1 1 ...
##  $ nrow    : int 352
##  $ ncol    : int 4613
##  $ dimnames:List of 2
##   ..$ Docs : chr [1:352] "1995-02-01_s" "1995-03-30_m" "1995-05-25_m" "1995-07-06_s" ...
##   ..$ Terms: chr [1:4613] "aaa" "aaronson" "abandon" "abat" ...
##  - attr(*, "class")= chr [1:2] "DocumentTermMatrix" "simple_triplet_matrix"
##  - attr(*, "weighting")= chr [1:2] "term frequency" "tf"
```

```r
dtm
```

```
## <<DocumentTermMatrix (documents: 352, terms: 4613)>>
## Non-/sparse entries: 176522/1447254
## Sparsity           : 89%
## Maximal term length: 24
## Weighting          : term frequency (tf)
```

```r
dtm_mat <- as.matrix(dtm)
```


```r
set.seed(1234)

dtm1 <- removeSparseTerms(dtm,
                          sparse = 0.3) 

dtm1
```

```
## <<DocumentTermMatrix (documents: 352, terms: 61)>>
## Non-/sparse entries: 17499/3973
## Sparsity           : 19%
## Maximal term length: 9
## Weighting          : term frequency (tf)
```

```r
dtm1_mat <- as.matrix(dtm1)

head(dtm1_mat)
```

```
##               Terms
## Docs           action activ anticip appear balanc busi condit consist
##   1995-02-01_s      2     1       0      0      0    0      0       0
##   1995-03-30_m     15    15       8      7     14   17     15       9
##   1995-05-25_m      7    18       8      8     20   19     11       9
##   1995-07-06_s      1     0       0      0      0    0      1       0
##   1995-07-10_m      2    12       6      9      9   18     10       5
##   1995-08-24_m      8    10       8      7     15   19      8      11
##               Terms
## Docs           continu current decid develop econom employ energi expand
##   1995-02-01_s       1       0     0       0      2      0      0      0
##   1995-03-30_m      26      14     3      19     52      4      6      3
##   1995-05-25_m      16       6     1      13     42      3      1      0
##   1995-07-06_s       0       0     1       0      0      0      0      0
##   1995-07-10_m      26      12     2      13     39      3      4      3
##   1995-08-24_m      20      16     2      20     59      6      4      5
##               Terms
## Docs           expect financi foster fund growth hous household improv
##   1995-02-01_s      0       0      1    0      2    0         0      0
##   1995-03-30_m     11      25      4    5     60    9         0      2
##   1995-05-25_m     16      14      3    6     34   10         1      3
##   1995-07-06_s      0       0      0    1      0    0         0      0
##   1995-07-10_m     16      15      1    3     34    6         2      6
##   1995-08-24_m     19      20      4    7     75    8         1      6
##               Terms
## Docs           incom increas indic inflat inform invest labor level like
##   1995-02-01_s     0       2     0      1      0      0     0     0    0
##   1995-03-30_m     7      39    11     25     12     10     4    23   15
##   1995-05-25_m     5      25     7     14      7     11     4    16    6
##   1995-07-06_s     0       0     0      0      0      0     0     0    0
##   1995-07-10_m     5      21     9     11      7      6     5    12   14
##   1995-08-24_m     7      28    14     20      5      5     7    15   12
##               Terms
## Docs           longer low maintain market measur moder month open outlook
##   1995-02-01_s      0   0        0      2      0     1     0    1       0
##   1995-03-30_m      3   5        4     40      7    20    20   16       5
##   1995-05-25_m      0   6       13     70      3    16    30   48       6
##   1995-07-06_s      0   0        0      1      0     0     0    1       0
##   1995-07-10_m      4   6        6     34      4    14    14   11       9
##   1995-08-24_m      3   2        3     30      9    21    25   10       9
##               Terms
## Docs           pace pressur price quarter rang rate recent remain risk run
##   1995-02-01_s    1       0     0       0    0    4      0      0    0   0
##   1995-03-30_m    7      14    36      33   32   33     16     10   10   9
##   1995-05-25_m    9       8    25      12    5   38     27     14    4   4
##   1995-07-06_s    0       2     0       0    0    1      0      0    0   0
##   1995-07-10_m   12      12    28      23   12   32     10     15    4   4
##   1995-08-24_m    8      18    35      31   61   45     27     26   13   7
##               Terms
## Docs           secur spend stabil support sustain target term time year
##   1995-02-01_s     0     0      0       0       1      0    0    0    0
##   1995-03-30_m     1    11     14       7      10      6    9   14   33
##   1995-05-25_m    21    15      9       4      10      0   14   18   12
##   1995-07-06_s     0     0      0       0       0      0    0    0    0
##   1995-07-10_m     3     8      6       4      11      0   13    4   18
##   1995-08-24_m     1    12     14       8      10      2   10   13   39
```

```r
#View(sort(colnames(dtm1_mat)))
```

# Model inputs


```r
##:::::::::::::::::::: Number of documents:
M <- nrow(dtm1_mat) 



##:::::::::::::::::::: Integer array of size M - each of its elements is the 
##:::::::::::::::::::: number of words in that document:
N <- rowSums(dtm1_mat) 



##:::::::::::::::::::: Number of words in the vocabulary which is the total
##:::::::::::::::::::: number of words: 
V <- ncol(dtm1_mat) 
```

Some experimentation analysis led me to choose the following priors/model 
inputs:


```r
##:::::::::::::::::::: Number of topics:
K <- 3

##:::::::::::::::::::: Beta:
beta_3 <- rep(
  200 / V, 
  V ## vector size V
  )



##:::::::::::::::::::: Alpha:
##::::::::: I experimented with alpha: lower values yield more 
##::::::::: polarized outcomes and higher yield more concentrated outcomes
##::::::::: so am aiming for higher values):
alpha_3 <- rep(
  100 / K,
  K ## size K
  )
```

# Prior predicitve distribution


```r
library(rstan)
library(rstanarm)
```


```r
setwd("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/code")

expose_stan_functions("LDApriors_rng.stan")

prior_LDA_dtm_1 <- LDA_rng(
  V = V,
  beta = beta_3,
  K = K,
  alpha = alpha_3,
  M = M, 
  N = N
)
```


```r
##:::::::::::::::::::: How closely does my prior document term matrix
##:::::::::::::::::::: (`prior_LDA_dtm_1`) resemble my actual document term
##:::::::::::::::::::: matrix (`dtm1_mat`):

#View(prior_LDA_dtm_1)
#View(dtm1_mat)

#summary(prior_LDA_dtm_1 - dtm1_mat) == 0
hist(prior_LDA_dtm_1 - dtm1_mat)

summary(
  rowSums(prior_LDA_dtm_1) - 
    rowSums(dtm1_mat)
)

#sum((prior_LDA_dtm_1 - dtm1_mat) == 0)
#sum((prior_LDA_dtm_1 - dtm1_mat) != 0)

round(
  (
  sum( ( prior_LDA_dtm_1 - dtm1_mat) == 0 ) / 
    ( ncol(dtm1_mat) * nrow(dtm1_mat) )
       * 100 
       ), 
      digits = 2
  ) ## percentage of `prior_LDA_dtm_1` values that match `dtm1_mat`
```

# LDA model and posterior prediction model of past data


```r
library(tidytext)

library(dplyr)
```


```r
##:::::::::::::::::::: Convert `df_corpus` into a single data frame column:
text_df <- as.data.frame(
  do.call(
    rbind, 
    lapply(
      df_corpus, 
      FUN = as.character
      )
  ), 
  stringsAsFactors = FALSE
) 



##:::::::::::::::::::: Rename the column:
colnames(text_df) <- "text"



##:::::::::::::::::::: `doc_id`:
text_df$doc <- 1:nrow(text_df)



##:::::::::::::::::::: Get word stems:
text_df <- text_df %>% 
  unnest_tokens(
    wordstem, 
    text
  )



##:::::::::::::::::::: Create a flattened dtm:
text_df <- text_df[text_df$wordstem %in% 
                     colnames(dtm1_mat), ]



##:::::::::::::::::::: Create a data frame of the response variables
##:::::::::::::::::::: (`close_perc_open` variables only):
response_vars <- meta(df_corpus)

response_vars_all <- response_vars[ , 49:54] 

summary(is.na(response_vars_all)) 
```

```
##  CLAComdty_close_perc_open H15T10Y_close_perc_open SPX_close_perc_open
##  Mode :logical             Mode :logical           Mode :logical      
##  FALSE:36                  FALSE:352               FALSE:352          
##  TRUE :316                                                            
##  TNX_close_perc_open USSOC_close_perc_open VIX_close_perc_open
##  Mode :logical       Mode :logical         Mode :logical      
##  FALSE:352           FALSE:267             FALSE:352          
##                      TRUE :85
```

```r
##^ `CLAComdty_close_perc_open` & `USSOC_close_perc_open` suffer from missingness
## which cannot be accepted by the below posterior prediction stan model so must 
## eliminate those variables.



##:::::::::::::::::::: Final response variables data frame (without 
##:::::::::::::::::::: `CLAComdty_close_perc_open` & `USSOC_close_perc_open`):
response_vars_sub <- response_vars_all[ , c(2:4, 6)] 



##:::::::::::::::::::: Input data for the below posterior prediction stan model:
dat <- with(text_df, 
            list(
              
              K = K, 
              
              V = ncol(dtm1_mat), 
              
              M = nrow(dtm1_mat), 
              
              N = nrow(text_df),
              
              w = as.integer(factor(
                wordstem, 
                levels = colnames(dtm1_mat)
                )
                             ),
              
              doc = doc, 
              
              alpha_mean = 1, 
              
              beta = rep( 1 / K, 
                          ncol(dtm1_mat)
                          ),
              
              P = ncol(response_vars_sub), ## number of stocks
              
              Y = response_vars_sub,  ## price changes 
              
              alpha_stock = 0, ## intercept: probability that this stock prices swings this much per day
              
              beta_stock_raw =  0, 
              
              tau_mean = 1, ## standard deviation in coefficient across words
              
              sigma_mean = 6, ## std devs on non-fomc and fomc dates for spx closing prices
              
              DTM = ( dtm1_mat / rowSums(dtm1_mat) )
              
              )
            )
```


```r
##:::::::::::::::::::: Posterior prediction stan model:
start_time <- Sys.time() ## record how long the model takes to run


post <- stan("LDA.stan", 
             data = dat, 
             cores = parallel::detectCores()
             ) 


end_time <- Sys.time() ## record how long the model takes to run

end_time - start_time ## how long the stan model took to run (about 8.5 hours on 4 cores)


setwd("/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files")

saveRDS(post,
        file = "post_20180424.rds") ## save the output of the stan mdoel
```


```r
test <- readRDS(
  "/Users/stephanielangeland/Desktop/Columbia/masters_thesis/Langeland_thesis/input_output_files/post_20180424.rds"
  )

post <- test

#post
```

## Explaination of the output (`post`)

_Refer to the paper for more details._

1) The chains appear to be going to different places because the labels of the
categories can be swapped without changing the probability of observing the
data, even though specifying `alpha` to be a positive ordered vector was supposed
to prevent that. But each of those imply the same distribution for everything
that is not `theta` or `alpha`. And within each chain, the Rhat values for
`theta` and `alpha` are acceptable.  Furthermore, the `Y_tilde` Rhat values
are all 1, which suggests that the chains converged.

2) The above output represents the results of the LDA model that fed a posterior
prediction model of past data.  This model first used LDA on the words of the
FOMC post-meeting announcements and minutes.  Then a prediction model was applied
to predict changes in securities on the days of Fed releases.  The above output
shows the posterior predictions of changes in the response variables, which is
represented within `Y_tilde`.  The response variables are the 10-year constant
maturity rate (`H15T10Y`), Chicago Board Options Exchange (CBOE) interest rate
on the 10 year Treasury Note (`TNX`), S&P 500 Index (`SPX`), and CBOE VIX Index
(`VIX`).  These variables were recorded as the closing value as a percentage of
the opening value.  Therefore, in the raw data (`response_vars_sub`), values
greater than 1 mean that the closing value was greater than the opening so the
security's value went up after the Fed released materials.  Inversely, values
less than 1 mean that the closing value was less than the opening so the security's
value went down after the Fed released materials.  To ensure that an increase (to
simplify the analysis of the output) is always "an improvement to market conditions",
the reciprocal of the transformed (close / open) VIX is recorded in the raw data
(`response_vars_sub`) because declines in the VIX signal subdued volatility but
I wanted all variables to be consistent in that an increase is always a "good"
market reaction. Please refer to the below section for an analysis of the
differences between the output (`Y_tilde`) and the actual response variables
(`response_vars_sub`).


```r
Y_tilde <- extract(post, 
                   pars = "Y_tilde", 
                   permuted = FALSE
                   )

#Y_tilde

length(Y_tilde)
```

```
## [1] 5632000
```

```r
dim(Y_tilde)
```

```
## [1] 1000    4 1408
```

## Explaination of the output (`Y_tilde`)

_Refer to the paper for more details._

`Y_tilde` is the number of iterations (1000) by the number of
chains (4) by the number of Fed days (352) times the number of
response variables (4) (which equals 1408).  Each `Y_tilde` matrix
was collapsed into a vector.  There is 1 matrix for each iteration and chain
within `Y_tilde`.

# Posterior predictive checks

_Refer to the paper for more details._

## Check how well the model predicts the response variables

Create a matrix that contains the differences between the predictions and actual
response variables, then calculate the average of all of those posterior draws:


```r
options(scipen = 999)

model_residuals <- apply(
  Y_tilde, 
  1:2, 
  FUN = function(y) response_vars_sub - matrix(y, 
                                               nrow = 352, 
                                               ncol = 4)
)


model_residuals_avg <- Reduce("+", model_residuals) / length(model_residuals)

str(model_residuals_avg)
```

```
## 'data.frame':	352 obs. of  4 variables:
##  $ H15T10Y_close_perc_open: num  -0.0385 -0.0914 -0.0478 -0.031 -0.0267 ...
##  $ SPX_close_perc_open    : num  -0.0111 -0.0046 -0.00207 0.00395 -0.00159 ...
##  $ TNX_close_perc_open    : num  0.002644 0.007951 0.000372 -0.028383 -0.003151 ...
##  $ VIX_close_perc_open    : num  -0.00944 -0.00453 0.00862 0.06281 -0.08484 ...
```

```r
head(model_residuals_avg)
```

```
##   H15T10Y_close_perc_open SPX_close_perc_open TNX_close_perc_open
## 1             -0.03846488        -0.011095936        0.0026439958
## 2             -0.09142136        -0.004595859        0.0079513124
## 3             -0.04783348        -0.002066869        0.0003724572
## 4             -0.03103787         0.003953571       -0.0283830650
## 5             -0.02671477        -0.001589004       -0.0031514207
## 6              0.04743968        -0.003010088       -0.0161601666
##   VIX_close_perc_open
## 1        -0.009438363
## 2        -0.004527864
## 3         0.008615711
## 4         0.062806372
## 5        -0.084840707
## 6         0.025230688
```

```r
model_residuals_avg_abs <- abs(model_residuals_avg) ## take the absolute value
```

### Range rediction accuracy

_Check to see how close the predictions are to the actual response variables_
_(within a range - one standard deviation of the corresponding response variable)._

__`H15T10Y_close_perc_open`	- 10-year constant maturity rate:__


```r
##:::::::::::::::::::: Are the predictions within 1 standard deviation of
##:::::::::::::::::::: `H15T10Y_close_perc_open`?:
model_residuals_avg_abs_rng_h15 <- ifelse(
  model_residuals_avg_abs$H15T10Y_close_perc_open <= sd(response_vars_sub$H15T10Y_close_perc_open),
  "within_range",
  "out_of_range"
)

model_residuals_avg_abs_rng_h15 <- as.data.frame(model_residuals_avg_abs_rng_h15)

summary(model_residuals_avg_abs_rng_h15)
```

```
##  model_residuals_avg_abs_rng_h15
##  out_of_range: 91               
##  within_range:261
```

```r
##::::::::: Percentage of each variable that was predicted "within range":
rng_h15_pred <- sum(model_residuals_avg_abs_rng_h15 == "within_range") / 
  nrow(model_residuals_avg_abs_rng_h15) * 100

rng_h15_pred
```

```
## [1] 74.14773
```

74.15% of the time, the model correctly predicted
the change in the 10-year constant maturity rate within 1 standard deviation of
observed data for the 10-year constant maturity rate.

__`SPX_close_perc_open` - S&P 500 Index:__


```r
##:::::::::::::::::::: Are the predictions within 1 standard deviation of
##:::::::::::::::::::: `SPX_close_perc_open`?:
model_residuals_avg_abs_rng_spx <- ifelse(
  model_residuals_avg_abs$SPX_close_perc_open <= sd(response_vars_sub$SPX_close_perc_open),
  "within_range",
  "out_of_range"
)

model_residuals_avg_abs_rng_spx <- as.data.frame(model_residuals_avg_abs_rng_spx)

summary(model_residuals_avg_abs_rng_spx)
```

```
##  model_residuals_avg_abs_rng_spx
##  out_of_range: 68               
##  within_range:284
```

```r
##::::::::: Percentage of each variable that was predicted "within range":
rng_spx_pred <- sum(model_residuals_avg_abs_rng_spx == "within_range") / 
  nrow(model_residuals_avg_abs_rng_spx) * 100

rng_spx_pred
```

```
## [1] 80.68182
```

80.68% of the time, the model correctly predicted
the change in the S&P 500 Index within 1 standard deviation of observed data for
the S&P 500 Index.

__`TNX_close_perc_open` - CBOE interest rate on the 10 year Treasury Note:__


```r
##:::::::::::::::::::: Are the predictions within 1 standard deviation of
##:::::::::::::::::::: `TNX_close_perc_open`?:
model_residuals_avg_abs_rng_tnx <- ifelse(
  model_residuals_avg_abs$TNX_close_perc_open <= sd(response_vars_sub$TNX_close_perc_open),
  "within_range",
  "out_of_range"
)

model_residuals_avg_abs_rng_tnx <- as.data.frame(model_residuals_avg_abs_rng_tnx)

summary(model_residuals_avg_abs_rng_tnx)
```

```
##  model_residuals_avg_abs_rng_tnx
##  out_of_range: 82               
##  within_range:270
```

```r
##::::::::: Percentage of each variable that was predicted "within range":
rng_tnx_pred <- sum(model_residuals_avg_abs_rng_tnx == "within_range") / 
  nrow(model_residuals_avg_abs_rng_tnx) * 100

rng_tnx_pred
```

```
## [1] 76.70455
```

76.7% of the time, the model correctly predicted
the change in the CBOE interest rate on the 10 year Treasury Note within 1
standard deviation of observed data for this rate.

__`VIX_close_perc_open`	- VIX Index:__


```r
##:::::::::::::::::::: Are the predictions within 1 standard deviation of
##:::::::::::::::::::: `VIX_close_perc_open`?:
model_residuals_avg_abs_rng_vix <- ifelse(
  model_residuals_avg_abs$VIX_close_perc_open <= sd(response_vars_sub$VIX_close_perc_open),
  "within_range",
  "out_of_range"
)

model_residuals_avg_abs_rng_vix <- as.data.frame(model_residuals_avg_abs_rng_vix)

summary(model_residuals_avg_abs_rng_vix)
```

```
##  model_residuals_avg_abs_rng_vix
##  out_of_range: 80               
##  within_range:272
```

```r
##::::::::: Percentage of each variable that was predicted "within range":
rng_vix_pred <- sum(model_residuals_avg_abs_rng_vix == "within_range") / 
  nrow(model_residuals_avg_abs_rng_vix) * 100

rng_vix_pred
```

```
## [1] 77.27273
```

77.27% of the time, the model correctly predicted
the change in the VIX within 1 standard deviation of observed data for the VIX.

Overall, the model correctly predicted the changes in the closing value as a
percentage of the opening value, within 1 standard deviation of each respective
response variable, well.

### Directional prediction accuracy

It is also a problem if the predictions are going in the wrong  direction e.g.,
if any of the `response_vars_sub` values > 1 then the close > open price so the
value went up after the Fed release. On the other hand, if the
`response_vars_sub` < 1 then the close < open price so the value went down after
the Fed release.  If the prediction > 1 and the corresponding `response_vars_sub`
value < 1 then the model predicted the security moving in the wrong direction
which is an issue.  In this section , I create a measure of whether
`model_residuals_avg` accurately predicts the direction of the response variables
(`response_vars_sub`).

Put the predictions in matrix format for each chain and iteration:


```r
##:::::::::::::::::::: Convert the post output to a matrix:
post_mat <- as.matrix(post) 

str(post_mat)
```

```
##  num [1:4000, 1:3128] 0.463 0.46 0.46 0.446 0.404 ...
##  - attr(*, "dimnames")=List of 2
##   ..$ iterations: NULL
##   ..$ parameters: chr [1:3128] "alpha[1]" "alpha[2]" "alpha[3]" "theta[1,1]" ...
```

```r
##:::::::::::::::::::: Extract the `Y_tilde` predictions from `post_mat`:
post_mat_Y_tilde <- post_mat[ , 
                              grep("Y_tilde", 
                                   colnames(post_mat), 
                                   fixed = TRUE)
                              ]

str(post_mat_Y_tilde)
```

```
##  num [1:4000, 1:1408] 1.017 0.959 0.959 1 1.06 ...
##  - attr(*, "dimnames")=List of 2
##   ..$ iterations: NULL
##   ..$ parameters: chr [1:1408] "Y_tilde[1,1]" "Y_tilde[2,1]" "Y_tilde[3,1]" "Y_tilde[4,1]" ...
```

```r
dim(post_mat_Y_tilde)
```

```
## [1] 4000 1408
```

_Note:_ The column names in `post_mat_Y_tilde` correspond to the variables in 
`response_vars_sub` so:

  * Any column name with a ",1]" corresponds to "H15T10Y_close_perc_open"
  (`colnames(response_vars_sub)[1]`).
  
  * Any column name with a ",2]" corresponds to "SPX_close_perc_open"
  (`colnames(response_vars_sub)[2]`).
  
   * Any column name with a ",3]" corresponds to "TNX_close_perc_open"
  (`colnames(response_vars_sub)[3]`).
  
   * Any column name with a ",4]" corresponds to "VIX_close_perc_open"
  (`colnames(response_vars_sub)[4]`).

__Overall:__


```r
overall_direction <- table(
  observed = sign(as.matrix(response_vars_sub) - 1), 
  predicted = sign(colMeans(post_mat_Y_tilde) - 1)
  )

rownames(overall_direction)[1] <- "closing < opening"
colnames(overall_direction)[1] <- "closing < opening"

rownames(overall_direction)[2] <- "closing = opening (no change)"

rownames(overall_direction)[3] <- "closing > opening"
colnames(overall_direction)[2] <- "closing > opening"

overall_direction
```

```
##                                predicted
## observed                        closing < opening closing > opening
##   closing < opening                            10               563
##   closing = opening (no change)                 0               138
##   closing > opening                             6               691
```

Overall, for all variables:

1) The model best predicts `closing > opening`

2) It does a bad job of predicting `closing < opening`

3) It NEVER predicts `closing = opening`

__`H15T10Y_close_perc_open`	- 10-year constant maturity rate:__


```r
H15T10Y_direction <- table(
  observed = sign(as.matrix(response_vars_sub$H15T10Y_close_perc_open) - 1), 
  predicted = sign(colMeans(post_mat_Y_tilde[ ,
                           grep(",1]", ## because colnames(response_vars_sub)[1]
                                colnames(post_mat_Y_tilde), 
                                fixed = TRUE)
                           ]) - 1)
  )

rownames(H15T10Y_direction)[1] <- "closing < opening"
colnames(H15T10Y_direction)[1] <- "closing < opening"

rownames(H15T10Y_direction)[2] <- "closing = opening (no change)"

rownames(H15T10Y_direction)[3] <- "closing > opening"
colnames(H15T10Y_direction)[2] <- "closing > opening"


H15T10Y_direction
```

```
##                                predicted
## observed                        closing < opening closing > opening
##   closing < opening                             3               127
##   closing = opening (no change)                 0               124
##   closing > opening                             1                97
```

For the 10-year constant maturity rate: bad predictions overall.

__`SPX_close_perc_open` - S&P 500 Index:__


```r
SPX_direction <- table(
  observed = sign(as.matrix(response_vars_sub$SPX_close_perc_open) - 1), 
  predicted = sign(colMeans(post_mat_Y_tilde[ ,
                           grep(",2]", ## because colnames(response_vars_sub)[2]
                                colnames(post_mat_Y_tilde), 
                                fixed = TRUE)
                           ]) - 1)
  )

rownames(SPX_direction)[1] <- "closing < opening"
colnames(SPX_direction)[1] <- "closing < opening"

rownames(SPX_direction)[2] <- "closing > opening"
colnames(SPX_direction)[2] <- "closing > opening"

SPX_direction
```

```
##                    predicted
## observed            closing < opening closing > opening
##   closing < opening                 2               157
##   closing > opening                 2               191
```

For the S&P 500 Index:

1) The model best predicts `closing > opening`

2) It does a horrible job of predicting `closing < opening`

__`TNX_close_perc_open` - CBOE interest rate on the 10 year Treasury Note:__


```r
TNX_direction <- table(
  observed = sign(as.matrix(response_vars_sub$TNX_close_perc_open) - 1), 
  predicted = sign(colMeans(post_mat_Y_tilde[ ,
                           grep(",3]", ## because colnames(response_vars_sub)[3]
                                colnames(post_mat_Y_tilde), 
                                fixed = TRUE)
                           ]) - 1)
  )

rownames(TNX_direction)[1] <- "closing < opening"
colnames(TNX_direction)[1] <- "closing < opening"

rownames(TNX_direction)[2] <- "closing = opening (no change)"

rownames(TNX_direction)[3] <- "closing > opening"
colnames(TNX_direction)[2] <- "closing > opening"

TNX_direction
```

```
##                                predicted
## observed                        closing < opening closing > opening
##   closing < opening                             1               162
##   closing = opening (no change)                 0                12
##   closing > opening                             3               174
```

For the CBOE interest rate on the 10 year Treasury Note:

1) The model does an okay job of predicting `closing > opening`

2) It does a horrible job of predicting `closing < opening`

3) It NEVER predicts `closing = opening`

__`VIX_close_perc_open`	- VIX Index:__


```r
VIX_direction <- table(
  observed = sign(as.matrix(response_vars_sub$VIX_close_perc_open) - 1), 
  predicted = sign(colMeans(post_mat_Y_tilde[ ,
                           grep(",4]", ## because colnames(response_vars_sub)[4]
                                colnames(post_mat_Y_tilde), 
                                fixed = TRUE)
                           ]) - 1)
  )

rownames(VIX_direction)[1] <- "closing < opening"
colnames(VIX_direction)[1] <- "closing < opening"

rownames(VIX_direction)[2] <- "closing = opening (no change)"

rownames(VIX_direction)[3] <- "closing > opening"
colnames(VIX_direction)[2] <- "closing > opening"

VIX_direction
```

```
##                                predicted
## observed                        closing < opening closing > opening
##   closing < opening                             4               117
##   closing = opening (no change)                 0                 2
##   closing > opening                             0               229
```

For the VIX:

1) The model does a good job of predicting `closing > opening`

2) It does a horrible job of predicting `closing < opening`

3) It NEVER predicts `closing = opening`

## Model fit 


```r
library(ggplot2)
library(bayesplot)
```

### Density overlay plots

Compare the empirical distribution of the data _Y_ to the distributions of
simulated/replicated data _Yrep_ from the posterior predictive distribution.
How well do predictions from the posterior distribution match the true outcome
variable that was used to fit the data?

__`H15T10Y_close_perc_open`	- 10-year constant maturity rate:__


```r
pp_check(
  object = response_vars_sub$H15T10Y_close_perc_open, 
  yrep = post_mat_Y_tilde[ ,
                           grep(",1]", ## because colnames(response_vars_sub)[1]
                                colnames(post_mat_Y_tilde), 
                                fixed = TRUE)
                           ],
  fun = ppc_dens_overlay
  ) + 
  ggtitle("10Y Constant Maturity Rate (Y) versus Predictions (Yrep)")
```

![](11_text_preprocessing_and_LDA_model_20180426_v1_files/figure-html/unnamed-chunk-43-1.png)<!-- -->

It appears that observed data are more concentrated than the predictions.  Thus,
the model produced more extreme predictions than what actually occurred in the data. 

__`SPX_close_perc_open` - S&P 500 Index:__


```r
pp_check(
  object = response_vars_sub$SPX_close_perc_open, 
  yrep = post_mat_Y_tilde[ ,
                           grep(",2]", ## because colnames(response_vars_sub)[2]
                                colnames(post_mat_Y_tilde), 
                                fixed = TRUE)
                           ],
  fun = ppc_dens_overlay
  ) + 
  ggtitle("S&P 500 Index (Y) versus Predictions (Yrep)")
```

![](11_text_preprocessing_and_LDA_model_20180426_v1_files/figure-html/unnamed-chunk-44-1.png)<!-- -->

It appears that observed data are more concentrated than the predictions.  Thus,
the model produced more extreme predictions than what actually occurred in the data. 

__`TNX_close_perc_open` - CBOE interest rate on the 10 year Treasury Note:__


```r
pp_check(
  object = response_vars_sub$TNX_close_perc_open, 
  yrep = post_mat_Y_tilde[ ,
                           grep(",3]", ## because colnames(response_vars_sub)[3]
                                colnames(post_mat_Y_tilde), 
                                fixed = TRUE)
                           ],
  fun = ppc_dens_overlay
  ) + 
  ggtitle("10Y Treasury Note CBOE Interest Rate (Y)\nversus Predictions (Yrep)")
```

![](11_text_preprocessing_and_LDA_model_20180426_v1_files/figure-html/unnamed-chunk-45-1.png)<!-- -->

It appears that observed data are more concentrated than the predictions.  Thus,
the model produced more extreme predictions than what actually occurred in the data. 

__`VIX_close_perc_open`	- VIX Index:__


```r
pp_check(
  object = response_vars_sub$VIX_close_perc_open, 
  yrep = post_mat_Y_tilde[ ,
                           grep(",4]", ## because colnames(response_vars_sub)[4]
                                colnames(post_mat_Y_tilde), 
                                fixed = TRUE)
                           ],
  fun = ppc_dens_overlay
  ) + 
  ggtitle("VIX Index (Y) versus Predictions (Yrep)")
```

![](11_text_preprocessing_and_LDA_model_20180426_v1_files/figure-html/unnamed-chunk-46-1.png)<!-- -->

It appears that predictions are more concentrated than the observed data.  Thus,
the model produced less extreme predictions than what actually occurred in the data. 

### Marginal distribution plots

Compare the proportion of the observed data that fall within 50% predictive
intervals.

__`H15T10Y_close_perc_open`	- 10-year constant maturity rate:__


```r
##:::::::::::::::::::: Numerical Assessment of Calibration:
lower_h15 <- apply(
  post_mat_Y_tilde[ ,
                                       grep(",1]", ## because colnames(response_vars_sub)[1]
                                            colnames(post_mat_Y_tilde), 
                                            fixed = TRUE)
                                       ], 
  MARGIN = 2, 
  quantile, 
  probs = 0.25
)


upper_h15 <- apply(
  post_mat_Y_tilde[ ,
                                       grep(",1]", ## because colnames(response_vars_sub)[1]
                                            colnames(post_mat_Y_tilde), 
                                            fixed = TRUE)
                                       ], 
  MARGIN = 2, 
  quantile, 
  probs = 0.75
)


h15_fit <- mean(response_vars_sub$H15T10Y_close_perc_open > lower_h15 & 
       response_vars_sub$H15T10Y_close_perc_open < upper_h15)

h15_fit
```

```
## [1] 0.5965909
```

```r
##:::::::::::::::::::: Graphical Assessment of Calibration:
ppc_intervals(y = response_vars_sub$H15T10Y_close_perc_open, 
              yrep = post_mat_Y_tilde[ ,
                                       grep(",1]", ## because colnames(response_vars_sub)[1]
                                            colnames(post_mat_Y_tilde), 
                                            fixed = TRUE)
                                       ], 
              prob = 0.5
) +
  ggtitle(
    paste(
      "Posterior Predictive Interval with Probability of 0.5 for 10Y Constant\nMaturity Rate (Y) versus Predictions (Yrep)",
      "\n(Numerical Assessment of Fit = ",
      print(
        round( h15_fit * 100, digits = 2)
      ),
      "%)"
    )
  )
```

```
## [1] 59.66
```

![](11_text_preprocessing_and_LDA_model_20180426_v1_files/figure-html/unnamed-chunk-47-1.png)<!-- -->

Since the `h15_fit` > 50%, the model is over fitting because
59.66% of the data are fitting in the 50%
interval.

__`SPX_close_perc_open` - S&P 500 Index:__


```r
##:::::::::::::::::::: Numerical Assessment of Calibration:
lower_spx <- apply(
  post_mat_Y_tilde[ ,
                                       grep(",2]", ## because colnames(response_vars_sub)[2]
                                            colnames(post_mat_Y_tilde), 
                                            fixed = TRUE)
                                       ], 
  MARGIN = 2, 
  quantile, 
  probs = 0.25
)


upper_spx <- apply(
  post_mat_Y_tilde[ ,
                                       grep(",2]", ## because colnames(response_vars_sub)[2]
                                            colnames(post_mat_Y_tilde), 
                                            fixed = TRUE)
                                       ], 
  MARGIN = 2, 
  quantile, 
  probs = 0.75
)


spx_fit <- mean(response_vars_sub$SPX_close_perc_open > lower_spx & 
       response_vars_sub$SPX_close_perc_open < upper_spx)

spx_fit
```

```
## [1] 0.9545455
```

```r
##:::::::::::::::::::: Graphical Assessment of Calibration:
ppc_intervals(y = response_vars_sub$SPX_close_perc_open, 
              yrep = post_mat_Y_tilde[ ,
                                       grep(",2]", ## because colnames(response_vars_sub)[2]
                                            colnames(post_mat_Y_tilde), 
                                            fixed = TRUE)
                                       ], 
              prob = 0.5
) +
  ggtitle(
    paste(
      "Posterior Predictive Interval with Probability of 0.5 for\nS&P 500 Index (Y) versus Predictions (Yrep)",
      "\n(Numerical Assessment of Fit = ",
      print(
        round( spx_fit * 100, digits = 2)
      ),
      "%)"
    )
  )
```

```
## [1] 95.45
```

![](11_text_preprocessing_and_LDA_model_20180426_v1_files/figure-html/unnamed-chunk-48-1.png)<!-- -->

Since the `spx_fit` > 50%, the model is over fitting because
95.45% of the data are fitting in the 50%
interval.

__`TNX_close_perc_open` - CBOE interest rate on the 10 year Treasury Note:__


```r
##:::::::::::::::::::: Numerical Assessment of Calibration:
lower_tnx <- apply(
  post_mat_Y_tilde[ ,
                                       grep(",3]", ## because colnames(response_vars_sub)[3]
                                            colnames(post_mat_Y_tilde), 
                                            fixed = TRUE)
                                       ], 
  MARGIN = 2, 
  quantile, 
  probs = 0.25
)


upper_tnx <- apply(
  post_mat_Y_tilde[ ,
                                       grep(",3]", ## because colnames(response_vars_sub)[3]
                                            colnames(post_mat_Y_tilde), 
                                            fixed = TRUE)
                                       ], 
  MARGIN = 2, 
  quantile, 
  probs = 0.75
)


tnx_fit <- mean(response_vars_sub$TNX_close_perc_open > lower_tnx & 
       response_vars_sub$TNX_close_perc_open < upper_tnx)

tnx_fit
```

```
## [1] 0.8522727
```

```r
##:::::::::::::::::::: Graphical Assessment of Calibration:
ppc_intervals(y = response_vars_sub$TNX_close_perc_open, 
              yrep = post_mat_Y_tilde[ ,
                                       grep(",3]", ## because colnames(response_vars_sub)[3]
                                            colnames(post_mat_Y_tilde), 
                                            fixed = TRUE)
                                       ], 
              prob = 0.5
) +
  ggtitle(
    paste(
      "Posterior Predictive Interval with Probability of 0.5\nfor 10Y Treasury Note CBOE Interest Rate (Y)\nversus Predictions (Yrep)",
      "\n(Numerical Assessment of Fit = ",
      print(
        round( tnx_fit * 100, digits = 2)
      ),
      "%)"
    )
  )
```

```
## [1] 85.23
```

![](11_text_preprocessing_and_LDA_model_20180426_v1_files/figure-html/unnamed-chunk-49-1.png)<!-- -->

Since the `tnx_fit` > 50%, the model is over fitting because
85.23% of the data are fitting in the 50%
interval.

__`VIX_close_perc_open`	- VIX Index:__


```r
##:::::::::::::::::::: Numerical Assessment of Calibration:
lower_vix <- apply(
  post_mat_Y_tilde[ ,
                                       grep(",4]", ## because colnames(response_vars_sub)[4]
                                            colnames(post_mat_Y_tilde), 
                                            fixed = TRUE)
                                       ], 
  MARGIN = 2, 
  quantile, 
  probs = 0.25
)


upper_vix <- apply(
  post_mat_Y_tilde[ ,
                                       grep(",4]", ## because colnames(response_vars_sub)[4]
                                            colnames(post_mat_Y_tilde), 
                                            fixed = TRUE)
                                       ], 
  MARGIN = 2, 
  quantile, 
  probs = 0.75
)


vix_fit <- mean(response_vars_sub$VIX_close_perc_open > lower_vix & 
       response_vars_sub$VIX_close_perc_open < upper_vix)

vix_fit
```

```
## [1] 0.4261364
```

```r
##:::::::::::::::::::: Graphical Assessment of Calibration:
ppc_intervals(y = response_vars_sub$VIX_close_perc_open, 
              yrep = post_mat_Y_tilde[ ,
                                       grep(",4]", ## because colnames(response_vars_sub)[4]
                                            colnames(post_mat_Y_tilde), 
                                            fixed = TRUE)
                                       ], 
              prob = 0.5
) +
  ggtitle(
    paste(
      "Posterior Predictive Interval with Probability of 0.5\nfor VIX Index (Y) versus Predictions (Yrep)",
      "\n(Numerical Assessment of Fit = ",
      print(
        round( vix_fit * 100, digits = 2)
      ),
      "%)"
    )
  )
```

```
## [1] 42.61
```

![](11_text_preprocessing_and_LDA_model_20180426_v1_files/figure-html/unnamed-chunk-50-1.png)<!-- -->

Since the `vix_fit` < 50%, the model is under fitting because
42.61% of the data are fitting in the 50%
interval.


