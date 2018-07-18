if(!require('BBmisc')) install.packages('BBmisc')
suppressPackageStartupMessages(require(BBmisc))
pkgs <- c('data.table', 'plyr', 'dplyr', 'magrittr', 'purrr', 
          'tidyr', 'stringr', 'lubridate')

suppressAll(lib(pkgs))
rm(pkgs)

## --------------------- Read Data -------------------------------
dr <- 'data/USDJPY/'
fls <- dir(dr, pattern = '.csv$')
#dfm <- read.csv(paste0(dr, fls), skipNul = TRUE)

# start <- seq(1, 186, 31)
# stop <- start - 1
# stop <- c(stop[-1], length(fls))
# paste0('fls = fls[', start, ':', stop, ']')

nm <- str_replace_all(fls, '.csv', '')

##
for(i in seq(length(fls))) {
  #if(!file.exists(paste0(dr, nm[i], '.rds'))) {
  assign(nm[i], read.csv(
    paste0(dr, fls[i]), skipNul = TRUE) %>% tbl_df)
  
  ## save dataset.
  eval(parse(text = paste0(
    "saveRDS(", nm[i], ", '", dr, nm[i], ".rds')")))
  eval(parse(text = paste0("rm(", nm[i], ")")))
  cat(paste0(dr, nm[i], '.rds saved!\n'))
  #}
}; rm(i, fls, nm)

## --------------------- Check Files -------------------------------
## check the number of *.rds files in directory.
drt <- dir(dr, pattern = '[^_HL].rds')

# start <- seq(1, 186, 31)
# stop <- start - 1
# stop <- c(stop[-1], length(drt))
# paste0('drt = drt[', start, ':', stop, ']')

##
drt %>% str_split_fixed('Y|W|.rds', 4) %>% tbl_df %>% 
  select(V2, V3) %>% filter(V2 == 2015)

## If the downloaded files are display in Asia/Tokyo timezone, then 
##   we can change to default UTC timezone.
#'@ Y2015W2 %>% mutate(DateTime = mdy_hms(DateTime, tz = 'Asia/Tokyo'), 
#'@                    DateTime = with_tz(DateTime, 'UTC'))

## --------------------- Filter Data -------------------------------
dr <- 'data/USDJPY/'
drt <- dir(dr, pattern = '[^_HL].rds')
nm <- str_replace_all(drt, '.rds', '')
# start <- seq(1, 186, 31)
# stop <- start - 1
# stop <- c(stop[-1], length(drt))
# data.frame(drt = paste0('drt = drt[', start, ':', stop, ']'), colm = ';', 
#            nm = paste0('nm = nm[', start, ':', stop, ']'))

for(i in seq(length(drt))) {
  assign(nm[i], readRDS(paste0(dr, drt[i])))
  
  ## filter daily highest and lowest price.
  #'@ assign(paste0(nm[i], '_HL'), nm[i] %>% mutate(Date = as.Date(mdy_hms(DateTime))) %>% 
  #'@   group_by(Date) %>% 
  #'@   filter(Bid == min(Bid)|Bid == max(Bid)|Ask == min(Ask)|Ask == max(Ask)) %>% 
  #'@   filter(!duplicated(Bid)|!duplicated(Ask)))
  ## Error : assign() cannot handle nm[i] %>% mutate(...) since 'nm[i]' is class character.
  
  ## convert timezone, will take time around 20 minutes for 1 million plus rows due to not vectorised handling.
  #'@ eval(parse(text = paste0(
  #'@   nm[i], "_HL <- ", nm[i], " %>% mutate(DateTime = mdy_hms(DateTime, tz = 'UTC')) %>% rowwise() %>% do(DateTime = with_tz(.$DateTime, tzone = 'GMT')) %>% mutate(Date = as.Date(DateTime))")))
  
  ## filter daily highest and lowest price.
  eval(parse(text = paste0(
    nm[i], "_HL <- ", nm[i], " %>% mutate(DateTime = with_tz(mdy_hms(DateTime), 'GMT'), Date = as.Date(DateTime)) %>% group_by(Date) %>% filter(Bid == min(Bid)|Bid == max(Bid)|Ask == min(Ask)|Ask == max(Ask))")))
  eval(parse(text = paste0(nm[i], '_HL %<>% filter(!duplicated(Bid)|!duplicated(Ask))')))
  
  ## save dataset.
  eval(parse(text = paste0(
    "saveRDS(", nm[i], "_HL, '", dr, nm[i], "_HL.rds')")))
  eval(parse(text = paste0("rm(", nm[i], ")")))
  eval(parse(text = paste0("rm(", nm[i], "_HL)")))
  cat(paste0(dr, nm[i], '_HL.rds saved!\n'))
}


test <- Y2018W9
test %<>% mutate(DateTime = mdy_hms(DateTime, tz = 'UTC')) %>% rowwise() %>% 
  do(DateTime = with_tz(.$DateTime, tzone = 'GMT')) %>% 
  mutate(Date = as.Date(DateTime))

## -----------------------------------------------------------------
## convert timezone, will take time around 20 minutes for 1 million plus rows due to not vectorised handling.
eval(parse(text = paste0(
  nm[i], "_HL <- ", nm[i], " %>% mutate(DateTime = mdy_hms(DateTime, tz = 'UTC')) %>% rowwise() %>% do(DateTime = with_tz(.$DateTime, tzone = 'GMT')) %>% mutate(Date = as.Date(DateTime))")))




## check the number of *.rds files in directory.
dr <- 'data/USDJPY/'
drt <- dir(dr, pattern = '_HL.rds')

#'@ drt %>% str_split_fixed('Y|W|_HL.rds', 4) %>% tbl_df %>% 
#'@   select(V2, V3) %>% filter(V2 == 2015)

nm <- str_replace_all(drt, '.rds', '')

## filter_HL() to get daily unique high low price.
filter_HL <- function(mbase) {
  ## filter to be unique min bid, max bid and also min ask, max ask price.
  B.Min <- ddply(mbase, .(Date), summarise, 
                 Bid = min(Bid))
  B.Min.Date <- ddply(mbase, .(Date), summarise, 
                      DateTime = DateTime[which.min(Bid)])
  B.Min <- join(B.Min, B.Min.Date, by = 'Date')
  rm(B.Min.Date)
  
  B.Max <- ddply(mbase, .(Date), summarise, 
                 Bid = max(Bid))
  B.Max.Date <- ddply(mbase, .(Date), summarise, 
                      DateTime = DateTime[which.max(Bid)])
  B.Max <- join(B.Max, B.Max.Date, by = 'Date')
  rm(B.Max.Date)
  
  A.Min <- ddply(mbase, .(Date), summarise, 
                 Ask = min(Ask))
  A.Min.Date <- ddply(mbase, .(Date), summarise, 
                      DateTime = DateTime[which.min(Ask)])
  A.Min <- join(A.Min, A.Min.Date, by = 'Date')
  rm(A.Min.Date)
  
  A.Max <- ddply(mbase, .(Date), summarise, 
                 Ask = max(Ask))
  A.Max.Date <- ddply(mbase, .(Date), summarise, 
                      DateTime = DateTime[which.max(Ask)])
  A.Max <- join(A.Max, A.Max.Date, by = 'Date')
  rm(A.Max.Date)
  
  res <- bind_rows(list(B.Min, B.Max, A.Min, A.Max)) %>% arrange(DateTime)
  rm(B.Min, B.Max, A.Min, A.Max)
  
  return(res)
}

## simulate secondary filter daily high-low price.
for(i in seq(length(drt))) {
  assign(nm[i], readRDS(paste0(dr, drt[i])))
  assign(nm[i], eval(parse(text = paste0('filter_HL(', nm[i], ')'))))
  
  ## save dataset.
  eval(parse(text = paste0(
    "saveRDS(", nm[i], ", '", dr, nm[i], ".rds')")))
  eval(parse(text = paste0("rm(", nm[i], ")")))
  cat(paste0(dr, nm[i], '.rds saved!\n'))  
}

read_HL_tick_data <- function(dr = 'data/USDJPY/', df.type = 'data.table') {
  
  if(!require('BBmisc')) install.packages('BBmisc')
  suppressPackageStartupMessages(require(BBmisc))
  pkgs <- c('data.table', 'plyr', 'dplyr', 'magrittr', 'purrr', 
            'tidyr', 'stringr', 'lubridate')
  
  suppressAll(lib(pkgs))
  rm(pkgs)
  
  #'@ dr <- 'data/USDJPY/'
  
  ## unzip dataset.
  if(file.exists(paste0(dr, 'USDJPY.zip'))) {
    unzip(paste0(dr, 'USDJPY.zip'), exdir = dr)
  }
  
  res <- ldply(paste0(dr, dir(dr, pattern = '_HL.rds')), readRDS)
  
  if(df.type == 'data.table') {
    res %<>% data.table
  } else if(df.type == 'tbl_df') {
    res %<>% tbl_df
  } else {
    stop("Kindly choose df.type = 'data.table' or df.type = 'tbl_df'.")
  }
  
  file.remove(paste0(dr, dir(dr, pattern = '_HL.rds')))
  
  return(res)
  }





