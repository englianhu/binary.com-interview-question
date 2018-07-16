require('BBmisc')
require('plyr')
require('dplyr')
require('purrr')
require('stringr')
require('devtools')

## http://coin.wne.uw.edu.pl/pwojcik/hfd_en.html

if(!dir.exists('data')){
  dir.create('./data')
  
  url1 <- 'http://coin.wne.uw.edu.pl/pwojcik/hfd/HFD_assess_rules_and_data.zip'
  url2 <- 'http://coin.wne.uw.edu.pl/pwojcik/hfd/HFD_data_out.zip'
  url3 <- 'http://coin.wne.uw.edu.pl/pwojcik/hfd/HFD_assess_sample_report_and_pres.zip'
  url4 <- 'http://coin.wne.uw.edu.pl/pwojcik/hfd/HFD_sample_questions.zip'
  
  download.file(url1, destfile = './data/HFD_assess_rules_and_data.zip')
  download.file(url2, destfile = './data/HFD_data_out.zip')
  download.file(url3, destfile = './data/HFD_assess_sample_report_and_pres.zip')
  download.file(url4, destfile = './data/HFD_sample_questions.zip')
  rm(url1, url2, url3, url4)
}

## extract zipped dataset.
#'@ llply(dir('data'), function(x) unzip(paste0('data/', x), exdir = 'data'))
## 
## password protected zipped files unable to unzip through R.


## file:///C:/Users/Nijat/AppData/Local/Temp/Rar$EXb0.553/HDF_sections_assessment_201718.html
## http://coin.wne.uw.edu.pl/pwojcik/hfd_en.html
## password : #!hFd$Zo16%
## 
## This is password
## You will see
## 
## Research project: rules and in-sample data
## Research project: out-of-sample data
## Research project: template of presentation and final report
## 
## Sample exam questions
## I also sent my project that my teacher did not accept
## We should make it again
## I sent you all on gmail
## 

lnk1 <- 'https://github.com/englianhu/Quant-Strategies-HFT/blob/master/data/data_all_2011Q1.RData'
lnk2 <- 'https://github.com/englianhu/Quant-Strategies-HFT/blob/master/data/data_all_2011Q2.RData'
lnk3 <- 'https://github.com/englianhu/Quant-Strategies-HFT/blob/master/data/data_all_2011Q3.RData'
lnk4 <- 'https://github.com/englianhu/Quant-Strategies-HFT/blob/master/data/data_all_2011Q4.RData'
lnk5 <- 'https://github.com/englianhu/Quant-Strategies-HFT/blob/master/data/data_all_2012Q1.RData'
lnk <- 'https://github.com/englianhu/Quant-Strategies-HFT/blob/master/data/data_201112.zip'

download.file(lnk1, destfile = 'data/data_2011Q1.RData')
download.file(lnk2, destfile = 'data/data_2011Q2.RData')
download.file(lnk3, destfile = 'data/data_2011Q3.RData')
download.file(lnk4, destfile = 'data/data_2011Q4.RData')
download.file(lnk5, destfile = 'data/data_2012Q1.RData')
download.file(lnk, destfile = 'data/data_201112.zip')

unzip('data/data_201112.zip', exdir = 'data/data')

## load tick dataset
data.US <- llply(dir('data/', pattern = 'data'), function(x) {
  BBmisc::load2(paste0('data/', x)) %>% xts
})
data.US <- do.call(rbind, data.US)

## high frequency data week 26 to week 28.
unzip('data/W25.zip', exdir = 'data')

## https://www.r-bloggers.com/faster-files-in-r/
con <- file('data/W25.txt', open = "rb")
result <- readChar(con, file.info('data/W25.txt')$size, useBytes = TRUE)
close(con)

## read tick data
W25 <- data.table::fread('testRealTimeTransc/data/tickdata/W25.txt')
W26 <- data.table::fread('testRealTimeTransc/data/tickdata/W26.txt')
W27 <- data.table::fread('testRealTimeTransc/data/tickdata/W27.txt')

## find the daily highest and lowest price.
library('plyr')
library('dplyr')
library('magrittr')
library('purrr')
library('tidyr')
library('xts')
library('zoo')
library('lubridate')

W25T <- W25 %>% tbl_df %>% mutate(DateTime = mdy_hms(W25$DateTime, tz = 'GMT'), 
                                  Date = as.Date(DateTime))

W26T <- W26 %>% tbl_df %>% mutate(DateTime = mdy_hms(W26$DateTime, tz = 'GMT'), 
                                  Date = as.Date(DateTime))

W27T <- W27 %>% tbl_df %>% mutate(DateTime = mdy_hms(W27$DateTime, tz = 'GMT'), 
                                  Date = as.Date(DateTime))

W25T %<>% dplyr::filter(Ask == min(Ask)|Bid == min(Bid)|Ask == max(Ask)|Bid == max(Bid))
W26T %<>% dplyr::filter(Ask == min(Ask)|Bid == min(Bid)|Ask == max(Ask)|Bid == max(Bid))
W27T %<>% dplyr::filter(Ask == min(Ask)|Bid == min(Bid)|Ask == max(Ask)|Bid == max(Bid))

WT <- do.call(rbind, list(W25T, W26T, W27T))

## ================ READ DATA =================================
options(warn = -1, 'getSymbols.yahoo.warning' = FALSE)
suppressPackageStartupMessages(suppressWarnings(require('BBmisc')))
suppressAll(require('shiny'))
suppressAll(require('cronR'))
suppressAll(require('xts'))
suppressAll(require('quantmod'))
suppressAll(require('TFX'))
suppressAll(require('lubridate'))
suppressAll(require('plyr'))
suppressAll(require('dplyr'))
suppressAll(require('data.table'))
suppressAll(require('tidyr'))
suppressAll(require('magrittr'))
suppressAll(require('memoise'))
suppressAll(require('stringr'))
suppressAll(require('RCurl'))
suppressAll(require('rugarch'))
suppressAll(require('rmgarch'))
suppressAll(require('forecast'))
suppressAll(require('formattable'))


## ---------------- Get from the draft.R ----------------------------
## read files into 1 file.
WT <- ldply(dir('testRealTimeTransc/data/tickdata', pattern = '.txt$'), function(x) {
  data.table::fread(paste0('testRealTimeTransc/data/tickdata/', x))
}) %>% tbl_df %>% mutate(
  DateTime = mdy_hms(DateTime), 
  Date = as.Date(DateTime))

#> ddply(WT, .(Date), summarize,
#        +       DateTime.Bid.MN = DateTime[!duplicated(Bid == min(Bid))][1], 
#        +       DateTime.Ask.MN = DateTime[!duplicated(Ask == min(Ask))][1], 
#        +       DateTime.Bid.MX = DateTime[!duplicated(Bid == max(Bid))][1], 
#        +       DateTime.Bid.MN = DateTime[!duplicated(Bid == min(Bid))][1], 
#        +       Bid.MN = min(Bid), Ask.MN = min(Ask), 
#        +       Bid.MX = min(Bid), Ask.MX = min(Ask))
#         Date     DateTime.Bid.MN     DateTime.Ask.MN     DateTime.Bid.MX  Bid.MN  Ask.MN  Bid.MX  Ask.MX
#1  2018-06-17 2018-06-17 21:00:02 2018-06-17 21:00:02 2018-06-17 21:00:02 1.15862 1.15898 1.15862 1.15898
#2  2018-06-18 2018-06-18 00:00:00 2018-06-18 00:00:00 2018-06-18 00:00:00 1.15653 1.15654 1.15653 1.15654
#3  2018-06-19 2018-06-19 00:00:00 2018-06-19 00:00:00 2018-06-19 00:00:00 1.15306 1.15306 1.15306 1.15306
#4  2018-06-20 2018-06-20 00:00:00 2018-06-20 00:00:00 2018-06-20 00:00:00 1.15362 1.15370 1.15362 1.15370
#5  2018-06-21 2018-06-21 00:00:00 2018-06-21 00:00:00 2018-06-21 00:00:00 1.15080 1.15083 1.15080 1.15083
#6  2018-06-24 2018-06-24 21:00:12 2018-06-24 21:00:12 2018-06-24 21:00:12 1.16569 1.16603 1.16569 1.16603
#7  2018-06-25 2018-06-25 00:00:00 2018-06-25 00:00:00 2018-06-25 00:00:00 1.16284 1.16285 1.16284 1.16285
#8  2018-06-26 2018-06-26 00:00:00 2018-06-26 00:00:00 2018-06-26 00:00:00 1.16348 1.16349 1.16348 1.16349
#9  2018-06-27 2018-06-27 00:00:00 2018-06-27 00:00:00 2018-06-27 00:00:00 1.15405 1.15406 1.15405 1.15406
#10 2018-07-01 2018-07-01 21:00:15 2018-07-01 21:00:15 2018-07-01 21:00:15 1.16604 1.16643 1.16604 1.16643
#11 2018-07-02 2018-07-02 00:00:00 2018-07-02 00:00:00 2018-07-02 00:00:00 1.15914 1.15913 1.15914 1.15913
#12 2018-07-03 2018-07-03 00:00:00 2018-07-03 00:00:00 2018-07-03 00:00:00 1.16205 1.16206 1.16205 1.16206
#13 2018-07-04 2018-07-04 00:00:00 2018-07-04 00:00:00 2018-07-04 00:00:00 1.16306 1.16307 1.16306 1.16307
#14 2018-07-05 2018-07-05 00:00:00 2018-07-05 00:00:00 2018-07-05 00:00:00 1.16498 1.16499 1.16498 1.16499
#> 
#  > ddply(WT, .(Date), summarize,
#          +       DateTime.Bid.MN = DateTime[!duplicated(Bid == min(Bid))][2], 
#          +       DateTime.Ask.MN = DateTime[!duplicated(Ask == min(Ask))][2], 
#          +       DateTime.Bid.MX = DateTime[!duplicated(Bid == max(Bid))][2], 
#          +       DateTime.Bid.MN = DateTime[!duplicated(Bid == min(Bid))][2], 
#          +       Bid.MN = min(Bid), Ask.MN = min(Ask), 
#          +       Bid.MX = min(Bid), Ask.MX = min(Ask))
#         Date     DateTime.Bid.MN     DateTime.Ask.MN     DateTime.Bid.MX  Bid.MN  Ask.MN  Bid.MX  Ask.MX
#1  2018-06-17 2018-06-17 21:00:07 2018-06-17 21:05:07 2018-06-17 21:19:37 1.15862 1.15898 1.15862 1.15898
#2  2018-06-18 2018-06-18 07:17:13 2018-06-18 07:17:13 2018-06-18 23:42:05 1.15653 1.15654 1.15653 1.15654
#3  2018-06-19 2018-06-19 09:21:00 2018-06-19 09:21:00 2018-06-19 03:29:08 1.15306 1.15306 1.15306 1.15306
#4  2018-06-20 2018-06-20 08:08:23 2018-06-20 08:08:26 2018-06-20 13:17:44 1.15362 1.15370 1.15362 1.15370
#5  2018-06-21 2018-06-21 09:15:06 2018-06-21 09:15:06 2018-06-21 00:55:00 1.15080 1.15083 1.15080 1.15083
#6  2018-06-24 2018-06-24 21:22:53 2018-06-24 23:59:08 2018-06-24 23:15:40 1.16569 1.16603 1.16569 1.16603
#7  2018-06-25 2018-06-25 07:00:12 2018-06-25 07:00:12 2018-06-25 18:45:39 1.16284 1.16285 1.16284 1.16285
#8  2018-06-26 2018-06-26 17:20:58 2018-06-26 17:20:58 2018-06-26 03:02:16 1.16348 1.16349 1.16348 1.16349
#9  2018-06-27 2018-06-27 19:16:41 2018-06-27 19:16:41 2018-06-27 07:27:11 1.15405 1.15406 1.15405 1.15406
#10 2018-07-01 2018-07-01 21:36:08 2018-07-01 21:35:43 2018-07-01 21:04:42 1.16604 1.16643 1.16604 1.16643
#11 2018-07-02 2018-07-02 15:16:01 2018-07-02 15:16:01 2018-07-02 00:00:28 1.15914 1.15913 1.15914 1.15913
#12 2018-07-03 2018-07-03 01:45:58 2018-07-03 01:45:58 2018-07-03 08:19:10 1.16205 1.16206 1.16205 1.16206
#13 2018-07-04 2018-07-04 09:46:45 2018-07-04 09:43:02 2018-07-04 06:59:32 1.16306 1.16307 1.16306 1.16307
#14 2018-07-05 2018-07-05 05:11:55 2018-07-05 05:11:55 2018-07-05 12:22:30 1.16498 1.16499 1.16498 1.16499

WT %>% 
  group_by(Date) %>% 
  filter(Ask == min(Ask)|Ask == max(Ask), Bid == min(Bid)|Bid == max(Bid))

ddply(WT, .(Date), summarize, 
      Ask.MN = min(Ask), Bid.MN = min(Bid), 
      Ask.MX = max(Ask), Bid.MX = max(Bid))

WTM <- ddply(WT, .(Date), summarize, 
      Bid.MN = min(Bid), Ask.MN = min(Ask), 
      Bid.MX = min(Bid), Ask.MX = min(Ask), 
      DateTime.Bid.MN = DateTime[Bid.MN], 
      DateTime.Ask.MN = DateTime[Ask.MN], 
      DateTime.Bid.MX = DateTime[Bid.MX], 
      DateTime.Ask.MX = DateTime[Ask.MX])

WTN <- WT[(WT$Bid %in% WTM$Bid.MN)|
          (WT$Ask %in% WTM$Ask.MN)|
          (WT$Bid %in% WTM$Bid.MX)|
          (WT$Ask %in% WTM$Ask.MX),]

WTN %<>% mutate(Date = as.Date(DateTime))
WTN <- WTN[!duplicated(WTN),]
WTN <- WTN[!duplicated(WTN$Date) | !duplicated(WTN$Bid) | !duplicated(WTN$Ask),]


ldply(split(WT, WT$Date), function(x) {
  x %>% dplyr::filter(Ask == min(Ask)|Bid == min(Bid)|Ask == max(Ask)|Bid == max(Bid))
  }) %>% tbl_df

#WT %>% dplyr::filter(Date == unique(Date), Ask == min(Ask)|Bid == min(Bid)|Ask == max(Ask)|Bid == max(Bid))

llply(split(WT, WT$Date), function(x) {
  y1 = data.frame(DateTime.Bid.Min = x[x$Bid == min(x$Bid),]$DateTime, Bid.Min = min(x$Bid)) 
  y2 = data.frame(DateTime.Ask.Min = x[x$Ask == min(x$Ask),]$DateTime, Ask.Min = min(x$Ask))
  y3 = data.frame(DateTime.Bid.Max = x[x$Bid == max(x$Bid),]$DateTime, Bid.Max = max(x$Bid)) 
  y4 = data.frame(DateTime.Ask.Max = x[x$Ask == max(x$Ask),]$DateTime, Ask.Max = max(x$Ask))
  do.call(rbind, list(y1, y2, y3, y4))
  })

## split into few datasets.
y1 <- llply(split(WT, WT$Date), function(x) {
  data.frame(DateTime.Bid.Min = x[x$Bid == min(x$Bid),]$DateTime, Bid.Min = min(x$Bid))
  })
  
y2 <- llply(split(WT, WT$Date), function(x) {
  data.frame(DateTime.Ask.Min = x[x$Ask == min(x$Ask),]$DateTime, Ask.Min = min(x$Ask))
  })

y3 <- llply(split(WT, WT$Date), function(x) {
  data.frame(DateTime.Bid.Max = x[x$Bid == max(x$Bid),]$DateTime, Bid.Max = max(x$Bid)) 
  })

y4 <- llply(split(WT, WT$Date), function(x) {
  data.frame(DateTime.Ask.Max = x[x$Ask == max(x$Ask),]$DateTime, Ask.Max = max(x$Ask))
  })



