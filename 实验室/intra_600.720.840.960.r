#pth <- 'C:/Users/User/Documents/GitHub/binary.com-interview-question-data/data/fx/USDJPY/intraday'
pth <- 'C:/Users/User/Desktop/intraday_18_20'
fls <- list.files(pth, pattern = '^ts_ets_MNN_1080_1.p_[0-9]{0,}.[0-9]{4}|^ts_ets_MNN_1200_1.p_[0-9]{0,}.[0-9]{4}')
fls <- sort(fls)
#ts_ets_MNN_960_1.p_504.2017-01-03
#ts_ets_MNN_840_1.p_473.2017-01-03
#ts_ets_MNN_720_1.p_716.2017-01-03
#ts_ets_MNN_600_1.p_791.2016-01-10

#fls_pth <- paste0(.dtr, 'data/fx/USDJPY/intraday/', fls)
fls_pth <- paste0(pth, '/', fls)

mds_ets_MNN_intraday2_1min <- ldply(1:length(fls), function(i) {
  
  nms <- fls[i] %>% 
    str_replace_all('.rds', '') %>% 
    str_split('_|\\.') %>% 
    .[[1]]
  
  nms <- nms[-6]
  names(nms) <- c('t_series', 'model', 'sub_model', 
                  'data_min', 'fc_min', 'part', 'date')
  
  datset2 <- t(nms) %>% 
    data.frame %>% 
    mutate(model =factor(model), sub_model = factor(sub_model), 
           data_min = as.numeric(data_min), 
           fc_min = as.numeric(fc_min), 
           part = as.numeric(part), 
           date = as_date(date))
  
  datset <- tryCatch({
    read_rds(fls_pth[i])
  }, error = function(e) NULL)
  
  if(is.null(datset)){
     write.table(paste0('Read error:', i, '/', length(fls), '=', nms, '\n'), file = paste0('err_', i, '.txt'), sep = '\t')
     cat('Read error:', i, '/', length(fls), '=', nms, '\n')
  } else {
     res <- tibble(datset2, datset) %>% 
       dplyr::select(index, t_series, model, sub_model, data_min, 
                     fc_min, part, date, mk.price, fc.price)
     res <- res[!is.na('mk.price'),]
     cat(i, '/', length(fls), '=', nms, '\n')
	 return(res)
  }
}) %>% 
  as_tibble

## https://tysonbarrett.com/jekyll/update/2019/10/06/datatable_memory/
## http://brooksandrew.github.io/simpleblog/articles/advanced-data-table/
## https://atrebas.github.io/post/2019-03-03-datatable-dplyr/

saveRDS(mds_ets_MNN_intraday2_1min, paste0(.dtr, 'intra_1080_1200.rds'))


### -------------------------------------------------

require('data.table')
require('dplyr')
require('magrittr')

mds_ets_MNN_intraday3_1min_1y <- readRDS(paste0(.dtr, 'data/fx/USDJPY/mds_ets_MNN_intraday3_1min_1y.rds'))
mds_ets_MNN_intraday3_1min_1y %<>% as.data.table
mds_ets_MNN_intraday3_1min_1y <- mds_ets_MNN_intraday3_1min_1y[order(index)]
mds_ets_MNN_intraday3_1min_1y %>% unique

llply(split(mds_ets_MNN_intraday3_1min_1y, mds_ets_MNN_intraday3_1min_1y$data_min), function(x) { as.data.table(x) })

mds_ets_MNN_intraday3_1min_1y2 <- ldply(split(mds_ets_MNN_intraday3_1min_1y, mds_ets_MNN_intraday3_1min_1y$data_min), function(x) { 
   unique(as.data.table(x), by = c('index')) }) %>% as.data.table
#mds_ets_MNN_intraday3_1min_1y <- mds_ets_MNN_intraday3_1min_1y[date <= as_date('2017-01-01')]
#saveRDS(mds_ets_MNN_intraday3_1min_1y, paste0(.dtr, 'data/fx/USDJPY/mds_ets_MNN_intraday3_1min_1y.rds'))

smmp <- dsmp[374401:748800]

llply(split(mds_ets_MNN_intraday3_1min_1y, mds_ets_MNN_intraday3_1min_1y$data_min), function(x) { 
   x <- as.data.table(x)
   y <- smmp$index[!unique(smmp$index) %in% unique(x$index)]
   #c(head(y), tail(y))
   length(y)
   })
$`180`
[1] 105

$`240`
[1] 105

$`360`
[1] 133

$`480`
[1] 125

$`600`
[1] 13063

$`720`
[1] 26876

$`840`
[1] 53639

$`960`
[1] 51930

refill <- llply(split(mds_ets_MNN_intraday3_1min_1y, mds_ets_MNN_intraday3_1min_1y$data_min), function(x) { 
   x <- as.data.table(x)
   y <- smmp$index[!unique(smmp$index) %in% unique(x$index)]
   y
   })

dte <- refill %>% llply(., function(x) unique(as_date(x)))
#ts_ets_MNN_960_1.p_124.2016-12-28
#ts_ets_MNN_840_1.p_473.2016-12-20
#ts_ets_MNN_720_1.p_716.2016-15-05
#ts_ets_MNN_600_1.p_791.2016-11-29

#########################################################################
# --------- eval=FALSE ---------
source('function/intra_min.R')

smmp <- dsmp[374401:748800]
timeID <- unique(smmp$date)
bse <- smmp[year == 2016]$date[1] #"2016-01-04" #1st trading date in 2nd year
timeID %<>% .[. >= bse]
#timeID %<>% .[. >= as_date('2016-12-12')]
#timeID %<>% .[. <= as_date('2016-01-31')]
timeID <- dte$`600`
data_len <- 600
hrz1 <- 1
intr <- data_len/hrz1

llply(ets.m, function(md) {
  intra_min(timeID = timeID, smmp, fl_pth = 'C:/Users/User/Desktop/intraday_refill', 
		    data_len = data_len, hrz1 = hrz1, 
            .model = md, vb = FALSE)
  })

#########################################################################
# --------- eval=FALSE ---------
source('function/intra_min.R')

smmp <- dsmp[374401:748800]
timeID <- unique(smmp$date)
bse <- smmp[year == 2016]$date[1] #"2016-01-04" #1st trading date in 2nd year
timeID %<>% .[. >= bse]
#timeID %<>% .[. >= as_date('2016-12-12')]
#timeID %<>% .[. <= as_date('2016-01-31')]
timeID <- dte$`720`
data_len <- 720
hrz1 <- 1
intr <- data_len/hrz1

llply(ets.m, function(md) {
  intra_min(timeID = timeID, smmp, fl_pth = 'C:/Users/User/Desktop/intraday_refill', 
		    data_len = data_len, hrz1 = hrz1, 
            .model = md, vb = FALSE)
  })

#########################################################################
# --------- eval=FALSE ---------
source('function/intra_min.R')

smmp <- dsmp[374401:748800]
timeID <- unique(smmp$date)
bse <- smmp[year == 2016]$date[1] #"2016-01-04" #1st trading date in 2nd year
timeID %<>% .[. >= bse]
#timeID %<>% .[. >= as_date('2016-12-12')]
#timeID %<>% .[. <= as_date('2016-01-31')]
timeID <- dte$`840`
data_len <- 840
hrz1 <- 1
intr <- data_len/hrz1

llply(ets.m, function(md) {
  intra_min(timeID = timeID, smmp, fl_pth = 'C:/Users/User/Desktop/intraday_refill', 
		    data_len = data_len, hrz1 = hrz1, 
            .model = md, vb = FALSE)
  })

#########################################################################
# --------- eval=FALSE ---------
source('function/intra_min.R')

smmp <- dsmp[374401:748800]
timeID <- unique(smmp$date)
bse <- smmp[year == 2016]$date[1] #"2016-01-04" #1st trading date in 2nd year
timeID %<>% .[. >= bse]
#timeID %<>% .[. >= as_date('2016-12-12')]
#timeID %<>% .[. <= as_date('2016-01-31')]
timeID <- dte$`960`
data_len <- 960
hrz1 <- 1
intr <- data_len/hrz1

llply(ets.m, function(md) {
  intra_min(timeID = timeID, smmp, fl_pth = 'C:/Users/User/Desktop/intraday_refill', 
		    data_len = data_len, hrz1 = hrz1, 
            .model = md, vb = FALSE)
  })




