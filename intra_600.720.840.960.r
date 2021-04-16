#pth <- 'C:/Users/User/Documents/GitHub/binary.com-interview-question-data/data/fx/USDJPY/intraday'
pth <- 'C:/Users/User/Desktop/intraday'
fls <- list.files(pth, pattern = '^ts_ets_MNN_960_1.p_[0-9]{0,}.[0-9]{4}|^ts_ets_MNN_840_1.p_[0-9]{0,}.[0-9]{4}|^ts_ets_MNN_720_1.p_[0-9]{0,}.[0-9]{4}|^ts_ets_MNN_600_1.p_[0-9]{0,}.[0-9]{4}')
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

saveRDS(mds_ets_MNN_intraday2_1min, paste0(.dtr, 'intra_600_720_840_960.rds'))




### -------------------------------------------------

require('data.table')
mds_ets_MNN_intraday2_1min <- readRDS(paste0(.dtr, 'data/fx/USDJPY/mds_ets_MNN_intraday2_1min.rds')) %>% as.data.table
mds_ets_MNN_intraday2_1min <- mds_ets_MNN_intraday2_1min[order(index)]

llply(split(mds_ets_MNN_intraday2_1min, mds_ets_MNN_intraday2_1min$data_min), function(x) { as.data.table(x) })

$`600`
                     index t_series model sub_model data_min fc_min part       date mk.price fc.price
    1: 2016-01-04 00:02:00       ts   ets       MNN      600      1    1 2016-01-04 120.2200 120.2104
    2: 2016-01-04 00:03:00       ts   ets       MNN      600      1    2 2016-01-04 120.2200 120.2200
    3: 2016-01-04 00:04:00       ts   ets       MNN      600      1    3 2016-01-04 120.2185 120.2200
    4: 2016-01-04 00:05:00       ts   ets       MNN      600      1    4 2016-01-04 120.2195 120.2185
    5: 2016-01-04 00:06:00       ts   ets       MNN      600      1    5 2016-01-04 120.2250 120.2195
   ---                                                                                               
44634: 2016-02-15 23:56:00       ts   ets       MNN      600      1 1436 2016-02-15 114.6015 114.5930
44635: 2016-02-15 23:57:00       ts   ets       MNN      600      1 1437 2016-02-15 114.5940 114.6015
44636: 2016-02-15 23:58:00       ts   ets       MNN      600      1 1438 2016-02-15 114.6050 114.5940
44637: 2016-02-15 23:59:00       ts   ets       MNN      600      1 1439 2016-02-15 114.5950 114.6050
44638: 2016-02-16 00:00:00       ts   ets       MNN      600      1 1440 2016-02-16 114.5960 114.5950

$`720`
                     index t_series model sub_model data_min fc_min part       date mk.price fc.price
    1: 2016-01-04 00:02:00       ts   ets       MNN      720      1    1 2016-01-04 120.2200 120.2104
    2: 2016-01-04 00:03:00       ts   ets       MNN      720      1    2 2016-01-04 120.2200 120.2200
    3: 2016-01-04 00:04:00       ts   ets       MNN      720      1    3 2016-01-04 120.2185 120.2200
    4: 2016-01-04 00:05:00       ts   ets       MNN      720      1    4 2016-01-04 120.2195 120.2185
    5: 2016-01-04 00:06:00       ts   ets       MNN      720      1    5 2016-01-04 120.2250 120.2195
   ---                                                                                               
68128: 2016-04-04 21:49:00       ts   ets       MNN      720      1 1309 2016-04-04 111.2390 111.2450
68129: 2016-04-04 21:50:00       ts   ets       MNN      720      1 1310 2016-04-04 111.2455 111.2390
68130: 2016-04-04 21:51:00       ts   ets       MNN      720      1 1311 2016-04-04 111.2510 111.2455
68131: 2016-04-04 21:52:00       ts   ets       MNN      720      1 1312 2016-04-04 111.2435 111.2510
68132: 2016-04-04 21:53:00       ts   ets       MNN      720      1 1313 2016-04-04 111.2505 111.2435

$`840`
                     index t_series model sub_model data_min fc_min part       date mk.price fc.price
    1: 2016-01-04 00:02:00       ts   ets       MNN      840      1    1 2016-01-04 120.2200 120.2104
    2: 2016-01-04 00:03:00       ts   ets       MNN      840      1    2 2016-01-04 120.2200 120.2200
    3: 2016-01-04 00:04:00       ts   ets       MNN      840      1    3 2016-01-04 120.2185 120.2200
    4: 2016-01-04 00:05:00       ts   ets       MNN      840      1    4 2016-01-04 120.2195 120.2185
    5: 2016-01-04 00:06:00       ts   ets       MNN      840      1    5 2016-01-04 120.2250 120.2195
   ---                                                                                               
45802: 2016-02-15 19:25:00       ts   ets       MNN      840      1 1164 2016-02-15 114.6500 114.6445
45803: 2016-02-15 19:26:00       ts   ets       MNN      840      1 1165 2016-02-15 114.6515 114.6500
45804: 2016-02-15 19:27:00       ts   ets       MNN      840      1 1166 2016-02-15 114.6575 114.6515
45805: 2016-02-15 19:28:00       ts   ets       MNN      840      1 1167 2016-02-15 114.6675 114.6575
45806: 2016-02-15 19:29:00       ts   ets       MNN      840      1 1168 2016-02-15 114.6675 114.6675

$`960`
                     index t_series model sub_model data_min fc_min part       date mk.price fc.price
    1: 2016-01-04 00:02:00       ts   ets       MNN      960      1    1 2016-01-04 120.2200 120.2104
    2: 2016-01-04 00:03:00       ts   ets       MNN      960      1    2 2016-01-04 120.2200 120.2200
    3: 2016-01-04 00:04:00       ts   ets       MNN      960      1    3 2016-01-04 120.2185 120.2200
    4: 2016-01-04 00:05:00       ts   ets       MNN      960      1    4 2016-01-04 120.2195 120.2185
    5: 2016-01-04 00:06:00       ts   ets       MNN      960      1    5 2016-01-04 120.2250 120.2195
   ---                                                                                               
44634: 2016-02-15 23:57:00       ts   ets       MNN      960      1 1436 2016-02-15 114.5940 114.6015
44635: 2016-02-15 23:58:00       ts   ets       MNN      960      1 1437 2016-02-15 114.6050 114.5940
44636: 2016-02-15 23:59:00       ts   ets       MNN      960      1 1438 2016-02-15 114.5950 114.6050
44637: 2016-02-15 23:59:00       ts   ets       MNN      960      1 1439 2016-02-15 114.5950 114.6050
44638: 2016-02-16 00:01:00       ts   ets       MNN      960      1 1440 2016-02-16 114.6040 114.5960

ldply(split(mds_ets_MNN_intraday2_1min_1y, mds_ets_MNN_intraday2_1min_1y$data_min), function(x) { unique(as.data.table(x), by = c('index')) }) %>% as.data.table



