#pth <- 'C:/Users/User/Documents/GitHub/binary.com-interview-question-data/data/fx/USDJPY/intraday'
pth <- 'C:/Users/User/Desktop/intraday'
fls <- list.files(pth, pattern = 'ts_ets_MNN_840_1.p_')
fls <- sort(fls)
#ts_ets_MNN_960_1.p_238.2016-10-19
#ts_ets_MNN_840_1.p_912.2016-10-18
#ts_ets_MNN_720_1.p_286.2016-10-14
#ts_ets_MNN_600_1.p_774.2016-09-08

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

saveRDS(mds_ets_MNN_intraday2_1min, paste0(.dtr, 'intra_840.rds'))




### -------------------------------------------------

require('data.table')
mds_ets_MNN_intraday2_1min <- readRDS(paste0(.dtr, 'data/fx/USDJPY/mds_ets_MNN_intraday2_1min.rds')) %>% as.data.table
mds_ets_MNN_intraday2_1min <- mds_ets_MNN_intraday2_1min[order(index)]

llply(split(mds_ets_MNN_intraday2_1min, mds_ets_MNN_intraday2_1min$data_min), function(x) { as.data.table(x) })


