日内分计 <- function(时间索引, 样本 = 样本, 数据量, 路径 = NULL, 
                      hrz1 = 1, 预测时间单位 = 1, .model, vb = TRUE) {
  ## 数据量 240, 360, 480; hrz1 = 1
  intr <- 数据量/hrz1
  dy <- 1440
  
  tmp <- llply(1:length(时间索引), function(i) {
    
    tmp2 <- llply(1:1440, function(j) {
      
      if(j == 1) {
        lst_sq <- 样本[date < 时间索引[i],][.N]$sq + 1
        
      } else {
        lst_sq <- 样本[date < 时间索引[i],][.N]$sq + 1 + hrz1 * (j - 1)
      }
      
      train <- 样本[(lst_sq - 数据量 + 1):lst_sq]
      ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
      
      if(vb == TRUE) {
        cat('\n===========================================\n')
        cat('train[', i, '-', j, ']\n')
        print(train)
      }
      
      train_test <- 样本[sq %in% ctr]
      if(vb == TRUE) {
        cat('\n-------------------------------------------\n')
        cat('train_test[', i, '-', j, ']\n')
        print(train_test)
      }
      
      sets <- train[, .(index, close)] %>% 
        as_tibble %>% 
        tk_ts(frequency = hrz1) %>% 
        ets(model = .model) %>% 
        forecast(h = 预测时间单位) %>% 
        tk_tbl %>% 
        dplyr::mutate(index = train_test[(.N - hrz1 + 1):.N,]$index, 
                      mk.price = train_test[(.N - hrz1 + 1):.N,]$close) %>% 
        dplyr::rename(fc.price = `Point Forecast`) %>% 
        dplyr::select(index, mk.price, fc.price)
      
      if(vb == TRUE) {
        cat('\n-------------------------------------------\n')
        cat('forecast[', i, '-', j, ']\n')
        print(sets %>% as.data.table)
      }
      
      if(is.null(路径)) {
        路径 <- paste0(.dtr, '文艺数据库/fx/USDJPY/仓库/ts_ets_', .model, '_', 
                         数据量, '_', hrz1, '.p_', j, '.', 
                         as_date(sets$index[1]), '.rds')
      } else {
        路径 <- paste0(路径, '/ts_ets_', .model, '_', 
                         数据量, '_', hrz1, '.p_', j, '.', 
                         as_date(sets$index[1]), '.rds')
        saveRDS(sets, 路径)
        
        cat('\n', i, '-', j, '=', 
            paste0('~/文艺数据库/fx/USDJPY/仓库/ts_ets_', .model, '_', 
                   数据量, '_', hrz1, '.p_', j, '.', 
                   as_date(sets$index[1]), '.rds saved!'))
      }
      
      
      cat('\n\n')
      rm(sets)
      gc()
    })
    return(tmp2)
  })
  return(tmp)
}



