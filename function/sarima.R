sarima <- function(timeID, data = dsmp, data_len, hrz1 = 1440, 
                   .model = c('auto', 'Arima'), .d = NA, .D = NA, 
                   .order = c(0, 0, 0), .seasonal = c(0, 0, 0)) {
  
  tmp <- llply(1:length(timeID), function(i) {
    if(i == 1) {
      
      cat('\n')
      cat('===========================================\n')
      cat('train[', i, ']\n')
      print(train <- dsmp[date < timeID[i]][(.N - (data_len - 1)):.N])
      ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
      
      cat('\n')
      cat('-------------------------------------------\n')
      cat('train_test[', i, ']\n')
      
      print(train_test <- dsmp[sq %in% ctr])
      
      srm <- train[, .(index, close)] %>% 
        tk_ts(frequency = hrz1)
      
      if(.model == 'auto') {
        srm %<>% auto.arima(d = .d, D = .D)
        
      } else if (.model == 'Arima') {
        srm %<>% Arima(order = .order, seasonal = .seasonal)
      }
      
      srm %<>% 
        forecast(h = hrz1) %>% 
        tk_tbl %>% 
        dplyr::mutate(index = train_test[(.N - hrz1 + 1):.N,]$index, 
                      mk.price = train_test[(.N - hrz1 + 1):.N,]$close) %>% 
        dplyr::rename(fc.price = `Point Forecast`) %>% 
        dplyr::select(index, mk.price, fc.price)
      
      cat('\n')
      cat('-------------------------------------------\n')
      cat('forecast[', i, ']\n')
      
      print(srm %>% as.data.table)
      
      saveRDS(srm, paste0(
        dtr, 'data/fx/USDJPY/sarima_', .model, '_', data_len, 
        '_', hrz1, '.', as_date(srm$index[1]), '.rds'))
      
      cat('\n')
      cat(i, '=', paste0('~/data/fx/USDJPY/sarima_', .model, '_', 
                         data_len, '_', hrz1, '.', 
                         as_date(srm$index[1]), '.rds saved!\n'))
      cat('\n\n')
      rm(sets)
      
    } else if(i %in% seq(1, length(timeID), by = 6)[-1]) {
      
      
    } else if(i == length(timeID)) {
      
      
    } else  {
      
      lst_sq <- dsmp[date < timeID[i],][.N]$sq + 1
      
      cat('\n')
      cat('===========================================\n')
      cat('train[', i, ']\n')
      
      print(train <- dsmp[(lst_sq - data_len + 1):lst_sq])
      ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
      
      cat('\n')
      cat('-------------------------------------------\n')
      cat('train_test[', i, ']\n')
      
      print(train_test <- dsmp[sq %in% ctr])
      
      srm <- train[, .(index, close)] %>% 
        tk_ts(frequency = hrz1)
      
      if(.model == 'auto') {
        srm %<>% auto.arima(d = .d, D = .D)
        
      } else if (.model == 'Arima') {
        srm %<>% Arima(order = .order, seasonal = .seasonal)
      }
      
      srm %<>% 
        forecast(h = hrz1) %>% 
        tk_tbl %>% 
        dplyr::mutate(index = train_test[(.N - hrz1 + 1):.N,]$index, 
                      mk.price = train_test[(.N - hrz1 + 1):.N,]$close) %>% 
        dplyr::rename(fc.price = `Point Forecast`) %>% 
        dplyr::select(index, mk.price, fc.price)
      
      cat('\n')
      cat('-------------------------------------------\n')
      cat('forecast[', i, ']\n')
      
      print(srm %>% as.data.table)
      
      saveRDS(srm, paste0(
        dtr, 'data/fx/USDJPY/sarima_', .model, '_', data_len, 
        '_', hrz1, '.', as_date(srm$index[1]), '.rds'))
      
      cat('\n')
      cat(i, '=', paste0('~/data/fx/USDJPY/sarima_', .model, '_', 
                         data_len, '_', hrz1, '.', 
                         as_date(srm$index[1]), '.rds saved!\n'))
      cat('\n\n')
      rm(srm)
    }
  })
  return(tmp)
}
