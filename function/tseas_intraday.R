tseas_intraday <- function(timeID, data = dsmp, data_len, 
                  hrz1 = c(1440, 7200), hrz2 = 720, .model, vb = TRUE) {
  
  if(hrz1 <= 1440) {
    intr <- data_len/hrz1
    
    tmp <- llply(1:length(timeID), function(i) {
      if(i == 1) {
        
        tmp2 <-llply(1:intr, function(j) {
          
          if(j == 1) {
            train <- dsmp[date < timeID[i]][(.N - (data_len - 1)):.N]
          } else {
            lst_sq <- dsmp[date < timeID[i]][(.N - (hrz1 - 1))]$sq
            train <- dsmp[lst_sq:(lst_sq + data_len - 1)]
          }
          ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
          
          if(vb == TRUE) {
            cat('\n===========================================\n')
            cat('train[', i, '-', j, ']\n')
            print(train)
          }
          
          train_test <- dsmp[sq %in% ctr]
          if(vb == TRUE) {
            cat('\n-------------------------------------------\n')
            cat('train_test[', i, '-', j, ']\n')
            print(train_test)
          }
          
          sets <- train[, .(index, close)] %>% 
            tk_ts(frequency = hrz1) %>% 
            ets(model = .model) %>% 
            forecast(h = hrz1) %>% 
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
          
          fl_pth <- paste0(.dtr, 'data/fx/USDJPY/ts_ets_', .model, '_', 
                           data_len, '_', hrz1, '.p', j, '.', 
                           as_date(sets$index[1]), '.rds')
          saveRDS(sets, fl_pth)
          
          cat('\n', i, '-', j, '=', 
              paste0('~/data/fx/USDJPY/ts_ets_', .model, '_', 
                     data_len, '_', hrz1, '.p', j, '.', 
                     as_date(sets$index[1]), '.rds saved!'))
          cat('\n\n')
          rm(sets)
        })
        
     } else if(i %in% seq(1, length(timeID), by = 6)[-1]) {
        
        
     } else if(i == length(timeID)) {
        
        
     } else  {
       
        tmp2 <-llply(1:intr, function(j) {
          
          if(j == 1) {
            lst_sq <- dsmp[date < timeID[i],][.N]$sq + 1
          } else {
            lst_sq <- dsmp[date < timeID[i],][.N]$sq + 1 + hrz1
          }
          
          train <- dsmp[(lst_sq - data_len + 1):lst_sq]
          ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
          
          if(vb == TRUE) {
            cat('\n===========================================\n')
            cat('train[', i, '-', j, ']\n')
            print(train)
          }
          
          train_test <- dsmp[sq %in% ctr]
          if(vb == TRUE) {
            cat('\n-------------------------------------------\n')
            cat('train_test[', i, '-', j, ']\n')
            print(train_test)
          }
          
          sets <- train[, .(index, close)] %>% 
            tk_ts(frequency = hrz1) %>% 
            ets(model = .model) %>% 
            forecast(h = hrz1) %>% 
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
          
          fl_pth <- paste0(.dtr, 'data/fx/USDJPY/ts_ets_', .model, '_', 
                           data_len, '_', hrz1, '.p', j, '.', 
                           as_date(sets$index[1]), '.rds')
          saveRDS(sets, fl_pth)
          
          cat('\n', i, '-', j, '=', 
              paste0('~/data/fx/USDJPY/ts_ets_', .model, '_', 
                     data_len, '_', hrz1, '.p', j, '.', 
                     as_date(sets$index[1]), '.rds saved!'))
          cat('\n\n')
          rm(sets)
       })
     }
    })
  } else if(hrz1 == 7200) {
    
    tmp <- llply(1:length(timeID), function(i) {
      
      if(i == 1) {
        
        cat('\n===========================================\n')
        cat('train[', i, ']\n')
        print(train <- dsmp[date < timeID[i]][(.N - (data_len - 1)):.N])
        ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
        
        cat('\n-------------------------------------------\n')
        cat('train_test[', i, ']\n')
        print(train_test <- dsmp[sq %in% ctr])
        
        sets <- train[, .(index, close)] %>% 
          tk_ts(frequency = hrz1) %>% 
          ets(model = .model) %>% 
          forecast(h = hrz1) %>% 
          tk_tbl %>% 
          dplyr::mutate(index = train_test[(.N - hrz1 + 1):.N,]$index, 
                        mk.price = train_test[(.N - hrz1 + 1):.N,]$close) %>% 
          dplyr::rename(fc.price = `Point Forecast`) %>% 
          dplyr::select(index, mk.price, fc.price)
        
        cat('\n-------------------------------------------\n')
        cat('forecast[', i, ']\n')
        print(sets %>% as.data.table)
        
        fl_pth <- paste0(.dtr, 'data/fx/USDJPY/ts_ets_', data_len, 
                         '_', hrz1, '.', as_date(sets$index[1]), '.rds')
        saveRDS(sets, fl_pth)
        
        cat('\n', i, '=', paste0('~/data/fx/USDJPY/ts_ets_', .model, '_', 
                                 data_len, '_', hrz1, '.', 
                                 as_date(sets$index[1]), '.rds saved!'))
        cat('\n\n')
        rm(sets)
        
      } else if(i > (length(timeID) - hrz1/hrz2) & i != length(timeID)) {
        
        lst_sq <- dsmp[date < timeID[i],][.N]$sq + 1
        ## filter the length of forecasted data to fit with train_test data 
        ##   when the length of forecasted data more then length of test data.
        #lst_date <- timeID[(length(timeID) - (hrz1/hrz2)):length(timeID)]
        lst_date <- timeID[timeID >= timeID[i]]
        lst_date_sq <- grep(
          timeID[i], timeID[
            (length(timeID) - (hrz1/hrz2 - 1)):length(timeID)])
        
        cat('\n===========================================\n')
        cat('train[', i, ']\n')
        print(train <- dsmp[(lst_sq - data_len + 1):lst_sq])
        ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
        
        cat('\n-------------------------------------------\n')
        cat('train_test[', i, ']\n')
        print(train_test <- dsmp[sq %in% ctr])
        
        sets <- train[, .(index, close)] %>% 
          tk_ts(frequency = hrz1) %>% 
          ets(model = .model) %>% 
          forecast(h = hrz1) %>% 
          tk_tbl
        
        sets <- sets[1:(hrz1 - (hrz2 * lst_date_sq)),] %>% 
          dplyr::mutate(index = train_test[
            (.N - (hrz1 - (hrz2 * lst_date_sq)) + 1):.N, ]$index, 
            mk.price = train_test[
              (.N - (hrz1 - (hrz2 * lst_date_sq)) + 1):.N, ]$close) %>% 
          dplyr::rename(fc.price = `Point Forecast`) %>% 
          dplyr::select(index, mk.price, fc.price)
        
        cat('\n-------------------------------------------\n')
        cat('forecast[', i, ']\n')
        print(sets %>% as.data.table)
        
        fl_pth <- paste0(.dtr, 'data/fx/USDJPY/ts_ets_', data_len, 
                         '_', hrz1, '.', as_date(sets$index[1]), '.rds')
        saveRDS(sets, fl_pth)
        
        cat('\n', i, '=', paste0('~/data/fx/USDJPY/ts_ets_', .model, '_', 
                                 data_len, '_', hrz1, '.', 
                                 as_date(sets$index[1]), '.rds saved!'))
        cat('\n\n')
        rm(sets)
        
      } else if(i %in% seq(1, length(timeID), by = 6)[-1]) {
        
        
      } else if(i == length(timeID)) {
        
        
      } else {
        
        lst_sq <- dsmp[date < timeID[i],][.N]$sq + 1
        
        cat('\n===========================================\n')
        cat('train[', i, ']\n')
        print(train <- dsmp[(lst_sq - data_len + 1):lst_sq])
        ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
        
        cat('\n-------------------------------------------\n')
        cat('train_test[', i, ']\n')
        print(train_test <- dsmp[sq %in% ctr])
        
        sets <- train[, .(index, close)] %>% 
          tk_ts(frequency = hrz1) %>% 
          ets(model = .model) %>% 
          forecast(h = hrz1) %>% 
          tk_tbl %>% 
          dplyr::mutate(index = train_test[(.N - hrz1 + 1):.N,]$index, 
                        mk.price = train_test[(.N - hrz1 + 1):.N,]$close) %>% 
          dplyr::rename(fc.price = `Point Forecast`) %>% 
          dplyr::select(index, mk.price, fc.price)
        
        cat('\n-------------------------------------------\n')
        cat('forecast[', i, ']\n')
        print(sets %>% as.data.table)
        
        fl_pth <- paste0(.dtr, 'data/fx/USDJPY/ts_ets_', data_len, 
                         '_', hrz1, '.', as_date(sets$index[1]), '.rds')
        saveRDS(sets, fl_pth)
        
        cat('\n', i, '=', paste0('~/data/fx/USDJPY/ts_ets_', .model, '_', 
                                 data_len, '_', hrz1, '.', 
                                 as_date(sets$index[1]), '.rds saved!'))
        cat('\n\n')
        rm(sets)
      }
    })
    
  } else {
    
    
  }
  return(tmp)
}
