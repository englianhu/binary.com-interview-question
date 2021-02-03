sarima <- function(timeID, data = dsmp, data_len, hrz1 = 1440, 
                   .model = .model, .d = .d, .D = .D, 
                   seas = seas, .order = .order, .seas = .seas, 
                   stationary = stationary, trace = trace, 
                   ic = ic, stepwise = stepwise, nmodels = nmodels, 
                   #approximation = (length(x) > 150 | frequency(x) > 12), 
                   truncate = truncate, #x = y, method = NULL, 
                   xreg = xreg, test = test, test.args = test.args, 
                   seasonal.test = seasonal.test, 
                   seasonal.test.args = seasonal.test.args, 
                   allowdrift = allowdrift, allowmean = allowmean, 
                   lambda = lambda, biasadj = biasadj, parallel = parallel, 
                   num.cores = num.cores, include.mean = include.mean, 
                   #xreg = NULL, include.constant, 
                   include.drift = include.drift, model = model, 
                   #lambda = model$lambda, x = y, biasadj = FALSE, 
                   method = method) {
  
  tmp <- llply(1:length(timeID), function(i) {
    
    if(i == 1) {
      
      cat('\n===========================================\n')
      cat('train[', i, ']\n')
      print(train <- dsmp[date < timeID[i]][(.N - (data_len - 1)):.N])
      ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
      
      cat('\n-------------------------------------------\n')
      cat('train_test[', i, ']\n')
      print(train_test <- dsmp[sq %in% ctr])
      
      srm <- train[, .(index, close)] %>% 
        tk_ts(frequency = hrz1)
      
      if(.model == 'auto') {
        
        srm <- tryCatch({
          auto.arima(srm, d = .d, D = .D, seasonal = seas, 
                     stationary = stationary, trace = trace, 
                     ic = ic, stepwise = stepwise, nmodels = nmodels, 
                     #approximation = approximation, 
                     method = method, truncate = truncate, #x = y, 
                     xreg = xreg, test = test, lambda = lambda, 
                     test.args = test.args, allowdrift = allowdrift, 
                     seasonal.test = seasonal.test, 
                     seasonal.test.args = seasonal.test.args, 
                     allowmean = allowmean, biasadj = biasadj, 
                     parallel = parallel, num.cores = num.cores)
        }, error = function(err) NULL)
        
      } else if (.model == 'Arima') {
        
        srm <- tryCatch({
          Arima(srm, order = .order, seasonal = .seas, 
                xreg = xreg, include.mean = include.mean, 
                include.drift = include.drift, 
                #include.constant = include.constant, 
                #model = model, lambda = lambda, x = y, 
                biasadj = biasadj, method = method)
          }, error = function(err) NULL)
        
      } else {
        
      }
      
      if(!is.null(srm)) {
        srm %<>% 
          forecast::forecast(h = hrz1) %>% 
          tk_tbl %>% 
          dplyr::mutate(index = train_test[(.N - hrz1 + 1):.N,]$index, 
                        mk.price = train_test[(.N - hrz1 + 1):.N,]$close) %>% 
          dplyr::rename(fc.price = `Point Forecast`) %>% 
          dplyr::select(index, mk.price, fc.price)
        
        cat('\n-------------------------------------------\n')
        cat('forecast[', i, ']\n')
        print(srm %>% as.data.table)
      }
      
      if(.model == 'auto') {
        nms <- paste0(.model, '_d', .d, 'D', .D, 
                      '_seas', seas, '_', data_len, '_', 
                      hrz1, '.', as_date(srm$index[1]), '.rds')
        
      } else if (.model == 'Arima') {
        nms <- paste0(.model, '_arma', 
          paste(.order, collapse = ''), '_seas', 
          paste(.seas, collapse = ''), '_', data_len, '_', 
          hrz1, '.', as_date(srm$index[1]), '.rds')
        
      } else {
        
      }
      
      saveRDS(srm, paste0(
        dtr, 'data/fx/USDJPY/sarima_', nms))
      cat('\n', i, '=', nms, '\n\n')
      rm(srm)
      
    } else if(i %in% seq(1, length(timeID), by = 6)[-1]) {
      
      
    } else if(i == length(timeID)) {
      
      
    } else  {
      
      lst_sq <- dsmp[date < timeID[i],][.N]$sq + 1
      
      cat('\n===========================================\n')
      cat('train[', i, ']\n')
      print(train <- dsmp[(lst_sq - data_len + 1):lst_sq])
      ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
      
      cat('\n-------------------------------------------\n')
      cat('train_test[', i, ']\n')
      print(train_test <- dsmp[sq %in% ctr])
      
      srm <- train[, .(index, close)] %>% 
        tk_ts(frequency = hrz1)
      
      if(.model == 'auto') {
        
        srm <- tryCatch({
          auto.arima(srm, d = .d, D = .D, seasonal = seas, 
                     stationary = stationary, trace = trace, 
                     ic = ic, stepwise = stepwise, nmodels = nmodels, 
                     #approximation = approximation, 
                     method = method, truncate = truncate, #x = y, 
                     xreg = xreg, test = test, lambda = lambda, 
                     test.args = test.args, allowdrift = allowdrift, 
                     seasonal.test = seasonal.test, 
                     seasonal.test.args = seasonal.test.args, 
                     allowmean = allowmean, biasadj = biasadj, 
                     parallel = parallel, num.cores = num.cores)
        }, error = function(err) NULL)
        
      } else if (.model == 'Arima') {
        
        srm <- tryCatch({
          Arima(srm, order = .order, seasonal = .seas, 
                xreg = xreg, include.mean = include.mean, 
                include.drift = include.drift, 
                #include.constant = include.constant, 
                #model = model, lambda = lambda, x = y, 
                biasadj = biasadj, method = method)
        }, error = function(err) NULL)
        
      } else {
        
      }
      
      if(!is.null(srm)) {
        srm %<>% 
          forecast::forecast(h = hrz1) %>% 
          tk_tbl %>% 
          dplyr::mutate(index = train_test[(.N - hrz1 + 1):.N,]$index, 
                        mk.price = train_test[(.N - hrz1 + 1):.N,]$close) %>% 
          dplyr::rename(fc.price = `Point Forecast`) %>% 
          dplyr::select(index, mk.price, fc.price)
        
        cat('\n-------------------------------------------\n')
        cat('forecast[', i, ']\n')
        print(srm %>% as.data.table)
      }
      
      if(.model == 'auto') {
        nms <- paste0(.model, '_d', .d, 'D', .D, 
                      '_seas', seas, '_', data_len, '_', 
                      hrz1, '.', as_date(srm$index[1]), '.rds')
        
      } else if (.model == 'Arima') {
        nms <- paste0(.model, '_arma', 
                      paste(.order, collapse = ''), '_seas', 
                      paste(.seas, collapse = ''), '_', data_len, '_', 
                      hrz1, '.', as_date(srm$index[1]), '.rds')
        
      } else {
        
      }
      
      saveRDS(srm, paste0(
        dtr, 'data/fx/USDJPY/sarima_', nms))
      cat('\n', i, '=', nms, '\n\n')
      rm(srm)
    }
  })
  return(tmp)
}
