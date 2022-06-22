sarima <- function(时间索引, 样本, 路径, 数据量, 预测时间单位 = 1440, 
                   .统计模型 = .统计模型, .d = .d, .D = .D, 
                   seas = seas, .order = .order, .seas = .seas, 
                   stationary = stationary, trace = trace, 
                   ic = ic, stepwise = stepwise, 多元统计模型 = 多元统计模型, 
                   #approximation = (length(x) > 150 | frequency(x) > 12), 
                   truncate = truncate, #x = y, method = NULL, 
                   xreg = xreg, test = test, test.args = test.args, 
                   seasonal.test = seasonal.test, 
                   seasonal.test.args = seasonal.test.args, 
                   allowdrift = allowdrift, allowmean = allowmean, 
                   lambda = lambda, biasadj = biasadj, parallel = parallel, 
                   num.cores = num.cores, include.mean = include.mean, 
                   #xreg = NULL, include.constant, 
                   include.drift = include.drift, 统计模型 = 统计模型, 
                   #lambda = 统计模型$lambda, x = y, biasadj = FALSE, 
                   method = method) {
  
  conflict_prefer('mutate', 'dplyr')
  
  tmp <- llply(1:length(时间索引), function(i) {
    
    if(i == 1) {
      
      cat('\n===========================================\n')
      cat('培训数据[', i, ']\n')
      print(培训数据 <- 样本[日期 < 时间索引[i]][(.N - (数据量 - 1)):.N])
      ctr <- 培训数据$序列[1]:(range(培训数据$序列)[2] + 预测时间单位)
      
      cat('\n-------------------------------------------\n')
      cat('培训数据_测试数据[', i, ']\n')
      print(培训数据_测试数据 <- 样本[序列 %in% ctr])
      
      srm <- 培训数据[, .(年月日时分, close)] %>% 
        tk_ts(frequency = 预测时间单位)
      
      if(.统计模型 == 'auto') {
        
        srm <- tryCatch({
          auto.arima(srm, d = .d, D = .D, seasonal = seas, 
                     stationary = stationary, trace = trace, 
                     ic = ic, stepwise = stepwise, 多元统计模型 = nmodels, 
                     #approximation = approximation, 
                     method = method, truncate = truncate, #x = y, 
                     xreg = xreg, test = test, lambda = lambda, 
                     test.args = test.args, allowdrift = allowdrift, 
                     seasonal.test = seasonal.test, 
                     seasonal.test.args = seasonal.test.args, 
                     allowmean = allowmean, biasadj = biasadj, 
                     parallel = parallel, num.cores = num.cores)
        }, error = function(err) NULL)
        
      } else if (.统计模型 == 'Arima') {
        
        srm <- tryCatch({
          Arima(srm, order = .order, seasonal = .seas, 
                xreg = xreg, include.mean = include.mean, 
                include.drift = include.drift, 
                #include.constant = include.constant, 
                #统计模型 = 统计模型, lambda = lambda, x = y, 
                biasadj = biasadj, method = method)
          }, error = function(err) NULL)
        
      } else {
        
      }
      
      if(!is.null(srm)) {
        srm %<>% 
          forecast::forecast(h = 预测时间单位) %>% 
          tk_tbl %>% 
          mutate(年月日时分 = 培训数据_测试数据[(.N - 预测时间单位 + 1):.N,]$年月日时分, 
                        mk.price = 培训数据_测试数据[(.N - 预测时间单位 + 1):.N,]$close) %>% 
          rename(fc.price = `Point Forecast`) %>% 
          select(年月日时分, mk.price, fc.price)
        
        cat('\n-------------------------------------------\n')
        cat('forecast[', i, ']\n')
        print(srm %>% as.data.table)
      }
      
      if(.统计模型 == 'auto') {
        nms <- paste0(.统计模型, '_d', .d, 'D', .D, 
                      '_seas', seas, '_', 数据量, '_', 
                      预测时间单位, '.', as_date(srm$年月日时分[1]), '.rds')
        
      } else if (.统计模型 == 'Arima') {
        nms <- paste0(.统计模型, '_arma', 
          paste(.order, collapse = ''), '_seas', 
          paste(.seas, collapse = ''), '_', 数据量, '_', 
          预测时间单位, '.', as_date(srm$年月日时分[1]), '.rds')
        
      } else {
        
      }
      
      saveRDS(srm, paste0(
        路径, 'data/fx/USDJPY/sarima_', nms))
      cat('\n', i, '=', nms, '\n\n')
      rm(srm)
      
    } else if(i %in% seq(1, length(时间索引), by = 6)[-1]) {
      
      
    } else if(i == length(时间索引)) {
      
      
    } else  {
      
      最终序列 <- 样本[日期 < 时间索引[i],][.N]$序列 + 1
      
      cat('\n===========================================\n')
      cat('培训数据[', i, ']\n')
      print(培训数据 <- 样本[(最终序列 - 数据量 + 1):最终序列])
      ctr <- 培训数据$序列[1]:(range(培训数据$序列)[2] + 预测时间单位)
      
      cat('\n-------------------------------------------\n')
      cat('培训数据_测试数据[', i, ']\n')
      print(培训数据_测试数据 <- 样本[序列 %in% ctr])
      
      srm <- 培训数据[, .(年月日时分, close)] %>% 
        tk_ts(frequency = 预测时间单位)
      
      if(.统计模型 == 'auto') {
        
        srm <- tryCatch({
          auto.arima(srm, d = .d, D = .D, seasonal = seas, 
                     stationary = stationary, trace = trace, 
                     ic = ic, stepwise = stepwise, 多元统计模型 = nmodels, 
                     #approximation = approximation, 
                     method = method, truncate = truncate, #x = y, 
                     xreg = xreg, test = test, lambda = lambda, 
                     test.args = test.args, allowdrift = allowdrift, 
                     seasonal.test = seasonal.test, 
                     seasonal.test.args = seasonal.test.args, 
                     allowmean = allowmean, biasadj = biasadj, 
                     parallel = parallel, num.cores = num.cores)
        }, error = function(err) NULL)
        
      } else if (.统计模型 == 'Arima') {
        
        srm <- tryCatch({
          Arima(srm, order = .order, seasonal = .seas, 
                xreg = xreg, include.mean = include.mean, 
                include.drift = include.drift, 
                #include.constant = include.constant, 
                #统计模型 = 统计模型, lambda = lambda, x = y, 
                biasadj = biasadj, method = method)
        }, error = function(err) NULL)
        
      } else {
        
      }
      
      if(!is.null(srm)) {
        srm %<>% 
          forecast::forecast(h = 预测时间单位) %>% 
          tk_tbl %>% 
          mutate(年月日时分 = 培训数据_测试数据[(.N - 预测时间单位 + 1):.N,]$年月日时分, 
                        mk.price = 培训数据_测试数据[(.N - 预测时间单位 + 1):.N,]$close) %>% 
          rename(fc.price = `Point Forecast`) %>% 
          select(年月日时分, mk.price, fc.price)
        
        cat('\n-------------------------------------------\n')
        cat('forecast[', i, ']\n')
        print(srm %>% as.data.table)
      }
      
      if(.统计模型 == 'auto') {
        nms <- paste0(.统计模型, '_d', .d, 'D', .D, 
                      '_seas', seas, '_', 数据量, '_', 
                      预测时间单位, '.', as_date(srm$年月日时分[1]), '.rds')
        
      } else if (.统计模型 == 'Arima') {
        nms <- paste0(.统计模型, '_arma', 
                      paste(.order, collapse = ''), '_seas', 
                      paste(.seas, collapse = ''), '_', 数据量, '_', 
                      预测时间单位, '.', as_date(srm$年月日时分[1]), '.rds')
        
      } else {
        
      }
      
      saveRDS(srm, paste0(
        路径, 'data/fx/USDJPY/sarima_', nms))
      cat('\n', i, '=', nms, '\n\n')
      rm(srm)
    }
  })
  return(tmp)
}
