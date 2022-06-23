季节性自回归 <- function(时间索引, 样本, 路径, 数据量, 预测时间单位 = 1440, .统计模型 = .统计模型,
                   .阶数 = .阶数, .季节性差分 = .季节性差分, 季节性与否 = 季节性与否, 
                   .时序规律 = .时序规律, .季节性规律参数 = .季节性规律参数, 
                   静态与否 = 静态与否, 记载自回归与否 = 记载自回归与否, 
                   信息量准则 = 信息量准则, 逐步精化 = 逐步精化, 逐步精化量 = 逐步精化量, 
                   #approximation = (length(x) > 150 | frequency(x) > 12), 
                   缩写 = 缩写, #x = y, method = NULL, 
                   #代码中xreg作为 [公式] 。xreg可以指定多组相关序列，也就是说，动态回归就是多元回归。
                   #Dynamic Harmonic Regression
                   #使用FT（傅里叶）序列作为xreg。模型拟合时不指定seasonal，在预测时加入周期性的xreg（势头/趋势/气势）。
                   #https://zhuanlan.zhihu.com/p/29755934
                   趋势 = 趋势, 测试 = 测试, 测试参数 = 测试参数, 
                   测试季节性 = 测试季节性, 测试季节性参数 = 测试季节性参数, 
                   允许截距与否 = 允许截距与否, 允许包含均值与否 = 允许包含均值与否, 
                   博克斯考克斯变换 = 博克斯考克斯变换, 偏差调整与否 = 偏差调整与否, 多管其下 = 多管其下, 
                   核心量 = 核心量, 包含均值与否 = 包含均值与否, 
                   #xreg = NULL, include.constant, 
                   包含截距 = 包含截距, 统计模型 = 统计模型, 
                   #lambda = 统计模型$lambda, x = y, biasadj = FALSE, 
                   计策谋略 = 计策谋略) {
  require('dplyr', quietly = TRUE)
  conflict_prefer('mutate', 'dplyr')
  
  成品 <- llply(1:length(时间索引), function(数1) {
    
    if(数1 == 1) {
      
      cat('\n===========================================\n')
      cat('培训数据[', 数1, ']\n')
      print(培训数据 <- 样本[日期 < 时间索引[数1]][(.N - (数据量 - 1)):.N])
      序列号 <- 培训数据$序列[1]:(range(培训数据$序列)[2] + 预测时间单位)
      
      cat('\n-------------------------------------------\n')
      cat('培训数据_测试数据[', 数1, ']\n')
      print(培训数据_测试数据 <- 样本[序列 %in% 序列号])
      
      季回归 <- 培训数据[, .(年月日时分, 闭市价)] %>% 
        tk_ts(frequency = 预测时间单位)
      
      if(.统计模型 == 'auto') {
        
        季回归 <- tryCatch({
          auto.arima(季回归, d = .阶数, D = .季节性差分, seasonal = 季节性与否, 
                     stationary = 静态与否, trace = 记载自回归与否, 
                     ic = 信息量准则, stepwise = 逐步精化, nmodels = 逐步精化量, 
                     #approximation = approximation, 
                     method = 计策谋略, truncate = 缩写, #x = y, 
                     xreg = 趋势, test = 测试, lambda = 博克斯考克斯变换, 
                     test.args = 测试参数, allowdrift = 允许截距与否, 
                     seasonal.test = 测试季节性, 
                     seasonal.test.args = 测试季节性参数, 
                     allowmean = 允许包含均值与否, biasadj = 偏差调整与否, 
                     parallel = 多管其下, num.cores = 核心量)
        }, 错误信息 = function(错误信息参数) NULL)
        
      } else if (.统计模型 == 'Arima') {
        
        季回归 <- tryCatch({
          Arima(季回归, order = .时序规律, seasonal = .季节性规律参数, 
                xreg = 趋势, include.mean = 包含均值与否, 
                include.drift = 包含截距, 
                #include.constant = include.constant, 
                #model = 统计模型, lambda = 博克斯考克斯变换, x = y, 
                biasadj = 偏差调整与否, method = 计策谋略)
          }, 错误信息 = function(错误信息参数) NULL)
        
      } else {
        
      }
      
      if(!is.null(季回归)) {
        季回归 %<>% 
          forecast::forecast(h = 预测时间单位) %>% 
          tk_tbl %>% 
          mutate(年月日时分 = 培训数据_测试数据[(.N - 预测时间单位 + 1):.N,]$年月日时分, 
                        市场价 = 培训数据_测试数据[(.N - 预测时间单位 + 1):.N,]$闭市价) %>% 
          rename(预测价 = `Point Forecast`) %>% 
          select(年月日时分, 市场价, 预测价)
        
        cat('\n-------------------------------------------\n')
        cat('预测序列[', 数1, ']\n')
        print(季回归 %>% as.data.table)
      }
      
      if(.统计模型 == 'auto') {
        模型名称 <- paste0(.统计模型, '_阶数', .阶数, '季节性差分', .季节性差分, 
                      '_季节性与否', 季节性与否, '_', 数据量, '_', 
                      预测时间单位, '.', as_date(季回归$年月日时分[1]), '.rds')
        
      } else if (.统计模型 == 'Arima') {
        模型名称 <- paste0(.统计模型, '_自回归滑动平均', 
          paste(.时序规律, collapse = ''), '_季节性', 
          paste(.季节性规律参数, collapse = ''), '_', 数据量, '_', 
          预测时间单位, '.', as_date(季回归$年月日时分[1]), '.rds')
        
      } else {
        
      }
      
      saveRDS(季回归, paste0(
        路径, 'data/fx/USDJPY/季节性自回归_', 模型名称))
      cat('\n', 数1, '=', 模型名称, '\n\n')
      rm(季回归)
      
    } else if(数1 %in% seq(1, length(时间索引), by = 6)[-1]) {
      
      
    } else if(数1 == length(时间索引)) {
      
      
    } else  {
      
      最终序列 <- 样本[日期 < 时间索引[数1],][.N]$序列 + 1
      
      cat('\n===========================================\n')
      cat('培训数据[', 数1, ']\n')
      print(培训数据 <- 样本[(最终序列 - 数据量 + 1):最终序列])
      序列号 <- 培训数据$序列[1]:(range(培训数据$序列)[2] + 预测时间单位)
      
      cat('\n-------------------------------------------\n')
      cat('培训数据_测试数据[', 数1, ']\n')
      print(培训数据_测试数据 <- 样本[序列 %in% 序列号])
      
      季回归 <- 培训数据[, .(年月日时分, 闭市价)] %>% 
        tk_ts(frequency = 预测时间单位)
      
      if(.统计模型 == 'auto') {
        
        季回归 <- tryCatch({
          auto.arima(季回归, d = .阶数, D = .季节性差分, seasonal = 季节性与否, 
                     stationary = 静态与否, trace = 记载自回归与否, 
                     ic = 信息量准则, stepwise = 逐步精化, nmodels = 逐步精化量, 
                     #approximation = approximation, 
                     method = 计策谋略, truncate = 缩写, #x = y, 
                     xreg = 趋势, test = 测试, lambda = 博克斯考克斯变换, 
                     test.args = 测试参数, allowdrift = 允许截距与否, 
                     seasonal.test = 测试季节性, 
                     seasonal.test.args = 测试季节性参数, 
                     allowmean = 允许包含均值与否, biasadj = 偏差调整与否, 
                     parallel = 多管其下, num.cores = 核心量)
        }, 错误信息 = function(错误信息参数) NULL)
        
      } else if (.统计模型 == 'Arima') {
        
        季回归 <- tryCatch({
          Arima(季回归, order = .时序规律, seasonal = .季节性规律参数, 
                xreg = 趋势, include.mean = 包含均值与否, 
                include.drift = 包含截距, 
                #include.constant = include.constant, 
                #model = 统计模型, lambda = 博克斯考克斯变换, x = y, 
                biasadj = 偏差调整与否, method = 计策谋略)
        }, 错误信息 = function(错误信息参数) NULL)
        
      } else {
        
      }
      
      if(!is.null(季回归)) {
        季回归 %<>% 
          forecast::forecast(h = 预测时间单位) %>% 
          tk_tbl %>% 
          mutate(年月日时分 = 培训数据_测试数据[(.N - 预测时间单位 + 1):.N,]$年月日时分, 
                        市场价 = 培训数据_测试数据[(.N - 预测时间单位 + 1):.N,]$闭市价) %>% 
          rename(预测价 = `Point Forecast`) %>% 
          select(年月日时分, 市场价, 预测价)
        
        cat('\n-------------------------------------------\n')
        cat('预测序列[', i, ']\n')
        print(季回归 %>% as.data.table)
      }
      
      if(.统计模型 == 'auto') {
        模型名称 <- paste0(.统计模型, '_阶数', .阶数, '季节性差分', .季节性差分, 
                      '_季节性与否', 季节性与否, '_', 数据量, '_', 
                      预测时间单位, '.', as_date(季回归$年月日时分[1]), '.rds')
        
      } else if (.统计模型 == 'Arima') {
        模型名称 <- paste0(.统计模型, '_自回归滑动平均', 
                      paste(.时序规律, collapse = ''), '_季节性', 
                      paste(.季节性规律参数, collapse = ''), '_', 数据量, '_', 
                      预测时间单位, '.', as_date(季回归$年月日时分[1]), '.rds')
        
      } else {
        
      }
      
      saveRDS(季回归, paste0(
        路径, 'data/fx/USDJPY/季节性自回归_', 模型名称))
      cat('\n', i, '=', 模型名称, '\n\n')
      rm(季回归)
    }
  })
  return(成品)
}
