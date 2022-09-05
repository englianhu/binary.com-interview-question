日内高频指数平滑 <- function(时间索引, 样本 = 样本, 数据量, 
                     路径 = NULL, 频率 = 1200, 预测时间单位 = 1, 
                     .模型选项, 列印 = TRUE) {
  
  options(digits = 16)
  require('dplyr', quietly = TRUE)
  require('forecast', quietly = TRUE)
  require('data.table', quietly = TRUE)
  conflict_prefer('mutate', 'dplyr', quiet = TRUE)
  conflict_prefer('rename', 'dplyr', quiet = TRUE)
  conflict_prefer('select', 'dplyr', quiet = TRUE)
  conflict_prefer('forecast', 'forecast', quiet = TRUE)
  conflict_prefer('ets', 'forecast', quiet = TRUE)
  
  if(!'data.table' %in% class(样本)) 样本 %<>% as.data.table
  
  ## 假设数据量 = 1200分钟
  ## 频率 = 1200
  ## 20小时一周期（1200分钟循环1次）
  ## 10小时一周期（600分钟循环2次）
  循环周期 <- 数据量/频率
  迭代基准 <- 样本[日期 %chin% 时间索引]$序列
  
  成品 <- llply(迭代基准, function(迭数1) {
    
    迭数列表 <- (迭数1 - 数据量):(迭数1 - 1)
    培训样本 <- 样本[序列 %chin% 迭数列表]
    
    if(列印 == TRUE) {
      cat('\n===========================================\n')
      cat('培训样本[', '数据量：', 数据量, '频率：', 频率, '-', '培训样本最终序列号：', 培训样本[.N]$序列, ']\n')
      print(培训样本)
      
      cat('\n-------------------------------------------\n')
      cat('预测样本[', '数据量：', 数据量, '频率：', 频率, '-', '预测样本序列号：', 迭数1, ']\n')
      预测样本 <- 样本[序列 == 培训样本[.N]$序列 + 预测时间单位]
      print(预测样本)
    }
    
    半成品 <- 培训样本[, .(年月日时分, 闭市价)] |> 
      {\(.) as_tibble(.) }() |> 
      {\(.) tk_ts(., frequency = 频率)}() |> 
      {\(.) forecast::ets(., model = .模型选项)}() |> 
      {\(.) forecast::forecast(., h = 预测时间单位)}() |> 
      {\(.) tk_tbl(.)}() |> 
      {\(.) mutate(., 
                   年月日时分 = 预测样本[.N]$年月日时分, 
                   市场价 = 预测样本[.N]$闭市价)}() |> 
      {\(.) dplyr::rename(., 预测价 = `Point Forecast`)}() |> 
      {\(.) dplyr::select(., 年月日时分, 市场价, 预测价)}() |> 
      {\(.) as.data.table(.)}()
    
    if(列印 == TRUE) {
      cat('\n-------------------------------------------\n')
      cat('预测样本[', '数据量：', 数据量, '频率：', 频率, '-', 
          '预测数据序列号：', 迭数1, ']\n')
      print(半成品)
    }
    
    文件名 <- paste0('季平滑_', .模型选项, '_数据量', 数据量, 
                  '_频率', 频率, '_', 半成品$年月日时分, 'CST.rds')
    
    if(is.null(路径)) {
      .路径 <- getwd() |> 
        {\(.) str_split(., '/')}() |> 
        {\(.) c('/', .[[1]][2:5])}() |> 
        {\(.) c(., 'binary.com-interview-question-data/')}() |> 
        {\(.) paste(., collapse = '/')}() |> 
        {\(.) substring(., 2)}()
      if(!dir.exists(paste0(.路径, '文艺数据库/fx/USDJPY/仓库/', 频率)))
        dir.create(paste0(.路径, '文艺数据库/fx/USDJPY/仓库/', 频率))
        
      文件路径 <- paste0(.路径, '文艺数据库/fx/USDJPY/仓库/', 频率, 
                     '/', 文件名)
      
    } else {
      文件路径 <- paste0(路径, 文件名)
    }
    saveRDS(半成品, 文件路径)
    
    cat('\n-------------------------------------------\n预测数据序列号：', 
        迭数1, '\n', 
        paste0(
          文件路径, '\n已储存!\n\n进度由0-1：', 
          length(迭代基准[迭数1 >= 迭代基准]) / length(迭代基准), '\n\n'))
    rm(半成品)
    gc()
  })
  return(成品)
}



