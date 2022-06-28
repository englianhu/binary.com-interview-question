日内分计 <- function(时间索引, 样本 = 样本, 数据量, 路径 = NULL, 
                      循环周期 = 1, 预测时间单位 = 1, .模型选项, 列印 = TRUE) {
  
  options(digits = 16)
  require('dplyr', quietly = TRUE)
  require('forecast', quietly = TRUE)
  require('data.table', quietly = TRUE)
  conflict_prefer('mutate', 'dplyr', quiet = TRUE)
  conflict_prefer('rename', 'dplyr', quiet = TRUE)
  conflict_prefer('select', 'dplyr', quiet = TRUE)
  conflict_prefer('forecast', 'forecast', quiet = TRUE)
  
  ## 假设数据量 = 1200分钟
  ## 循环周期 = 1
  ## 20小时一周期（1200分钟循环1次）
  ## 10小时一周期（600分钟循环2次）
  日分计 <- 数据量/循环周期
  if(!'data.table' %in% class(样本)) 样本 %<>% as.data.table
  
  成品 <- llply(1:length(时间索引), function(迭数1) {
    
    ## 一日 = 1440分钟
    半成品 <- llply(1:1440, function(迭数2) {
      
      if(迭数2 == 1) {
        最终序列 <- 样本[日期 < 时间索引[迭数1],][.N]$序列
        
      } else {
        最终序列 <- 样本[日期 < 时间索引[迭数1],][.N]$序列 + 循环周期 * (迭数2 - 1)
      }
      
      培训数据 <- 样本[(最终序列 - 数据量 + 1):最终序列]
      序列号 <- 培训数据$序列[1]:(range(培训数据$序列)[2] + 预测时间单位)
      
      if(列印 == TRUE) {
        cat('\n===========================================\n')
        cat('培训数据[', 迭数1, '-', 迭数2, ']\n')
        print(培训数据)
      }
      
      培训数据_测试数据 <- 样本[序列 %in% 序列号]
      if(列印 == TRUE) {
        cat('\n-------------------------------------------\n')
        cat('培训数据_测试数据[', 迭数1, '-', 迭数2, ']\n')
        print(培训数据_测试数据)
      }
      
      季回归 <- 培训数据[, .(年月日时分, 闭市价)] %>% 
        as_tibble %>% 
        tk_ts(frequency = 循环周期) %>% 
        ets(model = .模型选项) %>% 
        forecast(h = 预测时间单位) %>% 
        tk_tbl %>% 
        mutate(年月日时分 = 培训数据_测试数据[.N]$年月日时分, 
                      市场价 = 培训数据_测试数据[.N]$闭市价) %>% 
        rename(预测价 = `Point Forecast`) %>% 
        select(年月日时分, 市场价, 预测价)
      
      if(列印 == TRUE) {
        cat('\n-------------------------------------------\n')
        cat('forecast[', 迭数1, '-', 迭数2, ']\n')
        print(季回归 %>% as.data.table)
      }
      
      if(is.null(路径)) {
        if(!exists('.路径')) {
          .路径 <- '/home/englianhu/Documents/GitHub/binary.com-interview-question-data/'}
        
        路径 <- paste0(
          .路径, '文艺数据库/fx/USDJPY/仓库/季平滑_', .模型选项, '_', 数据量, '_', 循环周期, '_', 
          as_date(季回归$年月日时分[1]), 'CST_第', 迭数2, '部_.rds')
        
      } else {
        路径 <- paste0(
          路径, '/季平滑_', .模型选项, '_', 数据量, '_', 循环周期, '_', 
          as_date(季回归$年月日时分[1]), 'CST_第', 迭数2, '部_.rds')
      }
      saveRDS(季回归, 路径)
      cat('\n', 迭数1, '-', 迭数2, '=', 
          paste0('~/文艺数据库/fx/USDJPY/仓库/季平滑_', .模型选项, '_', 数据量, '_', 循环周期, '_', 
                 as_date(季回归$年月日时分[1]), 'CST_第', 迭数2, '部_.rds 已储存!'))
      
      cat('\n\n')
      rm(季回归)
      gc()
    })
    return(半成品)
  })
  return(成品)
}



