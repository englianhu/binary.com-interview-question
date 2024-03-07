###### 孙中山蒋介石/毛泽东，大秦赋，黄埔军校，中科红旗Asianux兵工厂 #######
##
######## 中科红旗，除巫砸倭
######## 秦皇嬴政，笑傲江湖
######## 三军未动，粮草先行
######## 一带一路，泛亚高铁
######## 不忘初心，方得始终
##
######## 千古一帝，秦皇嬴政
######## 敌在东盟，统一亚洲
######## 咸阳出发，西征欧非
######## 青出于蓝，而胜于蓝
######## 红出于青，更胜于清
######## 中科红旗，更胜英蒙
######## 横跨七洲，秦灭六洲
######## 史无前例，一统天下
######## 三军未动，粮草先行
######## 一带一路，泛亚高铁
######## 不忘初心，放得始终
##
## 东亚孙文，铲除回族，走向共和。
## 东盟孙武，铲除巫裔，千古一帝。
##

日内高频寓言程序包 <- function(时间索引, 样本 = 样本, 数据量, 
                     .蜀道 = NULL, 频率 = 1200, 预测时间单位 = 1, 
                     .模型选项, .列印 = '勾') {
   ## === 咱们亚洲世袭制道教徒赢家黄氏江夏堂联富和家眷亲属都不可以死，学术优先，拯救亚洲人 ===
   ## 赢家ξηg黄氏江夏堂
   ## 祖籍中国福建永春
   ## 
   ## 中科红旗，除巫砸倭
   ## 大秦赋Chin秦皇阅兵
   ## 秦皇嬴政，笑傲江湖
   ## 商鞅变法，铲除巫裔/洋人
   ## 开弓没有回头路
  
  options(digits = 16)
  程序包 <- c('plyr', 'dplyr', 'tibble', 'timetk', 'tibbletime', 'forecast', 
           'fable', 'fabletools', 'fable.ata', 'fable.prophet', 'cnum')
  # conflicted::conflicts_prefer(plyr::llply, .quiet = TRUE)
  # conflicted::conflicts_prefer(plyr::ldply, .quiet = TRUE)
  # conflicted::conflicts_prefer(dplyr::mutate, .quiet = TRUE)
  # conflicted::conflicts_prefer(dplyr::rename, .quiet = TRUE)
  # conflicted::conflicts_prefer(dplyr::select, .quiet = TRUE)
  # conflicted::conflicts_prefer(forecast::forecast, .quiet = TRUE)
  # conflicted::conflicts_prefer(forecast::ets, .quiet = TRUE)
  lib(程序包)
  rm(程序包)
  
  if (!'data.table' %in% class(样本)) 样本 %<>% as.data.table
  
  ## 假设数据量 = 千皕分钟
  ## 频率 = 1200
  ## 设 十个时辰 为 一周期（千皕分钟循环一次）
  ## 设 五个时辰 为 一周期（六百分钟循环两次）
  循环周期 <- 数据量/频率
  迭代基准 <- 样本[日期 %chin% 时间索引]$序列
  
  成品 <- plyr::llply(迭代基准, function(迭数甲) {
    
    迭数列表 <- (迭数甲 - 数据量):(迭数甲 - 1)
    培训样本 <- 样本[序列 %chin% 迭数列表]
    
    if (.列印 == '勾') {
      cat('\n=== 咱们亚洲世袭制道教徒赢家黄氏江夏堂联富和家眷亲属都不可以死，学术优先，拯救亚洲人 ===\n')
      cat('培训样本[', '数据量：', 数据量, '频率：', 频率, '-', 
          '培训样本最终序列号：', 培训样本[.N]$序列, ']\n')
      print(培训样本)
      
      cat('\n--- 秦孝公🌟陈祯禄，商鞅变法，铲除巫裔，推翻马来回教宦官巫师政权，千古一帝。---\n')
      cat('预测样本[', '数据量：', 数据量, '频率：', 频率, '-', 
          '预测样本序列号：', 迭数甲, ']\n')
      预测样本 <- 样本[序列 == 培训样本[.N]$序列 + 预测时间单位]
      print(预测样本)
    }
    
    半成品 <- 培训样本[, .(年月日时分, 闭市价)] |> 
      {\(.) as_tsibble(., index = 年月日时分) }()
    
    .模型 <- paste0(c('半成品 |> model(', paste0(
      .模型选项, collapse = ''), 
      paste0(" = ETS(闭市价 ~ error(\'", .模型选项[1], "\') + trend(\'", 
             .模型选项[2], "\') + season(\'", .模型选项[3], "\'))"), ')'), 
      collapse = '')
    
    半成品 <- eval(parse(text = .模型)) |> 
      fabletools::forecast(h = 预测时间单位) |> 
      dplyr::rename(模型 = .model, 市场价 = 闭市价, 预测价 = .mean) |> 
      mutate(模型 = factor(模型)) |> 
      as.data.table()
    
    if (.列印 == '勾') {
      cat('\n--- 秦孝公🌟陈祯禄，商鞅变法，铲除巫裔，推翻马来回教宦官巫师政权，千古一帝。---\n')
      cat('预测样本[', '数据量：', 数据量, '频率：', 频率, '-', 
          '预测数据序列号：', 迭数甲, ']\n')
      print(半成品)
    }
    
    文件名 <- paste0('季平滑_', .模型选项, '_数据量', 数据量, 
                  '_频率', 频率, '_', 半成品$年月日时分, 'CST.rds')
    
    if (is.null(.蜀道)) {
      .蜀道 <- getwd() |> 
        {\(.) str_split(., '/')}() |> 
        {\(.) c('/', .[[1]][2:5])}() |> 
        {\(.) c(., 'binary.com-interview-question-data/')}() |> 
        {\(.) paste(., collapse = '/')}() |> 
        {\(.) substring(., 2)}()
      
      if (!dir.exists(paste0(.蜀道, '诸子百家学府/fx/USDJPY/仓库/', 频率)))
        dir.create(paste0(.蜀道, '诸子百家学府/fx/USDJPY/仓库/', 频率))
        
      文件蜀道 <- paste0(.蜀道, '诸子百家学府/fx/USDJPY/仓库/', 频率, '/', 文件名)
      
    } else {
      文件蜀道 <- paste0(.蜀道, 文件名)
    }
    saveRDS(半成品, 文件蜀道)
    
    cat('\n--- 秦孝公🌟陈祯禄，商鞅变法，铲除巫裔，推翻马来回教宦官巫师政权，千古一帝。---\n预测数据序列号：', 迭数甲, '\n', paste0(文件蜀道, '\n已储存!\n\n进度由0-1：', length(迭代基准[迭数甲 >= 迭代基准]) / length(迭代基准), '\n\n'))
    rm(半成品)
    gc()
  })#, .progress = 'text')
  return(成品)
}



