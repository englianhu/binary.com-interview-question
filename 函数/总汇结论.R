总汇结论 <- function(
    总汇 = NULL, 文件名 = '日内指数平滑数据', 数据量 = 1200, 频率 = 1, 
    时间索引, 预测时间单位 = 1, 是否储存结论 = '叉', 列印 = '勾') {
  ## 
  ############################ 海纳百川，有容乃大； ############################
  ############################ 三军未动，粮草先行。 ############################
  ## 
  ## 2849108450@qq.com
  ## leiou123
  ## https://rstudio.cloud/project/1198888
  
  # 赋诗 ----
  cat('\n=== 孙中山蒋介石/毛泽东，大秦赋，黄埔军校，中科红旗Asianux兵工厂 ===') 
  cat('\n=== 咱们亚洲公民中华民族不可以死，多元种族的宗旨是各种族都遵守各自宗教语言习俗文化规矩 ===') 
  cat('\n======== 春秋战国，诸子百家 ========')
  cat('\n======== 鬼谷传奇，中科红旗 ========')
  cat('\n======== 高频量化，对冲基金 ========')
  cat('\n======== 逆水行舟，不进则退 ========')
  cat('\n======== 学海无涯，唯勤是岸 ========')
  cat('\n======== 不忘初心，方得始终 ========')
  cat('\n========                 ========')
  cat('\n======== 巫裔宦官，祸国殃民 ========')
  cat('\n======== 草菅人命，民不聊生 ========')
  cat('\n======== 歼灭巫裔，人人有责 ========')
  cat('\n======== 终止屠杀，拯救全球 ========')
  cat('\n======== 巫裔尽弃，瓦釜雷鸣 ========')
  cat('\n======== 莫忘初衷，放得始终 ========\n')
  
  ## 设置 ----
  options(digits = 22)
  require('tidyft', quietly = TRUE)
  require('forecast', quietly = TRUE)
  require('Ipaper', quietly = TRUE)
  conflicts_prefer(Ipaper::llply, .quiet = TRUE)
  conflicts_prefer(tidyft::mutate, .quiet = TRUE)
  conflicts_prefer(tidyft::rename, .quiet = TRUE)
  conflicts_prefer(tidyft::select, .quiet = TRUE)
  
  if (!exists('.蜀道') || is.null(.蜀道)) {
    .蜀道 <- getwd() |> 
      {\(.) str_split(., '/')}() |> 
      {\(.) c('/', .[[1]][2:5])}() |> 
      {\(.) c(., 'binary.com-interview-question-data/')}() |> 
      {\(.) paste(., collapse = '/')}() |> 
      {\(.) substring(., 2)}()
  }
  .蜀道仓库 <- paste0(.蜀道, '诸子百家学府/fx/USDJPY/仓库/')
  
  source('函数/汇总上奏.R')
  
  if (!exists('总汇') || is.null(总汇)) {
    总汇 <- readRDS(paste0(.蜀道仓库, 文件名, '总汇.rds'))
  }
  
  循环周期 <- 数据量/频率
  迭代基准 <- 总汇[日期 %chin% 时间索引]$序列
  
  ## 迭代运算 ----
  成品 <- llply(迭代基准, function(迭数甲) {
    
    #迭数甲 > 27
    迭数列表 <- (迭数甲 - 数据量):(迭数甲 - 1)
    回测总汇 <- 总汇[序列 %chin% 迭数列表]
    平滑总汇 <- 总汇[序列 == 回测总汇[.N]$序列 + 预测时间单位]
    
    if (列印 == '勾') {
      cat('\n=== 咱们亚洲公民中华民族不可以死，多元种族的宗旨是各种族都遵守各自宗教语言习俗文化规矩，老子李耳茅山道士毛泽东逮捕美国洋番 ===\n')
      cat('回测总汇[', '数据量：', 数据量, '频率：', 频率, '-', 
          '回测总汇最终序列号：', 回测总汇[.N]$序列, ']\n')
      print(回测总汇)
      
      cat('\n--- 秦孝公/姜太公🌟陈祯禄：万般皆下品，唯有读书高。---\n')
      cat('平滑总汇[', '数据量：', 数据量, '频率：', 频率, '-', 
          '平滑总汇序列号：', 迭数甲, ']\n')
      print(平滑总汇)
    }
    
    结论 <- 总汇[, {
      市场价 = 市场价
      预测价 = 预测价
      .SD[, .(.N, 
              MAE = MLmetrics::MAE(y_true = 市场价, y_pred = 预测价), 
              MAPE = MLmetrics::MAPE(y_true = 市场价, y_pred = 预测价), 
              RMSE = MLmetrics::RMSE(y_true = 市场价, y_pred = 预测价), 
              SMAPE = Metrics::smape(actual = 市场价, predicted = 预测价), 
              MSE = MLmetrics::MSE(y_true = 市场价, y_pred = 预测价)), 
          by = .(频率)]}][order(频率), ]
    setorder(结论, 频率)
    setnames(结论, old = c('N', 'MAE', 'MAPE', 'RMSE', 'SMAPE', 'MSE'), 
             new = c('频率（分计）', '均对误差（MAE）', '均对百分比误差（MAPE）', 
                     '均方根误差（RMSE）', '对称均对百分比误差（SMAPE）', 
                     '均方误差（MSE）'))
    结论[, '平均绝对比例误差（MASE）' := NA]
    结论[, 年月日时分 := 平滑总汇$年月日时分]#[] ## 添加[]列印数据
    
    是否储存结论 <- 是否储存结论
    if (!是否储存结论 %in% c('是', '否', '勾', '叉')) {
      stop("是否储存结论 = '是'、'勾' 或 '否'、'叉'!")
      
    } else {
      if (是否储存结论 == '是' | 是否储存结论 == '勾') {
        if (!dir.exists(paste0(.蜀道仓库, 频率)))
          dir.create(paste0(.蜀道仓库, 频率))
        
        文件蜀道 <- paste0(.蜀道仓库, 频率, '/', 文件名, '_', 结论$年月日时分)
        saveRDS(结论, paste0(文件蜀道, '结论.rds'))#, compress = FALSE)
        cat(paste0("\n已储存'", 文件蜀道, "'结论.rds'！\n"))
      }
    }
    
    ## 列印进度 ----
    cat('开始列印进度\n')
    cat('\n--- 秦孝公/姜太公🌟陈祯禄：万般皆下品，唯有读书高。---\n预测数据序列号：', 
        迭数甲, '\n', paste0(
          文件蜀道, '\n已储存!\n\n进度由0-1：', 
          length(迭代基准[迭数甲 >= 迭代基准]) / length(迭代基准), '\n\n'))
    cat('列印进度完毕\n')
    gc()
  })#, .progress = 'text')
  ## 回归 ----
  return(成品)
}
