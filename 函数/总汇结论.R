总汇结论 <- function(文件名 = '日内指数平滑数据', 是否储存结论 = '否') {
  ## 
  ############################ 海纳百川，有容乃大； ############################
  ############################ 三军未动，粮草先行。 ############################
  ## 
  ## 2849108450@qq.com
  ## leiou123
  ## https://rstudio.cloud/project/1198888
  source('函数/汇总上奏.R')
  if (!exists('总汇')) {
    总汇 <- readRDS(paste0(.蜀道仓库, 文件名, '总汇.rds'))
  }
  
  结论 <- 总汇[, {
    市场价 = 市场价
    预测价 = 预测价
    .SD[, .(.N, 
            mae = MLmetrics::MAE(y_true = 市场价, y_pred = 预测价), 
            mape = MLmetrics::MAPE(y_true = 市场价, y_pred = 预测价), 
            rmse = MLmetrics::RMSE(y_true = 市场价, y_pred = 预测价), 
            smape = Metrics::smape(actual = 市场价, predicted = 预测价), 
            mse = MLmetrics::MSE(y_true = 市场价, y_pred = 预测价)), 
        by = .(频率)]}][order(频率), ]
  setorder(结论, 频率)
  setnames(结论, old = 'N', new = '频率（分计）')
  
  是否储存结论 <- 是否储存结论
  if (!是否储存结论 %in% c('是', '否')) {
    stop("是否储存结论 = '是' 或 '否'!")
  } else {
    if (是否储存结论 == '是') {
      saveRDS(结论, paste0(.蜀道仓库, 文件名, '结论.rds'))#, compress = FALSE)
      cat(paste0('\n已储存"', 文件名, '结论.rds"！\n'))
    }
  }
  
  return(结论)
}