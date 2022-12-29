总汇结论 <- function(总汇 = NULL, 文件名 = '日内指数平滑数据', 是否储存结论 = '否') {
  ## 
  ############################ 海纳百川，有容乃大； ############################
  ############################ 三军未动，粮草先行。 ############################
  ## 
  ## 2849108450@qq.com
  ## leiou123
  ## https://rstudio.cloud/project/1198888
  
  if (!exists('.蜀道') || is.null(.蜀道)) {
    .蜀道 <- getwd() |> 
      {\(.) str_split(., '/')}() |> 
      {\(.) c('/', .[[1]][2:5])}() |> 
      {\(.) c(., 'binary.com-interview-question-data/')}() |> 
      {\(.) paste(., collapse = '/')}() |> 
      {\(.) substring(., 2)}()
  }
  .蜀道仓库 <- paste0(.蜀道, '文艺数据库/fx/USDJPY/仓库/')
  
  source('函数/汇总上奏.R')
  
  if (!exists('总汇') || is.null(总汇)) {
    总汇 <- readRDS(paste0(.蜀道仓库, 文件名, '总汇.rds'))
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
           new = c('频率（分计）', '均对误差（MAE）', '均对百分比误差（MAPE）', '均方根误差（RMSE）', '对称均对百分比误差（SMAPE）', '均方误差（MSE）'))
  结论[, '平均绝对比例误差（MASE）' := NA]
  
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