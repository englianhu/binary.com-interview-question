自回归均移模型最优值 <- function(样本, 季节差分的次数 = NULL, 季节性 = '勾', 规律极限值 = 10, 自回归均移模型值=FALSE){
  # 通过设置并迭代筹算自回归均移模型中不同`p,d,q`的规律值，来比较并筛选出最低或最大负数的赤池信息量准则，也就是最优统计模型。
  # 
  # 《预测：方法与实践（第三版）》第九章第九节 - 季节性ARIMA模型
  # https://otexts.com/fpp3cn/seasonal-arima-cn.html
  
  if (季节性 %in% c('勾', '有')) 季节性 <- TRUE
  if (季节性 %in% c('叉', '冇')) 季节性 <- FALSE
  if (!季节性 %in% c('勾', '有', '叉', '冇')) stop('请选择季节性："勾"或"有"或"是"，或者"叉"或"冇"或"否"。')
  
  # 季节差分的次数，一般上使用到的数值是零到二。
  fit <- auto.arima(样本, D = 季节差分的次数, seasonal = 季节性, 
                    max.order = 规律极限值)
  if (自回归均移模型值 == FALSE) {
    成果 <- arimaorder(fit)
  } else {
    #https://stats.stackexchange.com/questions/178577/how-to-read-p-d-and-q-of-auto-arima
    成果 <- fit$arma
    #https://stackoverflow.com/questions/23617662/extract-arima-specificaiton
    names(成果) <- c('p', 'q', 'P', 'Q', 's', 'd', 'D')
    成果 %<>% .[c(1, 6, 2, 3, 7, 4, 5)]
    #(p,d,q) and (P,D,Q) and seasonal period
  } #范例：`s` seasonal period = 12 表示十二个月
  return(成果)
  #https://onlinecourses.science.psu.edu/stat510/node/67/
  
  # https://stackoverflow.com/questions/23617662/extract-arima-specificaiton
  #
  #function (object) {
  #  order <- object$arma[c(1, 6, 2, 3, 7, 4, 5)]
  #  result <- paste("ARIMA(", order[1], ",", order[2], ",", order[3], 
  #                  ")", sep = "")
  #  if (order[7] > 1 & sum(order[4:6]) > 0) 
  #    result <- paste(result, "(", order[4], ",", order[5], 
  #                    ",", order[6], ")[", order[7], "]", sep = "")
  #  if (is.element("constant", names(object$coef)) | is.element("intercept", 
  #                                                              names(object$coef))) 
  #    result <- paste(result, "with non-zero mean")
  #  else if (is.element("drift", names(object$coef))) 
  #    result <- paste(result, "with drift        ")
  #  else if (order[2] == 0 & order[5] == 0) 
  #    result <- paste(result, "with zero mean    ")
  #  else result <- paste(result, "                  ")
  #  return(result)
  #}
}
