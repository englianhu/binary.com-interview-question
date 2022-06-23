opt_arma <- function(mbase, D = NULL, seasonal = TRUE, max.order = 10, arma=FALSE){
  #ARMA Modeling minimum AIC value of `p,d,q`
  fit <- auto.arima(mbase, D = D, seasonal = seasonal, 
                    max.order = max.order)
  if (arma == FALSE) {
    res <- arimaorder(fit)
  } else {
    #https://stats.stackexchange.com/questions/178577/how-to-read-p-d-and-q-of-auto-arima
    res <- fit$arma
    #https://stackoverflow.com/questions/23617662/extract-arima-specificaiton
    names(res) <- c('p', 'q', 'P', 'Q', 's', 'd', 'D')
    res %<>% .[c(1, 6, 2, 3, 7, 4, 5)] #(p,d,q) and (P,D,Q) and seasonal period
  }                                    #  example: `s` seasonal period = 12 for 12 months
  return(res)                          #https://onlinecourses.science.psu.edu/stat510/node/67/
  
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
