#ARMA Modeling寻找AIC值最小的p,q
armaSearch <- suppressWarnings(function(data, .method = 'CSS-ML'){ 
  
  .methods = c('CSS-ML', 'ML', 'CSS')
  
  if(!.method %in% .methods) stop(paste('Kindly choose .method among ', paste0(.methods, collapse = ', '), '!'))
  
  armacoef <- data.frame()
  for (p in 0:5){
    for (q in 0:5) {
      #data.arma = arima(diff(data), order = c(p, 0, q))
      data.arma = arima(data, order = c(p, 1, q), method = .method)
      #cat('p =', p, ', q =', q, 'AIC =', data.arma$aic, '\n')
      armacoef <- rbind(armacoef,c(p, q, data.arma$aic))
    }
  }
  
  colnames(armacoef) <- c('p', 'q', 'AIC')
  pos <- which(armacoef$AIC == min(armacoef$AIC))
  cat('the min AIC =', armacoef$AIC[pos], ', p =', armacoef$p[pos], ', q =', armacoef$q[pos], '\n')
  return(armacoef)
})

