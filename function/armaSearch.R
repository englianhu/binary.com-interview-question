armaSearch <- suppressWarnings(function(data, .method = 'CSS-ML'){ 
  ## I set .method = 'CSS-ML' as default method since the AIC value we got is 
  ##  smaller than using method 'ML' while using method 'CSS' facing error.
  ## 
  ## https://stats.stackexchange.com/questions/209730/fitting-methods-in-arima
  ## According to the documentation, this is how each method fits the model:
  ##  - CSS minimises the sum of squared residuals.
  ##  - ML maximises the log-likelihood function of the ARIMA model.
  ##  - CSS-ML mixes both methods: first, CSS is run, the starting parameters 
  ##    for the optimization algorithm are set to zeros or to the values given 
  ##    in the optional argument init; then, ML is applied passing the CSS 
  ##    parameter estimates as starting parameter values for the optimization algorithm.
  
  .methods = c('CSS-ML', 'ML', 'CSS')
  
  if(!.method %in% .methods) stop(paste('Kindly choose .method among ', 
                                        paste0(.methods, collapse = ', '), '!'))
  
  armacoef <- data.frame()
  for (p in 0:5){
    for (q in 0:5) {
      #data.arma = arima(diff(data), order = c(p, 0, q))
      #'@ data.arma = arima(data, order = c(p, 1, q), method = .method)
      if(.method == 'CSS-ML') {
          data.arma = tryCatch({
            arma = arima(data, order = c(p, 1, q), method = 'CSS-ML')
            mth = 'CSS-ML'
            list(arma, mth)
          }, error = function(e) tryCatch({
            arma = arima(data, order = c(p, 1, q), method = 'ML')
            mth = 'ML'
            list(arma = arma, mth = mth)
          }, error = function(e) {
            arma = arima(data, order = c(p, 1, q), method = 'CSS')
            mth = 'CSS'
            list(arma = arma, mth = mth)
          }))
          
        } else if(.method == 'ML') {
          data.arma = tryCatch({
            arma = arima(data, order = c(p, 1, q), method = 'ML')
            mth = 'ML'
            list(arma = arma, mth = mth)
          }, error = function(e) tryCatch({
            arma = arima(data, order = c(p, 1, q), method = 'CSS-ML')
            mth = 'CSS-ML'
            list(arma = arma, mth = mth)
          }, error = function(e) {
            arma = arima(data, order = c(p, 1, q), method = 'CSS')
            mth = 'CSS'
            list(arma = arma, mth = mth)
          }))
          
        } else if(.method == 'CSS') {
          data.arma = tryCatch({
            arma = arima(data, order = c(p, 1, q), method = 'CSS')
            mth = 'CSS'
            list(arma = arma, mth = mth)
          }, error = function(e) tryCatch({
            arma = arima(data, order = c(p, 1, q), method = 'CSS-ML')
            mth = 'CSS-ML'
            list(arma = arma, mth = mth)
          }, error = function(e) {
            arma = arima(data, order = c(p, 1, q), method = 'ML')
            mth = 'ML'
            list(arma = arma, mth = mth)
          }))
          
      } else {
        stop(paste('Kindly choose .method among ', paste0(.methods, collapse = ', '), '!'))
      }
      names(data.arma) <- c('arma', 'mth')
      
      #cat('p =', p, ', q =', q, 'AIC =', data.arma$arma$aic, '\n')
      armacoef <- rbind(armacoef,c(p, q, data.arma$arma$aic))
    }
  }
  
  #ARMA Modeling寻找AIC值最小的p,q
  colnames(armacoef) <- c('p', 'q', 'AIC')
  pos <- which(armacoef$AIC == min(armacoef$AIC))
  cat(paste0('method = \'', data.arma$mth, '\', the min AIC = ', armacoef$AIC[pos], 
             ', p = ', armacoef$p[pos], ', q = ', armacoef$q[pos], '\n'))
  return(armacoef)
})

