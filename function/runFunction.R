runFunction <- function() {
    ## 3 months data for daily price.
    ETS.day.3m <- dir('./data/', pattern = '.Cl.d3m.rds$') %>% str_replace_all('.rds', '')
    d3m <- ldply(ETS.day.3m, function(x) {
      z = readRDS(paste0('./data/', x, '.rds')) %>% na.omit
      #data.frame(x$Point.Forecast, Cl(x))
      mse = mean((z$Point.Forecast - Cl(z))^2)
      data.frame(Currency = str_replace_all(x, '.Cl.d3m', ''), MSE.3M = mse)
      })
    
    
    ## 6 months data for daily price.
    ETS.day.6m <- dir('./data/', pattern = '.Cl.d6m.rds$') %>% str_replace_all('.rds', '')
    d6m <- ldply(ETS.day.6m, function(x) {
      z = readRDS(paste0('./data/', x, '.rds')) %>% na.omit
      #data.frame(x$Point.Forecast, Cl(x))
      mse = mean((z$Point.Forecast - Cl(z))^2)
      data.frame(Currency = str_replace_all(x, '.Cl.d6m', ''), MSE.6M = mse)
      })
    
    
    ## 1 year data for daily price.
    ETS.day.1y <- dir('./data/', pattern = '.Cl.d1y.rds$') %>% str_replace_all('.rds', '')
    d1y <- ldply(ETS.day.1y, function(x) {
      z = readRDS(paste0('./data/', x, '.rds')) %>% na.omit
      #data.frame(x$Point.Forecast, Cl(x))
      mse = mean((z$Point.Forecast - Cl(z))^2)
      data.frame(Currency = str_replace_all(x, '.Cl.d1y', ''), MSE.1Y = mse)
      })
    
    mse.ETS <- suppressAll(join_all(list(d3m, d6m, d1y)))
    mse.ETS %<>% tbl_df %>% mutate(
      Currency = as.character(Currency), 
      MSE.3M = as.numeric(MSE.3M), 
      MSE.6M = as.numeric(MSE.6M), 
      MSE.1Y = as.numeric(MSE.1Y))
    
    mse.ETS %<>% separate(Currency, c('Currency', 'Model'))
    mse.ETS %<>% ddply(.(Model), summarise, MSE.3M = mean(MSE.3M), 
                                            MSE.6M = mean(MSE.6M), 
		    								MSE.1Y = mean(MSE.1Y))
    #mse.ETS %>% kable(width = 'auto')
    print(mse.ETS)
    }

## Scheduling r functions after every particular time interval
## https://stackoverflow.com/questions/10069989/scheduling-r-functions-after-every-particular-time-interval

repeat {
    startTime <- Sys.time()
    runFunction()
    sleepTime <- startTime + 5*60 - Sys.time()
    if (sleepTime > 0)
        Sys.sleep(sleepTime)
    }

