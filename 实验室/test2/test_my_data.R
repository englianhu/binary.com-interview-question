
dateID <- getSymbols('JPY=X', from = (today('GMT') - 1) %m-% years(1), 
                     to = (today('GMT') - 1), auto.assign = FALSE) %>% index

my_forecast <- llply(dateID, function(x) {
  cbind(calC('JPY=X', x, Hi), calC('JPY=X', x, Lo))}) %>% 
  do.call(rbind, .)
saveRDS(my_forecast, 'test2/my_forecast.rds')






