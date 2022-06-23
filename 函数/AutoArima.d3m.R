## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.AutoArima.d3m <- simAutoArima2(mbase_day1[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(AUDUSD.AutoArima.d3m, './data/AUDUSD.AutoArima.d3m.rds')

## EURUSD
EURUSD.AutoArima.d3m <- simAutoArima2(mbase_day1[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(EURUSD.AutoArima.d3m, './data/EURUSD.AutoArima.d3m.rds')

## GBPUSD
GBPUSD.AutoArima.d3m <- simAutoArima2(mbase_day1[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(GBPUSD.AutoArima.d3m, './data/GBPUSD.AutoArima.d3m.rds')

## USDCAD
USDCAD.AutoArima.d3m <- simAutoArima2(mbase_day1[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDCAD.AutoArima.d3m, './data/USDCAD.AutoArima.d3m.rds')

## USDCHF
USDCHF.AutoArima.d3m <- simAutoArima2(mbase_day1[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDCHF.AutoArima.d3m, './data/USDCHF.AutoArima.d3m.rds')

## USDCNY
USDCNY.AutoArima.d3m <- simAutoArima2(mbase_day1[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDCNY.AutoArima.d3m, './data/USDCNY.AutoArima.d3m.rds')

## USDJPY
USDJPY.AutoArima.d3m <- simAutoArima2(mbase_day1[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDJPY.AutoArima.d3m, './data/USDJPY.AutoArima.d3m.rds')

## ----- Due to error, here I use simAutoArima -----------------------------
## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'
.baseDate = ymd(ymd_hms(.baseDate))

## AUDUSD
AUDUSD.AutoArima.d3m <- simAutoArima(mbase_day1[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'months', .unit = 3)
saveRDS(AUDUSD.AutoArima.d3m, './data/AUDUSD.AutoArima.d3m.rds')

## EURUSD
EURUSD.AutoArima.d3m <- simAutoArima(mbase_day1[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'months', .unit = 3)
saveRDS(EURUSD.AutoArima.d3m, './data/EURUSD.AutoArima.d3m.rds')

## GBPUSD
GBPUSD.AutoArima.d3m <- simAutoArima(mbase_day1[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'months', .unit = 3)
saveRDS(GBPUSD.AutoArima.d3m, './data/GBPUSD.AutoArima.d3m.rds')

## USDCAD
USDCAD.AutoArima.d3m <- simAutoArima(mbase_day1[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'months', .unit = 3)
saveRDS(USDCAD.AutoArima.d3m, './data/USDCAD.AutoArima.d3m.rds')

## USDCHF
USDCHF.AutoArima.d3m <- simAutoArima(mbase_day1[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'months', .unit = 3)
saveRDS(USDCHF.AutoArima.d3m, './data/USDCHF.AutoArima.d3m.rds')

## USDCNY
USDCNY.AutoArima.d3m <- simAutoArima(mbase_day1[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'months', .unit = 3)
saveRDS(USDCNY.AutoArima.d3m, './data/USDCNY.AutoArima.d3m.rds')

## USDJPY
USDJPY.AutoArima.d3m <- simAutoArima(mbase_day1[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'months', .unit = 3)
saveRDS(USDJPY.AutoArima.d3m, './data/USDJPY.AutoArima.d3m.rds')
