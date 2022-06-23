## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.AutoArima.d6m <- simAutoArima2(mbase_day1[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(AUDUSD.AutoArima.d6m, './data/AUDUSD.AutoArima.d6m.rds')

## EURUSD
EURUSD.AutoArima.d6m <- simAutoArima2(mbase_day1[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(EURUSD.AutoArima.d6m, './data/EURUSD.AutoArima.d6m.rds')

## GBPUSD
GBPUSD.AutoArima.d6m <- simAutoArima2(mbase_day1[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(GBPUSD.AutoArima.d6m, './data/GBPUSD.AutoArima.d6m.rds')

## USDCAD
USDCAD.AutoArima.d6m <- simAutoArima2(mbase_day1[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDCAD.AutoArima.d6m, './data/USDCAD.AutoArima.d6m.rds')

## USDCHF
USDCHF.AutoArima.d6m <- simAutoArima2(mbase_day1[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDCHF.AutoArima.d6m, './data/USDCHF.AutoArima.d6m.rds')

## USDCNY
USDCNY.AutoArima.d6m <- simAutoArima2(mbase_day1[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDCNY.AutoArima.d6m, './data/USDCNY.AutoArima.d6m.rds')

## USDJPY
USDJPY.AutoArima.d6m <- simAutoArima2(mbase_day1[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDJPY.AutoArima.d6m, './data/USDJPY.AutoArima.d6m.rds')

## ----- Due to error, here I use simAutoArima -----------------------------
## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'
.baseDate = ymd(ymd_hms(.baseDate))

## AUDUSD
AUDUSD.AutoArima.d6m <- simAutoArima(mbase_day1[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'months', .unit = 6)
saveRDS(AUDUSD.AutoArima.d6m, './data/AUDUSD.AutoArima.d6m.rds')

## EURUSD
EURUSD.AutoArima.d6m <- simAutoArima(mbase_day1[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'months', .unit = 6)
saveRDS(EURUSD.AutoArima.d6m, './data/EURUSD.AutoArima.d6m.rds')

## GBPUSD
GBPUSD.AutoArima.d6m <- simAutoArima(mbase_day1[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'months', .unit = 6)
saveRDS(GBPUSD.AutoArima.d6m, './data/GBPUSD.AutoArima.d6m.rds')

## USDCAD
USDCAD.AutoArima.d6m <- simAutoArima(mbase_day1[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'months', .unit = 6)
saveRDS(USDCAD.AutoArima.d6m, './data/USDCAD.AutoArima.d6m.rds')

## USDCHF
USDCHF.AutoArima.d6m <- simAutoArima(mbase_day1[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'months', .unit = 6)
saveRDS(USDCHF.AutoArima.d6m, './data/USDCHF.AutoArima.d6m.rds')

## USDCNY
USDCNY.AutoArima.d6m <- simAutoArima(mbase_day1[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'months', .unit = 6)
saveRDS(USDCNY.AutoArima.d6m, './data/USDCNY.AutoArima.d6m.rds')

## USDJPY
USDJPY.AutoArima.d6m <- simAutoArima(mbase_day1[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'months', .unit = 6)
saveRDS(USDJPY.AutoArima.d6m, './data/USDJPY.AutoArima.d6m.rds')
