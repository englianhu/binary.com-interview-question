## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.AutoArima.d1y <- simAutoArima2(mbase_day1[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 1, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(AUDUSD.AutoArima.d1y, './data/AUDUSD.AutoArima.d1y.rds')

## EURUSD
EURUSD.AutoArima.d1y <- simAutoArima2(mbase_day1[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 1, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(EURUSD.AutoArima.d1y, './data/EURUSD.AutoArima.d1y.rds')

## GBPUSD
GBPUSD.AutoArima.d1y <- simAutoArima2(mbase_day1[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 1, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(GBPUSD.AutoArima.d1y, './data/GBPUSD.AutoArima.d1y.rds')

## USDCAD
USDCAD.AutoArima.d1y <- simAutoArima2(mbase_day1[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 1, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDCAD.AutoArima.d1y, './data/USDCAD.AutoArima.d1y.rds')

## USDCHF
USDCHF.AutoArima.d1y <- simAutoArima2(mbase_day1[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 1, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDCHF.AutoArima.d1y, './data/USDCHF.AutoArima.d1y.rds')

## USDCNY
USDCNY.AutoArima.d1y <- simAutoArima2(mbase_day1[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 1, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDCNY.AutoArima.d1y, './data/USDCNY.AutoArima.d1y.rds')

## USDJPY
USDJPY.AutoArima.d1y <- simAutoArima2(mbase_day1[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 1, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDJPY.AutoArima.d1y, './data/USDJPY.AutoArima.d1y.rds')

## ----- Due to error, here I use simAutoArima -----------------------------
## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'
.baseDate = ymd(ymd_hms(.baseDate))

## AUDUSD
AUDUSD.AutoArima.d1y <- simAutoArima(mbase_day1[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'years', .unit = 1)
saveRDS(AUDUSD.AutoArima.d1y, './data/AUDUSD.AutoArima.d1y.rds')

## EURUSD
EURUSD.AutoArima.d1y <- simAutoArima(mbase_day1[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'years', .unit = 1)
saveRDS(EURUSD.AutoArima.d1y, './data/EURUSD.AutoArima.d1y.rds')

## GBPUSD
GBPUSD.AutoArima.d1y <- simAutoArima(mbase_day1[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'years', .unit = 1)
saveRDS(GBPUSD.AutoArima.d1y, './data/GBPUSD.AutoArima.d1y.rds')

## USDCAD
USDCAD.AutoArima.d1y <- simAutoArima(mbase_day1[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'years', .unit = 1)
saveRDS(USDCAD.AutoArima.d1y, './data/USDCAD.AutoArima.d1y.rds')

## USDCHF
USDCHF.AutoArima.d1y <- simAutoArima(mbase_day1[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'years', .unit = 1)
saveRDS(USDCHF.AutoArima.d1y, './data/USDCHF.AutoArima.d1y.rds')

## USDCNY
USDCNY.AutoArima.d1y <- simAutoArima(mbase_day1[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'years', .unit = 1)
saveRDS(USDCNY.AutoArima.d1y, './data/USDCNY.AutoArima.d1y.rds')

## USDJPY
USDJPY.AutoArima.d1y <- simAutoArima(mbase_day1[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, .verbose = TRUE, 
                                       .maPeriod = 'years', .unit = 1)
saveRDS(USDJPY.AutoArima.d1y, './data/USDJPY.AutoArima.d1y.rds')
