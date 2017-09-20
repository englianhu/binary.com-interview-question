## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.AutoArima.h1 <- simAutoArima2(mbase_hrs1[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .difftime = 'hours')
saveRDS(AUDUSD.AutoArima.h1, './data/AUDUSD.AutoArima.h1.rds')

## EURUSD
EURUSD.AutoArima.h1 <- simAutoArima2(mbase_hrs1[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .difftime = 'hours')
saveRDS(EURUSD.AutoArima.h1, './data/EURUSD.AutoArima.h1.rds')

## GBPUSD
GBPUSD.AutoArima.h1 <- simAutoArima2(mbase_hrs1[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .difftime = 'hours')
saveRDS(GBPUSD.AutoArima.h1, './data/GBPUSD.AutoArima.h1.rds')

## USDCAD
USDCAD.AutoArima.h1 <- simAutoArima2(mbase_hrs1[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .difftime = 'hours')
saveRDS(USDCAD.AutoArima.h1, './data/USDCAD.AutoArima.h1.rds')

## USDCHF
USDCHF.AutoArima.h1 <- simAutoArima2(mbase_hrs1[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .difftime = 'hours')
saveRDS(USDCHF.AutoArima.h1, './data/USDCHF.AutoArima.h1.rds')

## USDCNY
USDCNY.AutoArima.h1 <- simAutoArima2(mbase_hrs1[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .difftime = 'hours')
saveRDS(USDCNY.AutoArima.h1, './data/USDCNY.AutoArima.h1.rds')

## USDJPY
USDJPY.AutoArima.h1 <- simAutoArima2(mbase_hrs1[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .difftime = 'hours')
saveRDS(USDJPY.AutoArima.h1, './data/USDJPY.AutoArima.h1.rds')
