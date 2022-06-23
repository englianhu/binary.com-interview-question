## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.ETS.h1 <- simETS2(mbase_hrs1[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .verbose = TRUE, .difftime = 'hours')
saveRDS(AUDUSD.ETS.h1, './data/AUDUSD.ETS.h1.rds')

## EURUSD
EURUSD.ETS.h1 <- simETS2(mbase_hrs1[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .verbose = TRUE, .difftime = 'hours')
saveRDS(EURUSD.ETS.h1, './data/EURUSD.ETS.h1.rds')

## GBPUSD
GBPUSD.ETS.h1 <- simETS2(mbase_hrs1[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .verbose = TRUE, .difftime = 'hours')
saveRDS(GBPUSD.ETS.h1, './data/GBPUSD.ETS.h1.rds')

## USDCAD
USDCAD.ETS.h1 <- simETS2(mbase_hrs1[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .verbose = TRUE, .difftime = 'hours')
saveRDS(USDCAD.ETS.h1, './data/USDCAD.ETS.h1.rds')

## USDCHF
USDCHF.ETS.h1 <- simETS2(mbase_hrs1[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .verbose = TRUE, .difftime = 'hours')
saveRDS(USDCHF.ETS.h1, './data/USDCHF.ETS.h1.rds')

## USDCNY
USDCNY.ETS.h1 <- simETS2(mbase_hrs1[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .verbose = TRUE, .difftime = 'hours')
saveRDS(USDCNY.ETS.h1, './data/USDCNY.ETS.h1.rds')

## USDJPY
USDJPY.ETS.h1 <- simETS2(mbase_hrs1[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .verbose = TRUE, .difftime = 'hours')
saveRDS(USDJPY.ETS.h1, './data/USDJPY.ETS.h1.rds')
