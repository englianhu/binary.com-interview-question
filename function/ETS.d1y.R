## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.ETS.d1y <- simETS2(mbase_day1[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 6, 
                                       .difftime = 'days')
saveRDS(AUDUSD.ETS.d1y, './data/AUDUSD.ETS.d1y.rds')

## EURUSD
EURUSD.ETS.d1y <- simETS2(mbase_day1[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 6, 
                                       .difftime = 'days')
saveRDS(EURUSD.ETS.d1y, './data/EURUSD.ETS.d1y.rds')

## GBPUSD
GBPUSD.ETS.d1y <- simETS2(mbase_day1[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 6, 
                                       .difftime = 'days')
saveRDS(GBPUSD.ETS.d1y, './data/GBPUSD.ETS.d1y.rds')

## USDCAD
USDCAD.ETS.d1y <- simETS2(mbase_day1[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 6, 
                                       .difftime = 'days')
saveRDS(USDCAD.ETS.d1y, './data/USDCAD.ETS.d1y.rds')

## USDCHF
USDCHF.ETS.d1y <- simETS2(mbase_day1[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 6, 
                                       .difftime = 'days')
saveRDS(USDCHF.ETS.d1y, './data/USDCHF.ETS.d1y.rds')

## USDCNY
USDCNY.ETS.d1y <- simETS2(mbase_day1[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 6, 
                                       .difftime = 'days')
saveRDS(USDCNY.ETS.d1y, './data/USDCNY.ETS.d1y.rds')

## USDJPY
USDJPY.ETS.d1y <- simETS2(mbase_day1[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 6, 
                                       .difftime = 'days')
saveRDS(USDJPY.ETS.d1y, './data/USDJPY.ETS.d1y.rds')
