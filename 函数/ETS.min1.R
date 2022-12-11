## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.ETS.min1 <- simETS2(mbase_min1[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 6, 
                                       .difftime = 'mins')
saveRDS(AUDUSD.ETS.min1, './data/AUDUSD.ETS.min1.rds')

## EURUSD
EURUSD.ETS.min1 <- simETS2(mbase_min1[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 6, 
                                       .difftime = 'mins')
saveRDS(EURUSD.ETS.min1, './data/EURUSD.ETS.min1.rds')

## GBPUSD
GBPUSD.ETS.min1 <- simETS2(mbase_min1[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 6, 
                                       .difftime = 'mins')
saveRDS(GBPUSD.ETS.min1, './data/GBPUSD.ETS.min1.rds')

## USDCAD
USDCAD.ETS.min1 <- simETS2(mbase_min1[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 6, 
                                       .difftime = 'mins')
saveRDS(USDCAD.ETS.min1, './data/USDCAD.ETS.min1.rds')

## USDCHF
USDCHF.ETS.min1 <- simETS2(mbase_min1[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 6, 
                                       .difftime = 'mins')
saveRDS(USDCHF.ETS.min1, './data/USDCHF.ETS.min1.rds')

## USDCNY
USDCNY.ETS.min1 <- simETS2(mbase_min1[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 6, 
                                       .difftime = 'mins')
saveRDS(USDCNY.ETS.min1, './data/USDCNY.ETS.min1.rds')

## USDJPY
USDJPY.ETS.min1 <- simETS2(mbase_min1[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 6, 
                                       .difftime = 'mins')
saveRDS(USDJPY.ETS.min1, './data/USDJPY.ETS.min1.rds')
