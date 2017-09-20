## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.ETS.min5 <- simETS2(mbase_min5[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(AUDUSD.ETS.min5, './data/AUDUSD.ETS.min5.rds')

## EURUSD
EURUSD.ETS.min5 <- simETS2(mbase_min5[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(EURUSD.ETS.min5, './data/EURUSD.ETS.min5.rds')

## GBPUSD
GBPUSD.ETS.min5 <- simETS2(mbase_min5[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(GBPUSD.ETS.min5, './data/GBPUSD.ETS.min5.rds')

## USDCAD
USDCAD.ETS.min5 <- simETS2(mbase_min5[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDCAD.ETS.min5, './data/USDCAD.ETS.min5.rds')

## USDCHF
USDCHF.ETS.min5 <- simETS2(mbase_min5[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDCHF.ETS.min5, './data/USDCHF.ETS.min5.rds')

## USDCNY
USDCNY.ETS.min5 <- simETS2(mbase_min5[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDCNY.ETS.min5, './data/USDCNY.ETS.min5.rds')

## USDJPY
USDJPY.ETS.min5 <- simETS2(mbase_min5[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDJPY.ETS.min5, './data/USDJPY.ETS.min5.rds')
