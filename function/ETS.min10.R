## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.ETS.min10 <- simETS2(mbase_min10[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(AUDUSD.ETS.min10, './data/AUDUSD.ETS.min10.rds')

## EURUSD
EURUSD.ETS.min10 <- simETS2(mbase_min10[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(EURUSD.ETS.min10, './data/EURUSD.ETS.min10.rds')

## GBPUSD
GBPUSD.ETS.min10 <- simETS2(mbase_min10[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(GBPUSD.ETS.min10, './data/GBPUSD.ETS.min10.rds')

## USDCAD
USDCAD.ETS.min10 <- simETS2(mbase_min10[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDCAD.ETS.min10, './data/USDCAD.ETS.min10.rds')

## USDCHF
USDCHF.ETS.min10 <- simETS2(mbase_min10[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDCHF.ETS.min10, './data/USDCHF.ETS.min10.rds')

## USDCNY
USDCNY.ETS.min10 <- simETS2(mbase_min10[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDCNY.ETS.min10, './data/USDCNY.ETS.min10.rds')

## USDJPY
USDJPY.ETS.min10 <- simETS2(mbase_min10[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDJPY.ETS.min10, './data/USDJPY.ETS.min10.rds')
