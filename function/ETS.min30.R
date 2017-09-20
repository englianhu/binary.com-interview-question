## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.ETS.min30 <- simETS2(mbase_min30[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 7, 
                                       .difftime = 'mins')
saveRDS(AUDUSD.ETS.min30, './data/AUDUSD.ETS.min30.rds')

## EURUSD
EURUSD.ETS.min30 <- simETS2(mbase_min30[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 7, 
                                       .difftime = 'mins')
saveRDS(EURUSD.ETS.min30, './data/EURUSD.ETS.min30.rds')

## GBPUSD
GBPUSD.ETS.min30 <- simETS2(mbase_min30[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 7, 
                                       .difftime = 'mins')
saveRDS(GBPUSD.ETS.min30, './data/GBPUSD.ETS.min30.rds')

## USDCAD
USDCAD.ETS.min30 <- simETS2(mbase_min30[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 7, 
                                       .difftime = 'mins')
saveRDS(USDCAD.ETS.min30, './data/USDCAD.ETS.min30.rds')

## USDCHF
USDCHF.ETS.min30 <- simETS2(mbase_min30[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 7, 
                                       .difftime = 'mins')
saveRDS(USDCHF.ETS.min30, './data/USDCHF.ETS.min30.rds')

## USDCNY
USDCNY.ETS.min30 <- simETS2(mbase_min30[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 7, 
                                       .difftime = 'mins')
saveRDS(USDCNY.ETS.min30, './data/USDCNY.ETS.min30.rds')

## USDJPY
USDJPY.ETS.min30 <- simETS2(mbase_min30[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 7, 
                                       .difftime = 'mins')
saveRDS(USDJPY.ETS.min30, './data/USDJPY.ETS.min30.rds')
