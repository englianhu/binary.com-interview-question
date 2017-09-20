## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.ETS.min3 <- simETS2(mbase_min3[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 12, 
                                       .difftime = 'mins')
saveRDS(AUDUSD.ETS.min3, './data/AUDUSD.ETS.min3.rds')

## EURUSD
EURUSD.ETS.min3 <- simETS2(mbase_min3[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 12, 
                                       .difftime = 'mins')
saveRDS(EURUSD.ETS.min3, './data/EURUSD.ETS.min3.rds')

## GBPUSD
GBPUSD.ETS.min3 <- simETS2(mbase_min3[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 12, 
                                       .difftime = 'mins')
saveRDS(GBPUSD.ETS.min3, './data/GBPUSD.ETS.min3.rds')

## USDCAD
USDCAD.ETS.min3 <- simETS2(mbase_min3[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 12, 
                                       .difftime = 'mins')
saveRDS(USDCAD.ETS.min3, './data/USDCAD.ETS.min3.rds')

## USDCHF
USDCHF.ETS.min3 <- simETS2(mbase_min3[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 12, 
                                       .difftime = 'mins')
saveRDS(USDCHF.ETS.min3, './data/USDCHF.ETS.min3.rds')

## USDCNY
USDCNY.ETS.min3 <- simETS2(mbase_min3[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 12, 
                                       .difftime = 'mins')
saveRDS(USDCNY.ETS.min3, './data/USDCNY.ETS.min3.rds')

## USDJPY
USDJPY.ETS.min3 <- simETS2(mbase_min3[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 12, 
                                       .difftime = 'mins')
saveRDS(USDJPY.ETS.min3, './data/USDJPY.ETS.min3.rds')
