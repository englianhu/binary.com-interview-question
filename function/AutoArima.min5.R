## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.AutoArima.min5 <- simAutoArima2(mbase_min5[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(AUDUSD.AutoArima.min5, './data/AUDUSD.AutoArima.min5.rds')

## EURUSD
EURUSD.AutoArima.min5 <- simAutoArima2(mbase_min5[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(EURUSD.AutoArima.min5, './data/EURUSD.AutoArima.min5.rds')

## GBPUSD
GBPUSD.AutoArima.min5 <- simAutoArima2(mbase_min5[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(GBPUSD.AutoArima.min5, './data/GBPUSD.AutoArima.min5.rds')

## USDCAD
USDCAD.AutoArima.min5 <- simAutoArima2(mbase_min5[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDCAD.AutoArima.min5, './data/USDCAD.AutoArima.min5.rds')

## USDCHF
USDCHF.AutoArima.min5 <- simAutoArima2(mbase_min5[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDCHF.AutoArima.min5, './data/USDCHF.AutoArima.min5.rds')

## USDCNY
USDCNY.AutoArima.min5 <- simAutoArima2(mbase_min5[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDCNY.AutoArima.min5, './data/USDCNY.AutoArima.min5.rds')

## USDJPY
USDJPY.AutoArima.min5 <- simAutoArima2(mbase_min5[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDJPY.AutoArima.min5, './data/USDJPY.AutoArima.min5.rds')
