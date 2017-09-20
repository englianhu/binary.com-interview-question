## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.AutoArima.min10 <- simAutoArima2(mbase_min10[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(AUDUSD.AutoArima.min10, './data/AUDUSD.AutoArima.min10.rds')

## EURUSD
EURUSD.AutoArima.min10 <- simAutoArima2(mbase_min10[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(EURUSD.AutoArima.min10, './data/EURUSD.AutoArima.min10.rds')

## GBPUSD
GBPUSD.AutoArima.min10 <- simAutoArima2(mbase_min10[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(GBPUSD.AutoArima.min10, './data/GBPUSD.AutoArima.min10.rds')

## USDCAD
USDCAD.AutoArima.min10 <- simAutoArima2(mbase_min10[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDCAD.AutoArima.min10, './data/USDCAD.AutoArima.min10.rds')

## USDCHF
USDCHF.AutoArima.min10 <- simAutoArima2(mbase_min10[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDCHF.AutoArima.min10, './data/USDCHF.AutoArima.min10.rds')

## USDCNY
USDCNY.AutoArima.min10 <- simAutoArima2(mbase_min10[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDCNY.AutoArima.min10, './data/USDCNY.AutoArima.min10.rds')

## USDJPY
USDJPY.AutoArima.min10 <- simAutoArima2(mbase_min10[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 1, 
                                       .difftime = 'mins')
saveRDS(USDJPY.AutoArima.min10, './data/USDJPY.AutoArima.min10.rds')
