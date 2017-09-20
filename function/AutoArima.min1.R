## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.AutoArima.min1 <- simAutoArima2(mbase_min1[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 6, 
                                       .difftime = 'mins')
saveRDS(AUDUSD.AutoArima.min1, './data/AUDUSD.AutoArima.min1.rds')

## EURUSD
EURUSD.AutoArima.min1 <- simAutoArima2(mbase_min1[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 6, 
                                       .difftime = 'mins')
saveRDS(EURUSD.AutoArima.min1, './data/EURUSD.AutoArima.min1.rds')

## GBPUSD
GBPUSD.AutoArima.min1 <- simAutoArima2(mbase_min1[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 6, 
                                       .difftime = 'mins')
saveRDS(GBPUSD.AutoArima.min1, './data/GBPUSD.AutoArima.min1.rds')

## USDCAD
USDCAD.AutoArima.min1 <- simAutoArima2(mbase_min1[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 6, 
                                       .difftime = 'mins')
saveRDS(USDCAD.AutoArima.min1, './data/USDCAD.AutoArima.min1.rds')

## USDCHF
USDCHF.AutoArima.min1 <- simAutoArima2(mbase_min1[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 6, 
                                       .difftime = 'mins')
saveRDS(USDCHF.AutoArima.min1, './data/USDCHF.AutoArima.min1.rds')

## USDCNY
USDCNY.AutoArima.min1 <- simAutoArima2(mbase_min1[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 6, 
                                       .difftime = 'mins')
saveRDS(USDCNY.AutoArima.min1, './data/USDCNY.AutoArima.min1.rds')

## USDJPY
USDJPY.AutoArima.min1 <- simAutoArima2(mbase_min1[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 6, 
                                       .difftime = 'mins')
saveRDS(USDJPY.AutoArima.min1, './data/USDJPY.AutoArima.min1.rds')
