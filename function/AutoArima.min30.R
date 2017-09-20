## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.AutoArima.min30 <- simAutoArima2(mbase_min30[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 7, 
                                       .difftime = 'mins')
saveRDS(AUDUSD.AutoArima.min30, './data/AUDUSD.AutoArima.min30.rds')

## EURUSD
EURUSD.AutoArima.min30 <- simAutoArima2(mbase_min30[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 7, 
                                       .difftime = 'mins')
saveRDS(EURUSD.AutoArima.min30, './data/EURUSD.AutoArima.min30.rds')

## GBPUSD
GBPUSD.AutoArima.min30 <- simAutoArima2(mbase_min30[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 7, 
                                       .difftime = 'mins')
saveRDS(GBPUSD.AutoArima.min30, './data/GBPUSD.AutoArima.min30.rds')

## USDCAD
USDCAD.AutoArima.min30 <- simAutoArima2(mbase_min30[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 7, 
                                       .difftime = 'mins')
saveRDS(USDCAD.AutoArima.min30, './data/USDCAD.AutoArima.min30.rds')

## USDCHF
USDCHF.AutoArima.min30 <- simAutoArima2(mbase_min30[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 7, 
                                       .difftime = 'mins')
saveRDS(USDCHF.AutoArima.min30, './data/USDCHF.AutoArima.min30.rds')

## USDCNY
USDCNY.AutoArima.min30 <- simAutoArima2(mbase_min30[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 7, 
                                       .difftime = 'mins')
saveRDS(USDCNY.AutoArima.min30, './data/USDCNY.AutoArima.min30.rds')

## USDJPY
USDJPY.AutoArima.min30 <- simAutoArima2(mbase_min30[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 7, 
                                       .difftime = 'mins')
saveRDS(USDJPY.AutoArima.min30, './data/USDJPY.AutoArima.min30.rds')
