## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.AutoArima.min3 <- simAutoArima2(mbase_min3[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 12, 
                                       .difftime = 'mins')
saveRDS(AUDUSD.AutoArima.min3, './data/AUDUSD.AutoArima.min3.rds')

## EURUSD
EURUSD.AutoArima.min3 <- simAutoArima2(mbase_min3[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 12, 
                                       .difftime = 'mins')
saveRDS(EURUSD.AutoArima.min3, './data/EURUSD.AutoArima.min3.rds')

## GBPUSD
GBPUSD.AutoArima.min3 <- simAutoArima2(mbase_min3[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 12, 
                                       .difftime = 'mins')
saveRDS(GBPUSD.AutoArima.min3, './data/GBPUSD.AutoArima.min3.rds')

## USDCAD
USDCAD.AutoArima.min3 <- simAutoArima2(mbase_min3[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 12, 
                                       .difftime = 'mins')
saveRDS(USDCAD.AutoArima.min3, './data/USDCAD.AutoArima.min3.rds')

## USDCHF
USDCHF.AutoArima.min3 <- simAutoArima2(mbase_min3[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 12, 
                                       .difftime = 'mins')
saveRDS(USDCHF.AutoArima.min3, './data/USDCHF.AutoArima.min3.rds')

## USDCNY
USDCNY.AutoArima.min3 <- simAutoArima2(mbase_min3[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 12, 
                                       .difftime = 'mins')
saveRDS(USDCNY.AutoArima.min3, './data/USDCNY.AutoArima.min3.rds')

## USDJPY
USDJPY.AutoArima.min3 <- simAutoArima2(mbase_min3[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'hours', .unit = 12, 
                                       .difftime = 'mins')
saveRDS(USDJPY.AutoArima.min3, './data/USDJPY.AutoArima.min3.rds')
