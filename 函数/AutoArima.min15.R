## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.AutoArima.min15 <- simAutoArima2(mbase_min15[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'mins')
saveRDS(AUDUSD.AutoArima.min15, './data/AUDUSD.AutoArima.min15.rds')

## EURUSD
EURUSD.AutoArima.min15 <- simAutoArima2(mbase_min15[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'mins')
saveRDS(EURUSD.AutoArima.min15, './data/EURUSD.AutoArima.min15.rds')

## GBPUSD
GBPUSD.AutoArima.min15 <- simAutoArima2(mbase_min15[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'mins')
saveRDS(GBPUSD.AutoArima.min15, './data/GBPUSD.AutoArima.min15.rds')

## USDCAD
USDCAD.AutoArima.min15 <- simAutoArima2(mbase_min15[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'mins')
saveRDS(USDCAD.AutoArima.min15, './data/USDCAD.AutoArima.min15.rds')

## USDCHF
USDCHF.AutoArima.min15 <- simAutoArima2(mbase_min15[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'mins')
saveRDS(USDCHF.AutoArima.min15, './data/USDCHF.AutoArima.min15.rds')

## USDCNY
USDCNY.AutoArima.min15 <- simAutoArima2(mbase_min15[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'mins')
saveRDS(USDCNY.AutoArima.min15, './data/USDCNY.AutoArima.min15.rds')

## USDJPY
USDJPY.AutoArima.min15 <- simAutoArima2(mbase_min15[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'mins')
saveRDS(USDJPY.AutoArima.min15, './data/USDJPY.AutoArima.min15.rds')
