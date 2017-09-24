## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.ETS.min15 <- simETS2(mbase_min15[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'mins')
saveRDS(AUDUSD.ETS.min15, './data/AUDUSD.ETS.min15.rds')

## EURUSD
EURUSD.ETS.min15 <- simETS2(mbase_min15[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'mins')
saveRDS(EURUSD.ETS.min15, './data/EURUSD.ETS.min15.rds')

## GBPUSD
GBPUSD.ETS.min15 <- simETS2(mbase_min15[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'mins')
saveRDS(GBPUSD.ETS.min15, './data/GBPUSD.ETS.min15.rds')

## USDCAD
USDCAD.ETS.min15 <- simETS2(mbase_min15[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'mins')
saveRDS(USDCAD.ETS.min15, './data/USDCAD.ETS.min15.rds')

## USDCHF
USDCHF.ETS.min15 <- simETS2(mbase_min15[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'mins')
saveRDS(USDCHF.ETS.min15, './data/USDCHF.ETS.min15.rds')

## USDCNY
USDCNY.ETS.min15 <- simETS2(mbase_min15[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'mins')
saveRDS(USDCNY.ETS.min15, './data/USDCNY.ETS.min15.rds')

## USDJPY
USDJPY.ETS.min15 <- simETS2(mbase_min15[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'days', .unit = 3, 
                                       .verbose = TRUE, .difftime = 'mins')
saveRDS(USDJPY.ETS.min15, './data/USDJPY.ETS.min15.rds')
