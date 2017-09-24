## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD ------------------------------------------------------------------------
AUDUSD.AutoArima.h1 <- simAutoArima2(mbase_hrs1[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .verbose = TRUE, .difftime = 'hours')
#...
#frd= 1 ;dt= 2017-05-15 20:59:00 
#frd= 1 ;dt= 2017-05-15 21:59:00 
#frd= 1 ;dt= 2017-05-15 22:59:00 
#frd= 0.983333333333333 ;dt= 2017-05-15 23:58:00 
#Error in ts(z[[1L]] + xm, start = xtsp[2L] + deltat(rsd), frequency = xtsp[3L]) : 
#  'ts' object must have one or more observations
#> saveRDS(AUDUSD.AutoArima.h1, './data/AUDUSD.AutoArima.h1.rds')
#Error in saveRDS(AUDUSD.AutoArima.h1, "./data/AUDUSD.AutoArima.h1.rds") : 
#  object 'AUDUSD.AutoArima.h1' not found
saveRDS(AUDUSD.AutoArima.h1, './data/AUDUSD.AutoArima.h1.rds')


## EURUSD ------------------------------------------------------------------------
EURUSD.AutoArima.h1 <- simAutoArima2(mbase_hrs1[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .verbose = TRUE, .difftime = 'hours')
#...
#frd= 1 ;dt= 2017-05-15 20:59:00 
#frd= 1 ;dt= 2017-05-15 21:59:00 
#frd= 1 ;dt= 2017-05-15 22:59:00 
#frd= 0.966666666666667 ;dt= 2017-05-15 23:57:00 
#Error in ts(z[[1L]] + xm, start = xtsp[2L] + deltat(rsd), frequency = xtsp[3L]) : 
#  'ts' object must have one or more observations
#> saveRDS(EURUSD.AutoArima.h1, './data/EURUSD.AutoArima.h1.rds')
#Error in saveRDS(EURUSD.AutoArima.h1, "./data/EURUSD.AutoArima.h1.rds") : 
#  object 'EURUSD.AutoArima.h1' not found
saveRDS(EURUSD.AutoArima.h1, './data/EURUSD.AutoArima.h1.rds')


## GBPUSD ------------------------------------------------------------------------
GBPUSD.AutoArima.h1 <- simAutoArima2(mbase_hrs1[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .verbose = TRUE, .difftime = 'hours')
#...
#frd= 1 ;dt= 2017-05-15 20:59:00 
#frd= 1 ;dt= 2017-05-15 21:59:00 
#frd= 1 ;dt= 2017-05-15 22:59:00 
#frd= 0.983333333333333 ;dt= 2017-05-15 23:58:00 
#Error in ts(z[[1L]] + xm, start = xtsp[2L] + deltat(rsd), frequency = xtsp[3L]) : 
#  'ts' object must have one or more observations
#> saveRDS(GBPUSD.AutoArima.h1, './data/GBPUSD.AutoArima.h1.rds')
#Error in saveRDS(GBPUSD.AutoArima.h1, "./data/GBPUSD.AutoArima.h1.rds") : 
#  object 'GBPUSD.AutoArima.h1' not found
saveRDS(GBPUSD.AutoArima.h1, './data/GBPUSD.AutoArima.h1.rds')


## USDCAD ------------------------------------------------------------------------
USDCAD.AutoArima.h1 <- simAutoArima2(mbase_hrs1[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .verbose = TRUE, .difftime = 'hours')
#...
#frd= 1 ;dt= 2017-05-15 20:59:00 
#frd= 1 ;dt= 2017-05-15 21:59:00 
#frd= 1 ;dt= 2017-05-15 22:59:00 
#frd= 0.983333333333333 ;dt= 2017-05-15 23:58:00 
#Error in as.matrix(newxreg) %*% coefs[-(1L:narma)] : 
#  non-conformable arguments
#> saveRDS(USDCAD.AutoArima.h1, './data/USDCAD.AutoArima.h1.rds')
#Error in saveRDS(USDCAD.AutoArima.h1, "./data/USDCAD.AutoArima.h1.rds") : 
#  object 'USDCAD.AutoArima.h1' not found
saveRDS(USDCAD.AutoArima.h1, './data/USDCAD.AutoArima.h1.rds')


## USDCHF ------------------------------------------------------------------------
USDCHF.AutoArima.h1 <- simAutoArima2(mbase_hrs1[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .verbose = TRUE, .difftime = 'hours')
#...
#frd= 1 ;dt= 2017-05-15 20:59:00 
#frd= 1 ;dt= 2017-05-15 21:59:00 
#frd= 1 ;dt= 2017-05-15 22:59:00 
#frd= 0.983333333333333 ;dt= 2017-05-15 23:58:00 
#Error in ts(z[[1L]] + xm, start = xtsp[2L] + deltat(rsd), frequency = xtsp[3L]) : 
#  'ts' object must have one or more observations
#> saveRDS(USDCHF.AutoArima.h1, './data/USDCHF.AutoArima.h1.rds')
#Error in saveRDS(USDCHF.AutoArima.h1, "./data/USDCHF.AutoArima.h1.rds") : 
#  object 'USDCHF.AutoArima.h1' not found
saveRDS(USDCHF.AutoArima.h1, './data/USDCHF.AutoArima.h1.rds')


## USDCNY ------------------------------------------------------------------------
USDCNY.AutoArima.h1 <- simAutoArima2(mbase_hrs1[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .verbose = TRUE, .difftime = 'hours')
#...
#frd= 1 ;dt= 2017-05-15 19:59:00 
#frd= 1 ;dt= 2017-05-15 20:59:00 
#frd= 0.6 ;dt= 2017-05-15 21:35:00 
#Error in ts(z[[1L]] + xm, start = xtsp[2L] + deltat(rsd), frequency = xtsp[3L]) : 
#  'ts' object must have one or more observations
#> saveRDS(USDCNY.AutoArima.h1, './data/USDCNY.AutoArima.h1.rds')
#Error in saveRDS(USDCNY.AutoArima.h1, "./data/USDCNY.AutoArima.h1.rds") : 
#  object 'USDCNY.AutoArima.h1' not found
saveRDS(USDCNY.AutoArima.h1, './data/USDCNY.AutoArima.h1.rds')


## USDJPY ------------------------------------------------------------------------
USDJPY.AutoArima.h1 <- simAutoArima2(mbase_hrs1[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'weeks', .unit = 2, 
                                       .verbose = TRUE, .difftime = 'hours')
#...
#frd= 1 ;dt= 2017-05-15 20:59:00 
#frd= 1 ;dt= 2017-05-15 21:59:00 
#frd= 1 ;dt= 2017-05-15 22:59:00 
#frd= 0.983333333333333 ;dt= 2017-05-15 23:58:00 
#Error in ts(z[[1L]] + xm, start = xtsp[2L] + deltat(rsd), frequency = xtsp[3L]) : 
#  'ts' object must have one or more observations
#> saveRDS(USDJPY.AutoArima.h1, './data/USDJPY.AutoArima.h1.rds')
#Error in saveRDS(USDJPY.AutoArima.h1, "./data/USDJPY.AutoArima.h1.rds") : 
#  object 'USDJPY.AutoArima.h1' not found
saveRDS(USDJPY.AutoArima.h1, './data/USDJPY.AutoArima.h1.rds')
