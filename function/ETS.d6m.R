## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.ETS.d6m <- simETS2(mbase_day1[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(AUDUSD.ETS.d6m, './data/AUDUSD.ETS.d6m.rds')

## EURUSD
EURUSD.ETS.d6m <- simETS2(mbase_day1[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(EURUSD.ETS.d6m, './data/EURUSD.ETS.d6m.rds')

## GBPUSD
GBPUSD.ETS.d6m <- simETS2(mbase_day1[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(GBPUSD.ETS.d6m, './data/GBPUSD.ETS.d6m.rds')

## USDCAD
USDCAD.ETS.d6m <- simETS2(mbase_day1[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDCAD.ETS.d6m, './data/USDCAD.ETS.d6m.rds')

## USDCHF
USDCHF.ETS.d6m <- simETS2(mbase_day1[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDCHF.ETS.d6m, './data/USDCHF.ETS.d6m.rds')

## USDCNY
USDCNY.ETS.d6m <- simETS2(mbase_day1[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDCNY.ETS.d6m, './data/USDCNY.ETS.d6m.rds')

## USDJPY
USDJPY.ETS.d6m <- simETS2(mbase_day1[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'months', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDJPY.ETS.d6m, './data/USDJPY.ETS.d6m.rds')

## ----- Due to error, here I use simAutoArima -----------------------------
## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'
.baseDate = ymd(ymd_hms(.baseDate))

res <- llply(ets.m, function(x) {
  llply(mbase_day1, function(y) {
    z = simETS(y, .model = x, .prCat = 'Cl', .baseDate = .baseDate, 
               .maPeriod = 'months', .unit = 6, .verbose = TRUE, 
               .simulate = TRUE, .bootstrap = TRUE)
    txt1 <- paste0('saveRDS(z', ', file = \'./data/', x, '.Cl.d6m.rds\'); rm(z)')
    eval(parse(text = txt1))
    cat(paste0(txt1, ' done!', '\n'))
  })
  cat(paste(x, 'done', '\n'))
})

