## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'

## AUDUSD
AUDUSD.ETS.d1y <- simETS2(mbase_day1[[1]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(AUDUSD.ETS.d1y, './data/AUDUSD.ETS.d1y.rds')

## EURUSD
EURUSD.ETS.d1y <- simETS2(mbase_day1[[2]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(EURUSD.ETS.d1y, './data/EURUSD.ETS.d1y.rds')

## GBPUSD
GBPUSD.ETS.d1y <- simETS2(mbase_day1[[3]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(GBPUSD.ETS.d1y, './data/GBPUSD.ETS.d1y.rds')

## USDCAD
USDCAD.ETS.d1y <- simETS2(mbase_day1[[4]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDCAD.ETS.d1y, './data/USDCAD.ETS.d1y.rds')

## USDCHF
USDCHF.ETS.d1y <- simETS2(mbase_day1[[5]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDCHF.ETS.d1y, './data/USDCHF.ETS.d1y.rds')

## USDCNY
USDCNY.ETS.d1y <- simETS2(mbase_day1[[6]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDCNY.ETS.d1y, './data/USDCNY.ETS.d1y.rds')

## USDJPY
USDJPY.ETS.d1y <- simETS2(mbase_day1[[7]], .prCat = 'Cl', 
                                       .baseDate = .baseDate, 
                                       .maPeriod = 'years', .unit = 6, 
                                       .verbose = TRUE, .difftime = 'days')
saveRDS(USDJPY.ETS.d1y, './data/USDJPY.ETS.d1y.rds')

## ----- Due to error, here I use simAutoArima -----------------------------
## Using closing price to forecast.
.baseDate = '2014-01-01 00:00:00'
.baseDate = ymd(ymd_hms(.baseDate))

## 1 year
res <- llply(ets.m, function(x) {
  llply(mbase_day1, function(y) {
    z = simETS(y, .model = x, .prCat = 'Cl', .baseDate = .baseDate, 
               .maPeriod = 'years', .unit = 1, .verbose = TRUE, 
               .simulate = TRUE, .bootstrap = TRUE)
    nm = names(Cl(z)) %>% str_replace_all('.Close', '')
    txt1 = paste0('saveRDS(z', ', file = \'./data/', nm, '.', x, '.Cl.d1y.rds\'); rm(z)')
    eval(parse(text = txt1))
    cat(paste0(txt1, ' done!', '\n'))
  })
  cat(paste(x, 'done', '\n'))
})

