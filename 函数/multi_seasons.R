multi_seasons <- function(mbase, seasonal_periods = c(1440, 7200), auto_arima = FALSE, 
                          start = decimal_date(as_datetime('2016-01-05 00:00:00'))) {
  mbase <- msts(tk_ts(mbase), seasonal.periods = seasonal_periods, start = start)
  
  if (auto_arima == TRUE) {
    fit <- auto.arima(mbase, D = 1)
  } else {
    fit <- tbats(mbase)
  }
  forecast(fit, ahead = 1)
  }
