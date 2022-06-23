
#intraday (15 mins delay)
f.get.google.intraday <- function(symbol, freq, period) {
  ## https://github.com/frederickpelchat/quantitative-finance/blob/master/intraday-data.R
  library('quantmod')
  library('xts')
  library('TTR')
  
  #'@ base.url <- 'http://www.google.com/finance/getprices?'
  #'@ options.url <- paste('i=', freq, '&p=', period, '&f=d,o,h,l,c,v&df=cpct&q=', symbol, sep = '')
  #'@ full.url <- paste(base.url, options.url, sep = '')
  full.url <- paste0('https://finance.google.com/finance/getprices?i=', freq,'&p=', period, '&f=d,o,h,l,c,v&q=', symbol)
  data <- read.csv(full.url, skip = 7, header = FALSE, stringsAsFactors = FALSE)
  
  starting.times.idx <- which(substring(data$V1, 1, 1) == 'a')
  ending.seconds.idx <- c(starting.times.idx[-1] - 1, nrow(data))
  r.str.idx.use <- paste(starting.times.idx, ':', ending.seconds.idx, sep = '')
  
  starting.times <- as.numeric(substring(data[starting.times.idx, 1], 2))
  
  data[starting.times.idx, 1] <- 0
  clean.idx <- do.call(c, lapply(seq(1, length(r.str.idx.use)),
                                 function(i) {
                                   starting.times[i] + freq * as.numeric(data[eval(parse(text = r.str.idx.use[i])), 1])
                                 })
  )
  data.xts <- xts(data[,-1], as.POSIXct(clean.idx, origin = '1970-01-01', tz = 'GMT'))
  
  indexTZ(data.xts) <- 'America/New_York'
  colnames(data.xts) <- c('Open', 'High', 'Low', 'Close', 'Volume')
  
  data.xts
  }

#csv content
f.get.raw.daily.ticks <- function(sym, date, exchange) {
  if(exchange == 'AMEX') {
    exch <- 'A'
  } else if(exchange == 'NYSE') {
    exch <- 'N'
  } else if(exchange == 'NASDAQ' || exchange == 'OTC') {
    exch <- 'O'
  } else {
    cat('Unknown Exchange')
    return
  }
  link <- paste('http://hopey.netfonds.no/tradedump.php?csv_format=csv&date=', date, '&paper=', sym, '.', exch, sep = '')
  read.csv(link, stringsAsFactors = FALSE)
}

#xts object
f.get.daily.ticks <- function(sym, date, exchange) {
  raw <- f.get.raw.daily.ticks(sym, date, exchange)
  idx <- as.POSIXct(strptime(as.character(raw$time), format='%Y%m%dT%H%M%S', tz='Europe/Oslo'))
  attr(idx, 'tzone') <- 'America/New_York'
  raw$cvolume <- apply(raw, 1, function(row) as.numeric(row[2]) * as.integer(row[3])) #1=row iterator
  xts(raw[c('price', 'quantity', 'cvolume')], idx)
}

#aggregate tick data to nsecs (ohlc format + volume + $volume)
f.aggr.xts <- function(dat, nsecs) {
  dat.xts <- as.xts(dat)
  candlesticks <- period.apply(dat.xts, endpoints(dat.xts, 'secs', nsecs),
                               function(x) {
                                 ticks = coredata(x$price)
                                 c(first(ticks), max(ticks), min(ticks), last(ticks), sum(x$quantity), sum(x$cvolume))
                               }
  )
  colnames(candlesticks) <- c('Open', 'High', 'Low', 'Close', 'Volume', '$Volume')
  align.time(candlesticks, nsecs)
}

#alias for the most popular intraday time periods
f.aggr.1m <- function(dat) f.aggr.xts(dat, 60)
f.aggr.5m <- function(dat) f.aggr.xts(dat, 60 * 5)
f.aggr.10m <- function(dat) f.aggr.xts(dat, 60 * 10)
f.aggr.15m <- function(dat) f.aggr.xts(dat, 60 * 15)
f.aggr.30m <- function(dat) f.aggr.xts(dat, 60 * 30)
f.aggr.1h <- function(dat) f.aggr.xts(dat, 60 * 60)
f.aggr.1d <- function(dat) f.aggr.xts(dat, 60 * 60 * 24)

