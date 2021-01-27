# --------- eval=FALSE ---------
timeID <- unique(dsmp$date)
bse <- dsmp[year == 2016]$date[1] #"2016-01-04" #1st trading date in 2nd year
timeID %<>% .[. >= bse]
#timeID %<>% .[. >= as_date('2016-01-04')]
data_len <- 7200 #last 7200 observations DT[(.N - (data_len - 1)):.N]
hrz1 <- 1440

i = 1
if(i %in% seq(1, length(timeID), by = 6)) {
  train <- dsmp[date < timeID[i]][(.N - (data_len - 1)):.N]
  ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
  train_test <- dsmp[sq %in% ctr]
  list(train = train, train_test = train_test)
} else {
  lst_sq <- dsmp[date < timeID[i],][.N]$sq + 1
  train <- dsmp[(lst_sq - data_len + 1):lst_sq]
  ctr <- train$sq[1]:(range(train$sq)[2] + hrz1)
  train_test <- dsmp[sq %in% ctr]
  list(train = train, train_test = train_test)
}

sets <- train[, .(index, close)] %>% 
  tk_ts(frequency = hrz1) %>% 
  forecast(h = hrz1) %>% 
  tk_tbl %>% 
  dplyr::mutate(index = train_test[(.N - hrz1 + 1):.N,]$index, 
	            mk.price = train_test[(.N - hrz1 + 1):.N,]$close) %>% 
  dplyr::rename(fc.price = `Point Forecast`) %>% 
  dplyr::select(index, mk.price, fc.price) %>% 
  as.data.table

sets

range(train_test[(.N - hrz1 + 1):.N,]$index)


##---------------------------------

i = 2
train <- dsmp[date < timeID[i]][(.N - (data_len - 1)):.N]
ctr <- (train[,(sq)][1]):(train[.N,(sq)] + hrz1)
train_test <- dsmp[sq %in% ctr]

train
train_test



