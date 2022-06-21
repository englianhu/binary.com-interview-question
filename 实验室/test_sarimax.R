# some random data
x <- ts(rnorm(120,0,3) + 1:120 + 20*sin(2*pi*(1:120)/12), frequency=12)
temp = rnorm(length(x), 20, 30)

require(forecast)
# build the model (check ?auto.arima)
model = auto.arima(x, xreg = temp)

# some random predictors
temp.reg = data.frame(temp = rnorm(10, 20, 30))

# forecasting
forec = forecast(model, xreg = temp.reg)

# quick way to visualize things
plot(forec)

# model diagnosis
tsdiag(model)

# model info
summary(forec)





