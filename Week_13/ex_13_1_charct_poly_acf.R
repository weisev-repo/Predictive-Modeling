# characterstic polynomial and its roots

zeros <- polyroot(c(1, -1.21, 0.5, 0.13, -0.24))

# is process stationary?
abs(zeros)
# true if absolute values of the zeros are 
# all larger than 1

# plot the time series by given autoregression (ar)
for (i in 1:4) {
  my.ts <- arima.sim(model = list(ar = c(1.21, -0.5, -0.13, 0.24)), n = 200)
  plot(my.ts, xlab = "")
}

# autocorrealtion function
acf <- ARMAacf(ar = c(1.21, -0.5, -0.13, 0.24), pacf = F, lag.max = 50)
plot(acf, type="h", main="ACF")

# partial autocorrelation function
pacf <- ARMAacf(ar = c(1.21, -0.5, -0.13, 0.24), pacf = T, lag.max = 50)
plot(pacf, type="h", main="PACF")


