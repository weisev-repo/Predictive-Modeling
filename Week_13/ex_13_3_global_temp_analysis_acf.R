global_temp <- read.csv("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_13/Data/global_temp.csv",
                   sep=",", header=T)

global_temp.ts <- ts(global_temp[,2] , start = c(1850,2,1), end = c(2017,3,1), frequency = 12)

plot(global_temp.ts)
gird()

# taking the mean of every year
AnTemp = aggregate(global_temp.ts, nfrequency = 1, FUN = mean)
plot(AnTemp)
grid()

# correlogram
AnTemp.acf <- acf(AnTemp, lag.max = 100)
# partial correlogram
AnTemp.pacf <- pacf(AnTemp, lag.max = 100)

# The acf() shows that the time series is clearly no stationary

ar.model <- ar(AnTemp, aic=F, order.max = 4)
qqnorm(ar.model$resid)
plot(ar.model$resid)

# Predict mean annual temp. for next N years
pred <- predict(ar.model, n.ahead = 100)
plot(AnTemp, xlim = c(1850, 2217))
lines(pred$pred, col = "red")

# This results in a useless model since the acf is not decaying to zero in p = 4 
