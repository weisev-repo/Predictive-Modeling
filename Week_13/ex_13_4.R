load("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_13/Data/force.Rdata")
plot(force)

force.train = window(force, end = 280)

par(mfrow = c(1, 2), mar = c(3, 2, 3, 2))
acf(force.train, main = "acf")
pacf(force.train, main = "pacf")


# choose order p, so that pacf is not
# zero -> p = 7

model = ar(force.train, aic = F, order.max = 7)
qqnorm(model$resid)

hist(model$resid, nclass = 20)


force.pred = predict(model, n.ahead = 40)$pred # prediction
force.se = predict(model, n.ahead = 40)$se # standard error
plot(force, lty = 3)
lines(force.train)
lines(force.pred, col = "red")

# 95% confidence intervals (optional)
lines(force.pred + 1.96 * force.se, col = "red", lty = 3)
lines(force.pred - 1.96 * force.se, col = "red", lty = 3)
