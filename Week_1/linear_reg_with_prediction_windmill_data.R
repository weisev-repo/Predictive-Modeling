
d_dir <- "C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_1/Daten/"
windmill <- read.table(file = paste(d_dir, "windmill.dat",
                                   sep = ""), header = T)

windmill$x <- 1/windmill$wind_speed

par(mfrow = c(1,2), mar = c(4,4,1,1))
plot(current ~ wind_speed, data = windmill)
plot(current ~ x, data = windmill)

# Create linear regression model

windmill_lm <- lm(current ~ x, data = windmill)
summary(windmill_lm)

# Confidence interval 99%

confint(windmill_lm, parm = 1, level = 0.99)

## 99.5%-quantile of t-distribution
qt(0.995, 23)

plot(current ~ wind_speed, data = windmill)
abline(h = coef(windmill_lm)[1], col = "blue")

# Prediction along confidence interval
wm_new <- data.frame(x = c(1,10))
predict(windmill_lm, newdata = wm_new, interval = "confidence",
        level = 0.95)

# Prediction along prediction interval
predict(windmill_lm, newdata = wm_new, interval = "prediction",
        level = 0.95)

