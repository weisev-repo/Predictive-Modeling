load("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_3/Daten/catheter.rda")

# Simple linear regression
fits1 <- lm(catlength ~ height, data = catheter)
fits2 <- lm(catlength ~ weight, data = catheter)

summary(fits1)
summary(fits2)

# Multiple linear regression

fit <- lm(catlength ~ height + weight, data = catheter)
summary(fit)

## Comment on Pr(>|t|): For height and weight the values
## are way to large. Means they are not significant.
## --> Both null hypothesis are retained (beibehalten).
## Due to the smaller p-value we would prefer to include
## the predictor weight in the regression model

# prediction intervals

newdat <- data.frame(height = 120, weight = 25)
predict(fits1, newdata = newdat, interval = "prediction")

predict(fits2, newdata = newdat, interval = "prediction")

predict(fit, newdata = newdat, interval = "prediction")


