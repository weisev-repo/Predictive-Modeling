# load data
load("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_3/Daten/farm.rda")

# check properties of the data
str(farm)

farm$region <- factor(farm$region)
farm$industry <- factor(farm$industry,
                        labels = c("wheat","wheat_sheep_cattle", "sheep", "cattle", "sheep_cattle"))

# Visualization
par(mfrow = c(1,2))
plot(table(farm$region), main = "Region")
plot(table(farm$industry), main = "Industry")

# Fit the complete regression model
fit1 <- lm(revenue ~ costs + region + industry, data = farm)

# Residual analysis
par(mfrow = c(1, 2))
plot(fit1, which = 1, caption = "", main = "Residuals vs. Fitted")
plot(fit1, which = 2, caption = "", main = "Normal Plot")

fit <- lm(log(revenue) ~ log(costs) + region + industry,
          data = farm)
summary(fit)

# Residual analysis (again)
par(mfrow = c(1, 2))
plot(fit, which = 1, caption = "", main = "Residuals vs. Fitted")
plot(fit, which = 2, caption = "", main = "Normal Plot")

# make a prediction

newdat <- data.frame(costs = 10^5, region = "111", industry = "cattle")
pred <- predict(fit, newdata = newdat)
exp(pred) # because of the log transformation


