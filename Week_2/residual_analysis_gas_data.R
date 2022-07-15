# load data

d_dir <- "C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_2/Daten/"
gas <- read.csv(file = paste(d_dir, "gas.csv",
                                    sep = ""), header = T)

# analysis of the gas consumption
par(mfrow = c(1,1))
plot(consumption ~ temperature, data = gas,
     pch = 20, ylab = "Consumption (kWh)",
     xlab = "Temperature")

title("Gas Consumption vs. Temperature")
fit <- lm(consumption ~ temperature, data = gas)
abline(fit, col = "red", lwd = 2)

par(mfrow = c(1,3))
plot(fit, which = 1:3)
