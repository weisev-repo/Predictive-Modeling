
d_dir <- "C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_1/Daten/"
windmill <- read.table(file = paste(d_dir, "windmill.dat",
                                    sep = ""), header = T)

# one could apply a transformation if the further analysis of
# the residuals does not good enough, e.g. a log-transformation
# indmill$lwind_speed <- log(windmill$wind_speed)
# windmill$lcurrent <- log(windmill$current)

fit.lm1 <- lm(current ~ wind_speed,
              data = windmill)

# plot standard residual plots
par(mfrow = c(2,3))
plot(fit.lm1, which = 1:3)

# Simulations for Tukey-Anscombe plot
plot(fitted(fit.lm1), resid(fit.lm1),
     col = "darkgrey", main = "Residuals vs Fitted",
     xlab = "Fitted", ylab = "Residuals")
set.seed(42)
for (i in 1:100) {
  sresid <- sample(resid(fit.lm1), replace = TRUE)
  lines(loess.smooth(fitted(fit.lm1), sresid),
        col = "lightgrey", lwd = 1)
}

lines(loess.smooth(fitted(fit.lm1), resid(fit.lm1)), col = "red", lwd = 1)
abline(h = 0, col = "blue", lty = "dashed")

# Simulation for QQ-plot

qq <- qqnorm(rstandard(fit.lm1), main = "Normal Q-Q",
             ylab = "Standardized Residuals")
for (i in 1:100){
  sresid <- rnorm(length(qq$x), mean(qq$y), sd(qq$y))
  lines(sort(qq$x), sort(sresid), col = "grey")
}

points(qq$x, qq$y, pch = 20)
qqline(rstandard(fit.lm1), lty = 2)

# Simulation for scale location plot

plot(fit.lm1, which = 3)

for (i in 1:100){
  sresid <- sqrt(abs(sample(rstandard(fit.lm1), replace = TRUE)))
  lines(loess.smooth(fitted(fit.lm1), sresid), col = "lightgrey", lwd = 1)
}
par(new = T)
plot(fit.lm1, which = 3)
