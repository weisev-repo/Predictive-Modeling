
load("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_4/Daten/fitness.rda")

# fit model to all features
fit <- lm(oxy ~ ., data = fitness)
summary(fit)

source("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_4/resplot.R")
resplot(fit)

library(ellipse)
par(mfrow = c(1,1))

plotcorr(cor(fitness[, -3]), cex.lab = 0.75, mar = c(1,
                                                     1, 1, 1))
# Ellipse plot shows strong positive
# correlation between the running pulse 
# and the maximal pulse.

# multicollinearity
library(faraway)
vif(fit)

# The VIF's of runpulse and maxpulse
# indicae the presence of critical
# multicollinearity. 

f.o <- fitted(fit)

# leave out maxpulse beacuse of collinearity
fit <- lm(oxy ~ age + weight + runtime + rstpulse + runpulse,
          data = fitness)
resplot(fit)
vif(fit)

# create a new variable, which is not collinear


