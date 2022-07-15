## No guarantee for correctness

# Problem 3 

load("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Mock_Exam/Files/conconi.rda")

fit <- lm(duncan.puls ~ speed, data = conconi)


plot(conconi$speed, conconi$duncan.puls, ylab = "Duncan.Puls", xlab = "Speed") # points
abline(fit) # regression line

summary(fit)
source("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Mock_Exam/resplot.R")
resplot(fit)
plot(fit)

# model depending on speed and runner

## preprocess
speed <- conconi$speed[c(1:19, 7:26)]
puls <- c(conconi$macduff.puls[1:19], conconi$duncan.puls[7:26])
runner <- factor(c(rep("macduff", 19), rep("duncan", 20)))
conconi2 <- data.frame(puls, speed, runner)

fit2 <- lm(puls ~ speed + runner, data = conconi2)
summary(fit2)

fit3 <- lm(puls ~ speed, data=list(conconi$duncan.puls, conconi$macduff.puls))
summary(fit3)
plot(fit3)

# Problem 4: Time Series

start(JohnsonJohnson)
end(JohnsonJohnson)
frequency(JohnsonJohnson)

plot(JohnsonJohnson)


newseries <- diff(log(JohnsonJohnson), lag = 1)
plot(newseries)
mean(newseries)
var(newseries)


pacf(newseries)
mod = ar(newseries, aic=FALSE, order.max = 4)
coefficients(mod)

pred <- predict(mod, n_ahead = 1)

## Problem 5: Logistic Regression

load("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Mock_Exam/Files/PimaIndians.Rda")
set.seed(983)
# train and test split
idx.train = sample(nrow(X), 600, replace = F)
X.train = X[idx.train, ]
X.test = X[-idx.train, ]

reg <- glm(Diagnosis ~ No_Pregnant + 
             Plasma + Blood_Pressure
           + Skin + Insuline + BMI
           + DBF + Age, data = X.train, family = binomial)
summary(reg)


