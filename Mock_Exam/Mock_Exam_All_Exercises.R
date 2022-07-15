
### Problem 3: Boston Crime Per Capita ###

load("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Mock_Exam/Files/Boston.RData")

# a) Fit multiple regression model
library(MASS)
lm.all <- lm(crim ~ ., data = Boston, method = "forward")
summary(lm.all)

# b) Hybrid stepwise selection
library(leaps)
f_full <- lm(crim ~ ., data = Boston)
f_empty <- lm(crim ~ NULL, data = Boston)
regfit <- step(f_empty, direction = "both",
               scope = list(lower = f_empty, upper = f_full),
               trace = 0, k = log(nrow(Boston)))
regfit
# On the basis of the BIC we select the following model:
# crim ~ rad + black + lstat

# c) Residual analysis
source("resplot.R")
resplot(regfit)

par(mfrow = c(2,2))
plot(regfit)

# d) model: crim ~ lstat * age

lm_interaction <- lm(crim ~ age * lstat,
                     data = Boston)
summary(lm_interaction)

# comments see sample solution

# e) 95% prediction interval and 95% confidence interval
# confidence interval on given data
predict(lm_interaction, data.frame(lstat = 40, age = 80),
        interval = "confidence", level=0.95)

# prediction interval on given data
predict(lm_interaction, data.frame(lstat = 40, age = 80),
        interval = "prediction", level=0.95)

### Problem 4: Pima Indians Diabetes Database ###

load("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Mock_Exam/Files/PimaIndians.Rda")

# a) Generate training and test set
set.seed(18)
idx.train = sample.int(nrow(X), 500)
train.set = X[idx.train, ]
test.set = X[-idx.train, ]

# b) logistic regression model
Pima.fit.complete <- glm(Diagnosis ~ ., data = train.set,
                         family = "binomial")
summary(Pima.fit.complete)

# all features with a or multiple * are significant

# c) Another fit invoking only signifcant predictors.
Pima.fit.sig <- glm(Diagnosis ~ No_Pregnant + Plasma + Blood_Pressure +
                    BMI + DBF, data = train.set, family = "binomial")

coefficients(Pima.fit.sig)
summary(Pima.fit.sig)

# d) Making a prediction on probabiltiy
library(evaluate)
Pima.fit.sig <- glm(Diagnosis ~ No_Pregnant + Plasma + Blood_Pressure +
                    BMI + DBF, data = train.set, family = "binomial")
prob <- predict(Pima.fit.sig, newdata = data.frame(No_Pregnant = 0, Plasma = 101, Blood_Pressure = 71, BMI = 28.1, DBF = 0.621), type = "response")
log_odds <- log(prob/(1 - prob))

# e) Confusion Matrix and Classification Error
train.prob = predict(Pima.fit.sig, type = "response")
train.class = as.integer(train.prob > 0.5)
table(train.class, train.set$Diagnosis)


# f) Provide confusion matrix and classification error
# on the test set

test.prob = predict(Pima.fit.sig, type = "response", newdata = test.set)
test.class = as.integer(test.prob > 0.5)
table(test.class, test.set$Diagnosis)


### Problem 5: School Data / DT ###
# see sample solutions

### Problem 6: Discrete stochastic process




