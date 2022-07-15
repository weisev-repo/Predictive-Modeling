# Exercise 5.5

print(names(Auto))
## [1] "mpg" "cylinders" "displacement"
## [4] "horsepower" "weight" "acceleration"
## [7] "year" "origin" "name"
# new data frame
print(!(names(Auto) == "mpg"))
Auto1 <- Auto[, !(names(Auto) == "mpg" | names(Auto) ==
                    "name")]

Auto1$mpg01 <- as.numeric(Auto$mpg > median(Auto$mpg))
head(Auto1)

# Examination of numeric predictors

# leave out non-numeric variables
idx = (names(Auto1) == "mpg01") | (names(Auto1) == "origin")
# scatterplot
plot(Auto1[, !idx], col = Auto1$mpg01 + 1)

# paralell coordinates
require(MASS)
parcoord(Auto1[, !idx], col = Auto1$mpg01 + 1)

# split data in train and test set.
# split ration 4:1
idx.test = sample(1:length(Auto[, 1]), size = round(length(Auto[,1])/5), replace = F)

testing = Auto1[idx.test,]
training = Auto1[-idx.test,]

# logistic regression
glm.fit = glm(mpg01 ~ ., data = training, family = binomial)
s = summary(glm.fit)
s$coefficients[, c(1, 4)]

# classification error
cost <- function(true.class, est.prob) {
  mean((est.prob < 0.5) & true.class == 1 | (est.prob >
                                               0.5) & true.class == 0)
}
# training error
pred.prob = predict(glm.fit, type = "response")
true.class = as.integer(training$mpg01)
error.train = cost(true.class, pred.prob)
# test error
pred.prob = predict(glm.fit, newdata = testing, type = "response")
true.class = as.integer(testing$mpg01)
error.test = cost(true.class, pred.prob)
# cv error
require(boot)
set.seed(123)
error.cv <- cv.glm(glmfit = glm.fit, data = training, cost = cost,
                   K = 5)

