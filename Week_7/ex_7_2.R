# a) Create a training set containing a random sample of 800 observations, and a test
# set containing the remaining observations.

set.seed(42)
train = sample(1:1070, 800)
test = (1:1070)[-train]
tb = c()
res = c()

# b) Fit a support vector classifier to the training data using cost=0.01, with
# <<Purchase>> as the response and the other variables as predictors. Use the
# summary() function to produce summary statistics, and describe the results
# obtained.

require(ISLR)
require(e1071)
svm.fit = svm(Purchase ~ ., data = OJ, subset = train, cost = 0.01,
              kernel = "linear")
summary(svm.fit)

# c) What are the training and test error rates? Also give the corresponding confusion
# matrices

# train
svm.pred = predict(svm.fit, OJ[train, ])
knitr::kable(table(OJ[train, "Purchase"], svm.pred))

mean(OJ$Purchase[train] != svm.pred)

res = cbind(res, train = mean(OJ$Purchase[train] != svm.pred))

# test
svm.pred = predict(svm.fit, OJ[test, ])
knitr::kable(table(OJ[test, "Purchase"], svm.pred))

mean(OJ$Purchase[test] != svm.pred)

res = cbind(res, test = mean(OJ$Purchase[test] != svm.pred))

# d) Use the tune() function to select an optimal cost. Consider 10 values in the
# range 0.01 to 10.

costs <- data.frame(cost = seq(0.01, 10, length.out = 10))
# 10-fold cross validation
svm.tune.linear = tune(svm, Purchase ~ ., data = OJ, ranges = costs,
                       kernel = "linear")

plot(svm.tune.linear$performance[, c(1, 2)], type = "l")

# e) Compute the training and test error rates using this new value for cost

C = 40 # optimal cost value

svm.fit = svm(Purchase ~ ., data = OJ, subset = train, cost = C, kernel = "linear")
summary(svm.fit)


# train
svm.pred = predict(svm.fit, OJ[train, ])
knitr::kable(table(OJ[train, "Purchase"], svm.pred))

mean(OJ$Purchase[train] != svm.pred)

res = cbind(res, train = mean(OJ$Purchase[train] != svm.pred))

# test
svm.pred = predict(svm.fit, OJ[test, ])
knitr::kable(table(OJ[test, "Purchase"], svm.pred))

mean(OJ$Purchase[test] != svm.pred)

res = cbind(res, test = mean(OJ$Purchase[test] != svm.pred))

# f) Repeat parts (b) through (e) using a support vector machine with a radial kernel.
# Use the default value for gamma.

params <- data.frame(cost = seq(0.01, 10, length.out = 10))
# 10-fold cross validation
svm.tune.radial = tune(svm, Purchase ~ ., data = OJ, ranges = params,
                       kernel = "radial")

plot(svm.tune.radial$performance[, c(1, 2)], type = "l")

G = 2.23

# ...

# g) Repeat parts (b) through (e) using a support vector machine with a polynomial
# kernel. Set degree=2

params <- data.frame(cost = seq(0.001, 240, length.out = 6), degree = c(0.5, 1, 2, 3, 4, 5))
svm.tune.poly <- tune(svm, Purchase ~ ., data = OJ, ranges = params, kernel='polynomial')
svm.tune.poly
plot(svm.tune.poly$performance[, c(2,3)], type='l')
