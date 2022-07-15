require(ISLR)
## Loading required package: ISLR

# a) Create a binary variable that takes on a 1 for cars with gas mileage above the
# median, and a 0 for cars with gas mileage below the median.
Auto$mpg <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
table(Auto$mpg)

# b) Fit a support vector classifier to the data with various values of cost, in order to
# predict whether a car gets high or low gas mileage. Report the cross-validation
# errors associated with different values of this parameter. Comment on your results.

require(e1071)
## Loading required package: e1071
# tuning grid for the cost parameter
costs <- data.frame(cost = seq(0.05, 100, length.out = 6))
# 10-fold cross validation
svm.tune.linear = tune(svm, mpg ~ ., data = Auto, ranges = costs,
                       kernel = "linear")
svm.tune.linear
plot(svm.tune.linear$performance[, c(1, 2)], type = "l")

# c) Now repeat (b), this time using SVMs with radial and polynomial basis kernels, 
# with different values of gamma and degree and cost. Comment on your results


## --> radial kernel

params <- data.frame(cost = seq(0.001, 240, length.out = 6),
                     gamma = c(0.5, 1, 2, 3, 4, 5))
svm.tune.radial <- tune(svm, mpg ~ ., data = Auto, ranges = params,
                        kernel = "radial")
svm.tune.radial

plot(svm.tune.radial$performance[, c(2,3)], type='l')

## --> polynomial kernel

params <- data.frame(cost = seq(0.001, 240, length.out = 6), degree = c(0.5, 1, 2, 3, 4, 5))
svm.tune.poly <- tune(svm, mpg ~ ., data = Auto, ranges = params, kernel='polynomial')
svm.tune.poly
plot(svm.tune.poly$performance[, c(2,3)], type='l')


