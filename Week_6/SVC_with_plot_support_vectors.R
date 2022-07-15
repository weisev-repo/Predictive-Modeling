# ex_6_1

# a)
x <- matrix(c(3, 2, 4, 1, 2, 4, 4, 4, 2, 4, 4, 1, 3, 1),ncol = 2)
y <- c(rep(1, 4), rep(-1, 3))

plot(x, col=3-y)

# b)

library(e1071)
dataset <- data.frame(x = x, y = as.factor(y))
svm.fit <- svm(y ~ ., data = dataset, kernel = "linear", cost = 10, scale = FALSE)


# Solve -> beta0 + beta1 * x1 + beta2 * x2 > 0 for x2 --> a = -beta0/beta1 ; b = -beta0/beta2
coefficients <- coef(svm.fit)
beta0 <- coefficients[["(Intercept)"]]
beta1 <- coefficients[["x.1"]]
beta2 <- coefficients[["x.2"]]
a <- -beta0/beta2
b <- -beta1/beta2
abline(a, b)

# d) inidicate support vectors, they are perpendicular to the seperating hyperplane
sv_ind <- svm.fit[["index"]] # indices of support vectors
x1 <- x[sv_ind, 1] # X1-coordinates of support vectors
x2 <- x[sv_ind, 2] # X2-coordinates of support vectors
points(x1, x2, pch = "x", col = 3 - y[sv_ind])

# e) indicate the margin for the maximal margin hyperplane.

shift <- x[sv_ind[1], 2] - (a + b * x[sv_ind[1], 1])
a.o <- a + shift
a.u <- a - shift
abline(a.o, b, lty = "longdash")
abline(a.u, b, lty = "longdash")

