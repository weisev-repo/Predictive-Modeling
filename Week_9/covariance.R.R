set.seed(123)
require(mvtnorm)
Sigma = matrix(c(5, -12, -12, 53), nrow = 2)
MV = rmvnorm(250, mean = c(12, 53), sigma = Sigma)
X = MV[, 1]
Y = MV[, 2]

SigmaHat = cov(cbind(X, Y))
print(SigmaHat)

sx = SigmaHat[1, 1]
sy = SigmaHat[2, 2]
sxy = SigmaHat[1, 2]

alpha_hat = (sy - sxy)/(sx + sy - 2 * sxy)
print(alpha_hat)


# c) c) Compute B = 2000 bootstraped estimators for ?? and estimate the standard deviation of ??^ by the empirical standard deviation of the bootstrapped estimators.
# Try different numbers of boostrap replicates. What is the minimum number of
# replicates needed for this problem to become stable?

library(boot) #load the package
# Now we need the function we would like to estimate
# In our case the alpha:
alpha.fun <- function(data, ind) {
  SigmaHat = cov(data[ind, ])
  sx = SigmaHat[1, 1]
  sy = SigmaHat[2, 2]
  sxy = SigmaHat[1, 2]
  return(alpha = (sy - sxy)/(sx + sy - 2 * sxy))
}
# now you can bootstrap: R is the number of bootstrap
# samples
bootbet = boot(data = cbind(X, Y), statistic = alpha.fun,
               R = 2000)
plot(bootbet) # do some plotting
sd(bootbet$t) # compute the standard deviation
