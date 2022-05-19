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

