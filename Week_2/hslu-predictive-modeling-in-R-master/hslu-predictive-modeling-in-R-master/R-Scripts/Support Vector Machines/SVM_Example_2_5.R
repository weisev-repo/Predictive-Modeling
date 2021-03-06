set.seed(1)
x <- matrix (rnorm (20*2,2) , ncol =2)
y <- c(rep (-1,10) , rep (1 ,10) )
x[y==1,] = x[y==1,] + 1

dat <- data.frame(x=x, y=as.factor (y))

library(e1071)

set.seed (1)
tune.out <- tune(svm, y~., data = dat ,kernel = 'linear',
                 ranges = list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100)))

# Accees the Validation errors:
summary (tune.out)

# Acces best model: 
bestmod <- tune.out$best.model
summary(bestmod)

# Predict scores:
xtest <- matrix (rnorm (20*2) , ncol =2)
ytest <- sample (c(-1,1) , 20, rep=TRUE)
xtest[ytest==1,] <- xtest[ytest==1,] + 1
testdat <- data.frame (x = xtest , y = as.factor(ytest))
ypred <- predict(bestmod, data=testdat)
table(predict =ypred, truth = testdat$y)

# Alternative model with cost = 0.01
svmfit=svm(y~., data=dat, kernel="linear", cost=.01,scale=FALSE)
ypred <- predict(svmfit, data=testdat)
table(predict = ypred, truth = testdat$y)

# Linear Seperable case:
x[y==1,] <- x[y==1,] + 0.5
plot(x, col = (3-y), pch = 19)

dat <- data.frame(x = x, y = as.factor (y))
svmfit <- svm(y~., data = dat , kernel = "linear", cost = 1e5)
summary(svmfit)
plot(svmfit, dat)

svmfit <- svm(y~., data = dat , kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)


