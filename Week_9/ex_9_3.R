library(MASS)

summary(Boston)

set.seed(42)
idx.train = sample.int(nrow(Boston), size=nrow(Boston)/2)

train.set = Boston[idx.train, ]
test.set = Boston[-idx.train, ]

library(randomForest)
rfm = randomForest(medv ~ ., data=train.set)
rfm

plot(rfm, main="Mean of squared error")

# prediction on test.set
pred.val = predict(rfm, newdata = test.set)

# calculate mean-squared-error
mse = sum((pred.val - test.set$medv)^2)/nrow(test.set)

# plot mse dependig on number of trees
plot(pred.val - test.set$medv, xlab = "", ylab = "error",
     pch = 20, col = "darkcyan", main = "Residuals on the test set")
grid()

# play on mtry parameter
rfm = randomForest(medv ~ ., data = train.set, mtry = 5)
pred.val = predict(rfm, newdata = test.set)
sum((pred.val - test.set$medv)^2)/nrow(test.set)


