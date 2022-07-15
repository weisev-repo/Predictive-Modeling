# ex_6_2

# exercise on barely separable data

# a) generate a (barely separable) data-set with dim = 2
set.seed(42)
x1 = runif(500) - 0.5
x2 = runif(500) - 0.5
y = ifelse(-0.15 * x1 + x2 > 0, "red", "blue")
plot(x1, x2, col = y)

# b) Compute the cross-validation error rates for support vector classifiers with a
# range of cost values.

library(e1071)
costs=seq(1, 50, length.out = 20)
y = as.factor(y)
perf=c()

for(c in costs){
  # fit svm on data with different c's (=costs)
  svm.fit = svm(y~., data=data.frame(x1,x2,y), type='C', kernel='linear', cost=c)
  # use predict-function to predict generated data-set
  svm.pred = predict(svm.fit, data.frame(x1,x2))
  error = mean(svm.pred!=y)
  perf = rbind(perf, c(c,error))
}

print(sprintf('Linear SVM training error rate= %10.6f', min(perf)))

set.seed(42)
costs=data.frame(cost=costs)
# tuning function with cross validation
svm.tune = tune(svm, y~., data=data.frame(x1, x2, y), ranges=costs, tunecontrol=tune.control(cross=10))

print(sprintf('Linear SVM CV error rate= %10.6f', min(svm.tune$performances[,2])))

plot(svm.tune$performances[,c(1,2)], type='l', ylim=c(0,0.02), xlim=c(0,50))
lines(perf, col='red')


# c) generate a test data-set and predict the y values

set.seed(87)
x1 = runif(500) - 0.5
x2 = runif(500) - 0.5
y = ifelse(-0.15* x1 + x2 > 0, "red", "blue")

dat_test = data.frame(x1 = x1, x2 = x2, y = y)
y_hat = predict(svm.tune$best.model, newdata = dat_test)
print(sum(y_hat == y))

print(sprintf("Linear SVM test error rate= %10.6f", 1 -
                sum(y_hat == y)/length(y)))
