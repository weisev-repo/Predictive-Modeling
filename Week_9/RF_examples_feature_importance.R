set.seed(123)
d_dir <- "C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_9/Daten/"
BC <- load(file = paste(d_dir, "BreastCancerData.csv",
                             sep = ""))
# train and test split
idx.train = sample.int(nrow(BC), size = 500)
train.set = BC[idx.train, ]
test.set = BC[-idx.train, ]

# fit random forest model and plot performance measures
library(randomForest)
set.seed(123)
rfm = randomForest(V2 ~ ., data = train.set[, -1], mtry = sqrt(ncol(BC)))
plot(rfm)
legend(x = "topright", cex = 0.7, legend = c("Out-Of-Bag Error (OOB)", "Misclassification Error for benign (B)", "Misclassification Error"),
col = c("black", "red", "green"), lty = c(1, 2, 3))

# fit again a random forest model, given the constraint of ntree = 5000
library(randomForest)
set.seed(123)
rfm = randomForest(V2 ~ ., data = train.set[, -1], mtry = sqrt(ncol(BC)), ntree = 5000)
plot(rfm)
legend(x = "topright", cex = 0.7, legend = c("Out-Of-Bag Error (OOB)", "Misclassification Error for benign (B)", "Misclassification Error"),
       col = c("black", "red", "green"), lty = c(1, 2, 3))


# plot importance of variables
varImpPlot(rfm)

plot(V30 ~ V25, data = train.set, col = as.integer(train.set$V2))

# make a prediction based on test set
pred.class = predict(rfm, newdata = test.set[, -1], type = "class")
# confusion matrix
cm_test = table(pred.class, test.set$V2)
addmargins(cm_test)

# fit tree on most important features
library(tree)
tm = tree(V2 ~ V25 + V30, data = train.set[, -1])
partition.tree(tm)
points(V30 ~ V25, data = train.set, col = as.integer(train.set$V2))


