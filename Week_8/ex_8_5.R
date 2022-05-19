
# We will now compare regression and classification trees by means of the Auto data
# set as already studied in Exercise 6.4

# a) Compute a classification tree that predicts the variable mpg01. Compute the
# optimal tree size by invoking the cv.tree()-function and provide the cross-
# validated classification error.

require(ISLR)
# additional column mpg01 and remove mpg
AutoC <- Auto[, -which(names(Auto) == "name")]
AutoC$mpg01 <- as.factor(AutoC$mpg > median(AutoC$mpg))
AutoC <- AutoC[, -which(names(AutoC) == "mpg")]

# a) Compute a classification tree that predicts the variable mpg01. Compute the
# optimal tree size by invoking the cv.tree()-function and provide the cross-
# validated classification error.

require(tree)
tree.model <- tree(mpg01 ~ ., data = AutoC)
tree.cv <- cv.tree(tree.model, method = "misclass", K = 10)
plot(tree.cv)

min.dev = min(tree.cv$dev)
idx.min = which(min.dev == tree.cv$dev)
opt.size = min(tree.cv$size[idx.min])
opt.size
## [1] 5
opt.alpha = tree.cv$k[which(tree.cv$size == opt.size)]
opt.alpha
## [1] 2
pruned.cla.model <- prune.tree(tree.model, k = opt.alpha)

# b) Compute a regression tree that predicts the variable mpg. Again, use the
# cv.tree() function to obtain the optimal tree. How large is the RSS?


AutoR <- Auto[, -which(names(Auto)=="name")]
tc = tree.control(nobs = nrow(AutoR), mindev = 1e-3)
reg.tree.model <- tree(mpg~.,data = AutoR, control = tc)
reg.tree.cv <- cv.tree(reg.tree.model, K=5)
min.rss = min(reg.tree.cv$dev)
min.rss

plot(reg.tree.cv)

idx.min = which(min.rss == reg.tree.cv$dev)
opt.size = min(reg.tree.cv$size[idx.min])
opt.size
## [1] 8
opt.alpha = reg.tree.cv$k[which(reg.tree.cv$
                                  size==opt.size)]
opt.alpha
## [1] 176.9088
pruned.reg.model <- prune.tree(reg.tree.model,
                               k = opt.alpha)
plot(pruned.reg.model)
text(pruned.reg.model, cex = 0.7)

# c) Construct a classifier for predicting mpg01 based on the regression model in
# b) by thresholding the predicted value of mpg at the median of the training set.
# Compare the training error of this classifier and the one in a).

pred.reg = as.factor(predict(pruned.reg.model)
                     > median(AutoR$mpg))
addmargins(table(pred.reg, AutoC$mpg01))

pred.cla = predict(pruned.cla.model, type="class")
addmargins(table(pred.cla, AutoC$mpg01))

