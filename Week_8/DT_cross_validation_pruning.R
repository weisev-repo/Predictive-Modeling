
# a) Create a training set containing a random sample of 800 observations, and a test
# set containing the remaining observations.

require(ISLR)
set.seed(123)
idx.train = sample.int(nrow(OJ), size = 800)
train.set = OJ[idx.train, ]
test.set = OJ[-idx.train, ]

# b) Fit a tree to the training data, with Purchase as the response and the other
# variables as predictors. Use the summary() function to produce summary sta-
# tistics about the tree, and describe the results obtained. What is the training
# error rate? How many terminal nodes does the tree have?

require(tree)
tc = tree.control(nobs = 800, mincut = 5,
                  minsize = 10, mindev = 0.005)

tree.model = tree(Purchase ~ ., data = train.set,
                  control = tc)

s=summary(tree.model)
err=(s$misclass[1])/(s$misclass[2])

# d) Create a plot of the tree, and interpret the results

plot(tree.model)
text(tree.model, cex = 0.1)

# e) Predict the response on the test data, and produce a confusion matrix comparing
# the test labels to the predicted test labels. What is the test error rate?

pred.class = predict(tree.model, newdata = test.set, type = "class")
cf = table(test.set$Purchase, pred.class)
addmargins(cf)

test.error.rate <- 49/270
test.error.rate

# Apply the cv.tree() function to the training set in order to determine the
# optimal tree size.

tree.cv = cv.tree(tree.model, FUN = prune.misclass, K = 5)
opt.alpha = tree.cv$k[which.min(tree.cv$dev)]
opt.alpha

opt.size = tree.cv$size[which.min(tree.cv$dev)]

plot(tree.cv)
grid()


# i) Produce a pruned tree corresponding to the optimal tree size obtained using
# cross-validation. If cross-validation does not lead to selection of a pruned tree,
# then create a pruned tree with five terminal nodes.


# there exists two possibilities to do this.

pruned.model <- prune.misclass(tree.model, k = opt.alpha)
# prune.model <- prune.misclass(tree.model,best =
# opt.size)
plot(pruned.model)
text(pruned.model, cex = 0.8)

