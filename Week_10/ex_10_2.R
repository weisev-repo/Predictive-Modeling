boston <- ISLR2::Boston

# inspect the data using
View(boston)

# split data into training and test set
sample <- sample.split(boston$crim, SplitRatio = 0.7)
train <- boston[sample==TRUE, ]
test <- boston[sample==FALSE, ]

library(leaps)
best_subset <- regsubsets(
  crim~.-1,
  data = train,
  nbest = 1,
  nvmax = NULL,
  force.in = NULL,
  force.out = NULL,
  intercept=FALSE,
  method = "exhaustive"
)
summary_best_subset <- summary(best_subset)
which.min(summary_best_subset$bic)

predictors = names(which(summary_best_subset$which[3,]))
as.formula(paste0('crim~', paste(predictors, collapse = "+"), '-1'))
lin_reg02 <- lm(crim ~ rad + lstat + medv - 1, data=train)
summary(lin_reg02)

# RIDGE REGRESSION

library(glmnet)
X_train <- model.matrix(~.-1, data=subset(train, select=-crim))
y_train <- subset(train, select=crim)
X_test <- model.matrix(~.-1, data=subset(test, select=-crim))
y_test <- subset(test, select=crim)

mod_cv_ridge = cv.glmnet(x=X_train,
                         y=y_train$crim,
                         alpha=1,
                         intercept=FALSE,
                         nfolds=5
)

coef(mod_cv_ridge, mod_cv_ridge$lambda.min)
y_pred <- predict(mod_cv_ridge, newx=X_test)
SSR <- sum((y_test$crim - y_pred)**2)
formatC(SSR, format = "e", digits=4)

coef(mod_cv_ridge, mod_cv_ridge$lambda.1se)

# LASSO REGRESSION

mod_cv_lasso = cv.glmnet(
  x=X_train,
  y=y_train$crim,
  alpha=1,
  intercept=FALSE,
  nfolds=5
)
coef(mod_cv_lasso, mod_cv_lasso$lambda.min)
