library(caTools)

college <- ISLR::College
set.seed(42)
sample <- sample.split(college$Apps, SplitRatio = 0.7)
train <- college[sample==TRUE, ]
test <- college[sample==FALSE, ]

library(leaps)

best_subset <- regsubsets(
  Apps~.-1,
  data = train,
  nbest = 1,
  
  nvmax = NULL,
  force.in = NULL,
  force.out = NULL,
  intercept = FALSE,
  method = "exhaustive"
)
summary_best_subset <- summary(best_subset)
which.min(summary_best_subset$bic)

predictors = names(which(summary_best_subset$which[6,]))
as.formula(paste0('Apps~', paste(predictors, collapse="+"), '-1'))

# LINEAR REGRESSION

lin_reg02 <- lm(Apps ~ Accept + Enroll + Top10perc + Top25perc + 
                Outstate + Expend - 1, data=train)
summary(lin_reg02)

y_pred <- predict(lin_reg02, newdata=test)
SSR <- sum((test$Apps - y_pred)**2)
formatC(SSR, format = "e", digits=4)

# RIDGE REGRESSION

library(glmnet)
X_train <- model.matrix(~.-1, data=subset(train, select=-Apps))
y_train <- subset(train, select=Apps)
X_test <- model.matrix(~.-1, data=subset(test, select=-Apps))
y_test <- subset(test, select=Apps)

mod_cv_ridge = cv.glmnet(x=X_train,
                         y=y_train$Apps,
                         alpha=1,
                         intercept=FALSE,
                         nfolds=5
                         )

coef(mod_cv_ridge, mod_cv_ridge$lambda.min)
y_pred <- predict(mod_cv_ridge, newx=X_test)
SSR <- sum((y_test$Apps - y_pred)**2)
formatC(SSR, format = "e", digits=4)

coef(mod_cv_ridge, mod_cv_ridge$lambda.1se)

# RIDGE REGRESSION

mod_cv_lasso <- cv.glmnet(
  x=X_train,
  y=y_train$Apps,
  alpha=1,
  intercept=FALSE,
  nfolds=5
)

coef(mod_cv_lasso, mod_cv_lasso$lambda.1se)

