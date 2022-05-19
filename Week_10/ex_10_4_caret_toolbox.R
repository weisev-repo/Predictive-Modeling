library(caret)

# Specify cross validation as training method with 10 folds
train_control <- trainControl(method='cv', number=10)
# Other models can be found in https://rdrr.io/cran/caret/man/

model <- train(Apps ~ ., data=train, method='lm', trControl=train_control)
# print model parameters
model
# and get the classic coefficients and metrics summary
summary(model)
# get the feature importance
vimp <- varImp(model)
vimp
plot(vimp)