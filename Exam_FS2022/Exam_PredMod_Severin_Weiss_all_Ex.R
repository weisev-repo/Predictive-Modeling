load("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_3/Daten/catheter.rda")

# Question 2

library(iml)
library(e1071)
svmfit <- svm(Species ~ ., data = iris[train,], kernel = "linear", cost = 0.1)

summary(svmfit)

mod <- Predictor$new(svmfit, data=iris[train,])
imp <- FeatureImp$new(mod, loss='mae')
plot(imp)


# Question 4

library(tree)
tree.model <- tree(Species ~ ., data = iris[train,])
plot(tree.model)
text(tree.model)

tree.pred = predict(tree.model, iris[train, ])
table(iris[train, "Species"], tree.pred)
knitr::kable(table(iris[train, "Species"], tree.pred))

# Question 11
tree.model <- tree(Species ~ ., data = iris)
plot(tree.model)
text(tree.model)

# Gini index
G = 2 * p * (1 - p)
tree.model$frame[,c(1,6)]

# Question 14
df <- ISLR::Auto
linfit <- lm(mpg ~ horsepower, data = df)
summary(linfit)
coef(linfit)
mhp<- mean(df$horsepower)
n = 392
h <- 1/n + ((90 - mhp)**2)/(sum((df$horsepower - mhp)**2))

# Question 17
my.ts <- c(0.6539,0.7017,0.5127)
ar.model <- ar(my.ts, aic=F, order.max = 1)
pred <- predict(ar.model, n.ahead = 31)


# Question 28
df2 = ISLR::Auto
#df$origin <- as.factor(df$origin, levels=c("America", "Europe", "Japan"))
fit2 <- lm(mpg ~ year + origin, data = df2)
summary(fit2)
