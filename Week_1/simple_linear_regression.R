# Simple Linear Regression

data("anscombe")

# Four different models are generated

reg1 <- lm(anscombe$y1 ~ anscombe$x1)
reg2 <- lm(anscombe$y2 ~ anscombe$x2)
reg3 <- lm(anscombe$y3 ~ anscombe$x3)
reg4 <- lm(anscombe$y4 ~ anscombe$x4)

summary(reg1)

# Scatter plot of regression lines

par(mfrow = c(2,2))
plot(anscombe$x1, anscombe$y1, ylab = "Y1", xlab = "X1") # points
abline(reg1) # regression line
plot(anscombe$x2, anscombe$y2, ylab = "Y2", xlab = "X2")
abline(reg2)
plot(anscombe$x3, anscombe$y3, ylab = "Y3", xlab = "X3")
abline(reg3)
plot(anscombe$x4, anscombe$y4, ylab = "Y4", xlab = "X4")
abline(reg4)


