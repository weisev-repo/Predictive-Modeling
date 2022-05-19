library(tree)
label = as.integer(mtcars$am) + 1
cols = c("darkcyan", "darkred")
data <- mtcars
data["am"] <- factor(data$am, levels = c(0, 1), labels = c("auto","man"))

plot(hp ~ mpg, data = data, col = cols[label], pch = 20)
legend(30, 250, legend = c("auto", "man"), col = cols, pch = 20)
grid()

par(mfrow = c(1, 2))
tc = tree.control(32, minsize = 4)
tm <- tree(am ~ mpg + hp, data = data, control = tc)

plot(tm)
text(tm, cex = 0.7)
partition.tree(tm, cex = 0.7)
points(hp ~ mpg, data = data, col = cols[label], pch = 20)
grid()

