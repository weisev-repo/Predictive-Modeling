# Linear Regression Model on antique clock data
# 

d_dir <- "C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_1/Daten/"
ant_clo <- read.table(file = paste(d_dir, "antique_clocks.dat",
                                   sep = ""), sep = ";", header = T)
summary(ant_clo)

plot(price ~ age, data = ant_clo)

ant_clo_fit <- lm(price ~ age, data = ant_clo)
summary(ant_clo_fit) # summary of the fit

plot(price ~ age, data = ant_clo)
abline(ant_clo_fit, col = "blue", lwd = 2)
