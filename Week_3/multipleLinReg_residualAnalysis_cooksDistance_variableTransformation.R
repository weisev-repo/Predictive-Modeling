load("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_3/Daten/savings.rda")
# source("resplot.R")

# Model without transformations
fit <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)

# Residuals and Cook's Distance

# Observations with the largest leverage
sort(hatvalues(fit), decreasing = TRUE)[1:3]

# Pairplot
weli <- which(rownames(savings) %in% c("Libya", "United States",
                                       "Japan"))
farb <- rep(1, nrow(savings))
farb[weli] <- c(2, 3, 4)
## Japan (red), USA (green), Libya (blue)
pairs(savings[, -1], pch = 19, col = farb)


## Analysis without data point with largest Cook's
## distance
plot(fit, which = 4) 

## exclude Libya
weli <- which(rownames(savings) == "Libya")
fit1 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings[-weli,
])

# Get the coefficients
coef(fit)


