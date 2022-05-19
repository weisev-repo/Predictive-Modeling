# Exercise 5.5

print(names(Auto))
## [1] "mpg" "cylinders" "displacement"
## [4] "horsepower" "weight" "acceleration"
## [7] "year" "origin" "name"
# new data frame
print(!(names(Auto) == "mpg"))
Auto1 <- Auto[, !(names(Auto) == "mpg" | names(Auto) ==
                    "name")]

Auto1$mpg01 <- as.numeric(Auto$mpg > median(Auto$mpg))
head(Auto1)