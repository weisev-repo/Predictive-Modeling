# a)
require(ISLR)
set.seed(123)

# resample
idx.yes = which(Default$default == "Yes")

idx.no = sample(which(Default$default == "No"), replace = F,
                size = 333)

idx = c(idx.yes, idx.no)
Default.ds = Default[idx, ]

# b)

glm.fit.stud <- glm(default ~ student, family = binomial,
                    data = Default.ds)

# coefficients
coeffs = coefficients(glm.fit.stud)
coeffs
## (Intercept) studentYes
## -0.1527628 0.4643503
# probabilites
prob.stud = 1/(1 + exp(-(coeffs[1] + coeffs[2] * 1)))
prob.non.stud = 1/(1 + exp(-(coeffs[1] + coeffs[2] * 0)))
prob.stud
## (Intercept)
## 0.5772727
prob.non.stud
## (Intercept)
## 0.4618834

# c)
boxplot(balance ~ student, data = Default, col = Default$student,
        notch = TRUE, horizontal = TRUE, ylab = "Student status",
        xlab = "Credit card balance")
grid()
