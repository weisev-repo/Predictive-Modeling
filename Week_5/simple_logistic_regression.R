# Exercise 5_3

simple_logistic_regression <- function(beta_0, beta_1, beta_2, x1, x2) {
 p = exp(beta_0 + beta_1 * x1 + beta_2*x2)/ (1 + exp(beta_0 + beta_1 * x1 + beta_2 * x2))
 return(p)
}

beta_0_star <- -6
beta_1_star <- 0.05
beta_2_star <- 1

print(simple_logistic_regression(beta_0_star, beta_1_star, beta_2_star, 40, 4.5))
