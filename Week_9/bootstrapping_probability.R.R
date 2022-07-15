
# Plot the probability in c) against n = 0, . . . , 100. What asymptotic value is ap-
# proached by the sequence? What is the exact value?

nvals <- c(1:100)
btprob <- (1 - 1/nvals)^nvals
plot(nvals, btprob, type = "l")

# last value of sequence
print(btprob[100])

# exact value: e^x = lim 0 -> inf : (1+x/n)^n
1/exp(1)
