# plot classification error rate, gini index and cross entropy
# as a function of p_hat_mk

p = seq(0, 1, by = 0.01)
cols = c("darkcyan", "darkred", "darkblue")

# classification error rate
E = 1 - pmax(p, 1 - p)
# Gini index
G = 2 * p * (1 - p)
# cross entropy (scaled)
D = -p * log(p) - (1 - p) * log(1 - p)
D = D * 0.5/max(D, na.rm = T)

plot(p, E, type = "l", col = cols[1])
lines(p, G, col = cols[2])
lines(p, D, col = cols[3])
grid()
legend("topright", legend = c("Class. error", "Gini", "Entropy"), lty = 1, col = cols)


