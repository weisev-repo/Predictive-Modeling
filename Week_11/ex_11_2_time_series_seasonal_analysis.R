Beer <- read.csv("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_11/Archive/AustralianBeer.csv", sep=";")

Beer.ts <- ts(Beer[,2], start = c(1956,1), end = c(1994,2), frequency=4)

plot(Beer.ts)
grid()

# aggregate the data

Beer.aggr.ts <- aggregate(Beer.ts, nfrequency = 1)
plot(Beer.aggr.ts)

# boxplot the data
boxplot(Beer.ts ~ cycle(Beer.ts), col = "darkcyan",
        main="Beer production in each quartal")
grid()

# decomposition of additive series
res.dec <- decompose(Beer.ts)
plot(res.dec)