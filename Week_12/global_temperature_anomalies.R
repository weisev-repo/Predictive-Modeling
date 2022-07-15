set.seed(42)
global <- read.csv("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_12/Archive/global_temp.csv",
           sep=",", header=T)

global.ts <- ts(global[,2] , start = c(1850,2,1), end = c(2017,3,1), frequency = 12)
length(global.ts)
plot(global.ts, main="Global Temperature Anomalies")

res.stl = stl(global.ts, s.window = 10)
plot(res.stl)

acf(res.stl$time.series[, 3], lag.max = 100)
