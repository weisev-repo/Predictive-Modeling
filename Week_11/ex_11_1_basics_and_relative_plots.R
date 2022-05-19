PW_data <- read.csv("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_11/Archive/PW_electric.csv")


# imporiting different cantons PW_data[row(canton), coliumn(values)]
PW_ZH_ts <- ts(as.numeric(PW_data[3, 3:29]), start=1990, frequency=1) 
PW_BE_ts <- ts(as.numeric(PW_data[4, 3:29]), start=1990, frequency=1)
PW_LU_ts <- ts(as.numeric(PW_data[5, 3:29]), start=1990, frequency=1)
plot(PW_BE_ts, main="Electric cars in canton Bern")
plot(PW_LU_ts, main="Electric cars in canton Luzern")
plot(PW_ZH_ts, main="Electric cars in canton Zürich")

# relative increas: log(X_k) - log(X_k-1)
LU.rel <- log(PW_LU_ts) - log(lag(PW_LU_ts, -1))
plot(LU.rel, main="Relative increas of electric cars: Luzern, Zürich and Bern")

ZH.rel <- log(PW_ZH_ts) - log(lag(PW_ZH_ts, -1))
lines(ZH.rel, col='red')

BE.rel <- log(PW_BE_ts) - log(lag(PW_BE_ts, -1))
lines(BE.rel, col='green')
grid()
legend("topleft", legend=c("LU", "ZH", "BE"), col("black", "red", "green"), cex=1)
