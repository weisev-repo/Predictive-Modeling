set.seed(42)
Elec <- read.table("C:/Users/seviw/Desktop/2021-SEP__SEVERIN/MSE-DATA-SCIENCE/FS_22/Predictive_Modeling/Week_11/Archive/AustralianElectricity.csv",
                 sep=";", header=T)

Elec.ts <- ts(Elec[,2], start = c(1956,1), end = c(1994,2), frequency=4)
plot(Elec.ts)

box.cox <- function(x, lambda) {
  if (lambda == 0) log(x) else (x^lambda - 1)/lambda
}

Elec.ts.tr <- box.cox(Elec.ts, 0.2)
plot(Elec.ts.tr, main="Transformed electricity
     production data")

res.dec <- decompose(Elec.ts.tr)
plot(res.dec)

res.stl <- stl(Elec.ts.tr, s.window = 10)
plot(res.stl)
