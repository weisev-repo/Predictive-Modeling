path <- "Daten/"
windmill <- read.table(paste(path, "windmill.dat", sep = ""), header = T)

fit.lml <- lm(current ~ wind_speed, data = windmill)
