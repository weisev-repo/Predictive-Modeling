BC = read.table("Daten/BreastCancerData.csv", sep = ",", header = F)
BC$V2 = as.factor(BC$V2)
