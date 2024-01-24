
linton <- read.csv("/Users/karen2/codes/R/frequtils/data/LAM_study_mod.csv",header=TRUE)

save(linton, file="data/linton.RData")

unique(linton$Condition)

unique(linton$Subject)

which(linton$Subject=="M10")

data("linton")
linton$Condition
linton$Segment
linton$VerticalSize

row.idxs <- which(linton$Condition=="GEN" & linton$Segment==1 )
linton[linton$Condition=="GEN" & linton$Segment==1,]
linton[row.idxs, ]


# Mark Gil's gasoline dataset
gas <- read.csv("/Users/karen2/codes/R/frequtils/data/gas.csv",header=TRUE)

save(gas, file="data/gas.RData", header=T)
class(gas)
hist(gasx$Ethylbenzene)
mean(gasx$Ethylbenzene)
sd(gasx$Ethylbenzene)

sd(gasx$Ethylbenzene)/mean(gasx$Ethylbenzene)*100

