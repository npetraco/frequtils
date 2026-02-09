library(frequtils)
library(lattice)
library(ggplot2)

# Make fake DNA air flow data from Crawley ozone data. This data has nice interactions

dat <- read.table("/Users/karen2/math/Data/crawley_sets/ozone.data.txt", header=T)

dna.amt  <- round(jitter(dat$ozone),2)
temp     <- dat$temp
air.flow <- round(jitter(dat$wind), 2)

hist(temp)
range(temp)

hist(dna.amt)
range(dna.amt)

hist(air.flow)
range(air.flow)

plot(temp,dna.amt)
plot(air.flow,dna.amt)

idx.mix <- sample(1:length(dna.amt), size = length(dna.amt), replace = F)
dna_touch_drift  <- data.frame(dna.amt, temp, air.flow)[idx.mix,]
row.names(dna_touch_drift) <- NULL
dna_touch_drift

save(dna_touch_drift, file="data/dna_touch_drift.RData", header=T)

data(dna_touch_drift)

