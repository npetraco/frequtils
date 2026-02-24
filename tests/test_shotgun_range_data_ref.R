#library(bayesutils)
library(frequtils)
library(dafs)

# Slice out the experimental data in the shotgun.df dataset in dafs

data(shotgun.df)
table(shotgun.df$range)
range.lbl <- c(10, 20, 30, 40, 50)

sgr.dat <- array(NA, c(12,5))
for(i in 1:length(range.lbl)) {
  range.idxs  <- which(shotgun.df$range==range.lbl[i] & shotgun.df$expt=="train")
  dat.range   <- shotgun.df[range.idxs, 2]
  sgr.dat[,i] <- dat.range
}

colnames(sgr.dat) <- paste0(range.lbl, ".ft")
gun.lbl <- gl(n = 2, k = 6, labels = c("Stevens", "Remington"))
sgr     <- data.frame(sgr.dat, gun.lbl)

# Save as a reference dataset:
save(sgr, file="data/sgr.RData")

data(sgr)
sgr$X10.ft
