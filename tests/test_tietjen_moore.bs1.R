library(frequtils)
library(mlbench)

data(Glass)
boxplot(Glass$RI ~ Glass$Type)

glass.type <- "6"
idx        <- which(Glass$Type == glass.type)
dat        <- Glass[idx, ]

x          <- dat$RI
x.std      <- (x-mean(x))/sd(x) # Standardize the data
boxplot(x.std, horizontal = T)

# Check for fit to standard normal
qqnorm(x.std, xlim=c(-3,3), ylim=c(-3,3))
qqline(x.std)

# Compute sample Tietjen-Moore test statistic:
k       <- 2 # assume two outliers
Ek.samp <- tmts(x.std, k)
Ek.samp

tm.parametric.bs.test(x, 2)
