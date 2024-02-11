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

# Tietjen-Moore test:
k       <- 2 # assume two outliers
tm.parametric.bs.test(xdat = x, k = k, confidence.level = 0.95)
tm.parametric.bs.test(xdat = c(1.55,x), k = k, confidence.level = 0.95)
x
