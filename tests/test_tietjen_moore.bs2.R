library(frequtils)

x <- c(266, 273, 273, 274, 275, 276, 276, 276, 277, 277,
       278, 278, 278, 279, 279, 280, 281, 282, 282, 284)
# x <- c(200, 273, 273, 274, 275, 276, 276, 276, 277, 277,
#        278, 278, 278, 279, 279, 280, 281, 282, 282, 284, 320)


x.std      <- (x-mean(x))/sd(x) # Standardize the data
boxplot(x.std, horizontal = T)

# Check for fit to standard normal
qqnorm(x.std, xlim=c(-5,5), ylim=c(-5,5))
qqline(x.std)

# Compute sample Tietjen-Moore test statistic:
k       <- 2 # assume two outliers
Ek.samp <- tmts(x.std, k)
Ek.samp

tm.parametric.bs.test(x, 2)
