library(frequtils)

?dropout.info

# Preprocess by gluing all data together into a matrix
all.dat <- do.call(rbind, dropout.info)
all.dat

# Separate dropout vs not dropout data
# Data we'll use log(H) scores for peaks and dropouts
lhp     <- all.dat[all.dat[,3] == 0, 2] # Peak Present
lhd     <- all.dat[all.dat[,3] == 1, 2] # Peak Dropout

boxplot(lhp,lhd, ylab="log(H)", names=c("peak","dropout"))

# One sample bootstrapped hypothesis test for:
# H0:log(H) mu<=3 vs. Ha:log(H) mu>3 for dropouts
one.sample.bs.hyp.for.mean(lhd, mu.null = 3, confidence  = 0.95, alternative = "greater")
t.test(lhd, mu = 3, conf.level = 0.95, alternative = "greater")
bs.test(lhd, mu = 3, conf.level = 0.95, alternative = "greater")
bs.test(lhd, mu = 3, conf.level = 0.95, alternative = "less")




# Two sample bootstrapped hypothesis test for:
# H0:log(H) pop(mu) peak == log(H) mu dropout vs. Ha:log(H) pop(mu) peak != log(H) mu dropout
two.sample.bs.hyp.for.pop.compare(lhp, lhd, confidence  = 0.95, alternative = "two.sided")
t.test(lhp, lhd, conf.level = 0.95, alternative = "greater")
bs.test(lhp, lhd, paired=F, conf.level = 0.95, alternative = "greater")


# Two sample bootstrapped hypothesis test for:
# H0:log(H) pop(mu) peak <= log(H) mu dropout vs. Ha:log(H) pop(mu) peak > log(H) mu dropout
two.sample.bs.hyp.for.pop.compare(lhp, lhd, confidence  = 0.95, alternative = "greater", print.type = NULL)


two.sample.bs.hyp.for.mean(lhp, lhd, confidence  = 0.95, alternative = "two.sided", print.type = NULL)
two.sample.bs.hyp.for.mean(lhp, lhd, confidence  = 0.95, alternative = "greater", print.type = NULL)
two.sample.bs.hyp.for.mean(lhp, lhd, confidence  = 0.95, alternative = "less", print.type = NULL)

A <- c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
B <- c(14.0, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)
boxplot(A,B)
t.test(A, B, paired = T, conf.level = 0.95, alternative = "two.sided")
bs.test(A, B, paired=T, conf.level = 0.95, alternative = "two.sided")
