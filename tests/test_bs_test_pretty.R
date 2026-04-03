library(frequtils)

# Preprocess by gluing all data together into a matrix
all.dat <- do.call(rbind, dropout.info)

# Separate dropout vs not dropout data
# Data we'll use log(H) scores for peaks and dropouts
lhp     <- all.dat[all.dat[,3] == 0, 2] # Peak Present
lhd     <- all.dat[all.dat[,3] == 1, 2] # Peak Dropout

boxplot(lhp,lhd, ylab="log(H)", names=c("peak","dropout"))

t.test(lhp, mu = 5, conf.level = 0.95, alternative = "greater")
bs.test(lhp, mu = 5, conf.level = 0.95, alternative = "greater")

t.test(lhp, mu = 5, conf.level = 0.95, alternative = "less")
bs.test(lhp, mu = 5, conf.level = 0.95, alternative = "less")

t.test(lhp, mu = 5, conf.level = 0.95, alternative = "two.sided")
bs.test(lhp, mu = 5, conf.level = 0.95, alternative = "two.sided")



t.test(lhp, lhd, conf.level = 0.95, paired = F, var.equal = F, alternative = "greater")
bs.test(lhp, lhd, paired=F, conf.level = 0.95, alternative = "greater")

t.test(lhp, lhd, conf.level = 0.95, paired = F, var.equal = F, alternative = "less")
bs.test(lhp, lhd, paired=F, conf.level = 0.95, alternative = "less")

t.test(lhp, lhd, conf.level = 0.95, paired = F, var.equal = F, alternative = "two.sided")
bs.test(lhp, lhd, paired=F, conf.level = 0.95, alternative = "two.sided")



A <- c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
B <- c(14.0, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)

boxplot(A,B)


t.test(A, B, paired = T, conf.level = 0.95, alternative = "greater")
bs.test(A, B, paired=T, conf.level = 0.95, alternative = "greater")

t.test(A, B, paired = T, conf.level = 0.95, alternative = "less")
bs.test(A, B, paired=T, conf.level = 0.95, alternative = "less")

t.test(A, B, paired = T, conf.level = 0.95, alternative = "two.sided")
bs.test(A, B, paired=T, conf.level = 0.95, alternative = "two.sided")
