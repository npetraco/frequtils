library(frequtils)
library(dafs)

#y   <- gas$Propylbenzene
#lbl <- as.factor(gas$ID)
#
# data(shotgun.df)
# dat <- shotgun.df[1:50, ]
# y   <- dat$sqrt.area
# lbl <- as.factor(dat$range)
#
data(abaz.df)
y   <- abaz.df$dna.conc
lbl <- abaz.df$sample


# Quick look at the data
# Data obvioulsy not normal and variances differ
tapply(y, lbl, mean)
tapply(y, lbl, sd)
boxplot(y ~ as.factor(lbl))

# Do the regular ANOVA:
fit1 <- aov(y ~ lbl)
summary(fit1)

# Welch doesn’t even work on this crazy data
bartlett.test(y ~ lbl)
fit2 <- oneway.test(y ~ lbl, var.equal = F)
fit2

# Find a significant result from regular ANOVA? Do a Tukey HSD:
fit1.tukey <- TukeyHSD(fit1) # Tukey on regular ANOVA
fit1.tukey                   # Tukey test outputs
plot(fit1.tukey)             # Graphical version


# For Welch ANOVA, TukeyHSD won't work. Check parwise t-test with corrected p-values
junk <- pairwise.t.test(y, lbl, p.adjust.method = "fdr", pool.sd = F)
junk
class(junk)
junk2 <- pairwise.wilcox.test(y, lbl, p.adjust.method = "fdr")
junk2$p.value
class(junk2)

pairwise.tests(y, lbl, type = "t.test", p.adjust.method = "fdr", pool.sd = F)
pairwise.tests(y, lbl, type = "t.test", p.adjust.method = "bonferroni", pool.sd = F)
pairwise.tests(y, lbl, type = "wilcox.test", p.adjust.method = "fdr", pool.sd = F)
pairwise.tests(y, lbl, type = "wilcox.test", p.adjust.method = "bonferroni", pool.sd = F)
pairwise.tests(y, lbl, type = "wilcox.test", confidence = 0.9999, p.adjust.method = "bonferroni", pool.sd = F)
