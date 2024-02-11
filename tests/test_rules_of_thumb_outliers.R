library(frequtils)

data(wine2)
x <- wine2$Color.intensity

hist(x)
boxplot(x, horizontal = T)

summary(x)
Q        <- quantile(x, probs = c(0.25, 0.5, 0.75))
mu.hat   <- mean(x)
sigx.hat <- sd(x)

# RULE: 3 standard deviations from the sample mean cutoffs:
mu.hat + 3*sigx.hat
mu.hat - 3*sigx.hat

# RULE: 3 standard deviations from the sample median cutoffs:
Q[2] + 3*sigx.hat
Q[2] - 3*sigx.hat

# RULE: 3 mad’s from the sample median cutoffs:
Q[2] + 3*mad(x)
Q[2] - 3*mad(x)

# Interquartile range:
IQR(x)

# RULE: Interquartile outlier cutoffs:
Q[3] + 1.5*IQR(x)
Q[1] - 1.5*IQR(x)

rule.of.thumb.outliers(x, plotQ = T)

