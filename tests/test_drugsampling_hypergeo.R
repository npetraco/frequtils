# cf. Table 1 of ENFSI or SWGDRUG docs above:
N   <- 10   # Population size
k   <- 6    # Sample size
x   <- k    # Number positives + (also test statistic)
ppi <- 0.75 # positive probability (also population proportion, the "at-least pi% of the seizure are positives")

K <- ceiling(N*ppi)
K          # ceiling floor needed??
m <- K-1   # M0 ENFSI doc??
n <- N-m
c(K, m, n)

fh <- rbind(
  c(x,   m-x),
  c(k-x, n-(k-x))
)
sum(fh)
rowSums(fh)
colSums(fh)

fisher.test(fh, alternative = "greater")

