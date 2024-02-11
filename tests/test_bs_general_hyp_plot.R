library(frequtils)

data(wine2)
x <- wine2$Color.intensity


bs.samp <- sapply(1:2000, function(xx){mean(sample(x, size = length(x), replace = T))})
hist(bs.samp)

bs.plot.with.obs.statistic(bs.samp,
                           obs.statistic=-10,
                           confidence = 0.99, aslQ = T,
                           alternative = "greater")
