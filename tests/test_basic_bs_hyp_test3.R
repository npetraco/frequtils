x3a <- rnorm(n = 30, mean = 10, sd = 4)
x3b <- rnorm(n = 30, mean = 11, sd = 4)
mean(c(x3a, x3b))

two.sample.bs.hyp.for.pop.compare(
  A.samp      = x3a,
  B.samp      = x3b,
  confidence  = 0.95,
  xlim.marg   = 0.1,
  alternative ="two.sided")
  #alternative="less")
  #alternative="greater")

x3b
