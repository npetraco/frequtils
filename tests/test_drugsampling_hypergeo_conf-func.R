#hypergeometric conf
hgc <- function(N, k, x, ppi){

  K <- ceiling(N*ppi)
  m <- K-1   # M0 ENFSI doc??
  n <- N-m

  # p-value
  pval <- dhyper(x = x, m = m, n = n, k = k) + 1-phyper(q = x, m = m, n = n, k = k)

  return(pval)
}

prob <- seq(from=0.01, to=1, by=0.01)
plot(prob, 1-hgc(N = 10, k = 6, x = 6, ppi = prob), typ="p")
abline(a=0.95, b = 0)
