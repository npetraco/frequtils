#' Grubbs outlier test as a parametric bootstrap.
#'
#' Grubbs outlier test as a parametric bootstrap.
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
grubbs.parametric.bs.test <- function(xdat, rdist.func, rdist.args.list, confidence.level=0.95, num.B=2000) {

  G.bs.samp <- array(NA, c(num.B))
  for(i in 1:num.B) {

    # Get a parametric BS Null data sample:
    x.bs <- do.call(rdist.func, rdist.args.list)
    #print(x.bs)

    # Compute Grubbs statistic on the BS data sample and store
    mu.hat.bs    <- mean(x.bs)
    sdx.hat.bs   <- sd(x.bs)
    G.bs         <- max(abs(x.bs - mu.hat.bs)) / sdx.hat.bs
    G.bs.samp[i] <- G.bs
  }

  hist(G.bs.samp)

  # Overlay "Critical" BS G-value at level of confidence:
  Gc.bs.CI <- quantile(G.bs.samp, prob=c(confidence.level))
  points(c(Gc.bs.CI), c(0), pch=17)

  #Overlay Sample G-value:
  G.samp <- max(abs(xdat-mean(xdat)))/sd(xdat)
  points(c(G.samp), c(0), col="red", pch=16)

  # Bootstrap p-value:
  asl  <- sum(G.bs.samp >= G.samp)/num.B
  asl2 <- (1 + sum(G.bs.samp >= G.samp) )/(num.B+1) # Unbiased form
  print(paste0("Sample Grubbs statistic:     ", G.samp))
  print(paste0("Achieved Significance Level: ", asl*100, "%"))
  print(paste0("Achieved Confidence Level:   ", (1-asl)*100, "%"))
  print(paste0("Reject H0 in favor of Ha?    ", asl < (1-confidence.level)))
  #print(paste0("ASL2: ", asl2))

}


#' Tietjen-Moore two-sided test statistic. From NIST Handbook code.
#'
#' Tietjen-Moore two-sided test statistic. From NIST Handbook code.
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
tmts <- function(xx, kk) {

  # Code from https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/tietjen.htm

  n.loc <- length(xx)

  # Compute the absolute residuals:
  res <- abs(xx - mean(xx))

  # Sort data according to size of residual:
  df  <- data.frame(xx,res)
  dfs <- df[order(df$res),]

  # Create a subset of the data without the largest k values:
  klarge = c((n.loc-kk+1):n.loc)
  subx = dfs$xx[-klarge]

  # Compute the sums of squares:
  ksub <- (subx - mean(subx))^2
  alld <- (df$xx - mean(df$xx))^2

  # Compute the test statistic:
  Ek <- sum(ksub)/sum(alld)

  return(Ek)

}


#' Tietjen-Moore outlier test as a parametric bootstrap.
#'
#' Tietjen-Moore outlier test as a parametric bootstrap.
#'
#' Set up to run assuming data is normal and should be standardized
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
tm.parametric.bs.test <- function(xdat, k, standardizeQ=T, rdist.func=rnorm, rdist.args.list=list(n=length(xdat), mean = 0, sd = 1), confidence.level=0.95, num.B=2000) {

  xdat.loc <- xdat
  if(standardizeQ == T) {
    xdat.loc <- (xdat.loc - mean(xdat.loc))/sd(xdat.loc)
  }

  Ek.bs.samp <- array(NA, c(num.B))
  for(i in 1:num.B) {
    # Get a parametric BS Null data sample:
    # No outlier data should be standard normal, so that is why the default data generating mechanism is asumed normal and standardized
    #x.bs <- rnorm(n, mean = 0, sd = 1)
    x.bs <- do.call(rdist.func, rdist.args.list)

    # Ek statistic on the BS data sample and store
    Ek.bs <- tmts(x.bs, k)
    Ek.bs.samp[i] <- Ek.bs
  }

  hist(Ek.bs.samp)

  # Overlay "Critical" BS Ek-value at level of confidence: Note it's lower tail
  Ek.bs.CI <- quantile(Ek.bs.samp, prob=c(1-confidence.level))
  points(c(Ek.bs.CI), c(0), pch=17)
  #print(Ek.bs.CI)

  #Overlay Sample Ek-value:
  Ek.samp <- tmts(xdat.loc, k)
  points(c(Ek.samp), c(0), col="red", pch=16)

  # Bootstrap p-value:
  asl  <- sum(Ek.bs.samp <= Ek.samp)/num.B            # Note it's lower tail
  asl2 <- (1 + sum(Ek.bs.samp >= Ek.samp) )/(num.B+1) # Unbiased form
  print(paste0("Sample T.-M. statistic:        ", Ek.samp))
  print(paste0("Achieved Significance Level: ", asl*100, "%"))
  print(paste0("Achieved Confidence Level:   ", (1-asl)*100, "%"))
  print(paste0("Reject H0 in favor of Ha?    ", asl < (1-confidence.level)))
  # #print(paste0("ASL2: ", asl2))

}
