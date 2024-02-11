#' One sample Non-Parametric Bootstrap Hypothesis Test for a Mean.
#'
#' One sample Non-Parametric Bootstrap Hypothesis Test for a Mean.
#'
#' @param dat.samp  Sample of data to be non-parametrically bootstrapped
#' @param mu.null  Fixed (null) value for mean. Compare sample mean to this.
#' @return The function will XX
#'
#'
#' @export
one.sample.bs.hyp.for.mean <- function(dat.samp, mu.null, confidence=0.95, alternative="two.sided", num.B=2000, xlim.marg=0.1) {

  # Sample mean
  mu.hat <- mean(dat.samp)

  # Shift sample location to be centered at mu.null
  null.samp <- dat.samp - mu.hat + mu.null

  # Bootstrap from the null sample and get a bootstrapped sampling distribution for the mean
  nn           <- length(null.samp)
  bs.null.samp <- sapply(1:num.B, function(xx){mean(sample(null.samp, size = nn, replace = T))})

  # Set up limits for bs histogram:
  # If the sample mean falls in the range of the bootstrapped values, we can plot things as normal
  # If not, this is needed to keep the sample mean AND the null distribution in frame in case they are far apart
  left.limQ  <- (mu.hat > min(bs.null.samp)) # Is the sample mean larger than the smallest bootstrapped value?
  right.limQ <- (mu.hat < max(bs.null.samp)) # Is the sample mean smaller than the largest bootstrapped value?


  # print(paste0("Sample mean is: ", mu.hat))
  # print(paste0("Min BS: ", min(bs.null.samp)))
  # print(paste0("Is ", mu.hat, " > Min.BS?: ", left.limQ))
  # print(paste0("Max BS: ", max(bs.null.samp)))
  # print(paste0("Is ", mu.hat, " < Max.BS?: ", right.limQ))
  # print("----------------------------------------")

  # Compute CIs at the stipulated level of confidence as well as the ASLs:
  alpha <- 1 - confidence
  if(alternative == "two.sided"){
    # Determine CI:
    bs.null.samp.CI <- quantile(bs.null.samp, probs = c(alpha/2, 1-alpha/2))

    # Compute ASL:
    delta.loc <- abs(mu.null - mu.hat) # Distance of mu.null from sample mean. Used for two sided interval
    asl       <- sum(bs.null.samp <= (mu.null - delta.loc))/num.B + sum(bs.null.samp >= (mu.null + delta.loc))/num.B

  } else if(alternative == "less") {
    # Determine CI:
    bs.null.samp.CI <- quantile(bs.null.samp, probs = c(alpha))

    # Compute ASL:
    asl <- sum(bs.null.samp <= mu.hat)/num.B

  } else if(alternative == "greater") {
    # Determine CI:
    bs.null.samp.CI <- quantile(bs.null.samp, probs = c(1-alpha))

    # Compute ASL:
    asl <- sum(bs.null.samp >= mu.hat)/num.B

  }

  # Plot BS histogram with sample mean and CI overlaid on top:
  if(left.limQ & right.limQ) {

    # Case 1: mu.hat is within the bounds of the bootstrapped samples
    hist(bs.null.samp, main="BS-ed Approx Null Sampling Dist")
    points(bs.null.samp.CI, rep(0, length(bs.null.samp.CI)), pch=17) # Put in CI
    points(mu.hat, 0, pch=16, col="red")                            # Put in sample statistic

  } else {

    # Case 2: mu.hat is somewhere outside the bounds of the bootstrapped samples

    # Determine xlims that show the histogram and mu.hat. Use 10% wiggle room:
    # ASSUMES mu IS POSITIVE. FIXED?????????
    if(left.limQ == F) {
      hist.lims <- c(mu.hat - xlim.marg * abs(mu.hat), max(bs.null.samp))
    } else if(right.limQ == F) {
      hist.lims <- c(min(bs.null.samp), mu.hat + xlim.marg * abs(mu.hat))
    }

    # print("Hisogram xlims:")
    # print(hist.lims)
    # print("----------------------------------------")
    hist(bs.null.samp, xlim=hist.lims, main="BS-ed Approx Null Sampling Dist")
    points(bs.null.samp.CI, rep(0, length(bs.null.samp.CI)), pch=17) # Put in CI
    points(mu.hat, 0, pch=16, col="red")                            # Put in sample statistic
    #print("mu hat out")

  }

  print(paste0("Assumed (Null) mean is:      ", mu.null))
  print(paste0("Sample mean is:              ", mu.hat))
  print(paste0("Achieved Significance Level: ", round(asl,4)*100, "%"))
  print(paste0("Achieved Confidence:         ", round(1-asl,4)*100, "%"))
  print(paste0("Reject H0 in favor of Ha?:   ", asl<alpha ))

}


#' Two sample Non-Parametric Bootstrap Hypothesis Test to Compare Populations.
#'
#' Two sample Non-Parametric Bootstrap Hypothesis Test to Compare Populations.
#'
#' @param A.samp  Sample of data to be non-parametrically bootstrapped
#' @param B.samp  Sample of data to be non-parametrically bootstrapped
#' @return The function will XX
#'
#'
#' @export
two.sample.bs.hyp.for.pop.compare <- function(A.samp, B.samp, confidence=0.95, alternative="two.sided", num.B=2000, xlim.marg = 0.1) {

  # Sample sizes
  nn <- length(A.samp)
  mm <- length(B.samp)

  # Requisite mean estimates
  mu.hat.AB <- mean(c(A.samp, B.samp))
  mu.hat.A  <- mean(A.samp)
  mu.hat.B  <- mean(B.samp)
  Delta.hat <- mu.hat.A - mu.hat.B

  # Shift sample means to be the combined sample mean
  A.null <- A.samp - mu.hat.A + mu.hat.AB
  B.null <- B.samp - mu.hat.B + mu.hat.AB

  bs.null.samp <- array(NA, c(num.B))
  for(i in 1:num.B) {

    A.star                <- sample(A.null, size = nn, replace = T)
    B.star                <- sample(B.null, size = mm, replace = T)
    Delta.star            <- mean(A.star) - mean(B.star)
    bs.null.samp[i] <- Delta.star

  }

  # Set up limits for bs histogram:
  # If the sample mean falls in the range of the bootstrapped values, we can plot things as normal
  # If not, this is needed to keep the sample mean AND the null distribution in frame in case they are far apart
  left.limQ  <- (Delta.hat > min(bs.null.samp)) # Is the sample mean larger than the smallest bootstrapped value?
  right.limQ <- (Delta.hat < max(bs.null.samp)) # Is the sample mean smaller than the largest bootstrapped value?


  # Compute CIs at the stipulated level of confidence as well as the ASLs:
  alpha <- 1 - confidence
  if(alternative == "two.sided"){
    # Determine CI:
    bs.null.samp.CI <- quantile(bs.null.samp, probs = c(alpha/2, 1-alpha/2))

    # Compute ASL:
    delta.loc <- abs(0 - Delta.hat) # Distance of mu.null=0 from sample mean. Used for two sided interval
    asl       <- sum(bs.null.samp <= (0 - delta.loc))/num.B + sum(bs.null.samp >= (0 + delta.loc))/num.B

  } else if(alternative == "less") {
    # Determine CI:
    bs.null.samp.CI <- quantile(bs.null.samp, probs = c(alpha))

    # Compute ASL:
    asl <- sum(bs.null.samp <= Delta.hat)/num.B

  } else if(alternative == "greater") {
    # Determine CI:
    bs.null.samp.CI <- quantile(bs.null.samp, probs = c(1-alpha))

    # Compute ASL:
    asl <- sum(bs.null.samp >= Delta.hat)/num.B

  }

  # Plot BS histogram with sample mean and CI overlaid on top:
  if(left.limQ & right.limQ) {

    # Case 1: Delta.hat is within the bounds of the bootstrapped samples
    hist(bs.null.samp, main="BS-ed Approx Null Sampling Dist")
    points(bs.null.samp.CI, rep(0, length(bs.null.samp.CI)), pch=17) # Put in CI
    points(Delta.hat, 0, pch=16, col="red")                             # Put in sample statistic

  } else {

    # Case 2: Delta.hat is somewhere outside the bounds of the bootstrapped samples

    # Determine xlims that show the histogram and Delta.hat. Use 10% wiggle room for a default:
    if(left.limQ == F) {
      hist.lims <- c(Delta.hat - xlim.marg * abs(Delta.hat), max(bs.null.samp))
    } else if(right.limQ == F) {
      hist.lims <- c(min(bs.null.samp), Delta.hat + xlim.marg * abs(Delta.hat))
    }

    # print("Hisogram xlims:")
    # print(hist.lims)
    # print("----------------------------------------")
    hist(bs.null.samp, xlim=hist.lims, main="BS-ed Approx Null Sampling Dist")
    points(bs.null.samp.CI, rep(0, length(bs.null.samp.CI)), pch=17) # Put in CI
    points(Delta.hat, 0, pch=16, col="red")                            # Put in sample statistic
    #print("Delta hat out")

  }

  print(paste0("Sample mean for A is:          ", mu.hat.A))
  print(paste0("Sample mean for B is:          ", mu.hat.B))
  print(paste0("Difference of sample means is: ", Delta.hat))
  print(paste0("Achieved Significance Level:   ", round(asl,4)*100, "%"))
  print(paste0("Achieved Confidence:           ", round(1-asl,4)*100, "%"))
  print(paste0("Reject H0 in favor of Ha?:     ", asl<alpha ))

}
