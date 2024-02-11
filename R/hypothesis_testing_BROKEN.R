#' One sample Non-Parametric Bootstrap Hypothesis Tests.
#'
#' One sample Non-Parametric Bootstrap Hypothesis Tests.
#'
#' @param dat.samp  Sample of data to be non-parametrically bootstrapped
#' @param stat.func Statistical function to bootstrap. Usually a mean, but can be anything that take a single vector of data and produces a statistic.
#' @param val.null  Fixed (null) value to compare sample statistic to. Usually a stipulated reference mean.
#' @return The function will XX
#'
#'
#' @export
one.sample.bs.hyp_BROKEN <- function(dat.samp, stat.func=mean, val.null, confidence=0.95, alternative="two.sided", num.B=2000) {

  val.hat   <- stat.func(dat.samp)    # Sample value of the statistic
  delta.loc <- abs(val.null - val.hat) # Distance of val.null from sample estimate. Used for two sided interval

  # Shift sample location to val.null
  # If val.null is a mean, then this shift the sample's mean to the val.null
  null.samp <- dat.samp - val.hat + val.null

  # Bootstrap from the null sample and get a bootstrapped sampling distribution for the statistic of interest
  nn           <- length(null.samp)
  bs.null.samp <- sapply(1:num.B, function(xx){stat.func(sample(null.samp, size = nn, replace = T))})

  # Set up limits for bs histogram:
  left.limQ  <- (val.hat > min(bs.null.samp))
  right.limQ <- (val.hat < max(bs.null.samp))

  #print(paste0("Min BS: ", min(bs.null.samp)))
  #print(paste0("Is ", val.hat, " > Min.BS?: ", left.limQ))
  #print(paste0("Max BS: ", max(bs.null.samp)))
  #print(paste0("Is ", val.hat, " < Max.BS?: ", right.limQ))

  alpha <- 1 - confidence
  if(alternative == "two.sided"){
    bs.null.samp.CI <- quantile(bs.null.samp, probs = c(alpha/2, 1-alpha/2))

    # Compute ASL:
    asl <- sum(bs.null.samp <= (val.null - delta.loc))/num.B + sum(bs.null.samp >= (val.null + delta.loc))/num.B

  } else if(alternative == "less") {
    bs.null.samp.CI <- quantile(bs.null.samp, probs = c(alpha))

    # Compute ASL:
    asl <- sum(bs.null.samp <= val.hat)/num.B

  } else if(alternative == "greater") {
    bs.null.samp.CI <- quantile(bs.null.samp, probs = c(1-alpha))

    # Compute ASL:
    asl <- sum(bs.null.samp >= val.hat)/num.B

  }

  if(left.limQ & right.limQ) { # val.hat is within the bounds of the bootstrapped samples
    hist(bs.null.samp, main="BS-ed Approx Null Sampling Dist")
    points(bs.null.samp.CI, rep(0, length(bs.null.samp.CI)), pch=17)
    points(val.hat, 0, pch=16, col="red") # Put in sample statistic
    #print("Val hat in")

  } else {                    # val.hat is somewhere outside the bounds of the bootstrapped samples

    # Determine xlims that show the histogram and val.hat:
    if(left.limQ == F) {
      hist.lims <- c(val.hat-0.1*val.hat, max(bs.null.samp))
    } else if(right.limQ == F) {
      hist.lims <- c(min(bs.null.samp), val.hat+0.1*val.hat)
    }

    hist(bs.null.samp, xlim=hist.lims, main="BS-ed Approx Null Sampling Dist")
    points(bs.null.samp.CI, rep(0, length(bs.null.samp.CI)), pch=17)
    points(val.hat, 0, pch=16, col="red") # Put in sample statistic
    #print("Val hat out")

  }

  print(paste0("Achieved Significance Level: ", round(asl,4)*100, "%"))
  print(paste0("Achieved Confidence in H0:   ", round(1-asl,4)*100, "%"))
  print(paste0("Reject H0 in favor of Ha?:   ", asl<alpha ))

}


#' One sample Non-Parametric Bootstrap Hypothesis Tests with Studentizing option.
#'
#' One sample Non-Parametric Bootstrap Hypothesis Tests Studentizing option.
#'
#' @param dat.samp    Sample of data to be non-parametrically bootstrapped
#' @param stat.func   Statistical function to bootstrap. Usually a mean, but can be anything that take a single vector of data and produces a statistic.
#' @param val.null    Fixed (null) value to compare sample statistic to. Usually a stipulated reference mean.
#' @param studentizeQ Whether or not to Studentize the test statistic
#' @return The function will XX
#'
#'
#' @export
one.sample.bs.hyp2_BROKEN <- function(dat.samp, stat.func=mean, val.null, studentizeQ=F, confidence=0.95, alternative="two.sided", num.B=2000) {

  val.hat   <- stat.func(dat.samp)     # Sample value of the statistic
  delta.loc <- abs(val.null - val.hat) # Distance of val.null from sample estimate. Used for two sided interval

  # Shift sample location to val.null
  # If val.null is a mean, then this shift the sample's mean to the val.null
  null.samp <- dat.samp - val.hat + val.null

  # Bootstrap from the null sample and get a bootstrapped sampling distribution for the statistic of interest
  nn           <- length(null.samp)
  if(studentizeQ == T ){ # Studentized Null

    # Standardize/Studentize the sample statistic:
    sd.val.hat   <- sd(dat.samp)
    val.hat      <- (val.hat - val.null)/(sd.val.hat/sqrt(nn))

    # Compute the Studentized bootstrap Null
    bs.null.samp <- array(NA, c(num.B))
    for(i in 1:num.B){
      xx.null.star     <- sample(null.samp, size = nn, replace = T)
      val.null.star    <- stat.func(xx.star)
      val.null.sd.star <- sd(xx.star)
      t.null.star      <- (val.null.star - val.null)/(val.null.sd.star/sqrt(nn))
      bs.null.samp[i]  <- t.null.star
    }

  } else {               # Regular Null
    bs.null.samp <- sapply(1:num.B, function(xx){stat.func(sample(null.samp, size = nn, replace = T))})
  }


  # Set up limits for bs histogram:
  left.limQ  <- (val.hat > min(bs.null.samp))
  right.limQ <- (val.hat < max(bs.null.samp))
  hist(bs.null.samp)

  #print(paste0("Min BS: ", min(bs.null.samp)))
  #print(paste0("Is ", val.hat, " > Min.BS?: ", left.limQ))
  #print(paste0("Max BS: ", max(bs.null.samp)))
  #print(paste0("Is ", val.hat, " < Max.BS?: ", right.limQ))

  # alpha <- 1 - confidence
  # if(alternative == "two.sided"){
  #   bs.null.samp.CI <- quantile(bs.null.samp, probs = c(alpha/2, 1-alpha/2))
  #
  #   # Compute ASL:
  #   asl <- sum(bs.null.samp <= (val.null - delta.loc))/num.B + sum(bs.null.samp >= (val.null + delta.loc))/num.B
  #
  # } else if(alternative == "less") {
  #   bs.null.samp.CI <- quantile(bs.null.samp, probs = c(alpha))
  #
  #   # Compute ASL:
  #   asl <- sum(bs.null.samp <= val.hat)/num.B
  #
  # } else if(alternative == "greater") {
  #   bs.null.samp.CI <- quantile(bs.null.samp, probs = c(1-alpha))
  #
  #   # Compute ASL:
  #   asl <- sum(bs.null.samp >= val.hat)/num.B
  #
  # }
  #
  # if(left.limQ & right.limQ) { # val.hat is within the bounds of the bootstrapped samples
  #   hist(bs.null.samp, main="BS-ed Approx Null Sampling Dist")
  #   points(bs.null.samp.CI, rep(0, length(bs.null.samp.CI)), pch=17)
  #   points(val.hat, 0, pch=16, col="red") # Put in sample statistic
  #   #print("Val hat in")
  #
  # } else {                    # val.hat is somewhere outside the bounds of the bootstrapped samples
  #
  #   # Determine xlims that show the histogram and val.hat:
  #   if(left.limQ == F) {
  #     hist.lims <- c(val.hat-0.1*val.hat, max(bs.null.samp))
  #   } else if(right.limQ == F) {
  #     hist.lims <- c(min(bs.null.samp), val.hat+0.1*val.hat)
  #   }
  #
  #   hist(bs.null.samp, xlim=hist.lims, main="BS-ed Approx Null Sampling Dist")
  #   points(bs.null.samp.CI, rep(0, length(bs.null.samp.CI)), pch=17)
  #   points(val.hat, 0, pch=16, col="red") # Put in sample statistic
  #   #print("Val hat out")
  #
  # }
  #
  # print(paste0("Achieved Significance Level: ", round(asl,4)*100, "%"))
  # print(paste0("Achieved Confidence in H0:   ", round(1-asl,4)*100, "%"))
  # print(paste0("Reject H0 in favor of Ha?:   ", asl<alpha ))

}
