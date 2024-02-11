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
one.sample.bs.hyp <- function(dat.samp, stat.func=mean, val.null, studentizeQ=F, confidence=0.95, alternative="two.sided", num.B=2000) {

  stat.func.name <- as.character(substitute(stat.func))
  print(paste0("One-Sample B.S. Hypothesis Test for: ", stat.func.name))

  val.hat <- stat.func(dat.samp)
  if(studentizeQ==T) {
    print("Do Studentized test?: TRUE")
    val.hat <- (val.hat - val.null)/XXXXXXXXXXX
  }


}
