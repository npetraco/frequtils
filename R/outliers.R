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
