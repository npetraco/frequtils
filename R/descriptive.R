#' Wraper function to give percentiles of input data with respect to an empirical distribution
#'
#' Wraper function to give percentiles of input data with respect to an empirical distribution
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
percentiles <- function(all.dat, quantiles) {

  Fxx <- ecdf(all.dat)
  return(Fxx(quantiles))

}
