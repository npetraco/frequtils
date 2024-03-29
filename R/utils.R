#' Easily update the frequtils library by installing the current version from the github site
#'
#' Easily update the frequtils library by installing the current version from the github site
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
update_frequtils <- function() {
  print("Updating frequtils")
  remotes::install_github("npetraco/frequtils", force=T)
  print("Done!")
}
