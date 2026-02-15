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

  print(paste0("frequtils loaded?: ", "frequtils" %in% .packages()))

  # First check is frequtils is loaded.
  # If it is, unload it. Lately updating with it loaded has caused problems.... 2.25.26
  if("frequtils" %in% .packages()) {
    print("Unloading frequtils first.")
    detach("package:frequtils", unload=TRUE, force = TRUE)
    print(paste0("frequtils loaded?: ", "frequtils" %in% .packages()))
    print("frequtils unloaded!")
  }

  print("Updating frequtils")
  remotes::install_github("npetraco/frequtils", force=TRUE, upgrade = FALSE)
  print("Done updating frequtils!")
}
