#' Convience wrapper for Box-Cox transform
#'
#' Convience wrapper for Box-Cox transform
#'
#' @param XX XX
#' @return The function will XX
#'
#'
#' @export
box.cox.transform <- function(dat, lambda) {

  if(lambda != 0) {
    dat.trans <- (dat^lambda - 1)/lambda
  } else {
    dat.trans <- log(dat)
  }

  return(dat.trans)

}
