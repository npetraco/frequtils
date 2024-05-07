#' Convience wrapper for Box-Cox transform
#'
#' Convience wrapper for Box-Cox transform
#'
#' @param dat    a vector or array of data
#' @param lambda chosen box-cox lambda
#'
#' @details If no lambda is given (NULL), no transformation is performed.
#'
#' @return The function will XX
#'
#'
#' @export
box.cox.transform <- function(dat, lambda=NULL) {

  if(is.null(lambda)) {
    dat.trans <- dat                     # Do nothing if no lambda is given
  } else if(lambda != 0){
    dat.trans <- (dat^lambda - 1)/lambda # lambda != 0
  } else {
    dat.trans <- log(dat)                # lambda = 0
  }

  return(dat.trans)

}
