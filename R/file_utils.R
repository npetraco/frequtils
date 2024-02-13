#' Download and load a script file
#'
#' Download and load a script file. Intended to get script files for the Notes
#'
#' @param XX XX
#' @return The function will XX
#'
#'
#' @export
get.script <- function(script.name, url_head=NULL, download_location=NULL) {

  if(is.null(url_head)) {
    url_head.loc <- "https://raw.githubusercontent.com/npetraco/705/main/R/script_bucket/"
  } else {
    url_head.loc <- url_head
  }

  if(is.null(download_location)) {
    download_location.loc <- tempdir()
  } else {
    download_location.loc <- download_location
  }

  url_loc <- paste0(url_head.loc, "/", script.name)
  #print(url_loc)

  file_loc <- paste0(download_location.loc,"/",script.name)
  #print(file_loc)

  download.file(url_loc, file_loc)
  print("File downloaded to:")
  print(file_loc)
  file.edit(file_loc)

}
