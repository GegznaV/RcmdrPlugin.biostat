#' Listing the last command
#'
#' This function lists the last command is executed by \code{RcmdrPlugin.KMggplot2}
#'
#' @rdname util-lastcom
#' @keywords documentation
#' @export
lastcom <- function() {
  
  if (exists(".lastcom", envir = .GlobalEnv)){
    cat(get(".lastcom", envir = .GlobalEnv))
  } else {
    stop("object '.lastcom' not found")
  }
  
}
