#' A \code{gettextRcmdr} Wrapper Function
#'
#' This function is a \code{gettextRcmdr} wrapper function for this package.
#'
#' @param ... arguments passed to gettext function
#' @seealso \code{\link[Rcmdr:Rcmdr.Utilities]{Rcmdr.Utilities}}
#'
#' @rdname util-gettext_Bio
#' @keywords documentation
#' @export
gettext_Bio <- function(...) {

  gettext(..., domain = "R-RcmdrPlugin.BioStat")

}
