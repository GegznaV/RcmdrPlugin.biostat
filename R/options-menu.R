# Create new window for plots
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
load_packages_command <- function() {
    Rcmdr::doItAndPrint(paste0(
        "library(tidyverse) \n",
        "library(BioStat)   \n",
        "library(magrittr)  \n"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
command_set_locale_lt <- function() {
    Rcmdr::doItAndPrint('Sys.setlocale(locale = "Lithuanian")')
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
command_set_locale_en <- function() {
    Rcmdr::doItAndPrint('Sys.setlocale(locale = "English")')
}