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
command_get_locale <- function() {
    Rcmdr::doItAndPrint(paste(
        'locale_info <- Sys.getlocale()',
        'writeLines(gsub(";", "\\n", locale_info))',
        sep = " \n"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
command_set_locale_lt <- function() {
    Rcmdr::doItAndPrint(paste(
        'locale_info <- Sys.setlocale(locale = "Lithuanian")',
        'writeLines(gsub(";", "\\n", locale_info))',
        sep = " \n"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
command_set_locale_en <- function() {
    Rcmdr::doItAndPrint(paste(
        'locale_info <- Sys.setlocale(locale = "English")',
        'writeLines(gsub(";", "\\n", locale_info))',
        sep = " \n"))
}