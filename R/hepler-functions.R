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
# ------------------------------------------------------------------------------
eval_glue <- function(..., envir = parent.frame(),
                      .sep = "", .open = "{", .close = "}") {

    x2 <- glue::glue(..., .envir = envir, .open = .open, .close = .close)
    eval(parse(text = x2), envir = envir)
}
# ------------------------------------------------------------------------------
eval_ <- function(x, envir = parent.frame(), ...) {
    eval(parse(text = x), envir = envir, ...)
}
# ------------------------------------------------------------------------------
get_text <- function(txt) {
    gettext(domain = "R-RcmdrPlugin.EZR", txt)

}
# ------------------------------------------------------------------------------
#' Does data contain characters?
#'
#' Return TRUE, if at least n character variables exist in the active dataset.
#'
#' @param n Minimum number of character variables
#'
#' @keywords internal
#' @export
characterP <- function(n = 1) {
    activeDataSetP() &&
        (sum(eval_glue("mapply(is.character, {activeDataSet()})")) >= n)
}
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#' Is the first class "data.frame"
#' @keywords internal
#' @export
first_class_is_dataframeP <- function() {
    activeDataSetP() &&
        (eval_glue("class({activeDataSet()})[1]") == "data.frame")
}
# ------------------------------------------------------------------------------

