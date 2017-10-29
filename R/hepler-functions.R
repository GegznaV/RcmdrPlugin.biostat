
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

