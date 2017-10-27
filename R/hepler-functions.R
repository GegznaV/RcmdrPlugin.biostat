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
#' characterP
#' Return TRUE, if at least n character variables exist in the active dataset.
#'
#' @param n Minimum number of character variables
#'
#' @keywords internal
#' @export
characterP <- function(n = 1) {
    .activeDataSet <- activeDataSet()
    sum(eval_glue("mapply(is.character, {.activeDataSet})")) >= n
}
# ------------------------------------------------------------------------------
#
#' Scale a vector
#'
#' Subtract center and divide by scale.
#'
#' @param x A numeric vector.
#' @param center Eiter a function that computes center of data
#'              (such as \code{mean}) or a simgle numeric value.
#' @param scale Eiter a function that computes variability of data
#'              (such as \code{sd}) or a simgle numeric value.
#'
#' @return A scaled vector
#' @export
#'
#' @examples
#'
#' x = 1:10
#' scale_vector(x)
#'
#' scale_vector(x, center = median, scale = IQR)
#'
#' scale_vector(x, center = 10, scale = 2)
#'
scale_vector <- function(x, center = mean, scale = sd) {
    cener_ <-
        if (is.function(center)) {
            center(x)
        } else if (length(center) == 1) {
            center
        } else {
            stop("Incorrect value of 'center'")
        }

    scale_ <-
        if (is.function(scale)) {
            scale(x)
        } else if (length(scale) == 1) {
            scale
        } else {
            stop("Incorrect value of 'scale'")
        }

    (x - cener_) / scale_
}