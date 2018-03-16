#' Activate/Disable TK Objects
#'
#' Modify state of tk objects.
#'
#' @param tk_obj TK object.
#' @param ... other options to be passed to \code{tkconfigure}.
#' @keywords internal
#'
#' @export
#'
#' @examples
#' # tk_activate(tk_obj)
#' # tk_disable(tk_obj)
#'
tk_activate <- function(tk_obj, ...) {
    tkconfigure(tk_obj, state = "active", ...)
}

#' @name tk_activate
#' @export
tk_disable <- function(tk_obj, ...) {
    tkconfigure(tk_obj, state = "disabled", ...)
}
#' @name tk_activate
#' @export
tk_normalize <- function(tk_obj, ...) {
    tkconfigure(tk_obj, state = "normal", ...)
}

# state_disabled <- function(widget) {
#     tkconfigure(widget, state = "disabled")
# }
#
# active <- function(widget) {
#     tkconfigure(widget, state = "active")
# }
#
# normal <- function(widget) {
#     tkconfigure(widget, state = "normal")
# }

tclvalue_lgl <- function(x) {
    # as.logical(as.integer(tclvalue(x)))
    as.logical(tclvalue_int(x))
}

tclvalue_int <- function(x) {
    x <- sapply(unlist(strsplit(tclvalue(x), " ")), as.integer)
    names(x) <- NULL
    x
}

tclvalue_chr <- function(x) {
    sapply(unlist(strsplit(tclvalue(x), " ")), as.character)
    names(x) <- NULL
    x
}

s2u <- function(str) {
    gsub(" ", "_", str)
}

u2s <- function(str) {
    gsub("_", " ", str)
}