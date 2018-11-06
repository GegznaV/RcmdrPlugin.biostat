# Set cursor ------------------------------------------------------------------
cursor_set_busy <- function(frame = NULL) {

    if (!is.null(frame)) {
        tkconfigure(frame, cursor = "watch")
    }

    .commander <- CommanderWindow()
    .menu      <- tkcget(.commander, menu = NULL)
    .log       <- LogWindow()
    .output    <- OutputWindow()
    .messages  <- MessagesWindow()

    tkconfigure(.commander, cursor = "watch")
    tkconfigure(.menu,      cursor = "watch")
    tkconfigure(.log,       cursor = "watch")
    tkconfigure(.output,    cursor = "watch")
    tkconfigure(.messages,  cursor = "watch")
}

cursor_set_idle <- function(frame = NULL) {

    if (!is.null(frame)) {
        tkconfigure(frame, cursor = "")
    }

    .commander <- CommanderWindow()
    .menu      <- tkcget(.commander, menu = NULL)
    .log       <- LogWindow()
    .output    <- OutputWindow()
    .messages  <- MessagesWindow()

    tkconfigure(.commander, cursor = "")
    tkconfigure(.menu,      cursor = "")
    tkconfigure(.log,       cursor = "xterm")
    tkconfigure(.output,    cursor = "xterm")
    tkconfigure(.messages,  cursor = "xterm")
}

# Set state -------------------------------------------------------------------
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

# Get values -----------------------------------------------------------------
#
# TODO:
# 1) senas `tclvalue_*` funkcijų versijas visame pakete pakeisti naujomis
#    versijomis.

tclvalue_lgl <- function(x) {
    # as.logical(as.integer(tclvalue(x)))
    as.logical(tclvalue_int(x))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Sena versija:

tclvalue_int <- function(x) {
    x <- sapply(unlist(strsplit(tclvalue(x), " ")), as.integer)
    names(x) <- NULL
    x
}
# Naujos versijos:
#
# tclvalue_int <- function(x) {
#     as.integer(tclvalue(x))
# }
#
# tclvalue_split_int <- function(x) {
#     x <- sapply(unlist(strsplit(tclvalue(x), " ")), as.integer)
#     names(x) <- NULL
#     x
# }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Sena versija:

tclvalue_chr <- function(x) {
    sapply(unlist(strsplit(tclvalue(x), " ")), as.character)
    names(x) <- NULL
    x
}

# Naujos versijos:

# tclvalue_split_chr <- function(x) {
#     sapply(unlist(strsplit(tclvalue(x), " ")), as.character)
#     names(x) <- NULL
#     x
# }
#
# tclvalue_chr <- function(x, trim = TRUE, ...) {
#     rez <- as.character(tclvalue(x))
#     if (isTRUE(trim)) {
#         rez <- trimws(rez, ...)
#     }
#     rez
# }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
