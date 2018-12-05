# Set cursor ------------------------------------------------------------------
#' @rdname Helper-functions
#' @param frame Tcl/Tk frame object.
#'
#' @keywords internal
#' @export
#'
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


#' @rdname Helper-functions
#' @keywords internal
#' @export
cursor_set_idle <- function(frame = NULL) {

    if (!is.null(frame)) {
        # tkconfigure(frame, cursor = "")
        tryCatch(
            tkconfigure(frame, cursor = ""),
            error   = function(e) {},
            finally = function(e) {}
        )
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

# Get state -------------------------------------------------------------------
#' @name tk_activate
#' @export
tk_get_state.default <- function(tk_obj, ...) {
    tclvalue_chr(tkcget(tk_obj, "-state"))
}


# Set state -------------------------------------------------------------------
#' @name tk_activate
#' @title Activate/Disable TK Objects
#'
#' @description Modify state of tk objects.
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
tk_activate.default <- function(tk_obj, ...) {
    tkconfigure(tk_obj, state = "active", ...)
}
#' @name tk_activate
#' @export
tk_normalize.default <- function(tk_obj, ...) {
    tkconfigure(tk_obj, state = "normal", ...)
}
#' @name tk_activate
#' @export
tk_read_only.default <- function(tk_obj, ...) {
    tkconfigure(tk_obj, state = "readonly", ...)
}
#' @name tk_activate
#' @export
tk_disable.default <- function(tk_obj, ...) {
    tkconfigure(tk_obj, state = "disabled", ...)
}
#' @name tk_activate
#' @export
tk_disable.default <- function(tk_obj, ...) {
    tkconfigure(tk_obj, state = "disabled", ...)
}

#' @name tk_activate
#' @export
tk_yview.default <- function(obj, ind, ...) {
    tkyview(obj, ind, ...)

}

#' @name tk_activate
#' @export
tk_xview.default <- function(obj, ind, ...) {
    tkxview(obj, ind, ...)

}

# Get values -----------------------------------------------------------------

#' @rdname Helper-functions
#' @keywords internal
#' @export

#
# TODO:
# 1) senas `tclvalue_*` funkcijų versijas visame pakete pakeisti naujomis
#    versijomis.

#' @rdname Helper-functions
#' @keywords internal
#' @export

tclvalue_lgl <- function(x) {
    # as.logical(as.integer(tclvalue(x)))
    as.logical(tclvalue_int(x))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Sena versija:

# #' @rdname Helper-functions
# #' @keywords internal
# #' @export
#
# tclvalue_int <- function(x) {
#     x <- sapply(unlist(strsplit(tclvalue(x), " ")), as.integer)
#     names(x) <- NULL
#     x
# }

# Naujos versijos:

#' @rdname Helper-functions
#' @keywords internal
#' @export

tclvalue_int <- function(x) {
    as.integer(tclvalue(x))
}

#' @rdname Helper-functions
#' @keywords internal
#' @export

tclvalue_int_split <- function(x) {
    x <- sapply(unlist(strsplit(tclvalue(x), " ")), as.integer)
    names(x) <- NULL
    x
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Sena versija:

# #' @param x Tcl/Tk object
# #'
# #' @rdname Helper-functions
# #' @keywords internal
# #' @export
# #'
# tclvalue_chr <- function(x) {
#     sapply(unlist(strsplit(tclvalue(x), " ")), as.character)
#     names(x) <- NULL
#     x
# }


# Naujos versijos:


#' @rdname Helper-functions
#' @keywords internal
#' @export

tclvalue_chr <- function(x, trim = TRUE, ...) {
    rez <- as.character(tclvalue(x))
    if (isTRUE(trim)) {rez <- trimws(rez, ...)}
    unname(rez)
}

#' @rdname Helper-functions
#' @keywords internal
#' @export
tclvalue_chr_split <- function(x) {
    sapply(unlist(strsplit(tclvalue(x), " ")), as.character)
    unname(x)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
