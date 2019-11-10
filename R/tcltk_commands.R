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
    .menu      <- tkcget(.commander, "-menu")
    .log       <- LogWindow()
    .Rmd       <- RmdWindow()
    .Rnw       <- RnwWindow()
    .output    <- OutputWindow()
    .messages  <- MessagesWindow()

    purrr::walk(
      list(.commander, .menu, .log, .Rmd, .Rnw, .output, .messages),
      tkconfigure, cursor = "watch"
    )
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
    .menu      <- tkcget(.commander, "-menu")
    .log       <- LogWindow()
    .Rmd       <- RmdWindow()
    .Rnw       <- RnwWindow()
    .output    <- OutputWindow()
    .messages  <- MessagesWindow()

    purrr::walk(list(.commander, .menu), tkconfigure, cursor = "")
    purrr::walk(
      list(.log, .Rmd, .Rnw, .output, .messages),
      tkconfigure, cursor = "xterm"
    )
}

# Get state -------------------------------------------------------------------
#' @rdname widget-state
#' @keywords internal
#' @export
tk_get_state.default <- function(obj, ...) {
    tclvalue_chr(tkcget(obj, "-state"))
}


# Set state -------------------------------------------------------------------
#' @rdname widget-state
#' @keywords internal
#' @export
tk_set_state.default <- function(obj, state, ...) {
    tkconfigure(obj, state = state, ...)
}

#' @rdname widget-state
#' @keywords internal
#' @export
tk_normalize.default <- function(obj, ...) {
    tkconfigure(obj, state = "normal", ...)
}

#' @rdname widget-state
#' @keywords internal
#' @export
tk_activate.default <- function(obj, ...) {
    tkconfigure(obj, state = "active", ...)
}

#' @rdname widget-state
#' @keywords internal
#' @export
tk_read_only.default <- function(obj, ...) {
    tkconfigure(obj, state = "readonly", ...)
}

#' @rdname widget-state
#' @keywords internal
#' @export
tk_disable.default <- function(obj, ...) {
    tkconfigure(obj, state = "disabled", ...)
}


#' @rdname Helper-functions
#' @keywords internal
#' @export
tk_yview.default <- function(obj, ind, ...) {
    tkyview(obj, ind, ...)

}

#' @rdname Helper-functions
#' @keywords internal
#' @export
tk_xview.default <- function(obj, ind, ...) {
    tkxview(obj, ind, ...)

}

# Get values -----------------------------------------------------------------

#' @rdname Helper-functions
#' @keywords internal
#' @export
tclvalue_lgl <- function(x) {
  if (inherits(x, "tclVar")) {x <- tclObj(x)}
    # as.logical(as.integer(tclvalue(x)))
    as.logical(tclvalue_int(x))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Helper-functions
#' @keywords internal
#' @export
tclvalue_int <- function(x) {
  if (inherits(x, "tclVar")) {x <- tclObj(x)}
  # as.integer(tclvalue(x))
  as.integer(as.character(x))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Helper-functions
#' @keywords internal
#' @export
#'
#' @param trim (logical) If `TRUE`, function [base::trimws()] will be applied.
#' @param ... `tclvalue_chr()` passes these parameters to [base::trimws()].
#' @md
tclvalue_chr <- function(x, trim = TRUE, ...) {
  # FIXME: Check if this function works as expected
    if (inherits(x, "tclVar")) {x <- tclObj(x)}
    rez <- as.character(x)
    if (isTRUE(trim)) {rez <- trimws(rez, ...)}
    unname(rez)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
