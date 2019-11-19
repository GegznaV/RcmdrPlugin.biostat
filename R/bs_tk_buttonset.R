# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # Examples -----------------------------------------------------------------
#
# top <- tcltk::tktoplevel()
# boxes_3 <- bs_checkboxes(top, c("A", "B", "C"), layout = "h",
#                         title = "Buttons")
# tcltk::tkgrid(boxes_3$frame)
#
# tk_disable(boxes_3, "A")              # disables box "A".
# tk_disable(boxes_3, .which = 3)       # disables box No. 3.
# tk_disable(boxes_3, .which = c("A"))  # disables box "A".
# tk_disable(boxes_3)                   # disables all boxes.

.check_change_state <- function(obj, ..., .which) {
  if (is.null(.which)) {
    if (length(list(...)) == 0) {
      .which <- names(obj$obj)

    } else  {
      .which <- c(...)
    }

  } else if (is.numeric(.which)) {
    return(.which)

  }

  if (!all(.which %in% names(obj$obj))) {

    stop("Possibly misspelled names: ",
      setdiff(.which, names(obj$obj)) %>% str_c(collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  .which
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_activate.bs_tk_buttonset <- function(obj, ..., .which = NULL) {
  .which <- .check_change_state(obj, ..., .which = .which)
  walk(obj$obj[.which], tk_activate)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_normalize.bs_tk_buttonset <- function(obj, ..., .which = NULL) {
  .which <- .check_change_state(obj, ..., .which = .which)
  walk(obj$obj[.which], tk_normalize)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_disable.bs_tk_buttonset <- function(obj, ..., .which = NULL) {
  .which <- .check_change_state(obj, ..., .which = .which)
  walk(obj$obj[.which], tk_disable)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_get_state.bs_tk_buttonset <- function(obj, ..., .out = "list") {

  switch(
    .out,
    chr  = map_chr(obj$obj, tk_get_state),
    list = map(obj$obj, tk_get_state),
    stop("Unrecognized value of `.out`.")
  )
}
