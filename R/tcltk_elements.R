# #' @name Helper-functions
# #' @title Helper functions for RcmdrPlugin.biostat.
# #' @description Helper functions for package \pkg{RcmdrPlugin.biostat}.
# #' @keywords internal
# NULL


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname TclTk-helper-functions
#' @export
#' @keywords internal
# Label for R Commander
# see also: labelRcmdr
bs_label_b <- function(..., fg = Rcmdr::getRcmdr("title.color")) {
  bs_label(..., fg = fg)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname TclTk-helper-functions
#' @export
#' @keywords internal
# Label for R Commander
# see also: labelRcmdr
bs_label <- function(..., fg = NULL) {
  if (is.null(fg)) ttklabel(...) else ttklabel(..., foreground = fg)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname TclTk-helper-functions
#' @export
#' @keywords internal
labeled_frame <- function(parent, label = NULL, ...) {
  # [???] needs review
  ttklabelframe(parent = parent,
                labelwidget = tklabel(
                  parent,
                  text = label,
                  font = "RcmdrTitleFont",
                  foreground = Rcmdr::getRcmdr("title.color"),
                  ...)
  )
}

#' @rdname TclTk-helper-functions
#' @export
#' @keywords internal
tk_title <- function(parent = top, text = "xxx_title", pady = c(5, 9),
                     font = tkfont.create(weight = "bold", size = 9),
                     fg = Rcmdr::getRcmdr("title.color"),
                     ...) {
  tkgrid(
    bs_label(
      parent,
      text = gettext_bs(text),
      font = font,
      fg = fg),
    pady = pady,
    ...)
}

#' @rdname TclTk-helper-functions
#' @export
#' @keywords internal
bs_title <- function(parent = top, text, pady = c(5, 9),
                     font = tkfont.create(weight = "bold", size = 9), ...) {
  tkgrid(
    bs_label(
      parent,
      text = gettext_bs(text),
      font = font,
      fg = Rcmdr::getRcmdr("title.color")),
    pady = pady,
    ...)
}



# Commands -------------------------------------------------------------------
#' @rdname TclTk-helper-functions
#' @export
#' @keywords internal
tcl_get_children <- function(obj) {
  tkwinfo("parent", obj) %>%
    tkwinfo("children", .) %>%
    tclvalue() %>%
    str_split(" ") %>%
    .[[1]]
}

