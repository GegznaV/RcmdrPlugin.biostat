#' @name TclTk-helper-functions
#' @title Helper functions for Tcl/Tk.
#' @keywords internal
NULL


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Labels for R Commander
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name TclTk-labels
#' @title Tcl/Tk labels
#' @description Functions, tat create Tcl/Tk labels.
#'
#' @param parent (`"tkwin"` object) Parant Tcl/Tk frame.
#'
#' @param text (character) Label text.
#' @param ... Other arguments to pass to `tcltk2::tk2label()`.
#' @param fg (character) Foreground color.
#'
#' @seealso
#' `tcltk2::tk2label()`, `Rcmdr::labelRcmdr()`, `tcltk::ttklabel()`.
#'
#' @export
#' @md
bs_label <- function(..., fg = NULL) {
  if (is.null(fg)) tk2label(...) else tk2label(..., foreground = fg)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname TclTk-labels
#' @export
bs_label_title <- function(parent, text, ...,
  fg = Rcmdr::getRcmdr("title.color")) {

  bs_label(parent = parent, text = text, ..., fg = fg)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname TclTk-labels
#' @export
bs_label_b <- function(...) {
  bs_label_title(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname TclTk-labels
#' @export
tk_label_blue <- function(...) {
    bs_label(..., foreground = getRcmdr("title.color"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname TclTk-helper-functions
#' @export
#' @keywords internal
labeled_frame <- function(parent, label = NULL, ...) {
  # TODO: needs review
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
tk_title <- function(parent = top, text = "", pady = c(5, 9),
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

# Commands -------------------------------------------------------------------
#' @rdname TclTk-helper-functions
#' @export
#' @keywords internal
tcl_get_parent <- function(obj) {

  obj$env$parent
  # tkwinfo("parent", obj)
}

# Commands -------------------------------------------------------------------
#' @rdname TclTk-helper-functions
#' @export
#' @keywords internal
tcl_get_children_id <- function(obj) {
    tkwinfo("children", obj) %>% as.character()
    # tkwinfo("children", obj) %>% tclvalue() %>% tcl_str_split()
}

tcl_str_split <- function(str) {
  str_split(str, " ")[[1]]
}


# Commands -------------------------------------------------------------------
#' @rdname TclTk-helper-functions
#' @export
#' @keywords internal
tcl_get_siblings_id <- function(obj) {
  tkwinfo("parent", obj) %>%
    tkwinfo("children", .) %>%
    as.character()

  # tkwinfo("parent", obj) %>%
  #   tkwinfo("children", .) %>%
  #   tclvalue() %>%
  #   str_split(" ") %>%
  #   .[[1]]

}

#' @rdname TclTk-helper-functions
#' @export
#' @keywords internal
tcl_get_obj_by_id <- function(id, main_win = CommanderWindow()) {
  # id -- Tcl/Tk object ID as string, e.g. ".1", ".1.24", ".1.35.4"
  # main Tcl/Tk window (Tcl/Tk object)
  to_widget <-
    stringr::str_split(id, "\\.") %>%
    .[[1]] %>%
    purrr::accumulate(str_c, sep = ".") %>%
    .[-1] %>%
    safe_names() %>%
    stringr::str_c(collapse = "$env$")

  str_glue_eval("main_win$env$parent$env${to_widget}")

}

#' Get Vaalue of Tcl/Tk Widget Property
#' @export
#' @param .widget Tcl/Tk widget.
#' @param property (character) name of a property (e.g., "-text", "-image").
#' @keywords internal
tcl_get_property <- function(.widget, property) {
  f <- function(.widget, property) {
    tcltk::tclvalue(tcltk::tkcget(.widget, property))
  }
  rez <- purrr::safely(f)(.widget, property)$result
  if (is.null(rez)) return("") else return(rez)
}

# tooltip::tooltip -----------------------------------------------------------
# .Tcl(str_glue('tooltip::tooltip {button_view0} "View and print data"'))


tcl_obj_exists <- function(tkobj) {
  tclvalue_lgl(tcltk::tkwinfo("exists", tkobj))
}


# Get tk object, if it exists, or return a default object
# (e.g., CommanderWindow())
tcl_get_if_exists <- function(tkobj, otherwise = CommanderWindow()) {
  if (tcl_obj_exists(tkobj)) {
    tkobj
  } else {
    otherwise
  }
}
