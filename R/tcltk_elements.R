#' @name TclTk-helper-functions
#' @title Helper functions for Tcl/Tk.
#' @keywords internal
NULL


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Labels for R Commander
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name TclTk-labels
#' @title Tcl/Tk labels
#' @description Functions, that create Tcl/Tk labels.
#'
#' - `tk_label()` Uses the default color for labels (usually, black).
#' - `tk_label_blue()` Uses the default color for titles (usually, blue).
#'
#' @param parent (`"tkwin"` object) Parent Tcl/Tk window or frame.
#' @param text (character) Label text.
#' @param weight (character) Font weight.
#' @param size (integer) Font size
#' @param ... Other arguments to pass to `tcltk2::tk2label()`.
#' @param fg (character) Foreground color.
#'
#' @seealso
#' `tcltk2::tk2label()`, `Rcmdr::labelRcmdr()`, `tcltk::ttklabel()`.
#'
#' @export
#' @md
tk_label <- function(parent, text = "", ..., fg = NULL) {
  if (is.null(fg)) {
    tk2label(parent = parent, text = text, ...)
  } else {
    tk2label(parent = parent, text = text, ..., foreground = fg)
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname TclTk-labels
#' @export
tk_label_blue <- function(parent, text = "", ...,
  fg = Rcmdr::getRcmdr("title.color")) {

  tk_label(parent = parent, text = text, ..., fg = fg)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname TclTk-labels
#' @export
tk_label_red <- function(parent, ..., size = 8, weight = "bold",
  fg = "darkred") {
  tk_label(parent, text = text, font = tkfont.create(weight = "bold", size = 8),
    ..., fg = fg)
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
  add_to_grid = TRUE,
  ...) {

  lab <- tk_label(parent, text = gettext_bs(text), font = font, fg = fg)
  if (add_to_grid) {
    tkgrid(lab, pady = pady, ...)
  }

  lab
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


tk_set_color <- function(obj, color, ...) {
  tkconfigure(obj, foreground = color, ...)
}
