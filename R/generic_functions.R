#' @name widget-state
#' @title Get and set state of Tk widgets.
#' @description
#' Get or modify the state of Tk widgets.
#'
#' @details
#' The function to get widget's state:
#' - `tk_get_state()`
#'
#' The functions to set widget's state to:
#' - `tk_normalize()` to "normal";
#' - `tk_activate()` to "active";
#' - `tk_read_only()` to "readonly";
#' - `tk_disable()` to "disable";
#' - `tk_set_state()` to the indicated state.
#'
#' @param obj Tk widget or a string with the ID of an existing Tk widget
#'        (e.g., `".1.2.5"`).
#' @param ... other options to be passed to [tcltk::tkconfigure()].
#' @param state (character) The state of widget.
#'        Usually one of "normal", "active", "diabled", "readonly".
#'
#' @md
#'
#' @seealso
#' [tcltk::tkconfigure()]
#'
#' @examples
#' # tk_get_state(obj)
#' # tk_get_state(".1.2.5")
#'
#' # tk_disable(obj)
#' # tk_normalize(obj)
#' # tk_set_state(obj, "normal")
NULL

#' @rdname widget-state
#' @export
tk_get_state <- function(obj, ...) {
  UseMethod("tk_get_state")
}

#' @rdname widget-state
#' @export
tk_set_state <- function(obj, state, ...) {
  UseMethod("tk_set_state")
}

#' @rdname widget-state
#' @export
tk_normalize <- function(obj, ...) {
  UseMethod("tk_normalize")
}

#' @rdname widget-state
#' @export
tk_activate <- function(obj, ...) {
  UseMethod("tk_activate")
}

#' @rdname widget-state
#' @export
tk_read_only <- function(obj, ...) {
  UseMethod("tk_read_only")
}

#' @rdname widget-state
#' @export
tk_disable <- function(obj, ...) {
  UseMethod("tk_disable")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_size <- function(obj, ...) {
  UseMethod("get_size")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_values <- function(obj, ...) {
  UseMethod("get_values")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
set_values <- function(obj, values, ...) {
  UseMethod("set_values")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_selection <- function(obj, ...) {
  UseMethod("get_selection")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_selection_length <- function(obj, ...) {
  UseMethod("get_selection_length")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
set_selection <- function(obj, sel, ...) {
  UseMethod("set_selection")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
add_selection <- function(obj, sel, ...) {
  UseMethod("add_selection")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_xview <- function(obj, ...) {
  UseMethod("get_xview")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
set_xview <- function(obj, ...) {
  UseMethod("set_xview")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_yview <- function(obj, ind,  ...) {
  UseMethod("get_yview")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
set_yview <- function(obj, ind,  ...) {
  UseMethod("set_yview")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_yview_moveto <- function(obj, ...) {
  UseMethod("tk_yview_moveto")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_see <- function(obj, ind, ...) {
  UseMethod("tk_see")
}


#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_get_n_lines <- function(obj) {
  UseMethod("tk_get_n_lines")
}


#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_bind_row_swap <- function(obj) {
  UseMethod("tk_bind_row_swap")
}


# Methods --------------------------------------------------------------------
#' @rdname Helper-functions
#' @export
#' @keywords internal
print.bs_tk_widget <- function(x, ...) {
  print(summary(x, ...))
  cat("Class: ", str_c(class(x), collapse = ", "))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set_values.default <- function(obj, values, ...) {
  tcltk::tclvalue(obj) <- values
}
set_values.tclVar <- function(obj, values, ...) {
  tcltk::tclvalue(obj) <- values
}
set_values.tclObj <- function(obj, values, ...) {
  tcltk::tclvalue(obj) <- values
}

get_values.default <- function(obj, ...) {
  tcltk::tclvalue(obj, ...)
}
get_values.tclVar <- function(obj, ...) {
  tcltk::tclvalue(obj, ...)
}
get_values.tclObj <- function(obj, ...) {
  tcltk::tclvalue(obj, ...)
}
