#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_get_state <- function(obj, ...) {
    UseMethod("tk_get_state")
}
#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_activate <- function(obj, ...) {
    UseMethod("tk_activate")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_disable <- function(obj, ...) {
    UseMethod("tk_disable")
 }

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_normalize <- function(obj, ...) {
    UseMethod("tk_normalize")
}

# #' @rdname Helper-functions
# #' @export
# #' @keywords internal
#  <- function(obj, ...) {
#     UseMethod("")
# }




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
set_values <- function(obj, ...) {
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
set_new_selection <- function(obj, sel, ...) {
    UseMethod("set_new_selection")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
set_yview <- function(obj, ...) {
    UseMethod("set_yview")
}

