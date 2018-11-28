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

