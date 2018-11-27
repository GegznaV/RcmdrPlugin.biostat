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