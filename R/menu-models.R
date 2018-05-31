# "Models" menu related functions ===========================================

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_std_lm_coeffs <- function() {
    Library("biostat")
    doItAndPrint(glue::glue("summary(coef_standardized({ActiveModel()}))"))
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_get_model_class <- function() {
    doItAndPrint(glue::glue("## The `R` class of the model\n",
                            "class({ActiveModel()})"))
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_model_print <- function() {
    doItAndPrint(glue::glue("print({ActiveModel()})"))
}

