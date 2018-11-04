# "Models" menu related functions ===========================================

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_std_lm_coeffs <- function() {
    Library("biostat")
    doItAndPrint(str_glue("summary(coef_standardized({ActiveModel()}))"))
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_get_model_class <- function() {
    doItAndPrint(str_glue("## The `R` class of the model\n",
                          "class({ActiveModel()})"))
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_model_print <- function() {
    doItAndPrint(str_glue("print({ActiveModel()})"))
}

