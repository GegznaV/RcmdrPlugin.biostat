# "Models" menu related functions ===========================================

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_std_lm_coeffs <- function() {
    Library("lm.beta")
    doItAndPrint(str_glue(
        "## Standardized regression coefficients\n",
        "coef(lm.beta::lm.beta({ActiveModel()}))"
    ))
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_get_model_class <- function() {
    doItAndPrint(str_glue(
        "## The class(es) of the model\n",
        "class({ActiveModel()})"))
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_model_print <- function() {
    doItAndPrint(str_glue("print({ActiveModel()})"))
}

