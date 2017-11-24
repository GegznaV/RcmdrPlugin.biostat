# "Models" menu related functions ===========================================

#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
command_std_lm_coeffs <- function() {
    Library("BioStat")
    doItAndPrint(glue::glue("summary(coef_standardized({ActiveModel()}))"))
}

#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
command_get_model_class <- function() {
    doItAndPrint(glue::glue("class({ActiveModel()})"))
}

