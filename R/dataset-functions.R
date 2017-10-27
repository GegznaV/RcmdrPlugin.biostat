
#' @export
#' @keywords internal
dataset_as_df <- function() {
    doItAndPrint(glue::glue("{ActiveDataSet()} <- as.data.frame({ActiveDataSet()})"))
}

#' @export
#' @keywords internal
dataset_class <- function() {
    doItAndPrint(glue::glue("class({ActiveDataSet()})"))
}

#' @export
#' @keywords internal
dataset_all_chr_to_fctr <- function() {
    doItAndPrint(glue::glue(
        "{ ActiveDataSet()} <- BioStat::all_chr_to_factor({ActiveDataSet()})"))
}

# ============================================================================
#' @export
#' @keywords internal
summary_glimpse <- function() {
    doItAndPrint(glue::glue("dplyr::glimpse({ActiveDataSet()})"))
}

#' @export
#' @keywords internal
summary_Hmisc_describe <- function() {
    doItAndPrint(glue::glue("Hmisc::describe({ActiveDataSet()})"))
}

#' @export
#' @keywords internal
summary_psych_describe <- function() {
    doItAndPrint(glue::glue("# Statistics of variables marked with  * should be \n",
                            "# interpreted cautiously (if at all) as they are \n",
                            "# either categorical or logical.\n",

                            "psych::describe({ActiveDataSet()}) \n"))
}

