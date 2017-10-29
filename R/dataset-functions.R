#' @name Menu-winow-functions
#' @title RcmdrPlugin.BioStat functions for menus and windows
#' @description Functions that open Rcmdr menus and windows windows
#' @keywords internal
NULL

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "Dataset" menu related functions ===========================================

# Manageme dataset -----------------------------------------------------------


#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
dataset_refresh <- function() {
    Rcmdr::activeDataSet(Rcmdr::ActiveDataSet())
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
dataset_class <- function() {
    doItAndPrint(glue::glue("class({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
dataset_as_df <- function() {
    doItAndPrint(glue::glue("{ActiveDataSet()} <- as.data.frame({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
dataset_all_chr_to_fctr <- function() {
    doItAndPrint(glue::glue(
        "{ ActiveDataSet()} <- BioStat::all_chr_to_factor({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Import dataset ----------------------------------------------------------
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
dataset_import_rds <- function() {
    Message("This fuction will work in future versions of the package.",
            type = "warning")
}

# Export dataset ----------------------------------------------------------
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_export_as_textfile <- function() {
    Message("This fuction will work in future versions of the package.",
            type = "warning")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_export_as_rds <- function() {
    Message("This fuction will work in future versions of the package.",
            type = "warning")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
window_export_as_excel <- function() {
    Message("This fuction will work in future versions of the package.",
            type = "warning")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# "Summany" menu related functions ============================================

#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
summary_glimpse <- function() {
    doItAndPrint(glue::glue("dplyr::glimpse({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
summary_Hmisc_describe <- function() {
    doItAndPrint(glue::glue("# Summary of all variables\n",

                            "Hmisc::describe({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
summary_psych_describe <- function() {
    doItAndPrint(glue::glue("# Summary of numeric variables\n",

                            "{ActiveDataSet()} %>% \n",
                            "dplyr::select_if(is.numeric) %>% \n",
                            "psych::describe() \n"))
    # logger(glue::glue("# * - Statistics of variables marked with  * should be interpreted\n",
    #                   "#     cautiously (if at all) as they are either categorical or logical. \n\n"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# "Plots" menu related functions ===========================================

# Create new window for plots
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
new_plots_window <- function() {
    grDevices::windows()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# "Plots" menu related functions ===========================================

# Create new window for plots
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
load_packages_command <- function() {
    Rcmdr::doItAndPrint(paste0(
        "library(tidyverse) \n",
        "library(BioStat)   \n",
        "library(magrittr)  \n"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~