# "Summany" menu related functions ============================================

#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
summary_glimpse <- function() {
    Library("tidyverse")

    doItAndPrint(glue::glue("dplyr::glimpse({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
summary_Hmisc_describe <- function() {
    Library("Hmisc")
    doItAndPrint(glue::glue("# Summary of all variables\n",

                            "Hmisc::describe({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
summary_psych_describe <- function() {
    Library("tidyverse")
    Library("psych")

    doItAndPrint(glue::glue("# Summary of numeric variables\n",

                            "{ActiveDataSet()} %>% \n",
                            "dplyr::select_if(is.numeric) %>% \n",
                            "psych::describe() \n"))
    # logger(glue::glue("# * - Statistics of variables marked with  * should be interpreted\n",
    #                   "#     cautiously (if at all) as they are either categorical or logical. \n\n"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
