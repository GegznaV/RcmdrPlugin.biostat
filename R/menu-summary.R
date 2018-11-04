# "Summany" menu related functions ============================================

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_glimpse <- function() {
    Library("tidyverse")

    command <-
        str_glue(
            "## Structure of dataset \n",
            "dplyr::glimpse({ActiveDataSet()})"
        ) %>%
        style_cmd()

    doItAndPrint(command)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_summary <- function() {
    # doItAndPrint('library("tidyverse")')
    doItAndPrint(str_glue("## Quick summary of whole dataset\n",
                          "summary({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_head_tail <- function() {
    Library("biostat")
    doItAndPrint(str_glue("## Top and bottom rows\n",
                          "biostat::head_tail({ActiveDataSet()}, 4)"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_head <- function() {
    doItAndPrint(str_glue("## Top rows\n",
                          "head({ActiveDataSet()}, 4)"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_tail <- function() {
    Library("biostat")
    doItAndPrint(str_glue("## Bottom rows\n",
                          "tail({ActiveDataSet()}, 4)"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_Hmisc_describe <- function() {
    Library("Hmisc")
    # doItAndPrint('library("Hmisc")')

    doItAndPrint(str_glue("## Summary of all variables\n",
                          "Hmisc::describe({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_ds_screener <- function() {
    Library("descriptr")

    doItAndPrint(str_glue("## Screen dataset for missing values\n",
                          "descriptr::ds_screener({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_psych_describe <- function() {
    Library("tidyverse")
    Library("psych")

    # doItAndPrint('library("tidyverse")\n library("psych")\n')

    doItAndPrint(str_glue("## Summary of numeric variables\n",
                          "{ActiveDataSet()} %>% \n",
                          "  dplyr::select_if(is.numeric) %>% \n",
                          "  psych::describe(quant = c(.05, .25, .75, .95),  IQR = TRUE, trim = .10) \n"))
    # logger(str_glue("# * - Statistics of variables marked with  * should be interpreted\n",
    #                   "#     cautiously (if at all) as they are either categorical or logical. \n\n"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_psych_describeBy <- function() {
    Library("tidyverse")
    Library("psych")

    doItAndPrint(str_glue("## Summary of numeric variables\n",

                          "{ActiveDataSet()} %>% \n",
                          "  dplyr::select_if(is.numeric) %>% \n",
                          "  psych::describeBy(group = .groups,\n",
                          "                    digits = {round_to}, \n",
                          "                    mat = {mat_status}) \n"
    ))

}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
