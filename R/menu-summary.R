# "Summany" menu related functions ============================================

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_glimpse <- function() {
    Library("tidyverse")
    # doItAndPrint('library("tidyverse")')
    doItAndPrint(glue::glue("dplyr::glimpse({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_head_tail <- function() {
    Library("biostat")
    doItAndPrint(glue::glue("biostat::head_tail({ActiveDataSet()}, 4)"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_head <- function() {
    doItAndPrint(glue::glue("head({ActiveDataSet()}, 4)"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_tail <- function() {
    Library("biostat")
    doItAndPrint(glue::glue("tail({ActiveDataSet()}, 4)"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_Hmisc_describe <- function() {
    Library("Hmisc")
    # doItAndPrint('library("Hmisc")')

    doItAndPrint(glue::glue("# Summary of all variables\n",

                            "Hmisc::describe({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_psych_describe <- function() {
    Library("tidyverse")
    Library("psych")

    # doItAndPrint('library("tidyverse")\n library("psych")\n')

    doItAndPrint(glue::glue("# Summary of numeric variables\n",

                            "{ActiveDataSet()} %>% \n",
                            "  dplyr::select_if(is.numeric) %>% \n",
                            "  psych::describe() \n"))
    # logger(glue::glue("# * - Statistics of variables marked with  * should be interpreted\n",
    #                   "#     cautiously (if at all) as they are either categorical or logical. \n\n"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_psych_describeBy <- function() {
    Library("tidyverse")
    Library("psych")

#
#
#     "education", "gender"
#
#     .groups <- as.list('{ActiveDataSet()}[, c(gr_variables)]')
#
#     describeBy(sat.act$age, group = .groups), mat = TRUE, digits = 2)
#
# mat_status # TRUE, FALSE
# round_to   # integer

    # doItAndPrint('library("tidyverse")\n library("psych")\n')

    doItAndPrint(glue::glue("# Summary of numeric variables\n",

                            "{ActiveDataSet()} %>% \n",
                            "  dplyr::select_if(is.numeric) %>% \n",
                            "  psych::describeBy(group = .groups,\n",
                            "                    digits = {round_to}, \n",
                            "                    mat = {mat_status}) \n"

                            ))

}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
