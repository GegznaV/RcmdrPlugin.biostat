# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "Variable" menu related functions ===========================================

# Manage variables -----------------------------------------------------------


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_all_chr_to_fctr <- function() {
    # Library("biostat")

    # doItAndPrint(str_glue(
    #     "{ActiveDataSet()} <- biostat::all_chr_to_factor({ActiveDataSet()})"))

    # Library(c("tidyverse", "forcats", "dplyr"))
    Library("tidyverse")
    Library("forcats")
    Library("dplyr")

    command <-
        str_glue(
            "{ActiveDataSet()} <- {ActiveDataSet()} %>% \n",
            "dplyr::mutate_if(is.character, forcats::as_factor)"
        ) %>%
        style_cmd()

    doItAndPrint(command)

    command_dataset_refresh()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO:
#  1. Convert into window to allow choosing the case.
#
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_clean_names <- function() {
    Library("tidyverse")
    # Library("forcats")
    # Library("dplyr")

    command <-
        str_glue(
            "{ActiveDataSet()} <- {ActiveDataSet()} %>% \n",
            'janitor::clean_names(case = "snake")'
        ) %>%
        style_cmd()

    doItAndPrint(command)

    command_dataset_refresh()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


