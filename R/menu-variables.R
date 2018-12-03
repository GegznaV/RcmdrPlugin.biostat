# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "Variable" menu related functions ===========================================

# General functions ----------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# List column names
command_colnames <- function() {
    doItAndPrint(str_glue(
        "## Column names\n",
        "colnames({ActiveDataSet()})"))
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
            "## Clean names (to sanke case)\n",
            "{ActiveDataSet()} <- {ActiveDataSet()} %>% \n",
            'janitor::clean_names(case = "snake")'
        ) %>%
        style_cmd()

    doItAndPrint(command)

    command_dataset_refresh()
}

# Variable type specific functions -------------------------------------------
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_all_chr_to_fctr <- function() {

    .ds <- ActiveDataSet()

    Library("tidyverse")
    Library("forcats")
    Library("dplyr")

    command <-
        str_glue(
            "## Convert all text variables to factor variables\n",
            "{.ds} <- {.ds} %>% \n",
            "dplyr::mutate_if(is.character, forcats::as_factor)"
        ) %>%
        style_cmd()

    doItAndPrint(command)

    command_dataset_refresh()
}

