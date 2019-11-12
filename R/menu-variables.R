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
        "colnames({active_dataset_0()})"))
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

    .ds <- active_dataset_0()

    command <-
        str_glue(
            "## Clean names (to sanke case)\n",
            "{.ds} <- {.ds} %>% \n",
            'janitor::clean_names(case = "snake")'
        ) %>%
        style_cmd()

    doItAndPrint(command)

    command_dataset_refresh()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_check_names_validity <- function() {

    .ds <- active_dataset_0()
    ds  <- get_active_ds()
    col_names <- unique_obj_names("cols")

    if (all(colnames(ds) == make.names(colnames(ds)))) {
      doItAndPrint(str_glue(
        "## All names in '{.ds}' are syntactically valid. \n",
        "{col_names} <- colnames({.ds}) \n",
        "identical({col_names}, make.names({col_names}))"
      ))

    } else {
      command <- str_glue(
        "## Syntactically invalid column names in '{.ds}': \n",
        "{col_names} <- colnames({.ds}) \n",
        "{col_names}[{col_names} != make.names({col_names})]\n",
        "remove({col_names})"
      )
      doItAndPrint(command)
    }
}


# Variable type specific functions -------------------------------------------
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_all_chr_to_fctr <- function() {

    .ds <- active_dataset_0()

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

