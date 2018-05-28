#' @name Menu-window-functions
#' @title RcmdrPlugin.biostat functions for menus and windows.
#' @description Functions that open Rcmdr menus and windows windows.
#' @keywords internal
NULL

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "Dataset" menu related functions ===========================================

# Manageme dataset -----------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_refresh <- function() {
    Rcmdr::activeDataSet(Rcmdr::ActiveDataSet())
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_class <- function() {
    doItAndPrint(glue::glue("## The `R` class of the dataset\n",
                            "class({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_colnames <- function() {
    doItAndPrint(glue::glue("## Column names\n",
                            "colnames({ActiveDataSet()})"))
}


# ============================================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_print <- function() {
    doItAndPrint(glue::glue("print({ActiveDataSet()})"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TODO:
# 1. Convert into window.
# 2. Add ability to add custom caption;
# 3. Add ability to choose style of pander table ;
# 4. Add other parameters (???)
#
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_pander <- function() {
    # Library(c("tidyverse", "pander"))
    Library("tidyverse")
    Library("pander")
    doItAndPrint(style_cmd(glue::glue(
        '## The dataset \n',
        '# The dataset printed in a from that will be converted \n',
        '# to a table in an R Markdown report. \n',
        '{ActiveDataSet()} %>% \n',
        'pander::pander(caption = "Dataset `{ActiveDataSet()}`", missing = "")')))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_view <- function() {
    doItAndPrint(glue::glue("View({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_dim <- function() {
    doItAndPrint(glue::glue("## Number of rows and columns\n",
                            "dim({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_as_df <- function() {
    doItAndPrint(glue::glue(
        "## Change class of the dataset to `data.frame`\n",
        "{ActiveDataSet()} <- as.data.frame({ActiveDataSet()})"))
    command_dataset_refresh()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_as_tibble <- function() {
    Library("tibble")
    doItAndPrint(glue::glue(
        "## Change class of the dataset to `tibble`\n",
        "{ActiveDataSet()} <- as_tibble({ActiveDataSet()})"))
    command_dataset_refresh()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_as_dt <- function() {
    Library("data.table")
    doItAndPrint(glue::glue(
        "## Change class of the dataset to `data.table`\n",
        "{ActiveDataSet()} <- data.table({ActiveDataSet()})"))
    command_dataset_refresh()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

