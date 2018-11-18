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
    doItAndPrint(str_glue("## The R class of the dataset\n",
                            "class({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# List column names
command_colnames <- function() {
    doItAndPrint(str_glue("## Column names\n",
                            "colnames({ActiveDataSet()})"))
}


# ============================================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_print <- function() {
    doItAndPrint(str_glue("print({ActiveDataSet()})"))
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

    tbl_style   <- "simple"
    use_caption <- TRUE

    if (use_caption) {
       caption_text <- str_glue("Dataset `{ActiveDataSet()}`")
       tbl_caption  <- str_glue(',\n caption = "{caption_text}" ')

    } else {
        tbl_caption  <- ""
    }



    Library("tidyverse")
    Library("pander")


    command <- str_glue(
        '## Dataset as Markdown table \n',
        # '# The dataset printed in a from that will be converted \n',
        # '# to a table in an R Markdown report. \n',
        '{ActiveDataSet()} %>% \n',
        '    pander::pander(missing = "", style = "{tbl_style}"',
        '    {tbl_caption}',
        '    )') %>%
    style_cmd()

    doItAndPrint(command)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_view <- function() {
    doItAndPrint(str_glue("View({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_dim <- function() {
    doItAndPrint(str_glue("## Number of rows and columns\n",
                            "dim({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_as_df <- function() {
    doItAndPrint(str_glue(
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
    doItAndPrint(str_glue(
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
    doItAndPrint(str_glue(
        "## Change class of the dataset to `data.table`\n",
        "{ActiveDataSet()} <- data.table({ActiveDataSet()})"))
    command_dataset_refresh()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

