# ============================================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_rownames <- function() {
    Library("tidyverse")

    doItAndPrint(glue::glue("rownames({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TODO:
# 1. show which rows are removed.
#
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_row_rm_empty_rows <- function() {
    Library("tidyverse")

    ds <- ActiveDataSet()

    glue::glue("## Remove empty rows\n",
               "{ds} <- {ds}[rowSums(is.na({ds})) == 0, ]")

    doItAndPrint()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_rowid_to_col <- function() {
    Library("tidyverse")

    # This row deletes row names:
    # '# new_df <- tibble::rowid_to_column({ActiveDataSet()}, var = "rows_id")'


    # cabbages %>%
    #     mutate(row_number = 1:n()) %>%
    #     select(row_number, everything())

    new_var <- unique_colname("row_number")

    which_position <- "first"

    # Do commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_position <-
        switch(which_position,
            "first" = glue("%>% \n",
                           "dplyr::select({new_var}, everything())"),
            "last" = "")

    cmd_ungroup <- if (is_grouped_df(ActiveDataSet())) "ungroup() %>% \n" else ""

    command <- style_cmd(glue::glue(
        '## An example of code: \n\n',

        '## Add row numbers \n',
        "{ActiveDataSet()} <- {ActiveDataSet()} %>% \n",
        "{cmd_ungroup}",
        "dplyr::mutate({new_var} = 1:n())",
        "{cmd_position}"))

    doItAndPrint(command)
    command_dataset_refresh()
    tkfocus(CommanderWindow())


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #' @rdname Menu-window-functions
# #' @export
# #' @keywords internal
# command_rownames_to_col <- function() {
#     Library("tidyverse")
#
#     doItAndPrint(glue::glue(
#         '## An example of code: \n\n',
#         '# new_df <- tibble::rownames_to_column({ActiveDataSet()}, var = "row_names")'))
# }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_col_to_rownames <- function() {
    Library("tidyverse")


    row_name_cols <- variables_with_unique_values()

    row_name_col <- if (length(row_name_cols) == 0) {
        "row_names"
    } else {
        row_name_cols[1]
    }

    command <- glue::glue(
        '## An example of code: \n\n',
        'new_df <- tibble::column_to_rownames({ActiveDataSet()}, var = "{row_name_col}")')

    doItAndPrint(command)

    command_dataset_refresh()
    tkfocus(CommanderWindow())
}

