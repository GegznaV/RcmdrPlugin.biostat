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
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_rowid_to_col <- function() {
    Library("tidyverse")

    doItAndPrint(glue::glue(
        '# new_df <- tibble::rowid_to_column({ActiveDataSet()}, var = "rows_id")'))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_rownames_to_col <- function() {
    Library("tidyverse")

    doItAndPrint(glue::glue(
        '# new_df <- tibble::rownames_to_column({ActiveDataSet()}, var = "row_names")'))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_col_to_rownames <- function() {
    Library("tidyverse")

    doItAndPrint(glue::glue(
        '# new_df <- tibble::column_to_rownames({ActiveDataSet()}, var = "row_names")'))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_arrange <- function() {
    Library("tidyverse")

    doItAndPrint(glue::glue(
        '# Sort rows ascending: \n',
        '# new_df <- dplyr::arrange({ActiveDataSet()}, {listVariables()[1]}) \n',
        '\n# Sort rows descending:: \n',
        '# new_df <- dplyr::arrange({ActiveDataSet()}, desc({listVariables()[1]})) \n'))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_filter <- function() {
    Library("tidyverse")

    doItAndPrint(glue::glue(
        '# new_df <- dplyr::filter({ActiveDataSet()}, !is.na({listVariables()[1]})) \n'))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_slice <- function() {
    Library("tidyverse")

    doItAndPrint(glue::glue(

        '\n# Select the first row: \n',
        '# new_df <- dplyr::slice({ActiveDataSet()}, 1) \n',
        '\n# Select the last row: \n',
        '# new_df <- dplyr::slice({ActiveDataSet()}, n()) \n',
        '\n# Select several adjacent rows: \n',
        '# new_df <- dplyr::slice({ActiveDataSet()}, 5:n()) \n',
        '\n# Use negative indices to drop rows: \n',
        '# new_df <- dplyr::slice({ActiveDataSet()}, -5:-n()) \n'

    ))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
