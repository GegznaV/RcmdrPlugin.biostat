#' @name Menu-window-functions
#' @title RcmdrPlugin.biostat functions for menus and windows
#' @description Functions that open Rcmdr menus and windows windows
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
    doItAndPrint(glue::glue("class({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_colnames <- function() {
    doItAndPrint(glue::glue("colnames({ActiveDataSet()})"))
}

# ============================================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_rownames <- function() {
    doItAndPrint(glue::glue("rownames({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_rowid_to_col <- function() {
    doItAndPrint(glue::glue(
        '# new_df <- tibble::rowid_to_column({ActiveDataSet()}, var = "rows_id")'))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_rownames_to_col <- function() {
    doItAndPrint(glue::glue(
        '# new_df <- tibble::rownames_to_column({ActiveDataSet()}, var = "row_names")'))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_col_to_rownames <- function() {
    doItAndPrint(glue::glue(
        '# new_df <- tibble::column_to_rownames({ActiveDataSet()}, var = "row_names")'))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_filter <- function() {
    doItAndPrint(glue::glue(
        '# new_df <- dplyr::filter({ActiveDataSet()}, !is.na({listVariables()[1]})) \n'))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_slice <- function() {
    doItAndPrint(glue::glue(

        '\n# The first row: \n',
        '# new_df <- dplyr::slice({ActiveDataSet()}, 1) \n',
        '\n# The last row: \n',
        '# new_df <- dplyr::slice({ActiveDataSet()}, n()) \n',
        '\n# Select several adjacent rows: \n',
        '# new_df <- dplyr::slice({ActiveDataSet()}, 5:n()) \n',
        '\n# Drop rows with negative indices: \n',
        '# new_df <- dplyr::slice({ActiveDataSet()}, -5:-n()) \n'

        ))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ============================================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_print <- function() {
    doItAndPrint(glue::glue("print({ActiveDataSet()})"))
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
    doItAndPrint(glue::glue("dim({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_as_df <- function() {
    doItAndPrint(glue::glue("{ActiveDataSet()} <- as.data.frame({ActiveDataSet()})"))
    command_dataset_refresh()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_as_tibble <- function() {
    Library("tibble")
    doItAndPrint(glue::glue("{ActiveDataSet()} <- as_tibble({ActiveDataSet()})"))
    command_dataset_refresh()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_as_dt <- function() {
    Library("data.table")
    doItAndPrint(glue::glue("{ActiveDataSet()} <- data.table({ActiveDataSet()})"))
    command_dataset_refresh()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

