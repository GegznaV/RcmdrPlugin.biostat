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