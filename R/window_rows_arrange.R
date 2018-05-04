# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_arrange <- function() {
    Library("tidyverse")

    doItAndPrint(glue::glue(
        '## Examples of code \n\n',

        '# Sort rows ascending: \n',
        '# new_df <- dplyr::arrange({ActiveDataSet()}, {listVariables()[1]}) \n\n',

        '# Sort rows descending:: \n',
        '# new_df <- dplyr::arrange({ActiveDataSet()}, desc({listVariables()[1]})) \n'))

    # command_dataset_refresh()
}