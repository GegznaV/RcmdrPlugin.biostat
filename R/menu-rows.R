# "Rows" menu functions ======================================================

# General---------------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TODO:
#
#
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_row_rm_empty_rows <- function() {
    Library("tidyverse")

    ds            <- ActiveDataSet()
    empty_row_var <- unique_obj_names("empty_rows")

    dim_before <- eval_glue("dim({ds})", envir_eval = .GlobalEnv)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    empty_rows_tmp <- eval_glue("rowSums(is.na({ds})) == ncol({ds})",
                                envir_eval = .GlobalEnv)

    if (any(empty_rows_tmp)) {
        empty_rows_ind <-
            which(empty_rows_tmp) %>%
            stringr::str_c(collapse = ", ")

        logger(str_glue("# Indices of empty rows: \n# {empty_rows_ind} \n"))


        command_1 <- str_glue(
            "## Remove empty rows\n",
            "{empty_row_var} <- rowSums(is.na({ds})) == ncol({ds})\n",
            "which({empty_row_var}, useNames = FALSE) # Indices of empty rows \n")

        command_2 <- str_glue(
            "{ds} <- {ds} %>% dplyr::filter(!{empty_row_var}) # Remove the rows\n",
            "remove({empty_row_var}) # Clean up\n")

        doItAndPrint(command_1)
        doItAndPrint(command_2)

    } else {
        # No empty rows are present
        logger(str_glue("# No empty rows are present in `{ds}`"))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dim_after <- eval_glue("dim({ds})", envir_eval = .GlobalEnv)

    if (!identical(dim_before, dim_after))
        command_dataset_refresh()
}

