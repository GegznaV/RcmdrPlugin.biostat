# "Rows" menu functions ======================================================

# General---------------------------------------------------------------------
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_rownames <- function() {
    doItAndPrint(str_glue(
        "## Row names \n",
        "rownames({active_dataset_0()})"))
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_rows_has_rownames <- function() {
    .ds <- active_dataset_0()
    Library("tibble")
    doItAndPrint(str_glue(
        "## Does dataset '{.ds}' have real row names? \n",
        "has_rownames({.ds})"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_rows_rm_empty_rows <- function() {
    Library("tidyverse")

    ds            <- active_dataset_0()
    empty_row_var <- unique_obj_names("empty_rows")

    dim_before <- str_glue_eval("dim({ds})", envir_eval = .GlobalEnv)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    empty_rows_tmp <- str_glue_eval("rowSums(is.na({ds})) == ncol({ds})",
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
    dim_after <- str_glue_eval("dim({ds})", envir_eval = .GlobalEnv)

    if (!identical(dim_before, dim_after))
        command_dataset_refresh()
}

