# ============================================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_rownames <- function() {
    doItAndPrint(str_glue("## Row names\n",
                          "rownames({ActiveDataSet()})"))
}
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


        command <- str_glue("## Remove empty rows\n",
                            "{empty_row_var} <- rowSums(is.na({ds})) == ncol({ds})\n",
                            "which({empty_row_var}) # Indices of empty rows \n",
                            "{ds} <- {ds} %>% dplyr::filter(!{empty_row_var}) # Remove the rows\n",
                            "remove({empty_row_var}) # Clean up\n")

        doItAndPrint(command)

    } else {
        # No empty rows are present
        logger(str_glue("# No empty rows are present in `{ds}`"))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dim_after <- eval_glue("dim({ds})", envir_eval = .GlobalEnv)

    if (!identical(dim_before, dim_after))
        command_dataset_refresh()
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

    new_var <- unique_colnames("row_number")

    which_position <- "first"

    # Do commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_position <-
        switch(which_position,
               "first" = glue("%>% \n",
                              "dplyr::select({new_var}, everything())"),
               "last" = "")

    cmd_ungroup <- if (is_grouped_df(ActiveDataSet())) "ungroup() %>% \n" else ""

    command <- style_cmd(str_glue(
        '## Add column with row numbers \n',
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
#     doItAndPrint(str_glue(
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

    command <- str_glue(
        '## Move column values to row names: \n\n',
        'new_df <- tibble::column_to_rownames({ActiveDataSet()}, var = "{row_name_col}")'
    )

    doItAndPrint(command)

    command_dataset_refresh()
    tkfocus(CommanderWindow())
}

# ============================================================================

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Correctly initializes window `window_col_to_rownames()`
window_col_to_rownames0  <- function(variables) {
    window_col_to_rownames()
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# new_dsname (character) - data frame name
# init_conditions (character) - conditions to be evaluated to select rows
# incorrect_cond_msg (character) - Message for incorrect expression.
window_col_to_rownames <- function(new_dsname = NULL,
                                   init_conditions = NULL,
                                   incorrect_cond_msg = NULL) {

    # Dialog -----------------------------------------------------------------

    initializeDialog(
        title = gettext_EZR("Move column values to row names"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)
    y_var_box <-
        variableListBox2(
            upper_frame,
            title = gettext_EZR("Variables with unique values\n(select one)"),
            variableList = variables_with_unique_values(),
            listHeight = 7
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        col_name <- getSelection(y_var_box)
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("tibble")
        command <- glue("## ", gettext_EZR("Move column values to row names"), "\n",
                        '{ActiveDataSet()} <- tibble::column_to_rownames({ActiveDataSet()}, var = "{col_name}")'
        ) %>%
            style_cmd()

        result <- doItAndPrint(command)
        command_dataset_refresh()
        tkfocus(CommanderWindow())
    }

    # ========================================================================
    OKCancelHelp(helpSubject = "column_to_rownames", helpPackage = "tibble")
    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")
    tkgrid(label_rcmdr(
        top,
        text = gettextRcmdr("Move column values to row names"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        columnspan = 2,
        pady = c(5, 9))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame,
           columnspan = 2
           # , sticky = "nw"
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(getFrame(y_var_box),
           # sticky = "nw",
           columnspan = 2)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "ew", columnspan = 2)
    dialogSuffix(rows = 3,
                 columns = 2)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

