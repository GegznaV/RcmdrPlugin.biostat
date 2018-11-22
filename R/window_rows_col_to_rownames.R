#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Correctly initializes window `window_col_to_rownames()`
window_col_to_rownames0  <- function(variables) {
    window_col_to_rownames()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

    initializeDialog(title = gettext_bs("Set Row Names"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)
    y_var_box <-
        variableListBox2(
            upper_frame,
            title = gettext_bs("Variables with unique values\n(select one)"),
            variableList = variables_with_unique_values(),
            listHeight = 7
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        col_name <- getSelection(y_var_box)
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("tibble")
        command <- glue(
            "## ", gettext_bs("Move column values to row names"), "\n",
            '{ActiveDataSet()} <- ',
            'tibble::column_to_rownames({ActiveDataSet()}, var = "{col_name}")'
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
        text = gettext_bs("Move column values to row names"),
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
    dialogSuffix(rows = 3, columns = 2)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}