# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_rownames_to_col <- function(){
    ds <- activeDataSet()

    initializeDialog(title = gettext_bs("Move row names to column"))

    name_variable <- tclVar(unique_colnames("row_name"))
    name_frame <- tkframe(top)
    name_entry <- ttkentry(name_frame, width = "47", textvariable = name_variable)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        new_name <- trim.blanks(tclvalue(name_variable))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (!is.valid.name(new_name)) {
            errorCondition(
                recall = window_rows_rownames_to_col,
                message = paste0( '"',new_name,'" ', gettext_bs("is not a valid name."))
            )
            return()
        }

        if (is.element(new_name, listDataSets())) {
            if ("no" == tclvalue(checkReplace(new_name,
                                              type = gettext_bs("Variable")))) {
                closeDialog()
                window_rows_rownames_to_col()
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        Library("tibble")

        command <- glue(
            "## ", gettext_bs("Move row names to column"), "\n",
            "{ds} <- {ds} %>% \n",
            'tibble::rownames_to_column("{new_name}")') %>%
            style_cmd()

        logger(command)
        result <- justDoIt(command)

        if (class(result)[1] != "try-error")
            command_dataset_refresh()

        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "rownames_to_column", helpPackage = "tibble")

    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")
    tkgrid(bs_label(
        top,
        text = gettext_bs("Move row names to column"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(name_frame, sticky = "w")

    tkgrid(
        bs_label(
            name_frame,
            text = gettext_bs("Column name for row names:"),
            foreground = getRcmdr("title.color")),
        sticky = "w"
    )

    tkgrid(name_entry, sticky = "w")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
}
