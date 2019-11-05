# TODO:
# - rewrite the main function accotding to the new template
# - rewrite the onOK() function accotding to the new template

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_rownames_to_col <- function() {

    # Functions --------------------------------------------------------------
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

        command <- str_glue(
            "## ", gettext_bs("Move row names to column"), "\n",
            "{.ds} <- {.ds} %>% \n",
            'tibble::rownames_to_column("{new_name}")') %>%
            style_cmd()

        logger(command)
        result <- justDoIt(command)

        if (class(result)[1] != "try-error")
            command_dataset_refresh()

        tkfocus(CommanderWindow())
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Initial values ---------------------------------------------------------
    .ds <- active_dataset()

    # Initialize dialog window and title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dialogue_title <- gettext_bs("Move Row Names to Column")
    initializeDialog(title = dialogue_title)
    tk_title(top, dialogue_title)


    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    name_variable <- tclVar(unique_colnames("row_name"))


    # Widgets ----------------------------------------------------------------
    name_frame <- tkframe(top)
    name_entry <- ttkentry(name_frame, width = "47", textvariable = name_variable)

    # Help menus -------------------------------------------------------------
    help_menu <- function() {

      menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tkadd(menu_main, "command",
        label    = "Function `rownames_to_column`",
        command  = open_help("rownames_to_column", package = "tibble"))
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tkpopup(menu_main,
        tkwinfo("pointerx", top),
        tkwinfo("pointery", top))
    }
    # Finalize ---------------------------------------------------------------
    ok_cancel_help(on_help = help_menu)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(name_frame, sticky = "w")

    lab_1 <- bs_label_b(name_frame, text = gettext_bs("Column name:"))
    tkgrid(lab_1, sticky = "w")

    tkgrid(name_entry, sticky = "w")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
}
