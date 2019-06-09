# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_class <- function() {
    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        switch(tclvalue_chr(class_Variable),
               "df"     = command_dataset_as_df(),
               "dt"     = command_dataset_as_dt(),
               "tibble" = command_dataset_as_tibble(),
               "print"  = command_dataset_class()
        )
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command_dataset_refresh()
        tkfocus(CommanderWindow())
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    .ds <- active_dataset()

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    win_title <- gettext_bs("Class of Active Dataset")
    initializeDialog(title = win_title)
    tk_title(top, win_title)

    # Widgets ----------------------------------------------------------------

    classes <- str_c(str_glue_eval("class({.ds})", envir_eval = .GlobalEnv),
                     collapse = ", ")

    tkgrid(bs_label_b(top, text = "Current class(es):"))
    tkgrid(bs_label(top, text = classes))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)

    # Radiobuttons vertical
    class_outter_frame <- tkframe(upper_frame)
    Rcmdr::radioButtons(
        window  = class_outter_frame,
        name    = "class_",
        title   = gettext_bs("Options:"),
        buttons = c("print", "df", "dt", "tibble"),
        values  = c("print", "df", "dt", "tibble"),
        initialValue = "print",
        labels  = gettext_bs(
            c("Print class(es) of dataset to console",
              "Convert dataset to data frame (class 'data.frame')",
              "Convert dataset to data table (class 'data.table')",
              "Convert dataset to tibble (class 'tbl_df')")),
        command = do_nothing
    )
    # Layout
    tkgrid(upper_frame)
    tkgrid(class_outter_frame, sticky = "nw")
    tkgrid(class_Frame, padx = c(15, 5), pady = c(10, 10))

    # Help menus -------------------------------------------------------------
    help_menu <- function() {

        menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

        tkadd(menu_main, "command",
              label    = "Function 'class'",
              command  = open_help("class", package = "base"))

        tkadd(menu_main, "separator")

        tkadd(menu_main, "command",
              label    = "Function 'as.data.frame'",
              command  = open_help("as.data.frame", package = "base"))

        tkadd(menu_main, "command",
              label    = "Function 'as.data.table'",
              command  = open_help("as.data.table", package = "data.table"))

        tkadd(menu_main, "command",
              label    = "Function 'as_tibble'",
              command  = open_help("as_tibble", package = "tibble"))

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkpopup(menu_main,
                tkwinfo("pointerx", top),
                tkwinfo("pointery", top))
    }
    # Finalize ---------------------------------------------------------------
    ok_cancel_help(apply = "window_dataset_class", on_help = help_menu)
    tkgrid(buttonsFrame)
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
