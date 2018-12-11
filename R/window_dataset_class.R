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
    .ds     <- activeDataSet()
    fg_col <- Rcmdr::getRcmdr("title.color")

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Change Class of Active Dataset"))

    tk_title(top, "Change class of active dataset") # Title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Widgets ----------------------------------------------------------------

    classes <- str_c(eval_glue("class({.ds})", envir_eval = .GlobalEnv),
                     collapse = ", ")

    tkgrid(bs_label_b(top, text = "Classes of active dataset:"))
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
            c("Print current class to console",
              "Convert to data frame ('data.frame')",
              "Convert to data table ('data.table')",
              "Convert to tibble ('tbl_df')")),
        command = function(){}
    )
    # Layout
    tkgrid(upper_frame)
    tkgrid(class_outter_frame, sticky = "nw")
    tkgrid(class_Frame, padx = c(15, 5), pady = c(10, 10))

    # Finalize ---------------------------------------------------------------
    ok_cancel_help()
    tkgrid(buttonsFrame)
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
