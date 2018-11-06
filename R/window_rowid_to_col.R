# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rowid_to_col <- function() {
    # Initialize -------------------------------------------------------------
    defaults      <- list(initial_position = "first")
    dialog_values <- getDialog("window_rowid_to_col", defaults)

    initializeDialog(title = gettextRcmdr("Create Row Numbers"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ds     <- activeDataSet()
    fg_col <- Rcmdr::getRcmdr("title.color")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    name_variable <- tclVar(unique_colnames("row_number"))
    name_frame    <- tkframe(upper_frame)
    name_entry    <- ttkentry(name_frame, width = "28", textvariable = name_variable)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rb_frame <- tkframe(upper_frame)
    radioButtons_horizontal(rb_frame,
                            title = "Column position: ",
                            title.color = fg_col,

                            # right.buttons = FALSE,
                            name = "position",
                            sticky_buttons = "w",
                            buttons = c("first",  "last"),
                            values =  c("first",  "last"),
                            labels =  c("First  ","Last  "),
                            initialValue = dialog_values$initial_position
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        new_name       <- trim.blanks(tclvalue(name_variable))
        which_position <- tclvalue(positionVariable)

        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (!is_valid_name(new_name)) {
            return()
        }

        if (!replace_duplicated_variable(new_name)) {
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_rowid_to_col",
                  list(initial_position = which_position))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cmd_position <-
            switch(which_position,
                   "first" = str_glue(
                       "%>% \n dplyr::select({new_name}, everything())"),
                   "last" = "")

        cmd_ungroup <- if (is_grouped_df(ds)) "ungroup() %>% \n" else ""

        command <- style_cmd(str_glue(
            '## Add column with row numbers \n',
            "{ds} <- {ds} %>% \n",
            "{cmd_ungroup}",
            "dplyr::mutate({new_name} = 1:n())",
            "{cmd_position}"))

        # Do commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        doItAndPrint(command)
        command_dataset_refresh()

        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # OKCancelHelp(helpSubject = "rowid_to_column", helpPackage = "tibble")
    OKCancelHelp(helpSubject = "mutate", helpPackage = "dplyr")

    # Title ------------------------------------------------------------------
    tkgrid(label_rcmdr(
        top,
        text = gettextRcmdr("Create column with row numbers"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame, pady = c(0, 5))
    tkgrid(name_frame, rb_frame, sticky = "w")

    tkgrid(
        label_rcmdr(
            name_frame,
            text = gettextRcmdr("Column name for row numbers:"),
            foreground = getRcmdr("title.color")),
        sticky = "w"
    )
    tkgrid(name_entry, sticky = "w")
    tkgrid(positionFrame, padx = c(15, 0))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
}