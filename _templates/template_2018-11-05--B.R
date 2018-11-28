# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_xxx <- function() {

    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ds     <- activeDataSet()
    fg_col <- Rcmdr::getRcmdr("title.color")

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("xxx_title"))

    tk_title(top, "xxx_title") # Title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        position = "first"
    )
    initial <- getDialog("window_xxx", defaults)


    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ...

    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        new_name       <- tclvalue_chr(name_variable)
        which_position <- tclvalue_chr(positionVariable)

        # Reset widget properties before checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # tkconfigure(name_entry, foreground = "black")

        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (is_empty_name(new_name)) {
            return()
        }

        if (is_not_valid_name(new_name)) {
            return()
        }

        if (forbid_to_replace_variables(new_name)) {
            return()
        }

        if (variable_is_not_selected(new_name, "variable")) {
            return()
        }

        if (variable_is_not_selected(new_name, "group variable")) {
            return()
        }

        # if (forbid_to_replace_object(new_name)) {
        #     return()
        # }

        # if (is_empty_name(new_name))              {return()}
        # if (is_not_valid_name(new_name))          {return()}
        # if (forbid_to_replace_variable(new_name)) {return()}
        # if (forbid_to_replace_object(new_name))   {return()}

        # if (??? == "") {
        # show_error_messages(
        #     "No ???  was selected.\nPlease select a ???.",
        #     title = "")
        # return()
        # }

        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_xxx", list(
            initial_position = which_position
        ))

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cmd_position <-
            switch(which_position,
                   "first" = str_glue(
                       "%>% \n dplyr::select({new_name}, everything())"),
                   "last" = "")

        cmd_ungroup <- if (is_grouped_df(ds)) "ungroup() %>% \n" else ""

        command <- str_glue(
            '## Add column with row numbers \n',
            "{ds} <- {ds} %>% \n",
            "{cmd_ungroup}",
            "dplyr::mutate({new_name} = 1:n())",
            "{cmd_position}")

        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("tidyverse")

        # doItAndPrint(command)
        result <- justDoIt(command)

        # result <- try_command(command)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            logger(style_cmd(command))
            # doItAndPrint(style_cmd(command))

            activeDataSet(ds, flushModel = FALSE, flushDialogMemory = FALSE)

            # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            closeDialog()


        } else {
            logger_error(command, error_msg = result)
            show_code_evaluation_error_message()
            return()
        }

        # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command_dataset_refresh()
        tkfocus(CommanderWindow())
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }


    # ... Widgets ============================================================
    # Widgets ----------------------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)

    # Text entry box
    name_variable <- tclVar(unique_colnames("row_number"))
    name_frame    <- tkframe(upper_frame)
    name_entry    <- ttkentry(name_frame, width = "28",
                              textvariable = name_variable)
    # Radiobuttons
    rb_frame <- tkframe(upper_frame)
    radioButtons_horizontal(
        rb_frame,
        title = "Column position: ",
        title.color = fg_col,

        # right.buttons = FALSE,
        name = "position",
        sticky_buttons = "w",
        buttons = c("first",  "last"),
        values =  c("first",  "last"),
        labels =  c("First  ","Last  "),
        initialValue = initial$position
    )

    # Layout
    tkgrid(upper_frame, pady = c(0, 5))
    tkgrid(name_frame, rb_frame, sticky = "w")

    tkgrid(
        label_rcmdr(
            name_frame,
            text = gettext_bs("Column name for row numbers:"),
            foreground = getRcmdr("title.color")),
        sticky = "w"
    )
    tkgrid(name_entry, sticky = "w")
    tkgrid(positionFrame, padx = c(15, 0))

    # Check box
    bs_check_boxes(upper_frame,
                   frame         = "check_locale_frame",
                   boxes         = c("check_locale_", "hide_output_"),
                   # commands      = list("check_locale_" = cmd_checkbox),
                   initialValues = c("1", "0"),
                   labels        = gettext_bs(c(
                       "Check if the locale can be used on this computer",
                       "Hide output"))
    )


    # * Y and Groups box =====================================================

    defaults <- list(
        var_y      = NULL,
        var_gr     = NULL,
        use_groups = "0"
    )

    widget_y_gr <- tk_widget_boxes_y_gr(
        y_title        = title_var_1,
        y_var_type     = "num",
        y_initial      = initial$var_y,
        y_select_mode  = "single",

        gr_title       = title_gr_0_n,
        gr_var_type    = "fct_like",
        gr_initial     = initial$var_gr,
        gr_select_mode = "multiple",

        ch_initial     = initial$use_groups
    )

    use_groups <- tclvalue_lgl(widget_y_gr$var_checkbox)
    var_y      <- get_selection(widget_y_gr$listbox_y)
    var_gr     <- get_selection(widget_y_gr$listbox_gr)


    # Finalize ---------------------------------------------------------------

    # Help topic
    # OKCancelHelp(helpSubject = "mutate", helpPackage = "dplyr")

    ok_cancel_help(helpSubject = "xxx", helpPackage = "xxx",
                   reset = "window_xxx()",
                   apply = "window_xxx()")

    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # Interactive bindings ---------------------------------------------------

    # Add interactivity for `fname_frame` and `fname_label`
    # tkbind(file_label,     "<ButtonPress-1>", on_click)
    # tkbind(fname_frame,    "<ButtonPress-1>", on_click)
    # tkbind(fname_label,    "<ButtonPress-1>", on_click)
    #
    # tkbind(fname_frame, "<Enter>",
    #        function() tkconfigure(fname_label, foreground = "blue"))
    # tkbind(fname_frame, "<Leave>",
    #        function() tkconfigure(fname_label, foreground = "black"))
    # # tkconfigure(file_label,     cursor = "hand2")
    # tkconfigure(fname_frame,    cursor = "hand2")
    # tkconfigure(button_ch_file, cursor = "hand2")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


}
