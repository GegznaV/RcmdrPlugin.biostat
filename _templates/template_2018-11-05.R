# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_xxx <- function() {

    # Initial values ---------------------------------------------------------
    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ds     <- activeDataSet()
    fg_col <- Rcmdr::getRcmdr("title.color")

    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ...

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults      <- list(initial_position = "first")
    dialog_values <- getDialog("window_xxx", defaults)


    # Frames and widgets -----------------------------------------------------
    # Initialize ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettextRcmdr("xxx_title"))

    # Populate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    name_variable <- tclVar(unique_colnames("row_number"))
    name_frame    <- tkframe(upper_frame)
    name_entry    <- ttkentry(name_frame, width = "28",
                              textvariable = name_variable)
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
    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        new_name       <- trim.blanks(tclvalue(name_variable))
        which_position <- tclvalue(positionVariable)

        # Reset widget properties before checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # tkconfigure(name_entry, foreground = "black")

        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (is_empty_name(new_name)) {
            return()
        }

        if (is_not_valid_name(new_name)) {
            return()
        }

        if (forbid_to_replace_variable(new_name)) {
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

        # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("tidyverse")

        doItAndPrint(command)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command_dataset_refresh()
        tkfocus(CommanderWindow())
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

    # Grid of widgets ========================================================

    # Title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(label_rcmdr(
        top,
        text = gettextRcmdr("xxx_title"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9))

    # Grid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

    # Help topic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "mutate", helpPackage = "dplyr")

    # Finalize ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
