# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_ci_mean <- function() {

    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ds     <- active_dataset()
    fg_col <- Rcmdr::getRcmdr("title.color")

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Confidence Interval of Mean"))

    tk_title(top, "Confidence interval of mean") # Title ~~~~~~~~~~~~~~~~~~~~

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        var_y      = NULL,
        var_gr     = NULL,
        use_groups = "0",
        method     = "boot_bca",
        type       = "bca",
        sides      = "two.sided"
    )
    initial <- getDialog("window_ci_mean", defaults)


    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ...

    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))


        return()
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
        putDialog("window_ci_mean", list(
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

            active_dataset(ds, flushModel = FALSE, flushDialogMemory = FALSE)

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


    # Widgets ----------------------------------------------------------------
    # * Y and Groups box ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    widget_y_gr <- tk_widget_boxes_y_gr(
        parent_frame   = top,

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

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)
    tkgrid(upper_frame, sticky = "w", pady = c(0, 5))

    # Radiobuttons
    rb_1_frame <- tkframe(upper_frame)
    Rcmdr::radioButtons(
        window = rb_1_frame,
        name  = "method_",
        title = "Method: ",
        title.color = fg_col,

        # right.buttons = FALSE,
        # sticky_buttons = "w",
        buttons = c("classic", "boot_bca", "boot_perc",
                    "boot_basic", "boot_norm",  "boot_stud"),

        values =  c("classic", "boot_bca", "boot_perc",
                    "boot_basic", "boot_norm",  "boot_stud"),

        labels =  c("Classic (t distribution based)",
                    "Bootstrap BCa",
                    "Bootstrap percentile",
                    "Bootstrap basic",
                    "Bootstrap normal",
                    "Bootstrap studentized"
                    ),
        initialValue = initial$method
    )

    # Radiobuttons
    rb_2_frame <- tkframe(upper_frame)
    Rcmdr::radioButtons(
        window = rb_2_frame,
        name  = "sides_",
        title = "Sides: ",
        title.color = fg_col,

        # right.buttons = FALSE,
        # sticky_buttons = "w",
        buttons = c("two.sided", "left", "right"),
        values =  c("two.sided", "left", "right"),
        labels =  c("Two-sided  ","Left", "Right"),
        initialValue = initial$sides
    )

    tkgrid(rb_1_frame, rb_2_frame, sticky = "nw")

    tkgrid(sides_Frame,  sticky = "nw")
    tkgrid(method_Frame, sticky = "nw")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    middle_frame <- tkframe(top)
    tkgrid(middle_frame, sticky = "w", pady = c(0, 5))

    # Check box
    bs_check_boxes(middle_frame,
                   frame         = "trim_frame",
                   boxes         = c("trim_"),
                   # commands    = list("check_locale_" = cmd_checkbox),
                   initialValues = c("0"),
                   labels        = gettext_bs(c("Timmed mean"))
    )

    textbox_1 <- bs_tk_textbox(
        parent_frame = middle_frame,
        init_val = "0.",
        width    = "5",
        label    = "(fraction to trim, 0 – 0.5): ",
        validate = "key",
        validatecommand = validate_num_0_0.5,
        invalidcommand  = make_red_text
    )

    # Layout

    tkgrid(trim_frame, textbox_1$frame, sticky = "w", columnspan = "2")


    # ========================================================================



    # Finalize ---------------------------------------------------------------

    # Help topic
    # OKCancelHelp(helpSubject = "mutate", helpPackage = "dplyr")

    ok_cancel_help(helpSubject = "xxx", helpPackage = "xxx",
                   reset = "window_ci_mean()",
                   apply = "window_ci_mean()")

    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Interactive bindings ---------------------------------------------------


    # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


}
