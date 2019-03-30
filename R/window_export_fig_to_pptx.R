
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_fig_to_pptx <- function() {
    # Fonts ------------------------------------------------------------------
    font_consolas_regular <- tkfont.create(family = "Consolas", size = 8)


    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ...

    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Išsaugome į PowerPoint -----------------------------------------------------


        file_save         <- "r_paveikslas.pptx"
        open_after_saving <- TRUE

        width    <- 7
        height   <- 5
        left     <- 0
        top      <- 0
        in_units <- "inches" # "cm"

        code <- get_values(f3_input)
        gg   <- "..." # TODO ???

        source_for_plot   <- "code" #, "gg_obj"

        switch(
            source_for_plot,

            "code" = {
                # code <- get_values(f3_input)

                code_error <- svTools::lint(text = code, type = "flat", sep = "|")

                if (code_error != "") {

                    msg <- str_c(
                        "Error in input code:", "\n\n",
                        str_c(capture.output(code_error), collapse = "\n"))

                    tk_messageBox(
                        parent = top,
                        type = "ok",
                        icon = "error",
                        message = msg,
                        caption = "R Code Syntax Error")

                    return()
                }
            },

            "gg_obj" = {

                # gg <- "..." # TODO ???

            },

            stop("Unknown option of `plot_source`")
        )

        gg_code <- switch(
            source_for_plot,

            "code" = {
                str_glue(
                    "code = {{ \n",
                    "    # Code that draws base R or ggplot2 plot \n",
                    "    {code} \n",
                    "}}")
            },

            "gg_obj" = {
                str_glue("ggobj = {gg}")
            },

            stop("Unknown option of `plot_source`")
        )


        file_open <-
            if (file.exists(file_save)) {
                str_glue('"{file_save}" %>% \n')

            } else {
                ""
            }

        code__open_after_saving <-
            if (open_after_saving) {
                str_glue(
                    '\n\n',
                    '## Open file \n',
                    'browseURL("{file_save}")'
                )
            } else {
                ""
            }

        # Save plot
        command <- str_c(
            sep = "\n",
            '## Save plot',
            '    {file_open} officer::read_pptx() %>%',
            '    officer::add_slide(layout = "Blank", master = "Office Theme") %>%',
            '    rvg::ph_with_vg_at(',
            '        top    = {top},    # {in_units}',
            '        left   = {left},   # {in_units}',
            '        height = {height}, # {in_units}',
            '        width  = {width},  # {in_units}',
            '        {gg_code}',
            '    ) %>%',
            '    print(target = "{file_save}")',

            '{code__open_after_saving}'
        ) %>%
            str_glue() %>%
            style_cmd()

        command %>%
            structure(class = c("glue", "character"))

        Library("tidyverse")
        Library("officer")
        Library("rvg")


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

        # if (object_is_not_selected(new_name))     {return()}
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
        putDialog("window_export_fig_to_pptx", list(
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
        # closeDialog()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command_dataset_refresh()
        tkfocus(CommanderWindow())
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # .ds    <- active_dataset() # active_dataset_0()
    # fg_col <- Rcmdr::getRcmdr("title.color")

    # Initialize dialog window and title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    dialogue_title <- "Export Editable Plot to PowerPoint File"
    initializeDialog(title = gettext_bs(dialogue_title))
    tk_title(top, dialogue_title)

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        position = "first"
    )
    initial <- getDialog("window_export_fig_to_pptx", defaults)


    # ... Widgets ============================================================
    # Widgets ----------------------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # upper_frame <- tkframe(top)
    #
    # # Text entry box
    # name_entry <- bs_tk_textbox(
    #     parent = upper_frame,
    #     width = 28,
    #     value = unique_colnames("row_number"),
    #     label = "Column name for row numbers:"
    # )
    #
    # # name_variable <- tclVar(unique_colnames("row_number"))
    # # name_frame    <- tkframe(upper_frame)
    # # name_entry    <- ttkentry(name_frame, width = "28",
    # #                           textvariable = name_variable)
    #
    # # Radiobuttons horizontal
    # rb_frame <- tkframe(upper_frame)
    # radioButtons_horizontal(
    #     rb_frame,
    #     title = "Column position: ",
    #     title.color = fg_col,
    #
    #     # right.buttons = FALSE,
    #     name = "position",
    #     sticky_buttons = "w",
    #     buttons = c("first",  "last"),
    #     values =  c("first",  "last"),
    #     labels =  c("First  ","Last  "),
    #     initialValue = initial$position
    # )
    #
    # # Layout
    # tkgrid(upper_frame, pady = c(0, 5))
    # # tkgrid(name_frame, rb_frame, sticky = "w")
    # tkgrid(name_entry$frame, rb_frame, sticky = "w")
    #
    # # tkgrid(
    # #     bs_label(
    # #         name_frame,
    # #         text = gettext_bs("Column name for row numbers:"),
    # #         foreground = getRcmdr("title.color")),
    # #     sticky = "w"
    # # )
    # # tkgrid(name_entry, sticky = "w")
    # # tkgrid(positionFrame, padx = c(15, 0))
    #
    # # Check box
    # bs_check_boxes(upper_frame,
    #                frame         = "check_locale_frame",
    #                boxes         = c("check_locale_", "hide_output_"),
    #                # commands      = list("check_locale_" = cmd_checkbox),
    #                initialValues = c("1", "0"),
    #                labels        = gettext_bs(c(
    #                    "Check if the locale can be used on this computer",
    #                    "Hide output"))
    # )
    #

    # F3, Frame 3, Preview ---------------------------------------------------
    # f3 <- tk2labelframe(top, relief = "flat", text = "Code input")

    f3 <- tk2frame(top)

    f3_input <- bs_text(
        f3, width = 80, height = 13, wrap = "none",
        autoseparators = TRUE, undo = TRUE,
        state = "normal",
        font = font_consolas_regular,
        tip = "Code that generates either a base R or ggplot2 plot. \nLattice and other systems are not suported.",
        label = "R Code of Plot to Export"
    )

    tkgrid(f3_input$frame, sticky = "news")
    tkgrid(f3)

    #
    #
    # # Radiobuttons vertical
    # into_outter_frame <- tkframe(upper_frame)
    # Rcmdr::radioButtons(
    #     window  = into_outter_frame,
    #     name    = "into_",
    #     title   = gettext_bs("Convert into"),
    #     buttons = c("character", "nominal", "ordinal", "integer", "numeric", "logical"),
    #     values  = c("character", "nominal", "ordinal", "integer", "numeric", "logical"),
    #     # initialValue = dialog_values$into,
    #     labels  = gettext_bs(
    #         c("Text (character)",
    #           "Nominal factors",
    #           "Ordinal factors",
    #           "Integers",
    #           "Real numbers",
    #           "Logical"
    #         )),
    #     command = function(){}
    # )
    # Layout
    # tkgrid(upper_frame)
    # tkgrid(getFrame(var_y_box), into_outter_Frame, sticky = "nw")
    # tkgrid(into_outter_frame, sticky = "nw")
    # tkgrid(into_Frame, padx = c(15, 5))


    # * Y and Groups box =====================================================

    # defaults <- list(
    #     var_y      = NULL,
    #     var_gr     = NULL,
    #     use_groups = "0"
    # )
    #
    # widget_y_gr <- tk_widget_boxes_y_gr(
    #     y_title        = title_var_1,
    #     y_var_type     = "num",
    #     y_initial      = initial$var_y,
    #     y_select_mode  = "single",
    #
    #     gr_title       = title_gr_0_n,
    #     gr_var_type    = "fct_like",
    #     gr_initial     = initial$var_gr,
    #     gr_select_mode = "multiple",
    #
    #     ch_initial     = initial$use_groups
    # )
    #
    # use_groups <- tclvalue_lgl(widget_y_gr$var_checkbox)
    # var_y      <- get_selection(widget_y_gr$listbox_y)
    # var_gr     <- get_selection(widget_y_gr$listbox_gr)
    #

    # Finalize ---------------------------------------------------------------

    # Help topic
    # OKCancelHelp(helpSubject = "mutate", helpPackage = "dplyr")

    ok_cancel_help(helpSubject = "ph_with_vg_at", helpPackage = "rvg",
                   reset = "window_export_fig_to_pptx()",
                   apply = "window_export_fig_to_pptx()")

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
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


}

# svTools::lint(text = get_values(f3_input), type = "flat", sep = "|")
