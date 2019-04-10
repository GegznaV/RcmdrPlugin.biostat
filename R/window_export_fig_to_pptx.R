# TODO:
# - Test the function with all combinations of plot sources.
# - Add tips for fields.
# - Message field should be left-aligned.
# - After pessing "Apply" or "Reset", incorrect size of plot is generated.
# - Add buttons for code
# - Add better code checking or linting

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_fig_to_pptx <- function() {
    # Fonts ------------------------------------------------------------------
    font_consolas_regular <- tkfont.create(family = "Consolas", size = 8)

    # Functions --------------------------------------------------------------

    # ~~~ Path to file -------------------------------------------------------

    # Open file select dialogue
    open_file_selection_dialogue <- function(f_path = fs::path(getwd(), ".")) {

        initialdir <- fs::path_dir(f_path)
        if (initialdir %in% c("", ".") || !fs::dir_exists(initialdir)) {
            initialdir <- getwd()
        }

        initialfile <- fs::path_file(f_path)
        if (initialfile %in%  c("", ".")) {
            initialfile <- "editable_r_plots.pptx"
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        file_name <- tclvalue(
            tkgetSaveFile(
                parent = top,
                # typevariable = typevariable, # to capture selected type
                title = "Create or Choose Text File to Save Data to",
                confirmoverwrite = FALSE,
                initialfile = initialfile,
                initialdir  = initialdir,
                filetypes   = "{ {PowerPoint file} {.pptx} }"))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        file_name
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_path_to_file <- function() {

        file_name <- open_file_selection_dialogue(f_path = read_path_to_file())

        if (get_use_relative_path()) { # make relative path
            file_name <- make_relative_path(file_name)
        }

        file_name <- fs::path_ext_set(file_name, "pptx")

        set_values(f1_ent_file, file_name)

        check_pptx_file()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Read value of file name entry box
    read_path_to_file <- function() {
        get_values(f1_ent_file)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    set_msg <- function(msg, color = "darkred") {
        tkconfigure(file_msg, text = msg, foreground = color)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    check_pptx_file <- function() {
        file <- read_path_to_file()

        if (tolower(fs::path_file(file)) == ".pptx") {
            set_msg("File name is missing.")
            return()
        }

        # If extension is not pptx
        if (tolower(fs::path_ext(file)) != "pptx") {

            set_msg("Please, add '.pptx' as the file name extension.")
            return()
        }

        file_exists <- fs::is_file(file)

        if (file_exists) {
            if (!is_file_writable(file)) {
                # If file is not writable
                set_msg("Please, CLOSE the file before saving the plot.")

            } else {
                set_msg("A new slide will be added to the file.",
                        color = "green")
            }

            return()

        } else {
            set_msg("A new PowerPoint file will be created.",
                    color = "green")
            return()
        }

        set_msg("")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    activate_options <- function() {
        switch(
            get_values(f3_source_of_plot),

            code_base  = {
                tkgrid.remove(f3_gg) # List of available ggplot2 objects
                tkgrid(f4)           # Code input box
            },
            code_print = {
                tkgrid.remove(f3_gg)
                tkgrid(f4)
            },
            code_gg    = {
                tkgrid.remove(f3_gg)
                tkgrid(f4)
            },
            obj_gg     = {
                tkgrid(f3_gg)
                # tk_disable(f3_gg_obj_name_box)
                tkgrid.remove(f4)
            },
            last_gg    = {
                tkgrid.remove(f3_gg)
                tkgrid.remove(f4)
            }
        )

        # Important for the first time

        if (is.null(ggplot2::last_plot())) {
            tk_disable(f3_source_of_plot, "last_gg")

            if (get_values(f3_source_of_plot) == "last_gg") {
                # Deselect disabled value
                set_values(f3_source_of_plot, "code_print")
            }
        }

        if (length(gg_objects) == 0) {
            tkgrid.remove(f3_gg)
            tk_disable(f3_source_of_plot, "obj_gg")

            if (get_values(f3_source_of_plot) == "obj_gg") {
                # Deselect disabled value
                set_values(f3_source_of_plot, "code_print")
            }
        }

    }

    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        pptx_file           <- get_values(f1_ent_file)

        source_of_plot      <- get_values(f3_source_of_plot)
        code                <- get_values(f4_code_input)
        gg_object_name      <- get_selection(f3_gg_obj_name_box)

        open_after_saving   <- get_values(f2_open_file_box, "open_file")

        pos_width           <- as.numeric(get_values(f3_width))
        pos_height          <- as.numeric(get_values(f3_height))
        pos_left            <- as.numeric(get_values(f3_left))
        pos_top             <- as.numeric(get_values(f3_top))
        in_units            <- "inches" # "cm"


        # Reset widget properties before checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # tkconfigure(name_entry, foreground = "black")

        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Check input code, if appropriate

        switch(
            source_of_plot,

            obj_gg = {
                if (nchar(gg_object_name) < 1) {
                    msg <- "No 'ggplot2' object is selected.\nPlease select one."
                    tk_messageBox(
                        parent = top,
                        type = "ok",
                        icon = "error",
                        message = msg,
                        caption = "Object is Not Selected")

                    return()
                }
            },

            code_base  = ,
            code_print = ,
            code_gg    = {

                if (str_trim(code) == "") {

                    msg <- str_c(
                        "Code field is empty.\n",
                        "Please, enter the code that generates a plot.")

                    tk_messageBox(
                        parent = top,
                        type = "ok",
                        icon = "error",
                        message = msg,
                        caption = "Code is Missing")

                    return()
                }
            }
        )

        # Check R syntax ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        switch(
            source_of_plot,

            code_base  = ,
            code_print = ,
            code_gg    = {
                # code <- get_values(f4_code_input)

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
            }
        )

        # Check if file name is not empty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (is_empty_name(pptx_file)) {
            return()
        }

        # If file name is missing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (tolower(fs::path_file(pptx_file)) == ".pptx") {
            msg <-
                str_c(
                    "Only extension '.pptx' is found but file name is missing.\n",
                    "Please, create a file name.")

            tk_messageBox(
                parent = top,
                type = "ok",
                icon = "error",
                message = msg,
                caption = "File Name Is Missing")

            return()
        }

        # If extension is pptx ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (tolower(fs::path_ext(pptx_file)) != "pptx") {
            msg <-
                str_c(
                    "Please, add '.pptx' as a file name extension.\n",
                    "E.g., 'r_plots.pptx'.")

            tk_messageBox(
                parent = top,
                type = "ok",
                icon = "error",
                message = msg,
                caption = "Wrong File Name Extension")

            return()
        }


        if (fs::is_file(pptx_file)) {
            if (!is_file_writable(pptx_file)) {

                msg <- str_c(
                    "It seems that PowerPoint file is open, busy or read-only.\n",
                    "Please, CLOSE the file before saving the plot or choose \n",
                    "another file.")

                tk_messageBox(
                    parent = top,
                    type = "ok",
                    icon = "error",
                    message = msg,
                    caption = "Cannot Write to File")

                return()
            }
        }


        # if (is_not_valid_name(pptx_file)) {
        #     return()
        # }

        # if (forbid_to_replace_variables(new_name)) {
        #     return()
        # }
        #
        # if (variable_is_not_selected(new_name, "variable")) {
        #     return()
        # }
        #
        # if (variable_is_not_selected(new_name, "group variable")) {
        #     return()
        # }

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
            pptx_file      = pptx_file,
            source_of_plot = source_of_plot,
            code           = code,
            open_file      = open_after_saving,
            pos_width      = pos_width,
            pos_height     = pos_height,
            pos_left       = pos_left,
            pos_top        = pos_top
        ))

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        gg_code <- switch(
            source_of_plot,

            "code_base" = {
                str_glue(
                    "code = {{ \n",
                    "    # Code that draws the plot \n",
                    "    {code} \n",
                    "}}")
            },

            "code_print" = {
                str_glue(
                    "code = {{ \n",
                    "    print(\n",
                    "      # Code that draws the plot \n",
                    "      {code} \n",
                    "    ) \n",
                    "}}")
            },

            "code_gg" = {
                str_glue(
                    "ggobj = {{ \n",
                    "    # Code that draws the plot \n",
                    "    {code} \n",
                    "}}")
            },

            "last_gg" =
                "ggobj = ggplot2::last_plot()",

            "obj_gg"  = {
                str_glue("ggobj = {gg_object_name}")
            },

            stop("Unknown option of `source_of_plot`")
        )

        file_open <-
            if (file.exists(pptx_file)) {
                str_glue('"{pptx_file}" %>% \n')

            } else {
                ""
            }

        code__open_after_saving <-
            if (open_after_saving) {
                str_glue(
                    '\n\n',
                    '## Open PowerPoint file \n',
                    'browseURL("{pptx_file}")'
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
            '        width  = {pos_width},  # {in_units}',
            '        height = {pos_height}, # {in_units}',
            '        left   = {pos_left},   # {in_units}',
            '        top    = {pos_top},    # {in_units}',
            '        {gg_code}',
            '    ) %>%',
            '    print(target = "{pptx_file}")',

            '{code__open_after_saving}'
        ) %>%
            str_glue()

        # command %>% structure(class = c("glue", "character"))

        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("tidyverse")
        Library("officer")
        Library("rvg")

        result <- justDoIt(command)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            logger(style_cmd(command))

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
        tkfocus(CommanderWindow())
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    gg_objects <- list_objects_of_class("gg", envir = .GlobalEnv)

    # Initialize dialog window and title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    dialogue_title <- "Export Editable Plot to PowerPoint File"
    initializeDialog(title = gettext_bs(dialogue_title))
    tk_title(top, dialogue_title)

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        pptx_file   = "editable_r_plots.pptx",
        source_of_plot = "code_print",
        code        = "",
        open_file   = FALSE,
        pos_width   = 7,
        pos_height  = 5,
        pos_left    = 0,
        pos_top     = 0
    )
    initial <- getDialog("window_export_fig_to_pptx", defaults)


    # ... Widgets ============================================================
    # Widgets ----------------------------------------------------------------


    file_msg <- bs_label(parent = top, text = "", fg = "darkred")


    # F1, Frame 1, choose file and name --------------------------------------
    f1 <- tk2frame(top)

    f1_lab_file <- bs_label_b(f1, text = "File: ")
    f1_ent_file <- bs_entry(
        f1, width = 60, sticky = "we", tip = "PowerPoint file name (new or existing)",
        value = initial$pptx_file,
        on_key_release = check_pptx_file)

    f1_but_set_1 <- tk2frame(f1)

    f1_but_paste <- tk2button(
        f1_but_set_1,
        # width = 7,
        # text = "Paste",
        image = "::image::bs_paste",
        command = function() {
            set_values(f1_ent_file, read_clipboard())
            tkicursor(f1_ent_file$obj_text, "end")
            tkxview.moveto(f1_ent_file$obj_text, "1")

            check_pptx_file()
        },
        tip = "Paste file name."
    )

    f1_but_clear <- tk2button(
        f1_but_set_1,
        # width = 7,
        # text = "Delete",
        image = "::image::bs_delete",
        command = function() {
            set_values(f1_ent_file, "")
            check_pptx_file()
        },
        tip = "Clear file name."
    )

    f1_but_update <- tk2button(
        f1_but_set_1,
        # width = 6,
        # text = "Update",
        # compound = "right",
        image = "::image::bs_refresh",
        command = function() {
            # Add extension
            set_values(f1_ent_file,
                       values = fs::path_ext_set(read_path_to_file(), "pptx"))
            check_pptx_file()
            },
        tip = str_c("Check file and add\n'.pptx' if missing.")
    )

    f1_but_f_choose <- tk2button(
        f1_but_set_1,
        # width = 7,
        # text = "Browse",
        image = "::image::bs_open_file",
        command = function() {
            get_path_to_file()
        },
        tip = "Choose file to export to."
    )


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # F2 ---------------------------------------------------------------------

    f2 <- tk2frame(top)

    f2_open_file_box <- bs_checkboxes(
        parent = f2,
        boxes  = c(open_file = "Open file after saving the plot"),
        values = c(open_file = initial$open_file))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # F3 ---------------------------------------------------------------------

    f3     <- tk2frame(top)

    f3_but <- tk2frame(f3)
    f3_pos <- tk2frame(f3)
    f3_gg  <- tk2frame(f3)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f3_pos_lab <- bs_label_b(parent = f3_pos, text = "Size and position of plot:")

    bs_size_entry <- purrr::partial(
        bs_entry, parent = f3_pos, width = 3, justify = "center", label_color = "black")

    f3_width  <- bs_size_entry(label = "Width",              value = initial$pos_width)
    f3_left   <- bs_size_entry(label = "Left side position", value = initial$pos_left)

    f3_height <- bs_size_entry(label = "Height",             value = initial$pos_height)
    f3_top    <- bs_size_entry(label = "Top side position",  value = initial$pos_top)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f3_source_of_plot <- bs_radiobuttons(
        f3_but, title = "Source of plot:",
        value = initial$source_of_plot,
        buttons = c(
            code_base  = "Code of base plot",
            code_print = "Code of plot to print",
            code_gg    = "Code of ggplot2 plot",
            obj_gg     = "ggplot2 object",
            last_gg    = "Last ggplot2 plot"),
        default_command = activate_options)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f3_gg_obj_name_box <- bs_listbox(
        parent = f3_gg,
        values = gg_objects,
        title  = "List of ggplot2 objects:",
        width  = 25, height = 7)

    # F4, Frame 4, Preview ---------------------------------------------------
    # F4 <- tk2labelframe(top, relief = "flat", text = "Code input")

    f4 <- tk2frame(top)

    f4_code_input <- bs_text(
        f4, width = 80, height = 13, wrap = "none",
        # autoseparators = TRUE,
        undo = TRUE,
        state = "normal",
        font = font_consolas_regular,
        tip = str_c(
            "Code that generates either an 'R' plot.\n",
            "Try several options for the best result.\n",
            "Right-click to clear or paste."),
        label = "R Code of Plot to Export"
    )


    context_menu_for_code <- function() {

        top <- CommanderWindow()

        menu_i <- tk2menu(tk2menu(top), tearoff = FALSE)

        # tkadd(menu_i, "command",
        #       label    = "Copy",
        #       compound = "left",
        #       image    = "::image::bs_delete",
        #       command  = do_nothing)

        tkadd(menu_i, "command",
              label    = "Clear",
              compound = "left",
              image    = "::image::bs_delete",
              command  = function() {
                  set_values(f4_code_input, "")
              })

        tkadd(menu_i, "command",
              label    = "Clear and paste",
              compound = "left",
              image    = "::image::bs_paste",
              command  = function() {
                  set_values(f4_code_input, read_clipboard())
              })

        tkpopup(menu_i,
                tkwinfo("pointerx", top),
                tkwinfo("pointery", top))
    }

    tkbind(f4_code_input$text, "<ButtonPress-3>", context_menu_for_code)


    # Widgets ================================================================

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(file_msg, sticky = "")
    # F1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f1, padx = 10, sticky = "we")

    tkgrid(f1_lab_file, f1_ent_file$frame, f1_but_set_1, pady = c(5, 5), sticky = "we")
    tkgrid(f1_but_f_choose, f1_but_paste, f1_but_clear, f1_but_update, sticky = "e")

    tkgrid.configure(f1_lab_file,       sticky = "e")
    tkgrid.configure(f1_ent_file$frame, sticky = "we", padx = 2)
    tkgrid.configure(
        f1_ent_file$frame_text,
        f1_ent_file$obj_text,
        sticky = "we")

    # F3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f3,     sticky = "nw")
    tkgrid(f3_pos, f3_but, f3_gg, sticky = "nw", padx = c(10, 0))

    tkgrid(f3_pos_lab, sticky = "w")
    tkgrid(f3_width$frame,  padx = c(0, 10), sticky = "e", pady = c(0, 2))
    tkgrid(f3_height$frame, padx = c(0, 10), sticky = "e", pady = c(0, 2))
    tkgrid(f3_left$frame,   padx = c(0, 10), sticky = "e", pady = c(0, 2))
    tkgrid(f3_top$frame,    padx = c(0, 10), sticky = "e", pady = c(0, 2))

    tkgrid(f3_source_of_plot$frame, padx = 15)

    tkgrid(f3_gg_obj_name_box$frame, sticky = "n")

    # F4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f4_code_input$frame, sticky = "news")
    tkgrid(f4, pady = c(10, 2))

    # F2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f2, padx = 10, sticky = "e")
    tkgrid(f2_open_file_box$frame,  pady = c(0, 8), sticky = "e")
    tkgrid.configure(f2_open_file_box$frame, sticky = "e", padx = c(15, 0))

    # Finalize ---------------------------------------------------------------

    ok_cancel_help(helpSubject = "ph_with_vg_at", helpPackage = "rvg",
                   # reset = "window_export_fig_to_pptx()",
                   # apply = "window_export_fig_to_pptx()",
                   ok_label = "Save")

    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix(bindReturn = FALSE)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    activate_options()
    check_pptx_file()
}

# svTools::lint(text = get_values(f4_code_input), type = "flat", sep = "|")
