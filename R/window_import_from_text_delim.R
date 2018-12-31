# fread(
#     input,
#     sep              = "auto",
#     dec              = c(".", ","),
#     quote            = c("\"", "'", "")
#
#     header           = c("auto", TRUE, FALSE)
#     skip             = "__auto__",
#     nrows            = Inf,
#
#     check.names      = FALSE,
#
#     na.strings       = "NA",  # due to change to ""; see NEWS
#     encoding         = c("unknown", "UTF-8", "Latin-1")
#
#     strip.white      = TRUE,
#     fill             = FALSE,
#     blank.lines.skip = FALSE,
#
#     stringsAsFactors = FALSE,
#     data.table       = TRUE

# ============================================================================
# select           = NULL,
# drop             = NULL,
# colClasses       = NULL,
# integer64        = getOption("datatable.integer64", "integer64"),

# col.names,
# key              = NULL,
# index            = NULL,
# showProgress     = getOption("datatable.showProgress", interactive()),
# nThread          = getDTthreads(verbose),
# logical01        = getOption("datatable.logical01", FALSE)  # due to change to TRUE; see NEWS
# verbose          = getOption("datatable.verbose", FALSE),
# file,
# text,
# cmd,
# sep2           ="auto",
# )


# txt <- paste(
#     "Comments located above data records and header lines will be preserved;",
#     "all other comments are ignored."
# )
# f2_lab_4_1 <- ttklabel(f2, text = txt, foreground = "#A40802")


# input,
# sep              = "auto",
# dec              = c(".", ",")
# quote            = c("\"", "'", "")

# skip             = "__auto__"
# nrows            = Inf


# na.strings       = "NA",  # due to change to ""; see NEWS
# encoding         = c("unknown", "UTF-8", "Latin-1")

# check.names      = FALSE
# strip.white      = TRUE
# fill             = FALSE
# blank.lines.skip = FALSE
# stringsAsFactors = FALSE
#
# header           = c("auto", TRUE, FALSE)
# data.table       = TRUE

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    putDialog("window_import_from_text_delim", list(
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


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_import_from_text_delim <- function() {
    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ...




    # # Replace contents of text box widget
    # tk_replace <- function(obj, txt = "") {
    #     init_state = tk_get_state(obj)
    #
    #     if (init_state == "disabled") {
    #         tk_normalize(obj)
    #     }
    #
    #     tkdelete(obj, "1.0", "end")
    #     tkinsert(obj, "1.0", txt)
    #
    #     if (init_state == "disabled") {
    #         tk_disable(obj)
    #     }
    # }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_path_to_file <- function() {
        initialdir <- get_values(f1_ent_1_2) %>% fs::path_dir()

        if (initialdir == "" || !fs::dir_exists(initialdir)) {
            initialdir <- getwd()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        filename <- tclvalue(
            tkgetOpenFile(
                # parent = top,
                initialdir = initialdir,
                title = "Choose Text File to Import",
                filetypes = gettext_bs(
                    "{{Text files} {.txt .csv .dat .tab}}
                 {{All Files} *}")))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (filename == "") {
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        set_values(f1_ent_1_2, filename)

        tkicursor(f1_ent_1_2$obj_text, "end")
        tkxview.moveto(f1_ent_1_2$obj_text, "1") # 0 - beginning, 1 - end.

        if (fs::is_file(filename)) {
            update_all()
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_input_text <- function() {

        filename <- get_values(f1_ent_1_2)

        if (fs::is_file(filename) || is_url(filename)) {

            file_contents <-
                readr::read_lines(filename, n_max = 1000) %>%
                str_c(collapse = "\n")

            set_values(f3_txt_1, file_contents)


        }

        else {
            # Delete text
            set_values(f3_txt_1, "")

            tk_messageBox(
                "ok",
                message = str_c(
                    'The file was not found. Check if\n',
                    'the name and the path are correct.'),
                caption = "Not Found")
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update contents of dataset preview window
    update_df_preview <- function() {

        switch(
            get_values(f2_but_from),
            "file" = {
                input <- get_values(f1_ent_1_2)
            },

            "clipboard" = {
                input <- get_values(f3_txt_1) %>% str_c("\n")
            }
        )

        suppressWarnings(
            ds_contents <- try(
                data.table::fread(input, data.table = FALSE),
                silent = TRUE)
        )

        err_msg <- NULL

        if (inherits(ds_contents, "try-error")) {
            err_msg <-
                ds_contents %>%
                str_replace("Error in .*\n", "") %>%
                str_replace("(does not exist)", "\n\\1") %>%
                str_replace("\\. ", ".\n") %>%
                str_trim()

        } else {
            op <- options(width = 10000)
            ds_preview <-
                capture.output(
                    print(tibble::as_tibble(ds_contents), width = Inf,  n = 8)
                ) %>%
                str_subset("^(?!# A tibble.*)") %>%
                str_replace( "^# \\.\\.\\. with.*",
                             "... Other rows are not shown ...")
            options(op)

            if (length(ds_preview) <= 1) {
                err_msg <-
                    "Either file format, input text or import options are incorrect."
            }
        }


        if (!is.null(err_msg)) {
            # If no preview available
            set_values(f3_txt_2, str_c("Error! \n\n", err_msg))
            # Red font:
            tktag.add(f3_txt_2, "tmp", "1.0", "end")
            tktag.configure(f3_txt_2, "tmp", foreground = "red3")

        } else {
            # Regular preview

            ds_preview <- str_c(ds_preview, collapse = "\n")

            set_values(f3_txt_2, ds_preview)

            # Change colors
            font_italic <- tkfont.create(slant = "italic", size = 8, family = "Consolas")
            font_bold   <- tkfont.create(weight = "bold",  size = 8, family = "Consolas")

            # Variable names
            tktag.add(f3_txt_2, "h1", "1.0", "2.0")
            tktag.configure(f3_txt_2, "h1",
                            # background = "#EFEFEF",
                            background = "white",
                            foreground = "blue",
                            font = font_bold)

            # Variable types
            tktag.add(f3_txt_2, "h2", "2.0", "3.0")
            tktag.configure(f3_txt_2, "h2",
                            foreground = "grey50",
                            font = font_italic)


            # Information
            tktag.add(f3_txt_2, "las", "11.0", "end")
            tktag.configure(f3_txt_2, "las",
                            foreground = "grey50",
                            font = font_italic)
        }

    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update dataset preview if autopreview is enabled
    auto_update_df <- function() {
        if (get_values(f2_opts)$auto_pteview) {
            update_df_preview()
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update contents of dataset entry box.
    update_name <- function(variables) {

        switch(
            get_values(f2_but_from),
            "file" = {
                filename <- get_values(f1_ent_1_2)
                if (filename != "") {
                    new_name <-
                        filename %>%
                        fs::path_file() %>%
                        fs::path_ext_remove() %>%
                        clean_str() %>%
                        unique_df_name()

                    set_values(f1_ent_2_2, new_name)
                }
            },

            "clipboard" = {
                if (get_values(f1_ent_2_2) == "") {
                    new_name <- unique_obj_names("dataset", all_numbered = TRUE)
                    set_values(f1_ent_2_2, new_name)
                }
            },

            stop("Unknown option: ", get_values(f2_but_from))
        )
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update input preview, dataset (DS) preview, and DS name boxes.
    update_all <- function() {
        update_name()
        update_df_preview()
        update_input_text()
    }

    # Clear both preview windows
    clear_preview <- function() {
        set_values(f3_txt_1, "")
        set_values(f3_txt_2, "")
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Select data import mode:
    # - from clipboard, enter data maually
    # - from file, URL
    set_mode_clipboard <- function() {
        # set_values(f3_txt_2, "")
        set_values(f2_but_from, "clipboard")
        tk_disable(f1_ent_1_2)
        tk_normalize(f3_txt_1)

        update_name()

    }

    set_mode_file_url <- function() {
        if (get_values(f2_but_from) != "file") {
            clear_preview()
        }

        set_values(f2_but_from, "file")
        tk_normalize(f1_ent_1_2)
        tk_disable(f3_txt_1)

        update_name()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Enable import options entry boxes.
    enable_entry <- function(txt, default = "") {
        obj_1 <- get(str_glue("f2_box_{txt}"), envir = parent.frame())
        obj_2 <- get(str_glue("f2_ent_{txt}"), envir = parent.frame())

        cond <- str_detect(get_selection(obj_1), "Custom")
        if (cond) {
            if (disabled(obj_2$obj_text)) {
                set_values(obj_2, default)
            }
            tk_normalize(obj_2)

        } else {
            set_values(obj_2, "")
            tk_disable(obj_2)
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Change combobox selection to "Custom..."
    select_custom <- function(txt, default = "") {
        obj_1 <- get(str_glue("f2_box_{txt}"), envir = parent.frame())
        obj_2 <- get(str_glue("f2_ent_{txt}"), envir = parent.frame())


        if (disabled(obj_2$obj_text)) {
            set_values(obj_2, default)
        }
        set_selection(obj_1, "Custom\u2026")
        tk_normalize(obj_2)

    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # Function onOK ----------------------------------------------------------
    onOK <- function() {}
    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # .ds     <- activeDataSet()
    fg_col <- Rcmdr::getRcmdr("title.color")

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Import Data from Text"))

    tk_title(top, "Import Data from Text") # Title ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        position = "first"
    )
    initial <- getDialog("window_import_from_text_delim", defaults)


    # ... Widgets ============================================================
    # Widgets ----------------------------------------------------------------

    # Frame 1, choose file and name ------------------------------------------
    f1 <- tk2frame(top)

    tkgrid.columnconfigure(f1, 1, weight = 1)
    tkgrid(f1, padx = 10, sticky = "ew")

    f1_txt_1_1 <- bs_label_b(f1, text = "File, URL: ")
    f1_ent_1_2 <- bs_entry(
        f1, width = 82, sticky = "we", tip = "Path to file or URL.")
    f1_but_1_3 <- tk2button(f1, width = 8, text = "Browse",
                            command = function() {
                                set_mode_file_url()
                                get_path_to_file()
                            },
                            tip = "Browse file on your computer.")

    f1_but_1_4 <- tk2button(f1, width = 8, text = "Update",
                            command = update_all,
                            tip = "Update preview and name of data set.")

    f1_txt_2_1 <- bs_label_b(f1, text = "Name: ")
    f1_ent_2_2 <- bs_entry(
        f1, width = 82,  sticky = "we", tip = "Create a name for the dataset.")


    tkgrid(f1_txt_1_1, f1_ent_1_2$frame, f1_but_1_3, f1_but_1_4, pady = c(10, 2))
    tkgrid(f1_txt_2_1, f1_ent_2_2$frame, "x",        "x",        pady = c(0,  10))


    # tkgrid.configure(f1_ent_1_2$frame, columnspan = 2, padx = 2)
    # tkgrid.configure(f1_ent_2_2$frame, columnspan = 2, padx = 2)

    tkgrid.configure(f1_txt_1_1, f1_txt_2_1, sticky = "e")
    tkgrid.configure(f1_ent_1_2$frame, f1_ent_2_2$frame, sticky = "w")

    # ~~ Middle frame ~~ -----------------------------------------------------
    f_middle <- tk2frame(top)
    tkgrid(f_middle, sticky = "w")


    # Frame 2, parameters ------------ ---------------------------------------

    # initialize values

    dec0 <- c(NA, ".", ",")
    dec1 <- c("Default", "Period ( . )", "Comma ( , )")

    sep0 <- c("auto", " ", "\t", ",", ";", "|", NA)
    sep1 <- c("Auto", "White space ( )", "Tab ( \\t )", "Comma ( , )", "Semicolon ( ; )", "Pipe ( | )", "Custom\u2026")

    nas0 <- c("NA", "na", "N/A", "n/a", "#N/A", "?", "", NA)
    nas1 <- c("NA", "na", "N/A", "n/a", "#N/A", "?", "", "Custom\u2026")

    quo0 <- c("\"", "'", "", NA)
    quo1 <- c("Double ( \" )", "Single ( \' )", "None", "Custom\u2026" )

    max1  <- c("All",  "Custom\u2026")
    skip1 <- c("Auto", "Custom\u2026")

    enc0 <- c("unknown", "UTF-8", "Latin-1")
    enc1 <- enc0

    head1 = c("Auto", "Yes", "No")
    out1 = c("Data frame", "Data table")


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2 <- tk2labelframe(f_middle, relief = "flat",
                        borderwidth = 5, padding = 5, text = "Import options")

    f2a <- tk2frame(f2)

    f2_txt_from <- bs_label(f2a, text = "From")

    f2_but_from <- bs_radiobuttons(
        parent  = f2a,
        buttons = c("file", "clipboard"),
        layout  = "horizontal",

        commands = list(
            "file"      = set_mode_file_url,
            "clipboard" = set_mode_clipboard
        ),

        labels  = gettext_bs(c(
            "File, URL",
            "Clipboard")),

        tips = list(
            "file" = str_c(
                "Import data from text file either \n",
                "on your computer or in the Internet\n",
                "(i.e., from URL)."
            ),

            "clipboard" = str_c(
                "Import data from text pasted from \n",
                "the clipboard or entered manually."
            )
        ),
    )

    tkgrid(f2a, columnspan = 3, sticky = "ew", pady = c(0, 2))
    tkgrid(f2_txt_from, f2_but_from$frame, sticky = "ew")
    tkgrid.configure(f2_txt_from, padx = c(2, 7))


    f2_lab_head <- tk2label(f2, text = "Header")
    f2_lab_dec  <- tk2label(f2, text = "Decimal")
    f2_lab_sep  <- tk2label(f2, text = "Separator")
    f2_lab_skip <- tk2label(f2, text = "Skip lines")
    f2_lab_max  <- tk2label(f2, text = "Max. lines")
    f2_lab_na   <- tk2label(f2, text = "NA string")
    f2_lab_enc  <- tk2label(f2, text = "Encoding")
    f2_lab_quo  <- tk2label(f2, text = "Quote")
    f2_lab_out  <- tk2label(f2, text = "Return")



    tip_box_head <- "First row has column names."
    tip_box_dec  <- "Separator for decimal part \nof a number: 10.4 vs. 10,4."
    tip_box_sep  <- "Field (value) separator character."
    tip_box_skip <- "Number of rows to skip. \nInteger from 0 to infinity.\n0 equals to \"auto\"."
    tip_box_max  <- "Maximum number of rows to read. \nInteger from 0 to infinity."
    tip_box_na   <- "A character vector of strings which \nare interpreted as missing (NA) values."
    tip_box_quo  <- "Quoting characters. \nCharacters between quotes are read as one value."
    tip_box_enc  <- "Encoding."
    tip_box_out  <- "Class of imported data set."


    f2_box_head <- bs_combobox(f2, width = 13, values = head1, tip = tip_box_head, selection = 1, on_select = function() {auto_update_df()})
    f2_box_dec  <- bs_combobox(f2, width = 13, values = dec1,  tip = tip_box_dec,  selection = 1, on_select = function() {auto_update_df()})
    f2_box_sep  <- bs_combobox(f2, width = 13, values = sep1,  tip = tip_box_sep,  selection = 1, on_select = function() {enable_entry("sep", "||"); auto_update_df()})
    f2_box_skip <- bs_combobox(f2, width = 13, values = skip1, tip = tip_box_skip, selection = 1, on_select = function() {enable_entry("skip", "0"); auto_update_df()})
    f2_box_max  <- bs_combobox(f2, width = 13, values = max1,  tip = tip_box_max,  selection = 1, on_select = function() {enable_entry("max",  "0"); auto_update_df()})
    f2_box_na   <- bs_combobox(f2, width = 13, values = nas1,  tip = tip_box_na,   selection = 1, on_select = function() {enable_entry("na",   "?"); auto_update_df()})
    f2_box_quo  <- bs_combobox(f2, width = 13, values = quo1,  tip = tip_box_quo,  selection = 1, on_select = function() {enable_entry("quo",  "\""); auto_update_df()})
    f2_box_enc  <- bs_combobox(f2, width = 13, values = enc1,  tip = tip_box_enc,  selection = 1, on_select = function() {auto_update_df()})
    f2_box_out  <- bs_combobox(f2, width = 13, values = out1,  tip = tip_box_out,  selection = 1, on_select = function() {auto_update_df()})

    tip_ent <- "Double click to enable."
    # f2_ent_dec  <- bs_entry(f2, width = 4)
    f2_ent_sep  <- bs_entry(f2, width = 4, tip = tip_ent, on_double_click = function() {select_custom("sep", "||");  auto_update_df()}, on_key_release = auto_update_df)
    f2_ent_skip <- bs_entry(f2, width = 4, tip = tip_ent, on_double_click = function() {select_custom("skip", "0");  auto_update_df()}, on_key_release = auto_update_df)
    f2_ent_max  <- bs_entry(f2, width = 4, tip = tip_ent, on_double_click = function() {select_custom("max",  "0");  auto_update_df()}, on_key_release = auto_update_df)
    f2_ent_na   <- bs_entry(f2, width = 4, tip = tip_ent, on_double_click = function() {select_custom("na",   "?");  auto_update_df()}, on_key_release = auto_update_df)
    f2_ent_quo  <- bs_entry(f2, width = 4, tip = tip_ent, on_double_click = function() {select_custom("quo",  "\""); auto_update_df()}, on_key_release = auto_update_df)
    # f2_ent_enc  <- bs_entry(f2, width = 4)

    tkgrid(f2_lab_head, f2_box_head$frame, "x",               pady = c(2, 0))
    tkgrid(f2_lab_dec,  f2_box_dec$frame,  "x",               pady = c(2, 0))
    tkgrid(f2_lab_sep,  f2_box_sep$frame,  f2_ent_sep$frame,  pady = c(2, 0))
    tkgrid(f2_lab_skip, f2_box_skip$frame, f2_ent_skip$frame, pady = c(2, 0))
    tkgrid(f2_lab_max,  f2_box_max$frame,  f2_ent_max$frame,  pady = c(2, 0))
    tkgrid(f2_lab_na,   f2_box_na$frame,   f2_ent_na$frame,   pady = c(2, 0))
    tkgrid(f2_lab_quo,  f2_box_quo$frame,  f2_ent_quo$frame,  pady = c(2, 0))
    tkgrid(f2_lab_enc,  f2_box_enc$frame,  "x",               pady = c(2, 0))
    tkgrid(f2_lab_out,  f2_box_out$frame,  "x",               pady = c(2, 0))

    tkgrid.configure(
        f2_lab_head, f2_lab_dec, f2_lab_sep, f2_lab_skip, f2_lab_max, f2_lab_na,
        f2_lab_enc, f2_lab_quo, f2_lab_out,
        padx = c(3, 5), sticky = "w"
    )

    tkgrid.configure(
        f2_ent_sep$frame,
        f2_ent_skip$frame,
        f2_ent_max$frame,
        f2_ent_na$frame,
        f2_ent_quo$frame,
        padx = c(2, 0)
    )

    list(f2_ent_sep, f2_ent_skip, f2_ent_max, f2_ent_na, f2_ent_quo) %>%
        walk(tk_disable)

    # Check box
    f2_opts <- bs_checkboxes(
        parent = f2,
        boxes = c("check_names",
                  "strip_white",
                  "blank_lines_skip",
                  "fill",
                  "logical01",
                  "stringsAsFactors",
                  "auto_pteview"
        ),
        default_command = auto_update_df,
        # commands = list(auto_pteview = function(){}),
        values = c(0, 1, 0, 0, 0, 0, 1),
        # commands      = list("check_locale_" = cmd_checkbox),
        labels = gettext_bs(c(
            "Check and adjust names",
            "Strip leading and tailing spaces",
            "Skip empty lines",
            "Fill unequal length rows",
            "Read 0/1 as FALSE/TRUE",
            "Convert strings to factors",
            "Automatically update preview"
        )),

        tips = list(
            "check_names" = str_c(
                "Check variable names to ensure that they are syntactically \n",
                "valid variable names. If necessary they are adjusted\n",
                "(by function `make.names`)."
            ),

            "strip_white" = str_c(
                "Strip leading and trailing  \n",
                "whitespaces of unquoted fields."
            ),

            "blank_lines_skip" = str_c(
                "Empty lines in the input are ignored."
            ),

            "fill" = str_c(
                "In case the rows have unequal length,\n",
                "blank fields are implicitly filled."
            ),

            "logical01" = str_c(
                "A column containing only 0s and 1s will be \n",
                "read as logical, otherwise as integer."
            ),

            "stringsAsFactors" = str_c(
                "Convert strings (text variables) \n",
                "to factors (categorical variables)."
            ),

            "auto_pteview" = str_c(
                "Turn off for big data files."
            )

        ),
    )

    tkgrid(f2_opts$frame, padx = c(3, 0), pady = c(4, 2), columnspan = 3, sticky = "w")

    # Frames 3, Preview ------------ --------------------------------
    text_font <- tkfont.create(size = 8, family = "Consolas")

    f3 <- tk2labelframe(f_middle, relief = "flat", text = "Preview")




    bs_label_b(f3, text = "Input") %>% tkgrid()
    f3_txt_1 <- bs_text(f3, width = 70, height = 11, wrap = "none",
                        undo = TRUE, state = "disabled", font = text_font)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f3_but <- tk2frame(f3)

    f3_but_1 <- tk2button(
        f3_but,
        width = 8,
        text = "Paste",
        command = function() {
            set_mode_clipboard()
            set_values(f3_txt_1, str_c(clipr::read_clip(), collapse = "\n"))
            update_df_preview()
        },
        tip = "Paste data from clipboard.")

    f3_but_2 <- tk2button(f3_but, width = 8, text = "Clear",
                          command = clear_preview,
                          tip = "Clear input text.")

    f3_but_3 <- tk2button(f3_but, width = 8, text = "Refresh",
                          command = function() {
                              update_df_preview()
                          },
                          tip = "Refresh dataset preview.")

    tkgrid(f3_but, sticky = "e", columnspan = 2)
    tkgrid(f3_but_1, f3_but_2, f3_but_3, sticky = "e")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    bs_label_b(f3, text = "Dataset") %>% tkgrid()
    f3_txt_2 <- bs_text(f3, width = 70, height = 11, wrap = "none",
                        undo = FALSE, state = "disabled", font = text_font)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f2, f3, sticky = "nsw", padx = c(0, 5), pady = c(0, 15))





    # # Frame 4 ---- -----------------------------------------------------------
    # f4 <- tk2frame(top)
    #
    # f1_but_4_1 <- tk2button(f4, width = 8, text = "Paste", # command = update_all,
    #                         tip = "Paste data from clipboard.")
    #
    # f1_but_4_2 <- tk2button(f4, width = 8, text = "Clear", # command = update_all,
    #                         tip = "Clear input text")
    #
    #
    #
    # tkgrid(f4,  sticky = "e")
    # tkgrid(f1_but_4_1, f1_but_4_2)
    #
    # tkgrid.columnconfigure(f4, 1, weight = 1)
    #
    #
    # tkgrid.configure(f1_but_3_4, sticky = "e")
    # tkgrid.configure(f1_but_3_3, sticky = "e")
    # tkgrid.configure(f1_txt_3_1, sticky = "w")
    # tkgrid.configure(f1_from$frame, sticky = "w")

    # ====... =================== ============================================
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Finalize ---------------------------------------------------------------

    # Help topic
    ok_cancel_help(helpSubject = "fread", helpPackage = "data.table",
                   reset = "window_import_from_text_delim()",
                   # apply = "window_import_from_text_delim()",
                   ok_label = "Import")

    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # Interactive bindings ---------------------------------------------------

}

