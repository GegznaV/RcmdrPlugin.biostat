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
# f3_lab_4_1 <- ttklabel(f3, text = txt, foreground = "#A40802")


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

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_filename <- function() {
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
                    "{{Text files} {.csv .txt .dat .tab}}
                 {{All Files} *}")))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (filename == "") {
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        set_values(f1_ent_1_2, filename)

        tkicursor(f1_ent_1_2$obj_text, "end")
        tkxview.moveto(f1_ent_1_2$obj_text, "1") # 0 - beginning, 1 - end.

        if (fs::file_exists(filename)) {
            update_all()
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_input_text <- function() {

        filename <- get_values(f1_ent_1_2)

        if (fs::file_exists(filename) || is_url(filename)) {
            file_contents <-
                readr::read_lines(filename, n_max = 1000) %>%
                str_c(collapse = "\n")

            tk_normalize(f0_txt)
            tkdelete(f0_txt, "1.0", "end")
            tkinsert(f0_txt, "1.0", file_contents)
            tk_disable(f0_txt)

        } else {

            tkdelete(f0_txt, "1.0", "end")

            tk_messageBox(
                "ok",
                message = "The file was not found.",
                caption = "Not Found")
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_df_text <- function() {

        filename <- get_values(f1_ent_1_2)

        if (fs::file_exists(filename) || is_url(filename)) {
            ds_contents <- data.table::fread(filename, data.table = FALSE)

            op <- options(width = 10000)

            ds_preview <-
                capture.output(
                    print(tibble::as_tibble(ds_contents),
                          width = Inf,  n = 8)
                ) %>%
                str_subset("^(?!# A tibble.*)") %>%
                str_replace( "^# \\.\\.\\. with.*",
                             "... Other rows are not shown ...")

            options(op)


            if (length(ds_preview) <= 1) {
                # If no preview available

                tk_normalize(f00_txt)
                tkdelete(f00_txt, "1.0", "end")
                tkinsert(f00_txt, "1.0",
                         "Error! \n\nEither file format or import options may be inappropriate.")
                tk_disable(f00_txt)

                # Red font:
                tktag.add(f00_txt, "tmp", "1.0", "end")
                tktag.configure(f00_txt, "tmp", foreground = "red", justify = "center")


            } else {
                # Regular preview

                ds_preview <- str_c(ds_preview, collapse = "\n")

                tk_normalize(f00_txt)
                tkdelete(f00_txt, "1.0", "end")
                tkinsert(f00_txt, "1.0", ds_preview)
                tk_disable(f00_txt)

                # Change colors
                font_italic <- tkfont.create(slant = "italic", size = 8, family = "Consolas")
                font_bold   <- tkfont.create(weight = "bold",  size = 8, family = "Consolas")

                # Variable names
                tktag.add(f00_txt, "h1", "1.0", "2.0")
                tktag.configure(f00_txt, "h1",
                                # background = "#EFEFEF",
                                background = "white",
                                foreground = "blue",
                                font = font_bold)

                # Variable types
                tktag.add(f00_txt, "h2", "2.0", "3.0")
                tktag.configure(f00_txt, "h2",
                                foreground = "grey50",
                                font = font_italic)


                # Information
                tktag.add(f00_txt, "las", "11.0", "end")
                tktag.configure(f00_txt, "las", foreground = "grey50", font = font_italic)
            }


        } else {

            tkdelete(f00_txt, "1.0", "end")
        }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_name <- function(variables) {
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
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_all <- function() {
        update_input_text()
        update_df_text()
        update_name()

    }

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

    # frame 1, file locator ----------------------- --------------------------
    f1 <- ttkframe(top, relief = "flat", padding = 0, borderwidth = 0)

    tkgrid.columnconfigure(f1, 1, weight = 1)
    tkgrid(f1, padx = 10, sticky = "ew")


    f1_txt_1_1 <- bs_label_b(f1, text = "Import from: ")
    f1_ent_1_2 <- bs_entry(
        f1, width = 90, sticky = "we", tip = "Path to file or URL.")
    f1_but_1_3 <- tk2button(f1, width = 8, text = "Browse", command = get_filename,
                            tip = "Browse file on your computer.")
    f1_but_1_4 <- tk2button(f1, width = 8, text = "Update", command = update_all,
                            tip = "Update preview and name of data set.")

    f1_txt_2_1 <- bs_label_b(f1, text = "Name: ")
    f1_ent_2_2 <- bs_entry(
        f1, width = 90,  sticky = "we", tip = "Create a name for the dataset.")

    tkgrid(f1_txt_1_1, f1_ent_1_2$frame, f1_but_1_3, pady = c(10, 0))
    tkgrid(f1_txt_2_1, f1_ent_2_2$frame, f1_but_1_4, pady = c(2, 10))


    tkgrid.configure(f1_txt_1_1, f1_txt_2_1, sticky = "e")
    tkgrid.configure(f1_ent_1_2$frame, f1_ent_2_2$frame, sticky = "w")


    # frame 3, import parameters ------------ ---------------------------------


    # initialize values
    cb <- NULL

    nrows <- 50

    dec0 <- c(".", ",")
    dec1 <- c("Period ( . )", "Comma ( , )")

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


    enable_entry <- function(txt, default = "") {
        obj_1 <- get(str_glue("f3_box_{txt}"), envir = parent.frame())
        obj_2 <- get(str_glue("f3_ent_{txt}"), envir = parent.frame())

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

    select_custom <- function(txt, default = "") {
        obj_1 <- get(str_glue("f3_box_{txt}"), envir = parent.frame())
        obj_2 <- get(str_glue("f3_ent_{txt}"), envir = parent.frame())


        if (disabled(obj_2$obj_text)) {
            set_values(obj_2, default)
        }
        set_selection(obj_1, "Custom\u2026")
        tk_normalize(obj_2)

    }

    #     f_middle <- tk2frame(top) -----------------------------------------
    f_middle <- tk2frame(top)

    tkgrid(f_middle, sticky = "w")

    f3 <- tk2labelframe(f_middle, relief = "flat",
                        borderwidth = 5, padding = 5, text = "Import options")

    # tkdestroy(f3)

    f3_lab_head <- tk2label(f3, text = "Header")
    f3_lab_dec  <- tk2label(f3, text = "Decimal")
    f3_lab_sep  <- tk2label(f3, text = "Separator")
    f3_lab_skip <- tk2label(f3, text = "Skip lines")
    f3_lab_max  <- tk2label(f3, text = "Max. lines")
    f3_lab_na   <- tk2label(f3, text = "NA string")
    f3_lab_enc  <- tk2label(f3, text = "Encoding")
    f3_lab_quo  <- tk2label(f3, text = "Quote")
    f3_lab_out  <- tk2label(f3, text = "Return")


    tip_box_head <- "First row has column names."
    tip_box_dec  <- "Separator for decimal part \nof a number: 10.4 vs. 10,4."
    tip_box_sep  <- "Field (value) separator character."
    tip_box_skip <- "Number of rows to skip. \nInteger from 0 to infinity.\n0 equals to \"auto\"."
    tip_box_max  <- "Maximum number of rows to read. \nInteger from 0 to infinity."
    tip_box_na   <- "A character vector of strings which \nare interpreted as missing (NA) values."
    tip_box_quo  <- "Quoting characters. \nCharacters between quotes are read as one value."
    tip_box_enc  <- "Encoding."
    tip_box_out  <- "Class of imported data set."

    f3_box_head <- bs_combobox(f3, width = 13, values = head1, tip = tip_box_head, selection = 1)
    f3_box_dec  <- bs_combobox(f3, width = 13, values = dec1,  tip = tip_box_dec,  selection = 1)
    f3_box_sep  <- bs_combobox(f3, width = 13, values = sep1,  tip = tip_box_sep,  selection = 1, on_select = function() {enable_entry("sep", "||")})
    f3_box_skip <- bs_combobox(f3, width = 13, values = skip1, tip = tip_box_skip, selection = 1, on_select = function() {enable_entry("skip", "0")})
    f3_box_max  <- bs_combobox(f3, width = 13, values = max1,  tip = tip_box_max,  selection = 1, on_select = function() {enable_entry("max",  "0")})
    f3_box_na   <- bs_combobox(f3, width = 13, values = nas1,  tip = tip_box_na,   selection = 1, on_select = function() {enable_entry("na",   "?")})
    f3_box_quo  <- bs_combobox(f3, width = 13, values = quo1,  tip = tip_box_quo,  selection = 1, on_select = function() {enable_entry("quo",  "\"")})
    f3_box_enc  <- bs_combobox(f3, width = 13, values = enc1,  tip = tip_box_enc,  selection = 1)
    f3_box_out  <- bs_combobox(f3, width = 13, values = out1,  tip = tip_box_out,  selection = 1)


    tip_ent <- "Double click to enable."
    # f3_ent_dec  <- bs_tk_textbox(f3, width = 4)
    f3_ent_sep  <- bs_tk_textbox(f3, width = 4, tip = tip_ent, on_double_click = function() {select_custom("sep", "||")})
    f3_ent_skip <- bs_tk_textbox(f3, width = 4, tip = tip_ent, on_double_click = function() {select_custom("skip", "0")})
    f3_ent_max  <- bs_tk_textbox(f3, width = 4, tip = tip_ent, on_double_click = function() {select_custom("max",  "0")})
    f3_ent_na   <- bs_tk_textbox(f3, width = 4, tip = tip_ent, on_double_click = function() {select_custom("na",   "?")})
    f3_ent_quo  <- bs_tk_textbox(f3, width = 4, tip = tip_ent, on_double_click = function() {select_custom("quo",  "\"")})
    # f3_ent_enc  <- bs_tk_textbox(f3, width = 4)

    tkgrid(f3_lab_head, f3_box_head$frame, "x",               pady = c(2, 0))
    tkgrid(f3_lab_dec,  f3_box_dec$frame,  "x",               pady = c(2, 0))
    tkgrid(f3_lab_sep,  f3_box_sep$frame,  f3_ent_sep$frame,  pady = c(2, 0))
    tkgrid(f3_lab_skip, f3_box_skip$frame, f3_ent_skip$frame, pady = c(2, 0))
    tkgrid(f3_lab_max,  f3_box_max$frame,  f3_ent_max$frame,  pady = c(2, 0))
    tkgrid(f3_lab_na,   f3_box_na$frame,   f3_ent_na$frame,   pady = c(2, 0))
    tkgrid(f3_lab_quo,  f3_box_quo$frame,  f3_ent_quo$frame,  pady = c(2, 0))
    tkgrid(f3_lab_enc,  f3_box_enc$frame,  "x",               pady = c(2, 0))
    tkgrid(f3_lab_out,  f3_box_out$frame,  "x",               pady = c(2, 0))

    tkgrid.configure(
        f3_lab_head, f3_lab_dec, f3_lab_sep, f3_lab_skip, f3_lab_max, f3_lab_na,
        f3_lab_enc, f3_lab_quo, f3_lab_out,
        padx = c(3, 5), sticky = "w"
    )

    tkgrid.configure(
        f3_ent_sep$frame,
        f3_ent_skip$frame,
        f3_ent_max$frame,
        f3_ent_na$frame,
        f3_ent_quo$frame,
        padx = c(2, 0)
    )

    list(f3_ent_sep, f3_ent_skip, f3_ent_max, f3_ent_na, f3_ent_quo) %>%
        walk(tk_disable)

    # Check box
    f3_opts <- bs_checkboxes(
        parent = f3,
        boxes = c("check_names",
                  "strip_white",
                  "blank_lines_skip",
                  "fill",
                  "logical01",
                  "stringsAsFactors"
        ),
        values = c(0, 1, 0, 0, 0, 0),
        # commands      = list("check_locale_" = cmd_checkbox),
        labels = gettext_bs(c(
            "Check and adjust names",
            "Strip leading and tailing spaces",
            "Skip empty lines",
            "Fill unequal length rows",
            "Read 0/1 as FALSE/TRUE",
            "Convert strings to factors"
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
                "to factors (categorical variables).")

        ),
    )

    tkgrid(f3_opts$frame, padx = c(3, 0), pady = c(4, 2), columnspan = 3, sticky = "w")

    # frame b, text ------------ --------------------------------

    text_font <- tkfont.create(size = 8, family = "Consolas")

    f0 <- tk2labelframe(f_middle, relief = "flat", text = "Preview")

    bs_label_b(f0, text = "Input file") %>% tkgrid()
    f0_txt <- bs_text(f0, width = 70, height = 11, wrap = "none", undo = FALSE,
                      state = "disabled", font = text_font)

    bs_label_b(f0, text = "Dataset") %>% tkgrid()
    f00_txt <- bs_text(f0, width = 70, height = 11, wrap = "none", undo = FALSE,
                       state = "disabled", font = text_font)


    tkgrid(f3, f0, sticky = "nsw", padx = c(0, 5), pady = c(0, 15))

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

