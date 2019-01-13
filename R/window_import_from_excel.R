
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_import_from_excel <- function() {

    txt_trunc   <- "... Other rows are not shown ..."
    txt_not_all <- "... More rows may be present in the file ..."

    previous_file_name        <- tclVar("")
    previous_nrows_to_preview <- tclVar("")

    biostat_env$file_contents <- ""

    on.exit({
        biostat_env$file_contents <- ""
        biostat_env$possibly_more_rows <- NULL
    })


    # Functions ==============================================================
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~ Read import options --------------------------------------------------
    # Shortcut function

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_header <- function() {
        val <- get_selection(f2_box_head)
        switch(val,
               "Yes"  = TRUE,
               "No"   = FALSE,
               stop("Value '", val, "' is unknown (f2_box_head)."))
    }

    get_code_header <- function() {
        val <- get_selection(f2_box_head)
        switch(val,
               "Yes"  = ", header = TRUE",
               "No"   = ", header = FALSE",
               stop("Value '", val, "' is unknown (f2_box_head)."))
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_skip <- function() {
        # get_values(f2_box_skip)
        val <- get_selection(f2_box_skip)
        switch(val,
               "Auto"            = "__auto__",
               "Custom\u2026"    = as.numeric(get_values(f2_ent_skip)),
               stop("Value '", val, "' is unknown (f2_box_skip)."))
    }

    get_code_skip <- function() {
        val <- get_selection(f2_box_skip)
        switch(val,
               "Auto" = "",
               str_c(', skip = ', get_skip())
               # str_c(', skip = "', get_skip(), '"')
        )
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_nrows_to_import <- function() {
        if (get_selection(f2_box_max) == "All") {
            Inf
        } else {
            as.numeric(get_values(f2_ent_max))
        }
    }

    get_code_nrows <- function() {
        if (get_selection(f2_box_max) == "All") {
            ""
        } else {
            str_c(", nrows = ", get_nrows_to_import())
        }
    }

    # Select value for f3_box_nrow_1 box ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_nrows_preview_input <- function() {
        val <- get_selection(f3_box_nrow_1)
        switch(val, "All" = Inf, as.integer(val))
    }

    get_nrows_preview_ds <- function() {
        val <- get_selection(f3_box_nrow_2)
        switch(val, "All" = Inf, as.integer(val))
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_na_str <- function() {
        val <- get_selection(f2_box_na)
        switch(val,
               "Default"      = getOption("datatable.na.strings", "NA"),
               "Empty"        = "",
               "None"         = NULL,
               "Custom\u2026" = get_values(f2_ent_na),
               val)
    }

    get_code_na_str <- function() {
        val <- get_selection(f2_box_na)
        switch(val,
               "Default" = "",
               "None"    =  str_c(',\n na.strings = NULL'),
               str_c(',\n na.strings = "', get_na_str(), '"')
        )
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_df_vs_dt <- function() {
        val <- get_selection(f2_box_out)
        switch(val,
               "Data frame" = TRUE,
               # "Data table" = TRUE,
               "Tibble"     = FALSE,
               stop("Value '", val, "' is unknown (f2_box_out)."))
    }

    get_code_df_vs_dt <- function() {
        str_c(',\n data.table = ', get_df_vs_dt())
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # check.names      = get_values(f2_opts, "check_names"),
    get_code_check_names <- function() {
        val <- get_values(f2_opts, "check_names")
        if (isTRUE(val)) {
            str_c(',\n check.names = ', val)
        } else {
            ""
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_code_stringsAsFactors <- function() {
        val <- get_values(f2_opts, "stringsAsFactors")
        if (isTRUE(val)) {
            str_c(', stringsAsFactors = ', val)
        } else {
            ""
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_code_strip_white <- function() {
        val <- get_values(f2_opts, "strip_white")
        if (isTRUE(val)) {
            ""
        } else {
            str_c(',\n strip.white = ', val)
        }
    }

    # ~ File -----------------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Open file select dialogue
    get_path_to_file <- function() {
        initialdir <- read_path_to_file() %>% fs::path_dir()

        if (initialdir == "" || !fs::dir_exists(initialdir)) {
            initialdir <- getwd()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        filename <- tclvalue(
            tkgetOpenFile(
                # parent = top,
                initialdir = initialdir,
                title = "Choose Excel File to Import",
                filetypes = gettext_bs(
                    "{{Excel file}          {.xlsx .xls}}
                    {{Excel open XML file}   .xlsx}
                    {{Excel 97-2003 file}    .xls}
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
    # Read value of file name entry box
    read_path_to_file <- function() {
        get_values(f1_ent_1_2)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check, if file exist or is URL
    # Return TRUE on success
    #        FALSE on failure.
    check_file_name <- function(on_success = do_nothing, on_failure = do_nothing) {
        filename <- read_path_to_file()

        if (fs::is_file(filename) || is_url(filename)) {
            on_success()
            return(TRUE)

        } else {
            # Delete text
            # clear_input_window()

            tk_messageBox(
                parent = top,
                type = "ok",
                icon = "error",
                message = str_c(
                    'The file was not found. Check if the name and \n',
                    'the path in the box "File, URL" are correct and\n',
                    'not empty.'),
                caption = "File Not Found")
        }

        on_failure()
        return(FALSE)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check if file contents need to be updated and return TRUE or FALSE
    need_update_from_file <- function() {
        filename <- read_path_to_file()

        if (get_import_mode() != "file" || (filename == ""))
            return(FALSE)

        changed_filename <- tclvalue_chr(previous_file_name) != filename

        changed_nrows_to_preview <-
            tclvalue_chr(previous_nrows_to_preview) != get_selection(f3_box_nrow_1)

        any(changed_nrows_to_preview, changed_filename)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Values used to check if update from file is needed
    update_previous_values <- function(variables) {
        tclvalue(previous_nrows_to_preview) <- get_selection(f3_box_nrow_1)
        tclvalue(previous_file_name)        <- read_path_to_file()
    }


    # ~ Preview window ---------------------------------------------------------

    write_dataset_window <- function(contents, ...) {
        set_values(f3_dataset, values = contents, ...)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    clear_dataset_window <- function() {
        write_dataset_window("")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



    # ~ Read data ------------------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # `fread` with options from dialogue window
    do_read_excel <- function(input, nrows = get_nrows_to_import()) {
        readxl::read_excel(
            input,
            sheet        = get_xl_sheet(), # NULL
            range        = get_xl_range(), # NULL  # Takes precedence over skip, n_max and sheet.
            col_names    = get_header(),
            skip         = get_skip(),     # 0
            n_max        = nrows,          # Inf
            na           = get_na_str(),
            trim_ws      = get_values(f2_opts, "strip_white"),
            .name_repair = get_name_repair()
        )

        if (get_values(f2_opts, "stringsAsFactors")) {
            # [???]
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_xl_range <- function() {
        NULL
    }

    get_xl_sheet <- function() {
        NULL
    }

    get_name_repair <- function() {
        name_repair <- c("minimal", "unique", "check_unique", "universal")[3]
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    read_text_from_file <- function() {
        filename <- read_path_to_file()

        n_rows       <- get_nrows_preview_input()
        n_rows_readr <- if (is.infinite(n_rows)) -1L else n_rows

        # Read data
        file_contents <- try(
            readr::read_lines(filename, n_max = n_rows_readr),
            silent = TRUE)

        biostat_env$file_contents       <- file_contents
        biostat_env$possibly_more_rows  <- length(file_contents) >= n_rows

        # Update previous values
        update_previous_values()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Format fread error for display
    parse_fread_error <- function(err) {
        err %>%
            str_replace("Error in .*\n", "") %>%
            str_replace("(does not exist)", "\n\\1") %>%
            str_replace("\\. ", ".\n") %>%
            str_trim()
    }

    # ~ Preview --------------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update contents of dataset preview window
    refresh_dataset_window <- function() {

        if (is_nothing_to_import()) {
            clear_dataset_window()
            return()
        }

        input <- get_input_by_mode() # [???]

        # Get data from input
        suppressWarnings({
            ds_contents <- try(
                do_read_excel(str_c(input, collapse =  "\n")),
                silent = TRUE)
        })


        # Check errors
        err_msg <- NULL

        if (inherits(ds_contents, "try-error")) {
            err_msg <- parse_fread_error(ds_contents)

        } else {

            nrows_from_file <- nrow(ds_contents)
            nrow_preview_ds <- get_nrows_preview_ds()


            # Get contents to preview
            switch(
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                get_selection(f3_box_type),
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                "Tibble" = {

                    op <- options(width = 10000)
                    ds_preview <-
                        capture.output(
                            print(tibble::as_tibble(ds_contents),
                                  width = Inf,
                                  n = nrow_preview_ds)
                        ) %>%
                        str_subset("^(?!# A tibble.*)") %>%
                        str_replace( "^# \\.\\.\\. with.*", txt_trunc)

                    options(op)

                    if (possibly_more_rows()) {
                        ds_preview <- c(ds_preview, txt_not_all)
                    }

                },
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                "Data table" = {

                    topn <-
                        if (is.infinite(nrow_preview_ds)) {
                            nrows_from_file
                        } else {
                            floor(nrow_preview_ds/2)
                        }

                    op <- options(width = 10000)
                    ds_preview <-
                        capture.output(
                            print(data.table::as.data.table(ds_contents),
                                  col.names = "top",
                                  class = TRUE,
                                  topn  = topn,
                                  nrows = nrows_from_file)
                        )

                    options(op)

                    if (possibly_more_rows()) {

                        ds_preview[length(ds_preview)] <- txt_not_all
                    }

                },
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                "Structure" = {
                    ds_preview <- capture.output(glimpse(ds_contents, width = 74))

                    if (possibly_more_rows())  {
                        ds_preview <- str_replace(
                            ds_preview,
                            "(?<=^Observations: )(.*)", "\\1 or more")
                    }

                }
            )
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


            if (length(ds_preview) <= 1) {
                err_msg <- str_c(
                    "Possible reasons:\n",
                    " - file name is incorrent or missing;\n",
                    " - file is not a text file (incorrect format);\n",
                    " - file is empty;\n",
                    " - import options are incorrect.")
            }
        }


        if (!is.null(err_msg)) {
            # If no preview available:
            write_dataset_window(str_c("Error! \n\n", err_msg))

            # Red font:
            tktag.add(f3_dataset$text, "bold",  "1.0", "end")
            tktag.add(f3_dataset$text, "error", "1.0", "end")

        } else {
            # Write contents:
            write_dataset_window(str_c(ds_preview, collapse = "\n"))

            # Format contents:

            # Info messages
            tktag_add_row(ds_preview, txt_trunc,   f3_dataset$text, "info")
            tktag_add_row(ds_preview, txt_not_all, f3_dataset$text, "info")

            pattern_num <- "(?<=\\<)(num|int|dbl)(?=\\>)"
            pattern_chr <- "(?<=\\<)cha?r(?=\\>)"

            switch(
                get_selection(f3_box_type),
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                "Tibble" = ,
                "Data table" = {
                    # Variable names
                    tktag.add(f3_dataset$text, "var_names", "1.0", "2.0")

                    # Variable types
                    tktag.add(f3_dataset$text, "var_types", "2.0", "3.0")
                    tktag_add(ds_preview[1:2], pattern_chr, f3_dataset$text, "chr")
                    tktag_add(ds_preview[1:2], pattern_num, f3_dataset$text, "num")

                    # Separator
                    tktag_add_row(ds_preview, "^\\s*---\\s*$", f3_dataset$text, "red")

                },

                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                "Structure" = {
                    # Variable names
                    # tktag_add_row(ds_preview, "^Variables: ", f3_dataset$text, "var_names")

                    tktag_add_first(ds_preview, "(?<=\\$).*?(?=\\<)",
                                    f3_dataset$text, "var_names")

                    # Variable types
                    tktag_add_first(ds_preview, "^\\$",       f3_dataset$text, "var_types")
                    tktag_add_first(ds_preview, "\\.\\.\\.$", f3_dataset$text, "var_types")
                    tktag_add_first(ds_preview, "\\<.*?\\>",  f3_dataset$text, "var_types")
                    tktag_add_first(ds_preview, pattern_chr,  f3_dataset$text, "chr")
                    tktag_add_first(ds_preview, pattern_num,  f3_dataset$text, "num")

                    # Observations
                    tktag_add_row(ds_preview, "^Observations: \\d+ or more", f3_dataset$text, "grey")
                }
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            )
        }
    }


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update contents of dataset entry box.
    update_name_entry <- function(variables) {

        switch(
            get_import_mode(),
            "file" = {
                filename <- read_path_to_file()
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

            stop("Unknown option: ", get_import_mode())
        )
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Clear both preview windows
    clear_preview <- function() {
        clear_dataset_window()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update input preview, dataset (DS) preview, and DS name boxes.
    update_all <- function() {
        update_name_entry()
        read_text_from_file()

        check_file_name()
        # update_from_file()
        refresh_dataset_window()
        highlight_update_button()
    }

    # ~ Change properties ----------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Enable import options entry boxes and refresh_dataset_window().
    # txt     - ending of widget's name
    # default - default value on activation/normalization
    # tip_active  - tip in active/normal mode
    # tip_disabled - tip in disabled mode

    enable_entry <- function(txt, default = "", tip_disabled = NULL, tip_active = NULL) {
        obj_1 <- get(str_glue("f2_box_{txt}"), envir = parent.frame())
        obj_2 <- get(str_glue("f2_ent_{txt}"), envir = parent.frame())

        cond <- str_detect(get_selection(obj_1), "Custom")
        if (cond) {
            if (disabled(obj_2$obj_text)) {
                set_values(obj_2, default)
            }
            tk_normalize(obj_2)
            if (!is.null(tip_active)) {
                tk2tip(obj_2$obj_text, tip_active)
            }

        } else {
            set_values(obj_2, "")
            tk_disable(obj_2)
            if (!is.null(tip_disabled)) {
                tk2tip(obj_2$obj_text, tip_disabled)
            }
        }

        refresh_dataset_window()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    highlight_update_button <- function() {
        if (is_file_name_missing()) {
            tk_disable(f1_but_1_5)
        } else {
            tk_normalize(f1_but_1_5)
            if (need_update_from_file()) {
                tk_activate(f1_but_1_5)
                tkconfigure(f1_but_1_5, default = "active")
            } else {
                tkconfigure(f1_but_1_5, default = "normal")
            }
        }
    }

    # ~ Input validation -----------------------------------------------------
    make_red_text_reset_val <- function(to = "Inf") {
        function(P, W, S, v, s) {
            tcl("after", "idle", function() {tkconfigure(W, validate = v)})
            tkconfigure(W, foreground = "red2")
            tkdelete(W, "0", "end")
            tkinsert(W, "0", to)

            tcl("expr", "TRUE")
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    is_file_name_missing <- function() {
        str_trim(read_path_to_file()) == ""
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    is_nothing_to_import <- function() {
        switch(
            get_import_mode(),
            "file" = {
                if (is_file_name_missing()) {
                    clear_preview()
                    return(TRUE)
                }
            },

            "clipboard" = {
                if (str_trim(read_input_window()) == "") {
                    clear_preview()
                    return(TRUE)
                }
            }
        )
        FALSE
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    msgbox_clear_input <- function() {
        tk_messageBox(
            parent = top,
            type = "yesno",
            default = "no",
            icon = "warning",
            message = str_c(
                'The contents of the Input window will be deleted. \n',
                'Do you agree?'
            ),
            caption = "Clear Input")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Checks possibility that file has more rows than previewed
    possibly_more_rows <- function() {         # [???]
        isTRUE(biostat_env$possibly_more_rows)
    }


    # ~ onOK -------------------------------- --------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        new_name  <- get_values(f1_ent_2_2)
        from      <- get_import_mode()

        # Reset widget properties before checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # tkconfigure(name_entry, foreground = "black")

        # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_import_from_excel", list(
            preview_ds_type = get_selection(f3_box_type)
        ))


        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (is_empty_name(new_name)) {
            return()
        }

        if (is_not_valid_name(new_name)) {
            return()
        }

        if (forbid_to_replace_object(new_name)) {
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        new_name <- safe_names(new_name)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # ~~ From file -------------------------------------------------------
        file_name <- read_path_to_file()

        # Check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Check if file exists or is URL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (!check_file_name()) {
            return()
        }

        # If URL, check if internet connection is present.
        if (is_url(file_name) && !pingr::is_online()) {
            tk_messageBox(
                parent = top,
                message = str_c(
                    "It seems that your file is on the Internet, but you are offline.\n",
                    "Please, check Internet connection."
                ),
                icon  = "warning",
                caption = "No Internet Connection",
                type  = "ok")

            return()
        }

        #  Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        Library("data.table")

        command <- str_glue(
            '## Import data from text file \n',
            '{new_name} <- readxl::xlread(\n',
            '"{file_name}"',

            get_code_xl_sheet(),
            get_code_xl_range(),
            get_code_header(),
            get_code_skip(),
            get_code_nrows(),
            get_code_na_str(),
            get_code_strip_white(),
            get_code_check_names(),
            ")",
            get_code_df_vs_dt()
        )

        # ~~ Apply commands --------------------------------------------------
        result <- justDoIt(command)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            logger(style_cmd(command))
            active_dataset(new_name, flushModel = FALSE, flushDialogMemory = FALSE)

            # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Import Data from Excel"),
                     suppress.window.resize.buttons = FALSE)
    tk_title(top, "Import Data from Excel")

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        preview_ds_type = "Data table"
    )
    initial <- getDialog("window_import_from_excel", defaults)


    # Widgets ================== =============================================

    # F1, Frame 1, choose file and name --------------------------------------
    f1 <- tk2frame(top)

    f1_lab_1_1 <- bs_label_b(f1, text = "File, URL: ")
    f1_ent_1_2 <- bs_entry(
        f1, width = 90, sticky = "we", tip = "Path to file or URL.",
        on_key_release = highlight_update_button)

    f1_but_1 <- tk2frame(f1)

    f1_but_1_3 <- tk2button(
        f1_but_1,
        # width = 7,
        # text = "Paste",
        image = "::image::bs_paste",
        command = function() {
            set_values(f1_ent_1_2,
                       str_c(read_path_to_file(), read_clipboard()))
            tkicursor(f1_ent_1_2$obj_text, "end")
            tkxview.moveto(f1_ent_1_2$obj_text, "1")

            highlight_update_button()
        },
        tip = "Paste file name or URL."
    )

    f1_but_1_4 <- tk2button(
        f1_but_1,
        # width = 7,
        # text = "Delete",
        image = "::image::bs_delete",
        command = function() {
            set_values(f1_ent_1_2, "")
            highlight_update_button()
        },
        tip = "Clear file name or URL."
    )

    f1_but_1_5 <- tk2button(
        f1_but_1,
        # width = 6,
        # text = "Update",
        # compound = "right",
        image = "::image::bs_down",
        command = update_all,
        tip = str_c("Read file (URL) and update preview.")
    )

    f1_but_1_6 <- tk2button(
        f1_but_1,
        # width = 7,
        # text = "Browse",
        image = "::image::bs_open_file",
        command = function() {

            if (allow_switch_to_file_mode2() == "no") {
                return()
            }

            set_mode_file_url()
            get_path_to_file()
        },
        tip = "Choose file to import."
    )

    f1_lab_2_1 <- bs_label_b(f1, text = "Name: ")
    f1_lab_3_1 <- bs_label_b(f1, text = "Sheet: ")
    f1_ent_2_2 <- bs_entry(
        f1, width = 36,  sticky = "ew", tip = "Create a name for the dataset.")

    f1_box_wsh <- bs_combobox(
        label = "Sheet:",
        parent = f1,
        values = "",
        tip = "Choose worksheet to import data from.",
        # onSelect_fun     = correct_worksheet_selection,
        width = 25
    )

    f1_ent_rng <- bs_entry(
        label = "Range:",
        f1, width = 10, sticky = "we", tip = "Path to file or URL.",
        on_key_release = highlight_update_button)


    # F2-3, Middle frame -----------------------------------------------------

    f_middle <- tk2frame(top)


    # F2, Frame 2, parameters ------------------------------------------------

    f2 <- tk2labelframe(f_middle, relief = "flat",
                        borderwidth = 5, padding = 5, text = "Import options")

    f2a <- tk2frame(f2)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tip_head <- "First row has column names."

    f2_txt_head <- bs_label(f2a, text = "Header", tip = tip_head)
    f2_but_head <- bs_radiobuttons(
        parent  = f2a,
        buttons = c("TRUE"  = "Yes",
                    "FALSE" = "No"),
        layout  = "horizontal",
        default_tip = tip_head

        # tips = list(
        #     "TRUE"  = str_c(),
        #     "FALSE" = str_c()
        # )
    )

    # f2_txt <- bs_label(f2a, text = "Header")
    # f2_but <- bs_radiobuttons(
    #     parent  = f2a,
    #     buttons = c("TRUE"  = "Yes",
    #                 "FALSE" = "No"),
    #     layout  = "horizontal"
    #     # tips = list(
    #     #     "TRUE"  = str_c(),
    #     #     "FALSE" = str_c()
    #     # )
    # )

    tkgrid(f2a,         columnspan = 3,    sticky = "ew", pady = c(0, 2))
    tkgrid(f2_txt_head, f2_but_head$frame, sticky = "ew")
    tkgrid.configure(f2_txt_head, padx = c(2, 7))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2_lab_skip <- tk2label(f2, text = "Skip lines")
    f2_lab_max  <- tk2label(f2, text = "Max. lines")
    f2_lab_na   <- tk2label(f2, text = "NA string")
    f2_lab_rep  <- tk2label(f2, text = "Names repair")
    f2_lab_out  <- tk2label(f2, text = "Import as")

    tip_box_skip <- "Number of rows to skip. \nInteger from 0 to infinity.\n0 equals to \"auto\"."
    tip_box_max  <- "Maximum number of rows to read. \nInteger from 0 to infinity."
    tip_box_na   <- "A character vector of strings which \nare interpreted as missing (NA) values."
    tip_box_out  <- "Class of imported data set."

    tip_enable <- "Double click to enter custom value."


    # Possible options ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    nas1  <- c("Default", "Empty", "None",  "NA",  "na", "N/A", "n/a", "#N/A", "?", "(?)", "!", "Custom\u2026")

    f2_ent_skip <- bs_entry(
        f2,
        width           = 10,
        tip             = tip_enable,
        on_key_release  = refresh_dataset_window,
        validate        = "focus",
        value           = 0,
        validatecommand = validate_pos_int,
        invalidcommand  = make_red_text_reset_val(to = "0"))

    f2_ent_max  <- bs_entry(
        f2,
        width           = 10,
        tip             = tip_enable,
        on_key_release  = refresh_dataset_window,
        validate        = "focus",
        validatecommand = validate_int_0_inf,
        invalidcommand  = make_red_text_reset_val(to = ""))

    f2_ent_na   <- bs_entry(
        f2,
        width          = 10,
        tip            = tip_enable,
        on_key_release = refresh_dataset_window)

    f2_box_out  <- bs_combobox(
        f2, width = 13,
        values    = c("Data frame", "Tibble"),
        tip       = tip_box_out,
        selection = 1,
        on_select = refresh_dataset_window)

    f2_box_rep  <- bs_combobox(
        f2, width = 13,
        values    = c("Data frame", "Tibble"),
        tip       = tip_box_out,
        selection = 1,
        on_select = refresh_dataset_window)


    tkgrid(f2_lab_skip, f2_ent_skip$frame, pady = c(2, 0))
    tkgrid(f2_lab_max,  f2_ent_max$frame,  pady = c(2, 0))
    tkgrid(f2_lab_na,   f2_ent_na$frame,   pady = c(2, 0))
    tkgrid(f2_lab_out,  f2_box_out$frame,  pady = c(2, 0))
    tkgrid(f2_lab_rep,  f2_box_rep$frame,  pady = c(2, 0))

    tkgrid.configure(
        f2_lab_head, f2_lab_skip, f2_lab_max, f2_lab_na, f2_lab_rep, f2_lab_out,
        padx = c(3, 5), sticky = "w"
    )

    tkgrid.configure(
        f2_ent_skip$frame,
        f2_ent_max$frame,
        f2_ent_na$frame,
        padx = c(2, 0)
    )

    # list(f2_ent_skip, f2_ent_max, f2_ent_na) %>% walk(tk_disable)

    # Check boxes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2_opts <- bs_checkboxes(
        parent = f2,
        boxes = c(
            "check_names",
            "stringsAsFactors",
            "strip_white"
        ),
        default_command = refresh_dataset_window,
        values = c(0, 0, 1),
        labels = gettext_bs(c(
            "Make valid variable names",
            "Convert strings to factors",
            "Strip leading and tailing spaces"
        )),

        tips = list(
            "check_names" = str_c(
                "Check variable names to ensure that they are syntactically\n",
                "valid variable names: start with a letter, do not contain \n",
                "spaces and other special symbols. If necessary, the names \n",
                "are adjusted by function 'make.names'."
            ),

            "stringsAsFactors" = str_c(
                "Convert strings (text variables) \n",
                "to factors (categorical variables)."
            ),

            "strip_white" = str_c(
                "Strip leading and trailing  \n",
                "whitespaces of unquoted fields."
            )
        ),
    )

    tkgrid(f2_opts$frame,
           padx = c(3, 0), pady = c(4, 2), columnspan = 3, sticky = "w")


    # F3, Frame 3, Preview ---------------------------------------------------

    text_font <- tkfont.create(size = 8, family = "Consolas")

    f3 <- tk2labelframe(f_middle, relief = "flat", text = "Preview")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f3_but   <- tk2frame(f3)
    f3_but_w <- tk2frame(f3_but)
    f3_but_e <- tk2frame(f3_but)

    f3_lab_nrows <- bs_label(f3_but_w, text = "Options:", tip = str_c(
        "Preview options: number ow rows to\n",
        "display in each window and preview\n",
        "type."
    ))

    f3_box_nrow_1 <- bs_combobox(
        f3_but_w,
        width = 4,
        values = c("10", "100", "1000", "5000", "9999", "All"),
        tip = str_c(
            "Max. number of rows to read from file for preview.\n",
            "Changing this option does not automatically update the preview."),
        selection = 2,
        on_select = highlight_update_button)

    f3_box_nrow_2 <- bs_combobox(
        f3_but_w,
        width = 3,
        values = c("6", "8", "10", "14", "50", "100", "All"),
        tip = str_c("Max. number of rows to \n",
                    "preview in window below."),
        selection = 2,
        on_select = refresh_dataset_window)

    f3_box_type <- bs_combobox(
        f3_but_w,
        width = 9,
        values = c("Data table", "Tibble", "Structure"),
        tip = str_c(
            "Type of preview: \n",
            " - Data table: top and bottom rows. \n",
            " - Tibble (tbl): compact display of the top rows.\n",
            " - Structure (str): column names, column types and a few values."),
        value = initial$preview_ds_type,
        on_select = refresh_dataset_window)



    f3_but_2 <- tk2button(
        f3_but_e,
        # text = "Clear",
        # width = 7,
        image = "::image::bs_delete",
        command = clear_preview,
        tip = "Clear both preview windows."
    )

    f3_but_3 <- tk2button(
        f3_but_e,
        # width = 7,
        # text = "Refresh",
        image = "::image::bs_refresh",
        command = function() {
            refresh_dataset_window()
        },

        tip = str_c("Refresh Dataset's window and ",
                    "highligth tabs in Input window.")
    )

    # f3_but_4 <- tk2button(
    #     f3_but_e,
    #     # width = 7,
    #     # text = "Locale",
    #     # compound = "right",
    #     image = "::image::bs_locale",
    #     command = function() {window_locale_set_0(parent = top)},
    #     tip = str_c(
    #         "Change locale. \n",
    #         "Useful if pasting text results in encoding issues. \n",
    #         'It is useful to select correct "Enconding" too.'))


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # f3_lab_dataset <- bs_label_b(f3, text = "Dataset")

    f3_dataset <- bs_text(
        f3, width = 75, height = 11, wrap = "none",
        undo = FALSE, state = "disabled", font = text_font,
        label = "Dataset",
        tip = str_c(
            "Types of variables: \n",
            " - <int> whole numbers (integers);\n",
            ' - <dbl>, <num> real numbers ("doubles");\n',
            " - <chr>, <char> character (text) variables;\n",
            " - <fct>, <fctr> factors (categorical variables);\n",
            # " - <ord> ordinal factors;\n",
            " - <lgl>, <lgcl>, <logi> logical values."
        ))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Grid -------------------------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f1, padx = 10, sticky = "we")

    tkgrid(f1_lab_1_1, f1_ent_1_2$frame, "x", "x",  f1_but_1, pady = c(10, 2),  sticky = "we")

    tkgrid(f1_lab_3_1, f1_box_wsh$frame, f1_ent_rng$frame,
           pady = c(0,  10),
           sticky = "we")

    tkgrid(f1_lab_2_1, f1_ent_2_2$frame,
           pady = c(0,  10),
           sticky = "we")


    tkgrid(f1_but_1_5, f1_but_1_3, f1_but_1_4, f1_but_1_6, sticky = "e")

    tkgrid.configure(f1_lab_1_1, f1_lab_2_1, f1_lab_3_1, sticky = "e")
    tkgrid.configure(f1_ent_1_2$frame, f1_ent_2_2$frame, sticky = "we", padx = 2)
    tkgrid.configure(f1_ent_2_2$frame, f1_ent_1_2$frame, columnspan = 3)
    tkgrid.configure(
        f1_ent_1_2$frame_text,  f1_ent_2_2$frame_text,
        f1_ent_1_2$obj_text,    f1_ent_2_2$obj_text,
        sticky = "we")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f_middle, sticky = "news")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f2, f3, sticky = "nsw", padx = c(0, 5), pady = c(0, 15))
    tkgrid.configure(f2, sticky = "ns")
    tkgrid.configure(f3, sticky = "news")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f3_dataset$frame, sticky = "news")
    tkgrid(f3_but, sticky = "ew", columnspan = 2)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f3_but_w, f3_but_e, sticky = "ew", pady = c(2, 4))

    tkgrid(f3_lab_nrows, f3_box_nrow_1$frame, f3_box_nrow_2$frame,
           f3_box_type$frame, sticky = "w")

    tkgrid(f3_but_2, f3_but_3,
           # f3_but_4,
           sticky = "e")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid.configure(f3_lab_nrows,        padx = c(10, 2))
    tkgrid.configure(f3_box_nrow_2$frame, padx = c(2, 2))
    # tkgrid.configure(f3_but_4,            padx = c(0, 10))


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Finalize ---------------------------------------------------------------

    # Help topic
    ok_cancel_help(helpSubject = "read_excel", helpPackage = "readxl",
                   reset = "window_import_from_excel()",
                   ok_label = "Import")

    dialogSuffix(grid.buttons = TRUE, resizable = TRUE)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Fonts ------------------------------------------------------------------
    font_consolas_italic  <- tkfont.create(family = "Consolas", size = 8, slant = "italic")
    font_consolas_bold    <- tkfont.create(family = "Consolas", size = 8, weight = "bold")
    font_consolas_regular <- tkfont.create(family = "Consolas", size = 8)


    # Configuration ----------------------------------------------------------

    set_values(f1_ent_2_2, unique_obj_names("dataset", all_numbered = TRUE))
    highlight_update_button()

    # Configure text tags ----------------------------------------------------
    tktag.configure(f3_dataset$text, "var_names",
                    foreground = "blue",
                    font = font_consolas_bold)

    tktag.configure(f3_dataset$text, "var_types",
                    foreground = "grey50",
                    font = font_consolas_italic)

    tktag.configure(f3_dataset$text, "info",
                    foreground = "grey50",
                    font = font_consolas_italic)

    tktag.configure(f3_dataset$text, "error", foreground = "red3")
    tktag.configure(f3_dataset$text, "bold", font = font_consolas_bold)

    tktag.configure(f3_dataset$text, "grey",  foreground = "grey50")
    tktag.configure(f3_dataset$text, "green", foreground = "green")
    tktag.configure(f3_dataset$text, "red",   foreground = "red")
    tktag.configure(f3_dataset$text, "red3",  foreground = "red3")
    tktag.configure(f3_dataset$text, "red4",  foreground = "red4")

    tktag.configure(f3_dataset$text, "chr",  foreground = "tomato4")
    tktag.configure(f3_dataset$text, "fct",  foreground = "red4")
    tktag.configure(f3_dataset$text, "lgl",  foreground = "red4")
    tktag.configure(f3_dataset$text, "num",  foreground = "green4")


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Make resizable window --------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Rows (height)

    tkgrid.rowconfigure(top, 0, weight = 0)                # Title
    tkgrid.rowconfigure(top, 1, weight = 0, minsize = 2)   # F1 frame
    tkgrid.rowconfigure(top, 2, weight = 1)                # Middle frame
    tkgrid.rowconfigure(top, 3, weight = 0, minsize = 2)   # Buttons

    tkgrid.rowconfigure(f_middle, 0, weight = 1)
    tkgrid.rowconfigure(f3,       0, weight = 1)
    tkgrid.rowconfigure(f3,       2, weight = 1)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Columns (width)

    tkgrid.columnconfigure(top, 0, weight = 1, minsize = 50)

    tkgrid.columnconfigure(f1, 0, weight = 0) # Labels
    tkgrid.columnconfigure(f1, 1, weight = 1) # Text entries
    tkgrid.columnconfigure(f1, 2, weight = 0) # Buttons

    tkgrid.columnconfigure(f1_ent_1_2$frame_text, 0, weight = 1, minsize = 20)
    tkgrid.columnconfigure(f1_ent_2_2$frame_text, 0, weight = 1, minsize = 20)
    tkgrid.columnconfigure(f1_ent_1_2$obj_text,   0, weight = 1, minsize = 20)
    tkgrid.columnconfigure(f1_ent_2_2$obj_text,   0, weight = 1, minsize = 20)

    tkgrid.columnconfigure(f_middle, 0, weight = 0)
    tkgrid.columnconfigure(f_middle, 1, weight = 1)

    tkgrid.columnconfigure(f3,       0, weight = 1)

    tkgrid.columnconfigure(f3_but,   0, weight = 1)
    tkgrid.columnconfigure(f3_but,   1, weight = 0)
    tkgrid.columnconfigure(f3_but_w, 0, weight = 0)
    tkgrid.columnconfigure(f3_but_e, 0, weight = 1)


    # Interactive bindings ---------------------------------------------------

    # Prevents from closing window accidentally
    tkbind(top, "<Return>", do_nothing)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    invisible()
}

