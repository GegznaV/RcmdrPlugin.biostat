# TODO:
#
#  1. Add context menus with (clear, paste, clear and paste, cut, copy)
#  2. Enable more than one NA string.


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_import_from_clipboard <- function() {
    win <- window_import_from_text()
    win$set_mode_clipboard()
    win$paste_from_clipboard()
    win$update_preview()
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_import_from_text <- function() {

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
    get_import_mode <- function(variables) {
        get_values(f2_but_from)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_header <- function() {
        val <- get_selection(f2_box_head)
        switch(val,
               "Auto" = "auto",
               "Yes"  = TRUE,
               "No"   = FALSE,
               stop("Value '", val, "' is unknown (f2_box_head)."))
    }

    get_code_header <- function() {
        val <- get_selection(f2_box_head)
        switch(val,
               "Auto" = "",
               "Yes"  = ", header = TRUE",
               "No"   = ", header = FALSE",
               stop("Value '", val, "' is unknown (f2_box_head)."))
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_dec <- function() {
        val <- get_selection(f2_box_dec)
        switch(val,
               "Default"      = ".",
               "Period ( . )" = ".",
               "Comma ( , )"  = ",",
               stop("Value '", val, "' is unknown (f2_box_dec)."))
    }

    get_code_dec <- function() {
        val <- get_selection(f2_box_dec)
        switch(val, "Default" = "", str_c(',\n dec = "', get_dec(), '"'))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_sep <- function() {
        val <- get_selection(f2_box_sep)
        switch(val,
               "Auto"            = "auto",
               "White space ( )" = " ",
               "Tab ( \\t )"     = "\t",
               "Comma ( , )"     = ",",
               "Semicolon ( ; )" = ";",
               "Pipe ( | )"      = "|",
               "Custom\u2026"    = get_values(f2_ent_sep),
               stop("Value '", val, "' is unknown (f2_box_sep)."))
    }
    get_code_sep <- function() {
        val <- get_selection(f2_box_sep)


        switch(val,
               "Auto" = "",
               "Tab ( \\t )" = str_c(', sep = "\\t"'),
               str_c(', sep = "', get_sep(), '"'))
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

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    get_quote <- function() {
        val <- get_selection(f2_box_quo)
        switch(val,
               "Double ( \" )" = '"',
               "Single ( ' )"  = "'" ,
               "None"          = "",
               "Custom\u2026"    = get_values(f2_ent_quo),
               stop("Value '", val, "' is unknown (f2_box_quo)."))
    }
    get_code_quote <- function() {
        val <- get_selection(f2_box_quo)
        switch(val, "Double ( \" )" = "", str_c(', quote = "', get_quote(), '"'))
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
    get_encoding <- function() {
        get_selection(f2_box_enc)
    }

    get_code_encoding <- function() {
        val <- get_selection(f2_box_enc)
        switch(val, "unknown" = "", str_c(', encoding = "', get_encoding(), '"'))
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_df_vs_dt <- function() {
        val <- get_selection(f2_box_out)
        switch(val,
               "Data frame" = FALSE,
               "Data table" = TRUE,
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
    get_code_logical01 <- function() {
        val <- get_values(f2_opts, "logical01")
        if (isTRUE(val)) {
            str_c(', logical01 = ', val)
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
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_code_blank_lines_skip <- function() {
        val <- get_values(f2_opts, "blank_lines_skip")
        if (isTRUE(val)) {
            str_c(', blank.lines.skip = ', val)
        } else {
            ""
        }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_code_fill <- function() {
        val <- get_values(f2_opts, "fill")
        if (isTRUE(val)) {
            str_c(', fill = ', val)
        } else {
            ""
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


    # ~ Input window ---------------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    read_input_window <- function() {
        get_values(f3_input) %>% str_c("\n")
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    write_input_window <- function(contents, ...) {
        set_values(f3_input, values = contents, ...)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    clear_input_window <- function() {
        write_input_window("")
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    write_dataset_window <- function(contents, ...) {
        set_values(f3_dataset, values = contents, ...)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    clear_dataset_window <- function() {
        write_dataset_window("")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Paste data from clipboard
    paste_from_clipboard = function(add = TRUE) {
        write_input_window(read_clipboard(), add = add)
    }


    # ~ Read data ------------------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # `fread` with options from dialogue window
    do_fread <- function(input, nrows = get_nrows_to_import()) {
        data.table::fread(
            input,
            header       = get_header(),
            dec          = get_dec(),
            sep          = get_sep(),
            skip         = get_skip(),
            nrows        = nrows,
            na.strings   = get_na_str(),
            quote        = get_quote(),
            encoding     = get_encoding(),
            data.table   = get_df_vs_dt(),
            check.names      = get_values(f2_opts, "check_names"),
            stringsAsFactors = get_values(f2_opts, "stringsAsFactors"),
            logical01        = get_values(f2_opts, "logical01"),
            strip.white      = get_values(f2_opts, "strip_white"),
            blank.lines.skip = get_values(f2_opts, "blank_lines_skip"),
            fill             = get_values(f2_opts, "fill")
        )
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    get_input_by_mode <- function() {
        switch(
            get_import_mode(),
            file = {
                if (need_update_from_file()) {
                    read_text_from_file()
                    highlight_update_button()
                }

                biostat_env$file_contents

            },
            clipboard = read_input_window()
        )
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    read_text_from_file <- function() {
        filename <- read_path_to_file()

        n_rows <- get_nrows_preview_input()
        n_rows <- if (is.infinite(n_rows)) -1L else n_rows

        # Read data
        file_contents <- try(
            readr::read_lines(filename, n_max = n_rows),
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
    refresh_input_window <- function() {

        file_contents <- biostat_env$file_contents

        if (inherits(file_contents, "try-error")) {
            err_msg <-
                file_contents %>%
                str_replace_all(c(
                    "Error : " = "Error!\n\nFile: ",
                    "does not exist" = "\ndoes not exist",
                    "directory \\(\'" = "directory \n\\(\'"))

            write_input_window(err_msg)
            tktag.add(f3_input$text, "bold",  "1.0", "2.0")
            tktag.add(f3_input$text, "error", "1.0", "end")

        } else {
            write_input_window(str_c(file_contents, collapse = "\n"))

            # Add colors to tabs
            tktag_add(file_contents, pattern = "\t", obj = f3_input$text, tag = "tab")
        }
    }


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update contents of dataset preview window
    refresh_dataset_window <- function() {

        if (is_nothing_to_import()) {
            clear_dataset_window()
            return()
        }

        input <- get_input_by_mode()

        # Get data from input
        suppressWarnings({
            ds_contents <- try(
                do_fread(str_c(input, collapse =  "\n")),
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

                switch(
                    get_import_mode(),
                    "file" = {
                        err_msg <- str_c(
                            "Possible reasons:\n",
                            " - file name is incorrent or missing;\n",
                            " - file is not a text file (incorrect format);\n",
                            " - file is empty;\n",
                            " - import options are incorrect.")
                    },

                    "clipboard" = {
                        err_msg <- str_c(
                            "Possible reasons:\n",
                            " - input is empty;\n",
                            " - input contains one row and 'Header' is not 'No';\n",
                            " - other import options are incorrect.")
                    },
                    stop("Unrecognized value of 'f2_but_from'")
                )
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
        clear_input_window()
        clear_dataset_window()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update input preview, dataset (DS) preview, and DS name boxes.
    update_all <- function() {
        update_name_entry()
        read_text_from_file()
        refresh_input_window()
        check_file_name()
        # update_from_file()
        refresh_dataset_window()
        highlight_update_button()
    }

    # ~ Change properties ----------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Select data import mode:
    # - from clipboard, enter data maually
    # - from file, URL

    set_mode_clipboard <- function() {
        # clear_dataset_window()
        set_values(f2_but_from, "clipboard")
        tk_disable(f1_ent_1_2)
        tk_disable(f1_but_1_3)
        tk_disable(f1_but_1_4)
        tk_disable(f1_but_1_5)

        tkconfigure(f3_but_1, default = "active", state = "active")
        # image = "::image::bs_paste2"


        set_selection(f3_box_nrow_1, "All")
        tk_disable(f3_box_nrow_1)

        tkconfigure(f3_input$label, text = "Input (editable text from clipboard)")

        tk_normalize(f3_input)
        tcltk2::tip(f3_input$text) <- str_c(
            "Editable text. \n",
            'Grey shading represents tabs.\n',
            'Press Ctrl+S to highlight tabs.')

        tclvalue(previous_file_name) <- ""
        biostat_env$possibly_more_rows <- NULL

        update_name_entry()

    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    set_mode_file_url <- function() {
        if (get_import_mode() != "file") {
            clear_preview()
        }

        set_values(f2_but_from, "file")
        tk_normalize(f1_ent_1_2)
        tk_normalize(f1_but_1_3)
        tk_normalize(f1_but_1_4)
        tk_normalize(f1_but_1_5)
        highlight_update_button()

        tk_normalize(f3_box_nrow_1)
        set_selection(f3_box_nrow_1, 100)

        tkconfigure(f3_but_1, default = "normal")

        tkconfigure(f3_input$label, text = "Input (file contents preview)")
        tk_disable(f3_input)

        tcltk2::tip(f3_input$text) <- str_c(
            "Preview of input file contents.\n",
            "Not editable.\n",
            "Grey shading represents tabs.")

        update_name_entry()
    }


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
    # Change combobox selection to "Custom..." and refresh_dataset_window().
    # txt     - ending of widget's name
    # default - default value on activation/normalization
    # tip_active - tip in active/normal mode

    set_to_custom <- function(txt, default = "", tip_active = "") {

        obj_1 <- get(str_glue("f2_box_{txt}"), envir = parent.frame())
        obj_2 <- get(str_glue("f2_ent_{txt}"), envir = parent.frame())

        if (disabled(obj_2$obj_text)) {
            set_values(obj_2, default)
            if (!is.null(tip_active)) {
                tk2tip(obj_2$obj_text, tip_active)
            }
        }

        set_selection(obj_1, "Custom\u2026")
        tk_normalize(obj_2)

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

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    refresh_ds_show_tabs <-  function() {
        refresh_dataset_window()
        highlight_input_tabs()
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
    is_input_empty <- function() {
        str_trim(read_input_window()) == ""
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

    allow_switch_to_file_mode1 <- function() {
        if ((!is_input_empty())) {
            msgbox_clear_input()
        } else {
            "yes"
        }
    }

    allow_switch_to_file_mode2 <- function() {
        if (get_import_mode() == "clipboard" && (!is_input_empty())) {
            msgbox_clear_input()
        } else {
            "yes"
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Checks possibility that file has more rows than previewed
    possibly_more_rows <- function() {
        isTRUE(biostat_env$possibly_more_rows)
    }


    # ~ TCL/TK tags ----------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add tk text tags to rows that match pattern

    # @param str (character vector) strings to check.
    # @param pattern (string) Pattern to search for.
    # @param obj Tcl/Tk object.
    # @param tag (string) Tag to add.

    tktag_add_row  <- function(str, pattern, obj, tag) {
        # Find row
        info_row <- str_which(str, pattern)
        if (length(info_row) == 0)
            return()

        # Indices
        pos_start <- str_glue("{info_row}.0")
        pos_end   <- str_glue("{info_row}.0 + 1 line")

        # Set tags
        for (i in 1:length(info_row))
            tktag.add(obj, tag, pos_start[i], pos_end[i])
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add tk text tags to places that match pattern
    tktag_add <- function(str, pattern, obj, tag, all = TRUE) {
        # Find row
        # info_row <-

        if (isTRUE(all)) {
            mat <-
                str_locate_all(str, pattern) %>%
                setNames(seq_along(.)) %>%
                discard(~nrow(.) < 1) %>%
                map(as.data.frame) %>%
                dplyr::bind_rows(.id = "row")

        } else {
            mat <-
                str_locate(str, pattern) %>%
                as.data.frame() %>%
                dplyr::mutate(row = dplyr::row_number()) %>%
                dplyr::filter(!is.na(start))
        }

        if (nrow(mat) == 0)
            return()

        pos <-
            mat %>%
            dplyr::mutate(start = start - 1) %>%
            dplyr::transmute(
                start = str_glue("{row}.{start}"),
                end   = str_glue("{row}.{end}"))

        # Set tags
        for (i in 1:nrow(pos))
            tktag.add(obj, tag, pos$start[i], pos$end[i])
    }

    # Add tk text tags to places that match pattern
    # obj - Tcl/Tk text object
    tktag_add_obj <- function(obj, tag, pattern, all = TRUE) {

        str <- get_values(obj) %>% str_split("\n") %>% .[[1]]

        if (isTRUE(all)) {
            mat <-
                str_locate_all(str, pattern) %>%
                setNames(seq_along(.)) %>%
                discard(~nrow(.) < 1) %>%
                map(as.data.frame) %>%
                dplyr::bind_rows(.id = "row")

        } else {
            mat <-
                str_locate(str, pattern) %>%
                as.data.frame() %>%
                dplyr::mutate(row = dplyr::row_number()) %>%
                dplyr::filter(!is.na(start))
        }

        if (nrow(mat) == 0)
            return()

        pos <-
            mat %>%
            dplyr::mutate(start = start - 1) %>%
            dplyr::transmute(
                start = str_glue("{row}.{start}"),
                end   = str_glue("{row}.{end}"))

        # Set tags
        for (i in 1:nrow(pos))
            tktag.add(obj, tag, pos$start[i], pos$end[i])
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add tk text tags to places that match pattern
    # First occurance in each row only.
    tktag_add_first <- function(str, pattern, obj, tag) {
        tktag_add(str, pattern, obj, tag, all = FALSE)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add tags to highlight tab symbols in input window
    highlight_input_tabs <- function() {
        tktag_add_obj(f3_input$text, tag = "tab", "\t")
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

        # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_import_from_text", list(
            preview_ds_type = get_selection(f3_box_type)
        ))

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        switch(
            from,

            # ~~ From file --------------------------------------------------------
            "file" = {
                file_name <- read_path_to_file()

                # Check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                # Check if file exists or is URL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

                #  Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                Library("data.table")

                command <- str_glue(
                    '## Import data from text file \n',
                    '{new_name} <- data.table::fread(\n',
                    '"{file_name}"',
                    get_code_dec(),
                    get_code_sep(),
                    get_code_header(),
                    get_code_skip(),
                    get_code_nrows(),
                    get_code_encoding(),
                    get_code_na_str(),
                    get_code_quote(),
                    get_code_check_names() ,
                    get_code_stringsAsFactors() ,
                    get_code_logical01(),
                    get_code_strip_white(),
                    get_code_blank_lines_skip(),
                    get_code_fill(),
                    get_code_df_vs_dt(),
                    ")"
                )
            },

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # ~~ From clipboard --------------------------------------------------

            "clipboard" = {

                input_text <- read_input_window()

                # Check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                # If no imput is present
                if (input_text == "") {
                    ans <- tk_messageBox(
                        parent = top,
                        type = "ok",
                        icon = "error",
                        message = str_c(
                            "There is no data to import. \n",
                            "Did you paste it from clipboard?"
                        ),
                        caption = "No Data To Import")

                    return()
                }

                # Beggin import data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                suppressWarnings({
                    ds_contents <- try(
                        do_fread(input = read_input_window()), silent = TRUE)
                })

                if (inherits(ds_contents, "try-error")) {

                    err_msg <- parse_fread_error(ds_contents)
                    tk_messageBox(
                        parent = top,
                        message = err_msg,
                        icon  = "error",
                        caption = "Data Reading Error",
                        type  = "ok")

                    return()
                }

                # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                command <-
                    c("## Data from clipboard \n",
                      capture.output(dput(ds_contents)) %>%
                          str_replace("structure\\(list\\(",
                                      str_c(new_name, " <- structure(list(\n"))
                    )
            }
        )

        # ~~ Apply commands ------------------------------------------------------
        result <- justDoIt(command)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            switch(
                from,
                "file"      = logger(style_cmd(command)),
                "clipboard" = logger(str_c(command, collapse = "\n")) # Styling is slow
            )

            activeDataSet(new_name, flushModel = FALSE, flushDialogMemory = FALSE)

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

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Import Data from Text"),
                     suppress.window.resize.buttons = FALSE)
    tk_title(top, "Import Data from Text")

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        preview_ds_type = "Data table"
    )
    initial <- getDialog("window_import_from_text", defaults)


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
        image = "::image::bs_file_open",
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
    f1_ent_2_2 <- bs_entry(
        f1, width = 90,  sticky = "ew", tip = "Create a name for the dataset.")


    # F2-3, Middle frame -----------------------------------------------------

    f_middle <- tk2frame(top)


    # F2, Frame 2, parameters ------------------------------------------------

    f2 <- tk2labelframe(f_middle, relief = "flat",
                        borderwidth = 5, padding = 5, text = "Import options")

    f2a <- tk2frame(f2)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2_txt_from <- bs_label(f2a, text = "From")

    f2_but_from <- bs_radiobuttons(
        parent  = f2a,
        buttons = c("file"      = "File, URL",
                    "clipboard" = "Clipboard"),
        layout  = "horizontal",

        commands = list(
            "file" = function() {

                if (allow_switch_to_file_mode1() == "no") {
                    set_values(f2_but_from, "clipboard")
                    return()
                }
                clear_preview()
                set_mode_file_url()
            },

            "clipboard" = set_mode_clipboard
        ),

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
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2_lab_head <- tk2label(f2, text = "Header")
    f2_lab_dec  <- tk2label(f2, text = "Decimal")
    f2_lab_sep  <- tk2label(f2, text = "Separator")
    f2_lab_skip <- tk2label(f2, text = "Skip lines")
    f2_lab_max  <- tk2label(f2, text = "Max. lines")
    f2_lab_na   <- tk2label(f2, text = "NA string")
    f2_lab_enc  <- tk2label(f2, text = "Encoding")
    f2_lab_quo  <- tk2label(f2, text = "Quote")
    f2_lab_out  <- tk2label(f2, text = "Import as")

    tip_box_head <- "First row has column names."
    tip_box_dec  <- "Separator for decimal part \nof a number: 10.4 vs. 10,4."
    tip_box_sep  <- "Field (value) separator character."
    tip_box_skip <- "Number of rows to skip. \nInteger from 0 to infinity.\n0 equals to \"auto\"."
    tip_box_max  <- "Maximum number of rows to read. \nInteger from 0 to infinity."
    tip_box_na   <- "A character vector of strings which \nare interpreted as missing (NA) values."
    tip_box_quo  <- "Quoting characters. \nCharacters between quotes are read as one value."
    tip_box_enc  <- "Encoding."
    tip_box_out  <- "Class of imported data set."

    tip_enable <- "Double click to enter custom value."


    # Possible options ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    dec1  <- c("Period ( . )", "Comma ( , )") # "Default"
    sep1  <- c("Auto", "Tab ( \\t )", "Comma ( , )", "Semicolon ( ; )",
               "White space ( )", "Pipe ( | )", "Custom\u2026")
    nas1  <- c("Default", "Empty", "None",  "NA",  "na", "N/A", "n/a", "#N/A",
               "?", "(?)", "!", "Custom\u2026")
    quo1  <- c("Double ( \" )", "Single ( \' )", "None", "Custom\u2026" )
    max1  <- c("All",  "Custom\u2026")
    skip1 <- c("Auto", "Custom\u2026")
    enc1  <- c( "UTF-8", "Latin-1", "unknown")
    head1 <- c("Auto", "No", "Yes")
    out1  <- c("Data frame", "Data table")

    f2_box_head <- bs_combobox(
        f2, width = 13, values = head1, tip = tip_box_head,
        selection = 1, on_select = refresh_dataset_window)

    f2_box_dec  <- bs_combobox(
        f2, width = 13, values = dec1,  tip = tip_box_dec,
        selection = 1, on_select = refresh_dataset_window)

    f2_box_sep  <- bs_combobox(
        f2, width = 13, values = sep1,  tip = tip_box_sep,
        selection = 1, on_select = function() {enable_entry("sep", "-", tip_enable, "")})

    f2_box_skip <- bs_combobox(
        f2, width = 13, values = skip1, tip = tip_box_skip,
        selection = 1, on_select = function() {enable_entry("skip", "0", tip_enable, "")})

    f2_box_max  <- bs_combobox(
        f2, width = 13, values = max1,  tip = tip_box_max,
        selection = 1, on_select = function() {enable_entry("max",  "0", tip_enable, "")})

    f2_box_na   <- bs_combobox(
        f2, width = 13, values = nas1,  tip = tip_box_na,
        selection = 1, on_select = function() {enable_entry("na",   "?", tip_enable, "")})

    f2_box_quo  <- bs_combobox(
        f2, width = 13, values = quo1,  tip = tip_box_quo,
        selection = 1, on_select = function() {enable_entry("quo",  "\"", tip_enable, "")})

    f2_box_enc  <- bs_combobox(
        f2, width = 13, values = enc1,  tip = tip_box_enc,
        selection = 1, on_select = refresh_dataset_window)

    f2_box_out  <- bs_combobox(
        f2, width = 13, values = out1,  tip = tip_box_out,
        selection = 1, on_select = refresh_dataset_window)

    f2_ent_sep  <- bs_entry(
        f2, width = 4, tip = tip_enable,
        on_double_click = function() {set_to_custom("sep", "|")},
        on_key_release = refresh_dataset_window)

    f2_ent_skip <- bs_entry(
        f2, width = 4, tip = tip_enable,
        on_double_click = function() {set_to_custom("skip", "0")},
        on_key_release = refresh_dataset_window,
        validate = "focus",
        validatecommand = validate_pos_int,
        invalidcommand  = make_red_text_reset_val(to = "0"))

    f2_ent_max  <- bs_entry(
        f2, width = 4, tip = tip_enable,
        on_double_click = function() {set_to_custom("max", "0")},
        on_key_release = refresh_dataset_window,
        validate = "focus",
        validatecommand = validate_int_0_inf,
        invalidcommand  = make_red_text_reset_val(to = "Inf"))

    f2_ent_na   <- bs_entry(
        f2, width = 4, tip = tip_enable,
        on_double_click = function() {set_to_custom("na",   "?")},
        on_key_release = refresh_dataset_window)

    f2_ent_quo  <- bs_entry(
        f2, width = 4, tip = tip_enable,
        on_double_click = function() {set_to_custom("quo",  "\"")},
        on_key_release = refresh_dataset_window)

    tkgrid(f2_lab_head, f2_box_head$frame, "x",               pady = c(2, 0))
    tkgrid(f2_lab_dec,  f2_box_dec$frame,  "x",               pady = c(2, 0))
    tkgrid(f2_lab_sep,  f2_box_sep$frame,  f2_ent_sep$frame,  pady = c(2, 0))
    tkgrid(f2_lab_skip, f2_box_skip$frame, f2_ent_skip$frame, pady = c(2, 0))
    tkgrid(f2_lab_max,  f2_box_max$frame,  f2_ent_max$frame,  pady = c(2, 0))
    tkgrid(f2_lab_quo,  f2_box_quo$frame,  f2_ent_quo$frame,  pady = c(2, 0))
    tkgrid(f2_lab_na,   f2_box_na$frame,   f2_ent_na$frame,   pady = c(2, 0))
    tkgrid(f2_lab_enc,  f2_box_enc$frame,  "x",               pady = c(2, 0))
    tkgrid(f2_lab_out,  f2_box_out$frame,  "x",               pady = c(2, 0))

    tkgrid.configure(
        f2_lab_head, f2_lab_dec, f2_lab_sep, f2_lab_skip, f2_lab_max, f2_lab_quo,
        f2_lab_na, f2_lab_enc, f2_lab_out,
        padx = c(3, 5), sticky = "w"
    )

    tkgrid.configure(
        f2_ent_sep$frame,
        f2_ent_skip$frame,
        f2_ent_max$frame,
        f2_ent_quo$frame,
        f2_ent_na$frame,
        padx = c(2, 0)
    )

    list(f2_ent_sep, f2_ent_skip, f2_ent_max, f2_ent_quo, f2_ent_na) %>%
        walk(tk_disable)

    # Check boxes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2_opts <- bs_checkboxes(
        parent = f2,
        boxes = c("check_names",
                  "stringsAsFactors",
                  "logical01",
                  "strip_white",
                  "blank_lines_skip",
                  "fill"
        ),
        default_command = refresh_dataset_window,
        values = c(0, 0, 0, 1, 0, 0),
        labels = gettext_bs(c(
            "Make valid variable names",
            "Convert strings to factors",
            "Read 1/0 as TRUE/FALSE",
            "Strip leading and tailing spaces",
            "Skip empty lines",
            "Fill unequal length rows"
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

            "logical01" = str_c(
                "A column containing only 0s and 1s will be \n",
                "read as logical, otherwise as integer."
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
            )
        ),
    )

    tkgrid(f2_opts$frame,
           padx = c(3, 0), pady = c(4, 2), columnspan = 3, sticky = "w")


    # F3, Frame 3, Preview ---------------------------------------------------

    text_font <- tkfont.create(size = 8, family = "Consolas")

    f3 <- tk2labelframe(f_middle, relief = "flat", text = "Preview")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # f3_lab_input <- bs_label_b(f3, text = "Input")

    f3_input <- bs_text(
        f3, width = 70, height = 11, wrap = "none",
        autoseparators = TRUE, undo = TRUE,
        state = "disabled", font = text_font,
        label = "Input")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f3_but <- tk2frame(f3)
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
        values = c("10", "100", "1000", "All"),
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
        values = c("Tibble", "Data table", "Structure"),
        tip = str_c(
            "Type of preview: \n",
            " - Tibble (tbl): simplified values of the top rows.\n",
            " - Data table: top and bottom rows. \n",
            " - Structure (str): column names, column types and a few values."),
        value = initial$preview_ds_type,
        on_select = refresh_dataset_window)

    f3_but_1 <- tk2button(
        f3_but_e,
        # width = 7,
        # text = "Paste",
        image = "::image::bs_paste",
        command = function() {
            set_mode_clipboard()
            paste_from_clipboard(add = FALSE)
            highlight_input_tabs()
            refresh_dataset_window()
        },
        tip = "Clear input and paste data from clipboard.")

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
            if (get_import_mode() == "file") {
                refresh_input_window()
            } else {
                highlight_input_tabs()
            }
            refresh_dataset_window()
        },

        tip = str_c("Refresh: update preview and highligth tabs.")
    )

    f3_but_4 <- tk2button(
        f3_but_e,
        # width = 7,
        # text = "Locale",
        # compound = "right",
        image = "::image::bs_locale",
        command = function() {window_locale_set_0(parent = top)},
        tip = str_c(
            "Change locale. \n",
            "Useful if pasting text results in encoding issues. \n",
            'It is useful to select correct "Enconding" too.'))


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

    tkgrid(f1_lab_1_1, f1_ent_1_2$frame, f1_but_1, pady = c(10, 2),  sticky = "we")
    tkgrid(f1_lab_2_1, f1_ent_2_2$frame,           pady = c(0,  10), sticky = "we")

    tkgrid(f1_but_1_5, f1_but_1_3, f1_but_1_4, f1_but_1_6, sticky = "e")

    tkgrid.configure(f1_lab_1_1, f1_lab_2_1,             sticky = "e")
    tkgrid.configure(f1_ent_1_2$frame, f1_ent_2_2$frame, sticky = "we", padx = 2)
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
    tkgrid(f3_input$frame,  sticky = "news")
    tkgrid(f3_but, sticky = "ew", columnspan = 2)
    tkgrid(f3_dataset$frame, sticky = "news")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f3_but_w, f3_but_e, sticky = "ew", pady = c(2, 4))

    tkgrid(f3_lab_nrows, f3_box_nrow_1$frame, f3_box_nrow_2$frame,
           f3_box_type$frame, sticky = "w")

    tkgrid(f3_but_1, f3_but_2, f3_but_3, f3_but_4, sticky = "e")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid.configure(f3_lab_nrows,        padx = c(10, 2))
    tkgrid.configure(f3_box_nrow_2$frame, padx = c(2, 2))
    tkgrid.configure(f3_but_4,            padx = c(0, 10))


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Finalize ---------------------------------------------------------------

    # Help topic
    ok_cancel_help(helpSubject = "fread", helpPackage = "data.table",
                   reset = "window_import_from_text()",
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

    tktag.configure(f3_input$text, "tab",   background = "grey95")
    tktag.configure(f3_input$text, "grey",  foreground = "grey50")
    tktag.configure(f3_input$text, "error", foreground = "red3")
    tktag.configure(f3_input$text, "bold",  font = font_consolas_bold)

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

    tkbind(f3_input$text, "<Control-s>",       refresh_ds_show_tabs)
    tkbind(f3_input$text, "<Control-S>",       refresh_ds_show_tabs)
    tkbind(f3_input$text, "<Triple-Button-3>", refresh_dataset_window)

    # tkbind(f3_input$text, "<<Copy>>",     refresh_dataset_window)
    # tkbind(f3_input$text, "<Control-Shift-Z>",  "<<Redo>>")
    tkbind(f3_input$text, "<<Paste>>",    refresh_ds_show_tabs)
    tkbind(f3_input$text, "<<Undo>>",     refresh_dataset_window)
    tkbind(f3_input$text, "<<Redo>>",     refresh_dataset_window)
    tkbind(f3_input$text, "<KeyRelease>", refresh_dataset_window)

    tkbind(f3_input$text, "<Control-v>",  set_mode_clipboard)
    tkbind(f3_input$text, "<Control-V>",  set_mode_clipboard)

    # Prevents from closing window accidentally
    tkbind(top, "<Return>", do_nothing)

    # Output -----------------------------------------------------------------
    # Functions to modify state of the widget
    invisible(
        list(
            set_mode_clipboard   = set_mode_clipboard,
            set_mode_file_url    = set_mode_file_url,
            paste_from_clipboard = paste_from_clipboard,
            update_preview       = refresh_dataset_window
        )
    )
}

