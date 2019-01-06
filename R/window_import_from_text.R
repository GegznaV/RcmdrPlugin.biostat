# TODO:
#  update_preview_ds() (line "# Get initial input")
#  may unnecessarily read data from file or URL on every preview update.
#  This may be issue if file is big or is read from URL.
#  Some of read from file actions may be prevented.



#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_import_from_clipboard <- function() {
    win <- window_import_from_text()
    win$set_mode_clipboard()
    win$paste_from_clipboard()
    win$update_preview_ds()
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_import_from_text <- function() {

    txt_trunc   <- "... Other rows are not shown ..."
    txt_not_all <- "... More rows may be present in the file ..."


    # Functions ==============================================================
    # ~ Validation -----------------------------------------------------------
    make_red_text_reset_val <- function(to = "Inf") {
        function(P, W, S, v, s) {
            tcl("after", "idle", function() {tkconfigure(W, validate = v)})
            tkconfigure(W, foreground = "red2")
            tkdelete(W, "0", "end")
            tkinsert(W, "0", to)

            tcl("expr", "TRUE")
        }
    }

    # ~ Get values -----------------------------------------------------------
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



    # ~ Read data ------------------------------------------------------------
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
    paste_from_clipboard = function() {
        set_values(f3_txt_1, read_clipboard(), add = TRUE)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    read_input <- function(from = get_values(f2_but_from), nrows = get_nrows_to_import()) {
        from <- match.arg(from, c("file", "clipboard"))
        switch(
            from,
            get_values(f2_but_from),
            "file" = {
                input <- get_values(f1_ent_1_2)
            },

            "clipboard" = {
                input <- get_values(f3_txt_1) %>% str_c("\n")
            }
        )

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
    # Clear both preview windows
    clear_preview <- function() {
        set_values(f3_txt_1, "")
        set_values(f3_txt_2, "")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update contents of input window from file or URL
    update_input_from_file <- function() {

        check_file(

            on_success = function() {
                filename <- get_values(f1_ent_1_2)

                n_rows <- min(get_nrows_preview_input(), get_nrows_to_import())
                n_rows <- if (is.infinite(n_rows)) -1L else n_rows

                # Read file contents
                file_contents <- readr::read_lines(filename, n_max = n_rows)

                # Set values
                set_values(f3_txt_1, str_c(file_contents, collapse = "\n"))

                # Add colors to tabs
                tktag_add(file_contents, pattern = "\t", obj = f3_txt_1, tag = "tab")
            })
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check, if file exist or is URL
    # Return TRUE on success
    #        FALSE on failure.
    check_file <- function(on_success = function() {}, on_failure = function() {}) {
        filename <- get_values(f1_ent_1_2)

        if (fs::is_file(filename) || is_url(filename)) {
            on_success()

            return(TRUE)

        } else {
            # Delete text
            set_values(f3_txt_1, "")

            tk_messageBox(
                parent = top,
                type = "ok",
                icon = "warning",
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
    # Update contents of dataset preview window
    update_preview_ds <- function() {

        nrows_allowed <- min(get_nrows_to_import(), get_nrows_preview_input())

        # Get initial input
        suppressWarnings({
            ds_contents <- try(read_input(nrows = nrows_allowed), silent = TRUE)
        })

        err_msg <- NULL

        if (inherits(ds_contents, "try-error")) {
            err_msg <- parse_fread_error(ds_contents)

        } else {

            # Generate contents for preview
            nrows_from_file <- nrow(ds_contents)
            nrow_preview_ds <- get_nrows_preview_ds()

            switch(
                get_selection(f3_box_type),
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                "Tibble" = {

                    # if (nrows_from_file > nrow_preview_ds) {
                    #     nrow_preview_ds <- nrow_preview_ds - 1
                    # }

                    op <- options(width = 10000)
                    ds_preview <-
                        capture.output(
                            print(tibble::as_tibble(ds_contents),
                                  width = Inf,
                                  n = nrow_preview_ds)
                        ) %>%
                        str_subset("^(?!# A tibble.*)") %>%
                        str_replace( "^# \\.\\.\\. with.*", txt_trunc)

                    if (nrows_allowed == nrows_from_file) {
                        ds_preview <-  c(ds_preview, txt_not_all)
                    }

                    options(op)
                },
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                "DT" = {

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
                                  # class = TRUE,
                                  topn  = topn,
                                  nrows = nrows_from_file)
                        )

                    if (nrows_allowed == nrows_from_file) {
                        ds_preview <- c(ds_preview, txt_not_all)
                    }

                    options(op)
                },
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                "Structure" = {
                    ds_preview <- capture.output(glimpse(ds_contents, width = 74))


                    if (get_selection(f3_box_nrow_1) != "All" && nrows_allowed <= nrows_from_file) {
                        ds_preview <-
                            str_replace(ds_preview, "(?<=^Observations: )(.*)", "\\1 or more")
                    }
                }
            )

            if (length(ds_preview) <= 1) {

                switch(
                    get_values(f2_but_from),
                    "file" = {
                        err_msg <- str_c(
                            "Possible reasons:\n",
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
            # If no preview available
            set_values(f3_txt_2, str_c("Error! \n\n", err_msg))

            # Red font:
            tktag.add(f3_txt_2, "error", "1.0", "end")

        } else {
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Regular preview
            set_values(f3_txt_2, str_c(ds_preview, collapse = "\n"))

            # Add colors:
            switch(
                get_selection(f3_box_type),
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                "Tibble" = {
                    # Variable names
                    tktag.add(f3_txt_2, "var_names", "1.0", "2.0")

                    # Variable types
                    tktag.add(f3_txt_2, "var_types", "2.0", "3.0")
                    tktag_add(ds_preview[1:2], "\\<chr\\>", f3_txt_2, "red4")

                    # Info message
                    tktag_add_row(ds_preview, txt_trunc,   f3_txt_2, "info")
                    tktag_add_row(ds_preview, txt_not_all, f3_txt_2, "info")
                },

                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                "DT" = {
                    # Variable names
                    tktag.add(f3_txt_2, "var_names", "1.0", "2.0")

                    # Variable types
                    # tktag.add(f3_txt_2, "var_types", "2.0", "3.0")
                    # tktag_add(ds_preview[1:2], "\\<cha?r\\>", f3_txt_2, "red4")

                    # Info message
                    tktag_add_row(ds_preview, "^\\s*---\\s*$", f3_txt_2, "red")

                    # Info message
                    tktag_add_row(ds_preview, txt_trunc,   f3_txt_2, "info")
                    tktag_add_row(ds_preview, txt_not_all, f3_txt_2, "info")

                },

                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                "Structure" = {
                    # Variable names
                    # tktag_add_row(ds_preview, "^Variables: ", f3_txt_2, "var_names")

                    tktag_add_first(ds_preview, "(?<=\\$).*?(?=\\<)",
                                    f3_txt_2, "var_names")

                    # Variable types
                    tktag_add_first(ds_preview, "^\\$", f3_txt_2, "var_types")
                    tktag_add_first(ds_preview, "\\.\\.\\.$", f3_txt_2, "var_types")
                    tktag_add_first(ds_preview, "\\<.*?\\>", f3_txt_2, "var_types")
                    tktag_add_first(ds_preview, "\\<chr\\>", f3_txt_2, "red4")

                    # Observations
                    tktag_add_row(ds_preview, "^Observations: ", f3_txt_2, "grey")
                }
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            )
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update dataset preview if autopreview is enabled
    auto_update_preview_ds <- function() {
        if (get_values(f2_opts, "auto_preview") && get_values(f3_txt_1) != "") {
            update_preview_ds()
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update contents of dataset entry box.
    update_name_entry <- function(variables) {

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
        update_name_entry()
        update_preview_ds()
        update_input_from_file()
    }

    #  ~ ... -----------------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Select data import mode:
    # - from clipboard, enter data maually
    # - from file, URL
    set_mode_clipboard <- function() {
        # set_values(f3_txt_2, "")
        set_values(f2_but_from, "clipboard")
        tk_disable(f1_ent_1_2)
        tk_disable(f1_but_1_4)

        tkconfigure(f3_but_1, default = "active")


        set_selection(f3_box_nrow_1, "All")
        tk_disable(f3_box_nrow_1)

        tkconfigure(f3_lab_1, text = "Input (editable text from clipboard)")
        # nchar("(editable)")
        # tktag.add(f3_lab_1, "info_ed", "end - 10", "end")


        tk_normalize(f3_txt_1)
        tcltk2::tip(f3_txt_1) <- str_c(
            "Editable text. \n",
            'To update window "Dataset", press Ctrl+S \n',
            "or enable automatic updates.")

        update_name_entry()

    }

    set_mode_file_url <- function() {
        if (get_values(f2_but_from) != "file") {
            clear_preview()
        }

        set_values(f2_but_from, "file")
        tk_normalize(f1_ent_1_2)
        tk_normalize(f1_but_1_4)

        tk_normalize(f3_box_nrow_1)
        set_selection(f3_box_nrow_1, 100)

        tkconfigure(f3_but_1, default = "normal")

        tkconfigure(f3_lab_1, text = "Input (preview file contents)")
        tk_disable(f3_txt_1)

        tcltk2::tip(f3_txt_1) <- str_c(
            "Preview of input file contents.\n",
            "Not editable.\n",
            "Grey shading represents tabs.")

        update_name_entry()
    }


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Enable import options entry boxes and auto_update_preview_ds().
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

        auto_update_preview_ds()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Change combobox selection to "Custom..." and auto_update_preview_ds().
    # txt     - ending of widget's name
    # default - default value on activation/normalization
    # tip_active - tip in active/normal mode

    set_custom <- function(txt, default = "", tip_active = "") {

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

        auto_update_preview_ds()
    }
    # ~ TCL/TK tags ----------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add tk text tags to rows that match pattern
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
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add tk text tags to places that match pattern
    # First occurance in each row only.
    tktag_add_first <- function(str, pattern, obj, tag) {
        tktag_add(str, pattern, obj, tag, all = FALSE)
    }

    # ~ onOK -----------------------------------------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        new_name  <- get_values(f1_ent_2_2)
        from      <- get_values(f2_but_from)

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

        # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # putDialog("window_import_from_text", list(
        #     initial_position = which_position
        # ))

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        switch(
            from,

            # ~~ From file --------------------------------------------------------
            "file" = {
                file_name <- get_values(f1_ent_1_2)

                # Check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                # Check if file exists or is URL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                if (!check_file()) {
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

                input_text <- get_values(f3_txt_1)

                # Check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                # If no imput is present
                if (input_text != "") {
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
                    ds_contents <- try(read_input(), silent = TRUE)
                })

                if (inherits(ds_contents, "try-error")) {

                    err_msg <- parse_fread_error(ds_contents)
                    tk_messageBox(
                        parent = top,
                        message = err_msg,
                        icon  = "error",
                        caption = "File Reading Error",
                        type  = "ok")

                    return()
                }

                # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                command <-
                    c("## Data from clipboard \n\n",
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
    # defaults <- list(
    #     position = "first"
    # )
    # initial <- getDialog("window_import_from_text", defaults)


    # Widgets ================== =============================================

    # Frame 1, choose file and name ------------------------------------------
    f1 <- tk2frame(top)

    tkgrid.columnconfigure(f1, 1, weight = 1)
    tkgrid(f1, padx = 10, sticky = "e")

    f1_txt_1_1 <- bs_label_b(f1, text = "File, URL: ")
    f1_ent_1_2 <- bs_entry(
        f1, width = 87, sticky = "we", tip = "Path to file or URL.")
    f1_but_1_3 <- tk2button(
        f1,
        width = 8,
        text = "Browse",
        command = function() {

            if (get_values(f2_but_from) == "clipboard" && get_values(f3_txt_1) != "") {
                ans <- tk_messageBox(
                    parent = top,
                    type = "yesno",
                    default = "no",
                    message = str_c(
                        'The contents of Input window will be deleted. \n',
                        'Do you agree?'),
                    caption = "Clear Input")

                if (ans != "yes") {
                    return()
                }
            }

            set_mode_file_url()
            get_path_to_file()
        },
        tip = "Choose file to import.")

    f1_but_1_4 <- tk2button(f1, width = 8, text = "Update",
                            command = update_all,
                            tip = str_c(
                                "Read file (URL) and update\n",
                                "preview of dataset."))

    f1_txt_2_1 <- bs_label_b(f1, text = "Name: ")
    f1_ent_2_2 <- bs_entry(
        f1, width = 87,  sticky = "ew", tip = "Create a name for the dataset.")


    tkgrid(f1_txt_1_1, f1_ent_1_2$frame, f1_but_1_3, f1_but_1_4, pady = c(10, 2))
    tkgrid(f1_txt_2_1, f1_ent_2_2$frame, "x",        "x",        pady = c(0,  10))

    tkgrid.configure(f1_ent_1_2$frame, padx = 2)
    tkgrid.configure(f1_ent_2_2$frame, padx = 2)

    tkgrid.configure(f1_txt_1_1, f1_txt_2_1, sticky = "e")
    tkgrid.configure(f1_ent_1_2$frame, f1_ent_2_2$frame, sticky = "w")


    # Middle frame -----------------------------------------------------------

    f_middle <- tk2frame(top)
    tkgrid(f_middle, sticky = "w")


    # Frame 2, parameters ----------------------------------------------------

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
                if (get_values(f3_txt_1) != "") {
                    ans <- tk_messageBox(
                        parent = top,
                        type = "yesno",
                        default = "no",
                        message = str_c(
                            'The contents of Input window will be deleted. \n',
                            'Do you agree?'),
                        caption = "Clear Input")

                    if (ans != "yes") {
                        set_values(f2_but_from, "clipboard")
                        return()
                    }
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
        selection = 1, on_select = function() {auto_update_preview_ds()})

    f2_box_dec  <- bs_combobox(
        f2, width = 13, values = dec1,  tip = tip_box_dec,
        selection = 1, on_select = function() {auto_update_preview_ds()})

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
        selection = 1, on_select = function() {auto_update_preview_ds()})

    f2_box_out  <- bs_combobox(
        f2, width = 13, values = out1,  tip = tip_box_out,
        selection = 1, on_select = function() {auto_update_preview_ds()})

    f2_ent_sep  <- bs_entry(
        f2, width = 4, tip = tip_enable,
        on_double_click = function() {set_custom("sep", "|")},
        on_key_release = auto_update_preview_ds)

    f2_ent_skip <- bs_entry(
        f2, width = 4, tip = tip_enable,
        on_double_click = function() {set_custom("skip", "0")},
        on_key_release = auto_update_preview_ds,
        validate = "focus",
        validatecommand = validate_pos_int,
        invalidcommand  = make_red_text_reset_val(to = "0"))

    f2_ent_max  <- bs_entry(
        f2, width = 4, tip = tip_enable,
        on_double_click = function() {set_custom("max", "0")},
        on_key_release = auto_update_preview_ds,
        validate = "focus",
        validatecommand = validate_int_0_inf,
        invalidcommand  = make_red_text_reset_val(to = "Inf"))

    f2_ent_na   <- bs_entry(
        f2, width = 4, tip = tip_enable,
        on_double_click = function() {set_custom("na",   "?")},
        on_key_release = auto_update_preview_ds)

    f2_ent_quo  <- bs_entry(
        f2, width = 4, tip = tip_enable,
        on_double_click = function() {set_custom("quo",  "\"")},
        on_key_release = auto_update_preview_ds)

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
                  "fill",
                  "auto_preview"
        ),
        default_command = auto_update_preview_ds,
        values = c(0, 0, 0, 1, 0, 0, 1),
        labels = gettext_bs(c(
            "Make valid variable names",
            "Convert strings to factors",
            "Read 1/0 as TRUE/FALSE",
            "Strip leading and tailing spaces",
            "Skip empty lines",
            "Fill unequal length rows",
            "Automatically update preview"
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
            ),

            "auto_preview" = str_c(
                "Turn off for big data files \n",
                "or if data is on the Internet."
            )

        ),
    )

    tkgrid(f2_opts$frame, padx = c(3, 0), pady = c(4, 2), columnspan = 3, sticky = "w")


    # Frames 3, Preview ------------------------------------------------------

    text_font <- tkfont.create(size = 8, family = "Consolas")

    f3 <- tk2labelframe(f_middle, relief = "flat", text = "Preview")

    f3_lab_1 <- bs_label_b(f3, text = "Input")
    tkgrid(f3_lab_1)

    f3_txt_1 <- bs_text(f3, width = 70, height = 11, wrap = "none",
                        autoseparators = TRUE,
                        undo = TRUE, state = "disabled", font = text_font)

    tkconfigure(f3_txt_1, setgrid = TRUE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f3_but <- tk2frame(f3)
    f3_but_w <- tk2frame(f3_but)
    f3_but_e <- tk2frame(f3_but)

    f3_lab_nrows <- bs_label(f3_but_w, text = "Preview:", tip = str_c(
        "Preview options: number ow rows to\n",
        "display in each window and preview\n",
        "type."
    ))

    f3_box_nrow_1 <- bs_combobox(
        f3_but_w,
        width = 4,
        values = c("100", "1000", "All"),
        tip = "Max. number of rows to read from file for preview.",
        selection = 1,
        on_select = auto_update_preview_ds)

    f3_box_nrow_2 <- bs_combobox(
        f3_but_w,
        width = 3,
        values = c("8", "10", "100", "All"),
        tip = "Number of data rows to show in window below.",
        selection = 1,
        on_select = auto_update_preview_ds)

    f3_box_type <- bs_combobox(
        f3_but_w,
        width = 9,
        values = c("Tibble", "DT", "Structure"),
        tip = str_c(
            "Type of preview: \n",
            " - Tibble (tbl): column types and simplified values of the top rows.\n",
            " - Data table (DT): top and bottom rows. \n",
            " - Structure (str): column names, types and a few values"),
        selection = 1,
        on_select = auto_update_preview_ds)

    f3_but_1 <- tk2button(
        f3_but_e,
        width = 8,
        text = "Paste",
        command = function() {
            set_mode_clipboard()
            paste_from_clipboard()

            if (get_values(f2_opts, "auto_preview")) {
                update_preview_ds()
            }
        },
        tip = "Paste data from clipboard.")

    f3_but_2 <- tk2button(f3_but_e, width = 8, text = "Clear",
                          command = clear_preview,
                          tip = "Clear input text and dataset's preview.")

    f3_but_3 <- tk2button(f3_but_e, width = 8, text = "Refresh",
                          command = update_preview_ds,
                          tip = "Update dataset's preview.")

    f3_but_4 <- tk2button(
        f3_but_e,
        width = 8,
        text = "Locale",
        command = window_locale_set,
        tip = str_c(
            "Change current locale. \n",
            "Useful if pasting text results in encoding issues. \n",
            'It could be useful to select correct "Enconding" too.'))

    tkgrid(f3_but, sticky = "ew", columnspan = 2)
    tkgrid.columnconfigure(f3_but, 1, weight = 1)

    tkgrid(f3_but_w, f3_but_e, sticky = "e", pady = c(2, 4))

    tkgrid(f3_lab_nrows,
           f3_box_nrow_1$frame, f3_box_nrow_2$frame, f3_box_type$frame,
           sticky = "w")

    tkgrid(f3_but_1, f3_but_2, f3_but_3, f3_but_4, sticky = "e")

    tkgrid.configure(f3_box_nrow_2$frame, padx = c(2, 2))
    tkgrid.configure(f3_lab_nrows, padx = c(10, 2))
    tkgrid.configure(f3_but_4, padx = c(0, 10))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    bs_label_b(f3, text = "Dataset") %>% tkgrid()
    f3_txt_2 <- bs_text(f3, width = 75, height = 11, wrap = "none",
                        undo = FALSE, state = "disabled", font = text_font,
                        tip = str_c(
                            "Types of variables: \n",
                            " - <int> whole numbers (integers);\n",
                            ' - <dbl> real numbers ("double");\n',
                            " - <chr> character (text) variables;\n",
                            " - <fct> factors (categorical variables);\n",
                            " - <ord> ordinal factors;\n",
                            " - <lgl> logical values."
                        ))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f2, f3, sticky = "nsw", padx = c(0, 5), pady = c(0, 15))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Finalize ---------------------------------------------------------------

    # Help topic
    ok_cancel_help(helpSubject = "fread", helpPackage = "data.table",
                   reset = "window_import_from_text()",
                   ok_label = "Import")

    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Fonts ------------------------------------------------------------------
    font_consolas_italic  <- tkfont.create(family = "Consolas", size = 8, slant = "italic")
    font_consolas_bold    <- tkfont.create(family = "Consolas", size = 8, weight = "bold")
    font_consolas_regular <- tkfont.create(family = "Consolas", size = 8)


    # Configuration ----------------------------------------------------------

    tktag.configure(f3_txt_1, "tab", background = "grey95")

    tktag.configure(f3_txt_2, "var_names",
                    foreground = "blue",
                    font = font_consolas_bold)

    tktag.configure(f3_txt_2, "var_types",
                    foreground = "grey50",
                    font = font_consolas_italic)

    tktag.configure(f3_txt_2, "info",
                    foreground = "grey50",
                    font = font_consolas_italic)

    tktag.configure(f3_txt_2, "error", foreground = "red3")

    tktag.configure(f3_txt_2, "grey",  foreground = "grey50")
    tktag.configure(f3_txt_2, "green", foreground = "green")
    tktag.configure(f3_txt_2, "red",   foreground = "red")
    tktag.configure(f3_txt_2, "red3",  foreground = "red3")
    tktag.configure(f3_txt_2, "red4",  foreground = "red4")


    # Interactive bindings ---------------------------------------------------

    tkbind(f3_txt_1, "<Control-s>",       update_preview_ds)
    tkbind(f3_txt_1, "<Triple-Button-3>", update_preview_ds)

    # tkbind(f3_txt_1, "<Control-Shift-Z>",  "<<Redo>>")
    tkbind(f3_txt_1, "<<Copy>>",     auto_update_preview_ds)
    tkbind(f3_txt_1, "<<Paste>>",    auto_update_preview_ds)
    tkbind(f3_txt_1, "<<Undo>>",     auto_update_preview_ds)
    tkbind(f3_txt_1, "<<Redo>>",     auto_update_preview_ds)
    tkbind(f3_txt_1, "<KeyRelease>", auto_update_preview_ds)

    # Output -----------------------------------------------------------------
    # Functions to modify state of the widget
    invisible(
        list(
            set_mode_clipboard   = set_mode_clipboard,
            set_mode_file_url    = set_mode_file_url,
            paste_from_clipboard = paste_from_clipboard,
            update_preview_ds    = update_preview_ds
        )
    )
}

