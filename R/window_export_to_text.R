# TODO:
#
# 1. Warn (OK/CANCEL) if file name changes after pressing "Export"
# 2. Check if interaction betweet "ext_field" and "file" is correct in all cases
# (see TODO below)
# 3. onOk: check if filename is valid (e.g., not "C:")
# 4. When ext is ".csv", ensure only American (, .) or European (; ,) CSV styles,
#    not not a mix (; .): (field and decimal separators).
# 5. If field  separator is a whitespace, NA walue must not be space
#    (forbid this choice and warn).
# 6. Custom separator entrance field: add restrioctons to the number of symbols.
# 7. Forbid custom separator to be empty in all cases and comma if dec is comma.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .============================ ==============================================
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_to_text <- function() {

    # Functions ==============================================================
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Open file select dialogue
    open_get_path_to_file_window <- function(
        f_path = fs::path(getwd(), active_dataset())) {


        initialdir <- fs::path_dir(f_path)
        if (initialdir %in% c("", ".") || !fs::dir_exists(initialdir)) {
            initialdir <- getwd()
        }

        initialfile <- fs::path_file(f_path)
        if (initialfile == "") {
            initialfile <- .ds
        }

        # Remove extension to make easier corrections of file names in
        # pop-up window.
        # initialfile <- fs::path_ext_remove(initialfile)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        file_name <- tclvalue(
            tkgetSaveFile(
                parent = top,
                # typevariable = typevariable, # to capture selected type
                title = "Create or Choose Text File to Save Data to",
                confirmoverwrite = FALSE,
                initialfile = initialfile,
                initialdir  = initialdir,
                filetypes   = "{ {Text file} {.txt .csv .tsv .dat} } { {All Files} * }"))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        file_name
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    set_file_path_related_values <- function(file_name = "") {

        if (file_name == "") {
            tkfocus(top)
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Return extension
        # if (!str_detect(file_name, "\\.(txt|csv|tsv|dat)$")) {
        #     file_name <- str_c(file_name, ".txt")
        # }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (get_use_relative_path()) { # make relative path
            file_name <- make_relative_path(file_name)
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        set_values(f1_ent_file, file_name)
        update_file_ent_pos()

        set_ext_field()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_path_to_file <- function() {

        file_name <- open_get_path_to_file_window(f_path = read_path_to_file())
        set_file_path_related_values(file_name)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_file_ent_pos <- function() {
        tkxview.moveto(f1_ent_file$obj_text, "1") # 0 - beginning, 1 - end.
        tkicursor(f1_ent_file$obj_text, "end")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Read value of file name entry box
    read_path_to_file <- function() {
        get_values(f1_ent_file)
    }


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Get options
    get_sep <- function() {
        val <- get_selection(f2_box_sep)
        switch(val,
               # "Auto"            = "auto",
               "Space ( )"       = " ",
               "Tab"             = "\\t",
               "Comma ( , )"     = ",",
               "Semicolon ( ; )" = ";",
               "Pipe ( | )"      = "|",
               "Custom\u2026"    = get_values(f2_ent_sep),
               stop("Value '", val, "' is unknown (f2_box_sep)."))
    }


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_dec <- function() {
        val <- get_selection(f2_box_dec)
        switch(val,
               # "Default"      = ".",
               "Period ( . )" = ".",
               "Comma ( , )"  = ",",
               stop("Value '", val, "' is unknown (f2_box_dec)."))
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_ext <- function() {
        val <- get_selection(f2_box_ext)
        switch(val,
               # "Default"      = ".",
               ".txt"  = ,
               ".csv"  = ,
               ".tsv"  = ,
               ".dat"  = val,
               "other" = " ",
               stop("Value '", val, "' is unknown (f2_box_ext)."))
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    set_ext_in_file <- function() {

        fn <- read_path_to_file()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # TODO: review if these lines do, what's expected.
        # In these cases the name should be added:
        # "C:", "/", "dir/", ".txt", "/.txt", "dir/.txt"


        # If file name is missing, file name should be added
        file_name_is_missing <-
            file_name_is_missing == "" ||
            str_detect(
                fn,
                "([\\\\/]$)|([\\\\/]\\.(txt|csv|tsv|dat)$)|(^\\.(txt|csv|tsv|dat)$)")

        if (file_name_is_missing) {
            fn <- fs::path(fs::path_dir(fn), active_dataset())
            fn <- str_remove(fn, "^/")
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        fn_wo_ext <- str_replace(fn, "\\.(txt|csv|tsv|dat)$", "")

        if (str_sub(fn_wo_ext, -1) %in% c("/", "", ":")) {
            return()
        }

        new_ext <- get_ext()
        new_full_name <- str_c(fn_wo_ext, new_ext)
        set_values(f1_ent_file, values = new_full_name)

        set_sep_values()

        update_file_ent_pos()
        tip(f1_ent_file$obj_text) <- new_full_name

    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Return extension if it is txt, csv or dat or NA otherwise
    cur_ext_txt_csv_tsv_dat <- function() {
        str_extract(read_path_to_file(), "\\.(txt|csv|tsv|dat)$")
    }

    set_ext_field <- function() {
        cur_ext <- cur_ext_txt_csv_tsv_dat()

        # Set ext field value
        if (cur_ext %in% c(".txt", ".csv", ".tsv", ".dat")) {
            set_selection(f2_box_ext, cur_ext)

        } else {
            set_selection(f2_box_ext, "other")
        }

        set_sep_values()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    set_sep_values <- function(variables) {

        cur_sep <- get_selection(f2_box_sep)

        # Restrict separator options for csv, tsv
        cur_ext <- cur_ext_txt_csv_tsv_dat()

        sep1 <- switch(
            cur_ext,
            ".csv" = c("Comma ( , )", "Semicolon ( ; )"),
            ".tsv" = c("Tab"),
            c("Tab", "Space ( )", "Comma ( , )", "Semicolon ( ; )", "Pipe ( | )", "Custom\u2026")
        )

        if (get_dec() == ",") {
            sep1 <- sep1[sep1 != "Comma ( , )"]

            if (length(sep1) == 1) {
                sep1 <- str_c("{", sep1, "}")
            }
        }

        # Set possible separator options
        set_values(f2_box_sep, sep1)

        # Keep curently selected separatio, if possible
        if (cur_sep %in% sep1) {
            set_selection(f2_box_sep, cur_sep)
        }

        custom_sep_activation()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Sepatator entry field
    sep_ent_normalize <- function() {
        tk_normalize(f2_ent_sep)
        tk2tip(f2_ent_sep$frame, "Enter a custom field separator")
        set_selection(f2_box_sep, "Custom\u2026")
    }

    sep_ent_disable <- function() {
        set_values(f2_ent_sep, "")
        tk_disable(f2_ent_sep)
        tk2tip(f2_ent_sep$frame, "Double click to enter a custom value")
    }


    custom_sep_activation <- function() {
        # Always disable in CSV and TSV modes
        if (get_selection(f2_box_ext) %in% c(".csv", ".tsv")) {
            sep_ent_disable()
            tk2tip(f2_ent_sep$frame,
                   "Custom separator is not available\nif extension is either .csv or .tsv.")

            # if (get_selection(f2_box_sep) == "Custom\u2026") {
            #     set_selection(f2_box_sep, get_values(f2_box_sep)[1])
            # }

            return()
        }

        # Otherwise
        if (get_selection(f2_box_sep) == "Custom\u2026") {
            sep_ent_normalize()

        } else {
            sep_ent_disable()
        }
    }

    sep_ent_normalize_if_appropriate <- function() {
        if (get_selection(f2_box_ext) %in% c(".csv", ".tsv")) {
            sep_ent_disable()

        } else {
            sep_ent_normalize()
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~ onOK -------------------------------- --------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        file_name  <- read_path_to_file()
        sep <- get_sep()
        dec <- get_dec()
        na_txt <- get_values(f2_ent_na)


        if (is_empty_name(file_name, which_name = "file name")) {
            return()
        }

        if (forbid_to_replace_file(file_name)) {
            return()
        }

        #  Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        has_rownames <- tibble::has_rownames(get(.ds, envir = .GlobalEnv))

        command <-
            str_glue("## Save data to text file\n",
                     'data.table::fwrite( \n',
                     '     {.ds}, \n',
                     '     file = "{file_name}", \n',
                     '     sep = "{sep}", \n',
                     '     dec = "{dec}", \n',
                     '     row.names  = {has_rownames}, \n',
                     '     col.names  = TRUE, \n',
                     '     na = "{na_txt}")'
            )

        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("data.table")
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

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

    # Initial file name ------------------------------------------------------
    top <- CommanderWindow()

    .ds <- active_dataset()

    # Manke unique initial file name

    # file_name <- open_get_path_to_file_window()
    file_name <- .ds

    # if (file_name == "") {
    #     tkfocus(CommanderWindow())
    #     return()
    #
    # } else {

        # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        dialogue_title <- "Export Data to Text File"
        initializeDialog(title = gettext_bs(dialogue_title))
        tk_title(top, dialogue_title)

        .ds <- active_dataset()

        # Widgets ================== ===========================================

        # F1, Frame 1, choose file and name ------------------------------------
        f1 <- tk2frame(top)

        f1_lab_data_1 <- bs_label_b(f1, text = "Dataset: ")
        f1_lab_data_2 <- bs_label(f1, text = .ds)

        f1_lab_file <- bs_label_b(f1, text = "File: ")
        f1_ent_file <- bs_entry(
            f1, width = 60, sticky = "we",
            tip = "Path to file",
            state = "readonly",
            on_key_release = set_ext_field,
            use_context_menu = FALSE,
            bind_clear = FALSE,
            on_click = get_path_to_file)

        f1_but_paste <- tk2button(
            f1,
            image = "::image::bs_paste",
            command = function() {
                set_values(f1_ent_file,
                           str_c(read_path_to_file(), read_clipboard()))
                update_file_ent_pos()
            },
            tip = "Paste file name"
        )

        f1_but_copy <- tk2button(
            f1,
            image = "::image::bs_copy",
            command = function() {
                text <- get_values(f1_ent_file)
                clipr::write_clip(text, object_type = "character")
            },
            tip = "Copy file name"
        )

        f1_but_clear <- tk2button(
            f1,
            image = "::image::bs_delete",
            command = function() {
                set_values(f1_ent_file, "")
            },
            tip = "Clear file name"
        )

        f1_but_f_choose <- tk2button(
            f1,
            image = "::image::bs_open_file",
            command = get_path_to_file,
            tip = "Choose file to save to"
        )

        # Possible options ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ext1     <- c(".txt", ".csv", ".tsv", ".dat", "other")
        dec1     <- c("Period ( . )", "Comma ( , )") # "Default"

        sep1_all <- c("Tab", "Space ( )", "Comma ( , )", "Semicolon ( ; )",
                      "Pipe ( | )", "Custom\u2026")

        f2 <- tk2frame(top)

        f2_box_dec  <- bs_combobox(
            label = "Decimal:",
            tip = "Decimal separator. \nE.g., 10.2 vs. 10,2",
            f2, width = 11, values = dec1, selection = 1,
            on_select = set_sep_values)

        f2_box_sep  <- bs_combobox(
            label = "Separator:", tip = "Value (field) separator",
            f2, width = 13, values = sep1_all, selection = 1,
            on_select = custom_sep_activation)

        f2_ent_sep  <- bs_entry(
            f2, width = 3, on_double_click = sep_ent_normalize_if_appropriate)

        f2_ent_na   <- bs_entry(
            f2, width = 14, label = "NA value:", tip = "Missing value",
            value = "")

        f2_box_ext  <- bs_combobox(
            label = "Extension:", tip = str_c(
                "File extension: \n",
                " .txt - text file,\n",
                " .csv - comma separated values,\n",
                " .tsv - tab separated values,\n",
                " .dat - text file with data."),
            f2, width = 5, values = ext1, selection = 1,
            on_select = set_ext_in_file
        )

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkgrid(f1, padx = 10, sticky = "we")

        tkgrid(f1_lab_data_1, f1_lab_data_2, pady = c(10, 2), sticky = "we")

        tkgrid(f1_lab_file, f1_ent_file$frame, f1_but_f_choose,
               f1_but_paste, f1_but_copy, f1_but_clear,
               pady = c(2, 2),  sticky = "we")

        tkgrid.configure(f1_lab_data_1, padx = c(8, 0))
        tkgrid.configure(f1_lab_data_1, f1_lab_file, sticky = "e")
        tkgrid.configure(f1_ent_file$frame, sticky = "we", padx = 2)

        tkgrid.configure(f1_ent_file$frame_text, f1_ent_file$obj_text,
                         sticky = "we")

        tkgrid(f2, sticky = "w")
        tkgrid(f2_box_dec$frame, f2_box_sep$frame, f2_ent_sep$frame)
        tkgrid(f2_ent_na$frame,  f2_box_ext$frame, pady = 2)

        tkgrid.configure(
            f2_box_dec$frame,
            sticky = "e", padx = c(6, 0))

        tkgrid.configure(
            f2_box_sep$frame,
            f2_ent_na$frame,
            f2_box_ext$frame,
            sticky = "e", padx = c(9, 0))

        tkgrid.configure(
            f2_ent_sep$frame,
            sticky = "e", padx = c(2, 0))

        sep_ent_disable()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Finalize -------------------------------------------------------------

        # Help topic
        ok_cancel_help(helpSubject = "fwrite", helpPackage = "data.table",
                       # reset = "window_export_to_text()",
                       ok_label = "Export")

        dialogSuffix(grid.buttons = TRUE, bindReturn = FALSE)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Configuration --------------------------------------------------------
        set_file_path_related_values(file_name)

        # Set ext to .txt, if not chosen
        if (is.na(cur_ext_txt_csv_tsv_dat())) {
            set_values(f1_ent_file, str_c(read_path_to_file(), ".txt"))
        }

        # Set ext field value
        set_ext_field()

    # }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    invisible()
}
