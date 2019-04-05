# TODO:
#
# - Add tcl/tk check if sheetname is less than 30 characters length.
# - Enable option to add excel seet instead of replacing all document.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .============================ ==============================================
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_to_excel <- function() {

    # Functions ==============================================================
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Open file select dialogue
    get_path_to_file <- function() {

        f_path <- read_path_to_file()

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
        initialfile <- fs::path_ext_remove(initialfile)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        file_name <- tclvalue(tkgetSaveFile(
            parent = top,
            # typevariable = typevariable, # to capture selected type
            title = "Choose or Create Excel File to Save Data to",
            confirmoverwrite = FALSE,
            initialfile = initialfile,
            initialdir  = initialdir,
            filetypes   = "{ {Excel file} {.xlsx} } { {All Files} * }"))

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (file_name == "") {
            tkfocus(top)
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Return extension
        if (!str_detect(file_name, "\\.xlsx$")) {
            file_name <- str_c(file_name, ".xlsx") # TODO: maybe str_c is not
            # the most appriate function
            # here.
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (get_use_relative_path()) { # make relative path
            file_name <- make_relative_path(file_name)
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        set_values(f1_ent_file, file_name)
        update_file_ent_pos()
        set_unique_sheet_name(name = get_values(f1_ent_sheet))
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_file_ent_pos <- function() {
        tclAfter(1, function() {
            tkxview.moveto(f1_ent_file$obj_text, "1") # 0 - beginning, 1 - end.
            tkicursor(f1_ent_file$obj_text, "end")
        })
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Read value of file name entry box
    read_path_to_file <- function() {
        get_values(f1_ent_file)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Read sheet names, if file exists
    get_sheets <- function() {
        f_path <- read_path_to_file()

        if (fs::is_file(f_path) && fs::path_ext(fs::path_file(f_path)) == "xlsx") {
            sheets <- readxl::excel_sheets(f_path)
        } else {
            sheets <- NULL
        }
        sheets
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create (valid) sheet name
    set_unique_sheet_name <- function(name) {
        sheets <- get_sheets()
        # # Use these lines,when saving to multiple sheets is enabled:
        # sheet <- make.unique(c(str_trunc(name, 27), sheets), sep = "_")[1]
        sheet  <- str_trunc(name, 27)[1]

        set_values(f1_ent_sheet, sheet)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Make unique sheet name
    reset_sheet_name <- function() {
        set_unique_sheet_name(name = .ds)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~ onOK -------------------------------- --------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        file_name  <- read_path_to_file()
        sheet_name <- get_values(f1_ent_sheet)

        if (is_empty_name(sheet_name, which_name = "sheet name")) {
            return()
        }

        if (str_length(sheet_name) > 30) {
            show_error_messages(
                "Excel sheet names must not exceed 30 characters.",
                title = "Too Long Sheet Name")
            return()
        }

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
        file_overwrite <- TRUE

        command <-
            str_glue("## Save data to Excel file\n",
                     'openxlsx::write.xlsx( \n',
                     '     {.ds}, \n',
                     '     file = "{file_name}", \n',
                     '     sheetName = "{sheet_name}", \n',
                     '     rowNames  = {has_rownames}, \n',
                     '     colNames  = TRUE, \n',
                     '     colWidths = "auto",\n',
                     '     overwrite = {file_overwrite})'
            )

        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("openxlsx")
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

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dialogue_title <- "Export Data to Excel File"
    initializeDialog(title = gettext_bs(dialogue_title))
    tk_title(top, dialogue_title)

    .ds <- active_dataset()

    # Widgets ================== =============================================

    # F1, Frame 1, choose file and name --------------------------------------
    f1 <- tk2frame(top)

    f1_lab_data_1 <- bs_label_b(f1, text = "Dataset: ")
    f1_lab_data_2 <- bs_label(f1, text = .ds)

    f1_lab_file <- bs_label_b(f1, text = "File: ")

    f1_ent_file <- bs_entry(
        f1, width = 60, sticky = "we",
        tip = "Path to file",
        state = "readonly",
        # on_key_release = set_ext_field,
        use_context_menu = FALSE,
        bind_clear = FALSE,
        on_double_click = get_path_to_file)


    f1_but_paste <- tk2button(
        f1,
        image = "::image::bs_paste",
        command = function() {
            set_values(f1_ent_file, read_clipboard())
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

    f1_but_refresh <- tk2button(
        f1,
        image = "::image::bs_refresh",
        command = reset_sheet_name,
        tip = str_c("Reset sheet name")
    )

    f1_lab_sheet <- bs_label_b(f1, text = "Sheet: ")
    f1_ent_sheet <- bs_entry(
        f1, width = 60, sticky = "ew", tip = "Excel sheet name to save data to")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f1, sticky = "we")

    tkgrid(f1_lab_file, f1_ent_file$frame,
           f1_but_f_choose, f1_but_paste, f1_but_copy, f1_but_clear,
           pady = c(2, 2),  sticky = "we")

    tkgrid(f1_lab_sheet, f1_ent_sheet$frame, f1_but_refresh,
           pady = c(0,  2), sticky = "we")

    tkgrid(f1_lab_data_1, f1_lab_data_2, pady = c(0, 2), sticky = "we")

    tkgrid.configure(f1_lab_data_1, f1_lab_file, f1_lab_sheet, sticky = "e")
    tkgrid.configure(f1_ent_file$frame, f1_ent_sheet$frame, sticky = "we",
                     padx = 2)

    tkgrid.configure(
        f1_ent_file$frame_text, f1_ent_sheet$frame_text,
        f1_ent_file$obj_text,   f1_ent_sheet$obj_text,
        sticky = "we")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Finalize ---------------------------------------------------------------

    # Help topic
    ok_cancel_help(helpSubject = "read.xlsx", helpPackage = "openxlsx",
                   reset = "window_export_to_excel()",
                   ok_label = "Export")

    dialogSuffix(grid.buttons = TRUE, bindReturn = FALSE)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Configuration ----------------------------------------------------------
    set_values(f1_ent_file, str_c(.ds, ".xlsx"))
    # get_path_to_file()

    # set_values(f1_ent_file, file_name)
    update_file_ent_pos()
    reset_sheet_name()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    invisible()
}