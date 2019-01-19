# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .============================ =================================================
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_to_excel <- function() {

    # Functions ==============================================================
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Open file select dialogue
    get_path_to_file <- function() {

        f_path <- read_path_to_file()

        initialdir  <- fs::path_dir(f_path)
        if (initialdir == "" || !fs::dir_exists(initialdir)) {
            initialdir <- getwd()
        }

        initialfile <- fs::path_file(f_path)
        if (initialfile == "") {
            initialfile <- .ds
        }
        initialfile <- fs::path_ext_set(initialfile, "xlsx")

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        filename <- tclvalue(tkgetSaveFile(
            parent = top,
            # typevariable = typevariable, # to capture selected type
            title = "Save Data to Excel File",
            confirmoverwrite = FALSE,
            initialfile = initialfile,
            initialdir = initialdir,
            filetypes = "{ {Excel file} {.xlsx} } { {All Files} * }"))

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (filename == "") {
            tkfocus(top)
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        set_values(f1_ent_file, make_relative_path(filename))
        # set_values(f1_ent_file, (filename))

        uptate_file_ent_pos()

        update_sheet_name()
        # if (fs::is_file(filename)) {
        #     update_sheet_name()
        # }
    }

    uptate_file_ent_pos <- function() {
        tkxview.moveto(f1_ent_file$obj_text, "1") # 0 - beginning, 1 - end.
        tkicursor(f1_ent_file$obj_text, "end")
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
    create_sheet_name <- function(name) {
        sheets <- get_sheets()
        sheet <- make.unique(str_c(str_trunc(name, 27), sheets), sep = "_")

        set_values(f1_ent_sheet, sheet)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Make unique sheet name
    update_sheet_name <- function() {
        create_sheet_name(name = get_values(f1_ent_sheet))
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

        # # Check if file exists or is URL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # if (!check_file_name()) {
        #     return()
        # }
        #
        # # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # if (is_empty_name(new_name)) {
        #     return()
        # }
        #
        # if (is_not_valid_name(new_name)) {
        #     return()
        # }
        #
        # if (forbid_to_replace_object(new_name)) {
        #     return()
        # }

        #  Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        has_rownames <- tibble::has_rownames(get(.ds, envir = .GlobalEnv))
        file_overwrite <- TRUE

        command <-
            str_glue("## Save data to Excel file\n",
                     "openxlsx::write.xlsx({.ds}, \n",
                     '     file = "{file_name}", \n',
                     '     sheetName = "{sheet_name}", \n',
                     "     rowNames  = {has_rownames}, \n",
                     "     colNames  = TRUE, \n",
                     '     colWidths = "auto",\n',
                     "     overwrite = {file_overwrite})"
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
    initializeDialog(title = gettext_bs("Export Data to Excel File"))
    tk_title(top, "Export data to Excel file")

    .ds <- active_dataset()

    # Widgets ================== =============================================

    # F1, Frame 1, choose file and name --------------------------------------
    f1 <- tk2frame(top)

    f1_lab_data_1 <- bs_label_b(f1, text = "Dataset: ")
    f1_lab_data_2 <- bs_label(f1, text = .ds)

    f1_lab_file <- bs_label_b(f1, text = "File: ")
    f1_ent_file <- bs_entry(
        f1, width = 90, sticky = "we", tip = "Path to file")

    f1_but_paste <- tk2button(
        f1,
        image = "::image::bs_paste",
        command = function() {
            set_values(f1_ent_file,
                       str_c(read_path_to_file(), read_clipboard()))
            uptate_file_ent_pos()
        },
        tip = "Paste file name"
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
        command = update_sheet_name,
        tip = str_c("Choose automatic sheet name")
    )

    f1_lab_sheet <- bs_label_b(f1, text = "Sheet: ")
    f1_ent_sheet <- bs_entry(
        f1, width = 90, sticky = "ew", tip = "Excel sheet name to save data to")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f1, padx = 10, sticky = "we")

    tkgrid(f1_lab_data_1, f1_lab_data_2, pady = c(10, 2), sticky = "we")

    tkgrid(f1_lab_file, f1_ent_file$frame, f1_but_f_choose, f1_but_paste, f1_but_clear,
           pady = c(2, 2),  sticky = "we")

    tkgrid(f1_lab_sheet, f1_ent_sheet$frame, f1_but_refresh,
           pady = c(0,  10), sticky = "we")

    tkgrid.configure(f1_lab_data_1, f1_lab_file, f1_lab_sheet, sticky = "e")
    tkgrid.configure(f1_ent_file$frame, f1_ent_sheet$frame, sticky = "we", padx = 2)
    tkgrid.configure(
        f1_ent_file$frame_text, f1_ent_sheet$frame_text,
        f1_ent_file$obj_text,   f1_ent_sheet$obj_text,
        sticky = "we")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Finalize ---------------------------------------------------------------

    # Help topic
    ok_cancel_help(helpSubject = "readRDS",
                   reset = "window_export_to_excel()",
                   ok_label = "Export")

    dialogSuffix(grid.buttons = TRUE, bindReturn = FALSE)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Configuration ----------------------------------------------------------
    set_values(f1_ent_file, str_c(.ds, ".xlsx"))
    create_sheet_name(name = .ds)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    invisible()
}