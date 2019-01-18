# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_to_excel <- function() {


    show_error_messages(
        "Function `window_export_to_excel` needs to be fixed.",
        title = "Error!!!")
    return()

    # Initial values ---------------------------------------------------------
    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ds     <- active_dataset()
    fg_col <- Rcmdr::getRcmdr("title.color")

    file_name <- unique_colnames(
        ds, list_of_choices = extract_filename(dir(pattern = "\\.xls(x)?")))

    # xl_file        <- str_c(ds, ".xlsx")
    # worksheets     <- ds

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    select_file <- function() {

        file_obj <- tkgetSaveFile(
            title = "Save Excel File",
            filetypes = str_c(
                '{"Excel file" {".xlsx"}} ',
                '{"All Files" {"*"}}'),
            parent     = CommanderWindow(),
            confirmoverwrite = TRUE,
            initialfile = tclvalue(file_name_var),
            initialdir = tclvalue(dir_name_var))

        xl_file <- tclvalue(file_obj)
        # Update Excel worksheeds information ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if (xl_file == "") {
            # tkfocus(CommanderWindow())
            tkfocus(top)
            return()

        } else {
            worksheets <- excel_sheets(xl_file)
        }

        correct_input <- function(worksheets) {
            if (length(worksheets) == 1) {
                # { } prevents from a bug
                # which appears when an excel workbook
                # contains only one sheet and the name
                # of that sheet contains a speace, e.g., "a b".
                worksheets <- str_c("{", worksheets, "}")
            }
            worksheets
        }

        # Update Import window values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Update initial dir ~~~~~~~~~~~~~~
        xl_path <- sub("[\\/]$", "", extract_path(xl_file))

        tclvalue(dir_name_var)   <- xl_path
        # tclvalue(dir_name_label) <- path_truncate(xl_path, max_length = 40)

        # Update Excel file name ~~~~~~~
        tclvalue(file_name_var) <- xl_file

        # Update worksheet names ~~~~~~~
            tclvalue(sheet_name_var) <-
            tclvalue(sheet_name_var) %>%
            str_trunc(28, "left") %>%
            unique_obj_names(list_of_choices = worksheets)

        invisible()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # select_folder <- function() {
    #     xl_path <- tclvalue(
    #         tkchooseDirectory(initialdir = tclvalue(dir_name_var),
    #                           title  = "Select Folder to Save Excel File",
    #                           parent = CommanderWindow())
    #     )
    #     # Update Excel worksheeds information ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #     if (xl_path == "") {
    #         # tkfocus(CommanderWindow())
    #         tkfocus(top)
    #         return()
    #     }
    #
    #     # Update Import window values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
    #     # Update initial dir ~~~~~~~~~~~~~~
    #     xl_path <- sub("[\\/]$", "", extract_path(xl_file))
    #
    #     tclvalue(dir_name_var)   <- xl_path
    #     tclvalue(dir_name_label) <- path_truncate(xl_path, max_length = 40)
    #     invisible()
    # }




    # ...

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults      <- list(initial_position = "first")
    dialog_values <- getDialog("window_export_to_excel", defaults)


    # Frames and widgets -----------------------------------------------------
    # Initialize ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Export Data to Excel File"))

    # Populate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame   <- tkframe(top)
    upper_r_frame <- tkframe(upper_frame)
    upper_l_frame <- tkframe(upper_frame)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ds_name_var   <- tclVar(ds)
    ds_name_frame <- tkframe(upper_l_frame)
    ds_name_label <- bs_label(ds_name_frame, textvariable = ds_name_var)

    dir_name_var   <- tclVar(getwd())
    # dir_name_frame <- tkframe(upper_l_frame)
    # dir_name_label <- bs_label(dir_name_frame, textvariable = dir_name_var)

    file_name_var   <- tclVar(str_c(file_name, ".xlsx"))
    file_name_frame <- tkframe(upper_l_frame)
    file_name_entry <- bs_label(file_name_frame, textvariable = file_name_var)

    sheet_name_var   <- tclVar(str_trunc(ds, 30))
    sheet_name_frame <- tkframe(upper_l_frame)
    sheet_name_entry <- ttkentry(sheet_name_frame, width = "38",
                                 textvariable = sheet_name_var)

    get_buttons_frame <- tkframe(top)

    button_get_file <- tk2button(get_buttons_frame,
                                 text = "Choose file",
                                 command = select_file,
                                 cursor = "hand2")

    # button_get_dir <- tk2button(get_buttons_frame,
    #                              text = "Choose folder",
    #                              command = select_folder,
    #                              cursor = "hand2")


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # rb_frame <- tkframe(upper_frame)
    # radioButtons_horizontal(rb_frame,
    #                         title = "Column position: ",
    #                         title.color = fg_col,
    #
    #                         # right.buttons = FALSE,
    #                         name = "position",
    #                         sticky_buttons = "w",
    #                         buttons = c("first",  "last"),
    #                         values =  c("first",  "last"),
    #                         labels =  c("First  ","Last  "),
    #                         initialValue = dialog_values$initial_position
    # )

    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # dir_name_name <- trimws(tclvalue(dir_name_var))
        file_name     <- trimws(tclvalue(file_name_var))
        sheet_name    <- trimws(tclvalue(sheet_name_var))


        # xl_file <- file.path(dir_name_name, file_name)

        file.exists(file_name)


        # Reset widget properties before checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # tkconfigure(name_entry, foreground = "black")

        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # if (is_empty_name(new_name)) {
        #     return()
        # }
        #
        # if (is_not_valid_name(new_name)) {
        #     return()
        # }
        #
        # if (forbid_to_replace_variable(new_name)) {
        #     return()
        # }

        # if (forbid_to_replace_object(new_name)) {
        #     return()
        # }

        # if (is_empty_name(new_name))              {return()}
        # if (is_not_valid_name(new_name))          {return()}
        # if (forbid_to_replace_variable(new_name)) {return()}
        # if (forbid_to_replace_object(new_name))   {return()}

        # if (file.exists(file_name) &&
        #     msg_box_confirm_to_replace(file_name, "File") == "no")
        # {
        #     return()
        # }

        # if (??? == "") {
        # show_error_messages(
        #     "No ???  was selected.\nPlease select a ???.",
        #     title = "")
        # return()
        # }
        # if (??? == "") {
        # msg_box_confirm_to_replace(...)
        # return()
        # }

        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_export_to_excel", list(
            initial_position = "which_position"
        ))

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        has_rownames <- tibble::has_rownames(
            get(active_dataset(), envir = .GlobalEnv))

        file_overwrite <- TRUE


        command <-
            str_glue("## Save data to Excel file\n",
                     "openxlsx::write.xlsx({ds}, \n",
                     '     file = "{file_name}", \n',
                     '     sheetName = "{sheet_name}", \n',
                     "     rowNames  = {has_rownames}, \n",
                     "     colNames  = TRUE, \n",
                     '     colWidths = "auto",\n',
                     "     overwrite = {file_overwrite})"
            )

        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("openxlsx")

        # doItAndPrint(command)

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
    }

    # Grid of widgets ========================================================

    # Title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(bs_label(
        top,
        text = gettext_bs("Export data to Excel file"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9))

    # Grid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame, pady = c(0, 5))
    tkgrid(upper_r_frame, upper_l_frame, pady = c(0, 5))

    tkgrid(tk_label_blue(upper_r_frame, text = "Dataset:"),    sticky = "e", pady = c(0, 5))
    # tkgrid(tk_label_blue(upper_r_frame, text = "Folder:"),     sticky = "e")
    tkgrid(tk_label_blue(upper_r_frame, text = "File name:"),  sticky = "e", pady = 5)
    tkgrid(tk_label_blue(upper_r_frame, text = "Sheet name:"), sticky = "e")

    tkgrid(ds_name_frame,    sticky = "", pady = c(5, 5))
    # tkgrid(dir_name_frame,   sticky = "w", pady = c(0, 0))
    tkgrid(file_name_frame,  sticky = "w", pady = c(5, 5))
    tkgrid(sheet_name_frame, sticky = "w")

    tkgrid(ds_name_label,    sticky = "w")
    # tkgrid(dir_name_label,   sticky = "w")
    tkgrid(file_name_entry,  sticky = "w")
    tkgrid(sheet_name_entry, sticky = "w")


    tkgrid(get_buttons_frame, sticky = "e", pady = 5)
    tkgrid(button_get_file, sticky = "e")

    # Help topic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "write.xlsx", helpPackage = "openxlsx")

    # Finalize ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
