#' ===========================================================================
#' TODO:
#' 1) Save dialogue values (Put dialogue) in order not to reset if error occurs
#'    and window reappears.
#' 2) Better management if several missing values are used.
#'
#'
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Based on function from "Rcmdr"


# ============================================================================
window_import_excel <- function() {

    # Initial values ---------------------------------------------------------
    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xl_file       <- ""
    worksheets    <- ""
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dir_name_var  <- tclVar(getwd())
    file_name_var <- tclVar(xl_file)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # defaults      <- list(
    #     initial_??? = "???",
    #     initial_??? = "???",
    #     initial_??? = "???",
    #     initial_??? = "???",
    # )
    # dialog_values <- getDialog("window_import_excel", defaults)


    # if (length(worksheets) > 1) {
    #     worksheet <- tk_select.list(worksheets, title = gettext_bs("Select one table"))
    # } else {
    #     worksheet <- worksheets
    # }

    # Functions --------------------------------------------------------------
    select_xl_file <- function() {
        xl_file <- tclvalue(
            tkgetOpenFile(
                title = "Choose Excel File to Import",
                filetypes = str_c(
                    '{\"MS Excel file\" {\".xlsx\" \".XLSX\" \".xls\" \".XLS\"}} ',
                    '{\"All Files\" {\"*\"}}'),
                parent     = CommanderWindow(),
                initialdir = tclvalue(dir_name_var)
            ))
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


        # Update initial dir ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tclvalue(dir_name_var) <- extract_path(xl_file)

        # Update Import window values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Update Excel file name ~~~~~~~
        tclvalue(file_name_var) <- xl_file
        tkconfigure(fname_label, text = path_truncate(xl_file, max_length = 40))
        # Update worksheet names ~~~~~~~
        tkconfigure(worksheet_box$combobox, values = correct_input(worksheets))
        tclvalue(worksheet_box$combovar) <- worksheets[1]
        # Update DF name ~~~~~~~~~~~~~~~
        tclvalue(dsname) <- unique_df_name(clean_str(extract_filename(xl_file)))
    }

    # correct_worksheet_selection <- function() {
    #     # Do not allow select last empty option
    #     if (tclvalue(worksheet_box$combovar) == "") {
    #         tclvalue(worksheet_box$combovar) <- worksheets[1]
    #     }
    # }

    # Frames and widgets -----------------------------------------------------
    # Initialize ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Import from Excel"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame   <- tkframe(top)
    upper_l_frame <- tkframe(upper_frame)
    upper_r_frame <- tkframe(upper_frame)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    worksheet_box <-
        inputComboBox(
            upper_r_frame,
            variableList     = worksheets,
            initialSelection = worksheets[1],
            # onSelect_fun     = correct_worksheet_selection,
            width = 36
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dsname      <- tclVar(unique_df_name("new_data", all_numbered = TRUE))
    entryDsname <- ttkentry(upper_r_frame, width = "39", textvariable = dsname)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    checkBoxFrame <- tkframe(top)
    variableNames <- tclVar("1")
    variableNamesCheckBox <- ttkcheckbutton(checkBoxFrame,
                                            variable = variableNames)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rowNames <- tclVar("0")
    rowNamesCheckBox <- ttkcheckbutton(checkBoxFrame, variable = rowNames)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    stringsAsFactors <- tclVar("1")
    stringsAsFactorsCheckBox <-
        ttkcheckbutton(checkBoxFrame,
                       variable = stringsAsFactors
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    lower_frame <- tkframe(top)

    missingFrame    <- tkframe(lower_frame)
    missingVariable <- tclVar(gettext_bs("<empty cell>"))
    missingEntry    <- ttkentry(missingFrame,
                                width = "12",
                                textvariable = missingVariable)
    # onOK ===================================================================
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))
        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        xl_file     <- tclvalue(file_name_var)
        worksheet   <- getSelection(worksheet_box)
        new_ds_name <- trim.blanks(tclvalue(dsname))

        variableNamesValue    <- tclvalue(variableNames)
        rowNamesValue         <- tclvalue(rowNames)
        stringsAsFactorsValue <- tclvalue(stringsAsFactors)
        missingValues         <- as.character(tclvalue(missingVariable))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Reset widget properties before checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # ....

        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (is_empty_name(new_ds_name))            {return()}
        if (is_not_valid_name(new_ds_name))        {return()}
        if (forbid_to_replace_object(new_ds_name)) {return()}

        if (xl_file == "") {
            show_error_messages(
                "No Excel file was selected.\nPlease select a file.",
                title = "File Not Selected")
            return()
        }

        if (worksheet == "") {
            show_error_messages(
                "No Excel worksheet was selected.\nPlease select a worksheet.",
                title = "Worksheet Not Selected")
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # putDialog("window_import_excel", list(
        #     initial_??? = "???",
        #     initial_??? = "???",
        #     initial_??? = "???",
        #     initial_??? = "???"
        # ))

        # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        rownames_str <-
            if (rowNamesValue == "1") {"TRUE"} else {"FALSE"}

        header_str <-
            if (variableNamesValue == "1") {"TRUE"} else {"FALSE"}

        as_factor_str <-
            if (stringsAsFactorsValue == "1") {"TRUE"} else {"FALSE"}

        initial_mv <- missingValues %in% gettext_bs("<empty cell>")
        if (any(initial_mv)) {
            missingValues[initial_mv] <- ""
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("RcmdrMisc")
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command <-
            str_glue(
                "## Import data from Excel file\n ",
                '{new_ds_name} <-  RcmdrMisc::readXL(   \n',
                '    "{xl_file}",  ',
                '    sheet = "{worksheet}",             \n',
                '    rownames = {rownames_str},         \n',
                '    header = {header_str},             \n',
                '    na = "{missingValues}",            \n',
                '    stringsAsFactors = {as_factor_str} \n',
                '  )'
            ) %>%
            style_cmd()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # writeLines(command)

        # result <- justDoIt(command)
        # logger(command)

        result <- doItAndPrint(command)

        if (class(result)[1] != "try-error") {
            # gassign(new_ds_name, result)
            activeDataSet(new_ds_name)
        }
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Grid of widgets ========================================================

    # Title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fg_col <- Rcmdr::getRcmdr("title.color")

    tkgrid(label_rcmdr(
        top,
        text = gettext_bs("Import data from Excel file"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col
    ),
    pady = c(5, 9)
    )
    # Grid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame, sticky = "e")

    tkgrid(upper_l_frame, upper_r_frame, pady = c(0, 5))

    # upper_l_frame ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # on_click <- function() {select_xl_file()}


    file_label <- label_rcmdr(upper_l_frame, text = "File: ",  fg = fg_col)
    tkgrid(file_label,
           sticky = "e", pady = c(2.5, 5))
    tkgrid(label_rcmdr(upper_l_frame, text = "Sheet: ", fg = fg_col),
           sticky = "e", pady = c(0, 2.5))
    tkgrid(label_rcmdr(upper_l_frame, text = "Name: ",  fg = fg_col),
           sticky = "e", pady = c(5, 5))

    # upper_r_frame ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fname_frame <- tkframe(upper_r_frame)
    fname_label <- label_rcmdr(fname_frame, text = xl_file)
    tkgrid(fname_frame, sticky = "ew", pady = c(5, 5))

    tkgrid(fname_label, sticky = "w")
    tkgrid(getFrame(worksheet_box), sticky = "w")
    tkgrid(entryDsname, sticky = "w", pady = 5)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    button_get_file_frame <- tkframe(lower_frame)
    button_get_file <- tk2button(button_get_file_frame,
                                 text = "Choose file",
                                 command = select_xl_file,
                                 cursor = "hand2")

    # tkgrid(button_get_file, sticky = "e", pady = c(0, 0))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(variableNamesCheckBox,
           labelRcmdr(
               checkBoxFrame,
               text = gettext_bs("Variable names in first row of spreadsheet")
           ),
           sticky = "w"
    )
    tkgrid(rowNamesCheckBox,
           labelRcmdr(
               checkBoxFrame,
               text = gettext_bs("Row names in first column of spreadsheet")
           ),
           sticky = "w"
    )
    tkgrid(stringsAsFactorsCheckBox,
           labelRcmdr(
               checkBoxFrame,
               text = gettext_bs("Convert character data to factors")
           ),
           sticky = "w"
    )
    tkgrid(checkBoxFrame, sticky = "w")


    tkgrid(labelRcmdr(missingFrame,
                      text = gettext_bs("Missing data indicator: ")),
           missingEntry,
           sticky = "w"
    )
    tkgrid(button_get_file, padx = c(5, 0))
    tkgrid(missingFrame, button_get_file_frame, sticky = "w")
    tkgrid(lower_frame, pady = c(5, 0), sticky = "ew")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Help topic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "readXL")

    # Finalize ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame,  sticky = "w")
    dialogSuffix(focus = entryDsname)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Interactive bindings ---------------------------------------------------

    # Add interactivity for `fname_frame` and `fname_label`
    # tkbind(file_label,     "<ButtonPress-1>", select_xl_file)
    tkbind(fname_frame,    "<ButtonPress-1>", select_xl_file)
    tkbind(fname_label,    "<ButtonPress-1>", select_xl_file)

    tkbind(fname_frame, "<Enter>",
           function() tkconfigure(fname_label, foreground = "blue"))
    tkbind(fname_frame, "<Leave>",
           function() tkconfigure(fname_label, foreground = "black"))
    # tkconfigure(file_label,     cursor = "hand2")
    tkconfigure(fname_frame,    cursor = "hand2")
    # tkconfigure(button_get_file, cursor = "hand2")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
# ============================================================================
