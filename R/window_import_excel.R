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

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xl_file        <- ""
    worksheets     <- ""
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initial_dir_var <- tclVar(getwd())
    xl_file_var     <- tclVar(xl_file)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # if (length(worksheets) > 1) {
    #     worksheet <- tk_select.list(worksheets, title = gettextRcmdr("Select one table"))
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
                initialdir = tclvalue(initial_dir_var)
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
        tclvalue(initial_dir_var) <- extract_path(xl_file)

        # Update Import window values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Update Excel file name ~~~~~~~
        tclvalue(xl_file_var) <- xl_file
        tkconfigure(fname_label, text = path_truncate(xl_file, max_length = 40))
        # Update worksheet names ~~~~~~~
        tkconfigure(worksheet_box$combobox, values = correct_input(worksheets))
        tclvalue(worksheet_box$combovar) <- worksheets[1]
        # Update DF name ~~~~~~~~~~~~~~~
        tclvalue(dsname) <- unique_df_name(clean_str(extract_filename(xl_file)))
    }

    correct_worksheet_selection <- function() {
        # Do not allow select last empty option
        if (tclvalue(worksheet_box$combovar) == "") {
            tclvalue(worksheet_box$combovar) <- worksheets[1]
        }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettextRcmdr("Import from Excel"))
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
            onSelect_fun     = correct_worksheet_selection,
            width = 32
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dsname      <- tclVar(unique_df_name("new_data", all_numbered = TRUE))
    entryDsname <- ttkentry(upper_r_frame, width = "35", textvariable = dsname)
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
    missingFrame <- tkframe(top)
    missingVariable <- tclVar(gettextRcmdr("<empty cell>"))
    missingEntry <- ttkentry(missingFrame,
                             width = "15",
                             textvariable = missingVariable)
    # ========================================================================
    onOK <- function() {
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        setBusyCursor()
        on.exit(setIdleCursor())
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        xl_file     <- tclvalue(xl_file_var)
        worksheet   <- getSelection(worksheet_box)
        new_ds_name <- trim.blanks(tclvalue(dsname))

        variableNamesValue    <- tclvalue(variableNames)
        rowNamesValue         <- tclvalue(rowNames)
        stringsAsFactorsValue <- tclvalue(stringsAsFactors)
        missingValues         <- as.character(tclvalue(missingVariable))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (missingValues == gettextRcmdr("<empty cell>")) {
            missingValues <- ""
        }
        if (new_ds_name == "") {
            errorCondition(
                recall = window_import_excel,
                message = gettextRcmdr("You must enter the name of the dataset.")
            )
            return()
        }
        if (!is.valid.name(new_ds_name)) {
            errorCondition(
                recall = window_import_excel,
                message = paste0("\"", new_ds_name, "\" ",
                                 gettextRcmdr("is not a valid name."))
            )
            return()
        }
        if (is.element(new_ds_name, listDataSets())) {
            if ("no" == tclvalue(
                checkReplace(new_ds_name, gettextRcmdr("Data set")))) {
                window_import_excel()
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (xl_file == "") {
            errorCondition(message = gettextRcmdr("No Excel file was selected."))
            return()
        }
        if (worksheet == "") {
            errorCondition(message = gettextRcmdr("No Excel sheet was selected."))
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        rownames_str <-
            if (rowNamesValue == "1") {"TRUE"} else {"FALSE"}

        header_str <-
            if (variableNamesValue == "1") {"TRUE"} else {"FALSE"}

        as_factor_str <-
            if (stringsAsFactorsValue == "1") {"TRUE"} else {"FALSE"}
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("RcmdrMisc")
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command <-
            str_glue(
                "## Import data from Excel file\n ",
                '{new_ds_name} <- ',
                '  RcmdrMisc::readXL(\n',
                '    "{xl_file}",  ',
                '    sheet = "{worksheet}", ',
                "    rownames = {rownames_str}, header = {header_str}, \n",
                '    na = "{missingValues}",',
                "    stringsAsFactors = {as_factor_str} \n",
                "  )"
            ) %>%
            style_cmd()
       # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    OKCancelHelp(helpSubject = "readXL")
    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")

    tkgrid(label_rcmdr(
        top,
        text = gettextRcmdr("Import data from Excel file"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col
    ),
    pady = c(5, 9)
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame, sticky = "e")

    tkgrid(upper_l_frame, upper_r_frame)

    # upper_l_frame ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    on_click <- function() {select_xl_file()}

    button_ch_file <- tk2button(upper_l_frame,
                                text = "Choose file",
                                command = on_click,
                                cursor = "hand2")
    tkgrid(button_ch_file, sticky = "e", pady = c(0, 0))
    # file_label <- label_rcmdr(upper_l_frame, text = "File: ",  fg = fg_col)
    # tkgrid(file_label,
    #        sticky = "e", pady = c(0, 5))
    tkgrid(label_rcmdr(upper_l_frame, text = "Sheet: ", fg = fg_col),
           sticky = "e")
    tkgrid(label_rcmdr(upper_l_frame, text = "Name: ",  fg = fg_col),
           sticky = "e", pady = 5)

    # upper_r_frame ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fname_frame <- tkframe(upper_r_frame)
    fname_label <- label_rcmdr(fname_frame, text = xl_file)
    tkgrid(fname_frame, sticky = "ew", pady = c(5, 5))

    tkgrid(fname_label, sticky = "w")
    tkgrid(getFrame(worksheet_box), sticky = "w")
    tkgrid(entryDsname, sticky = "w", pady = 5)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(variableNamesCheckBox,
           labelRcmdr(checkBoxFrame,
                      text = gettextRcmdr("Variable names in first row of spreadsheet")
           ),
           sticky = "w"
    )
    tkgrid(rowNamesCheckBox,
           labelRcmdr(checkBoxFrame,
                      text = gettextRcmdr("Row names in first column of spreadsheet")
           ),
           sticky = "w"
    )
    tkgrid(stringsAsFactorsCheckBox,
           labelRcmdr(checkBoxFrame,
                      text = gettextRcmdr("Convert character data to factors")
           ),
           sticky = "w"
    )
    tkgrid(labelRcmdr(missingFrame,
                      text = gettextRcmdr("Missing data indicator:")),
           missingEntry,
           sticky = "w"
    )
    tkgrid(checkBoxFrame, sticky = "w")
    tkgrid(missingFrame,  sticky = "w")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame,  sticky = "w")
    dialogSuffix(focus = entryDsname)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add interactivity for `fname_frame` and `fname_label`
    # tkbind(file_label,     "<ButtonPress-1>", on_click)
    tkbind(fname_frame,    "<ButtonPress-1>", on_click)
    tkbind(fname_label,    "<ButtonPress-1>", on_click)

    tkbind(fname_frame, "<Enter>",
           function() tkconfigure(fname_label, foreground = "blue"))
    tkbind(fname_frame, "<Leave>",
           function() tkconfigure(fname_label, foreground = "black"))
    # tkconfigure(file_label,     cursor = "hand2")
    tkconfigure(fname_frame,    cursor = "hand2")
    # tkconfigure(button_ch_file, cursor = "hand2")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
# ============================================================================
