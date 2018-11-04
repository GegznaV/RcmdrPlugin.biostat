#' ===========================================================================
#' TODO:
#' 1) Better management if several missing values are used.
#'
#'
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Based on function from "Rcmdr"

window_import_excel <- function() {

    initial_dir_xl <- getwd()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    File <- tclvalue(tkgetOpenFile(

        filetypes = gettextRcmdr("{\"MS Excel file\" {\".xlsx\" \".XLSX\" \".xls\" \".XLS\"}} {\"All Files\" {\"*\"}}"),
        parent = CommanderWindow(),
        initialdir = initial_dir_xl
    ))

    if (File == "") {
        tkfocus(CommanderWindow())
        return()
    }

    worksheets <- excel_sheets(File)

    # if (length(worksheets) > 1) {
    #     worksheet <- tk_select.list(worksheets, title = gettextRcmdr("Select one table"))
    # } else {
    #     worksheet <- worksheets
    # }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  initializeDialog(title = gettextRcmdr("Import Excel Data Set"))
  # dsname <- tclVar(unique_df_name("new_dataset", all_numbered = TRUE))
  dsname <- tclVar(unique_df_name(clean_str(extract_filename(File))))
  dsnameFrame <- tkframe(top)
  entryDsname <- ttkentry(dsnameFrame, width = "35", textvariable = dsname)
  checkBoxFrame <- tkframe(top)
  variableNames <- tclVar("1")
  variableNamesCheckBox <- ttkcheckbutton(checkBoxFrame, variable = variableNames)
  rowNames <- tclVar("0")
  rowNamesCheckBox <- ttkcheckbutton(checkBoxFrame, variable = rowNames)
  stringsAsFactors <- tclVar("1")
  stringsAsFactorsCheckBox <-
      ttkcheckbutton(checkBoxFrame,
                     variable = stringsAsFactors
      )
  missingFrame <- tkframe(top)
  missingVariable <- tclVar(gettextRcmdr("<empty cell>"))
  missingEntry <- ttkentry(missingFrame, width = "15", textvariable = missingVariable)

  if (length(worksheets) == 1) {
      # c(worksheets, "") prevents from a bug
      # which appears when an excel workbook
      # contains only one sheet and the name
      # of that sheet contains a speace, e.g., "a b".
      worksheets <- c(worksheets, "")
  }

  worksheetFrame <- tkframe(top)
  worksheet_box  <- inputComboBox(worksheetFrame,
                                  variableList     = worksheets,
                                  initialSelection = worksheets[1],
                                  onSelect_fun     = function() {
                                      if (tclvalue(worksheet_box$combovar) == "")
                                          tclvalue(worksheet_box$combovar) <- worksheets[1]
                                  },
                                  width = 32)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onOK <- function() {
    closeDialog()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    setBusyCursor()
    on.exit(setIdleCursor())
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dsnameValue           <- trim.blanks(tclvalue(dsname))
    variableNamesValue    <- tclvalue(variableNames)
    rowNamesValue         <- tclvalue(rowNames)
    stringsAsFactorsValue <- tclvalue(stringsAsFactors)
    missingValues         <- as.character(tclvalue(missingVariable))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (missingValues == gettextRcmdr("<empty cell>")) {
      missingValues <- ""
    }
    if (dsnameValue == "") {
      errorCondition(recall = importExcel,
                     message = gettextRcmdr("You must enter the name of a data set."))
      return()
    }
    if (!is.valid.name(dsnameValue)) {
      errorCondition(recall = importExcel,
                     message = paste0("\"", dsnameValue, "\" ", gettextRcmdr("is not a valid name.")
      ))
      return()
    }
    if (is.element(dsnameValue, listDataSets())) {
      if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))) {
        importExcel()
        return()
      }
    }
    # File <- tclvalue(tkgetOpenFile(
    #   filetypes = gettextRcmdr("{\"All Files\" {\"*\"}} {\"MS Excel file\" {\".xlsx\" \".XLSX\" \".xls\" \".XLS\"}}"),
    #   parent = CommanderWindow()
    # ))
    # if (File == "") {
    #   tkfocus(CommanderWindow())
    #   return()
    # }
    # worksheets <- excel_sheets(File)
    # if (length(worksheets) > 1) {
    #   worksheet <- tk_select.list(worksheets, title = gettextRcmdr("Select one table"))
    # } else {
    #   worksheet <- worksheets
    # }
    # if (worksheet == "") {
    #   errorCondition(message = gettextRcmdr("No table selected"))
    #   return()
    # }

    worksheet <- getSelection(worksheet_box)

    if (worksheet == "") {
        errorCondition(message = gettextRcmdr("No table selected"))
        return()
    }


    rownames_str  <- if (rowNamesValue == "1")         {"TRUE"} else {"FALSE"}
    header_str    <- if (variableNamesValue == "1")    {"TRUE"} else {"FALSE"}
    as_factor_str <- if (stringsAsFactorsValue == "1") {"TRUE"} else {"FALSE"}

    # Library(readxl)

    command <- glue::glue(
        '## Read data from Excel file\n ',
        'readXL("{File}", \n ',
        ' sheet = "{worksheet}", ',
        ' rownames = {rownames_str}, header = {header_str},',
        ' na = "{missingValues}",',
        ' stringsAsFactors = {as_factor_str})')

    writeLines(style_cmd(glue::glue("{dsnameValue} <- \n {command}")))

    result <- justDoIt(command)
    logger(command)

    if (class(result)[1] != "try-error") {
      gassign(dsnameValue, result)
      activeDataSet(dsnameValue)
    }
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  OKCancelHelp(helpSubject = "readXL")
  tkgrid(labelRcmdr(dsnameFrame, text = gettextRcmdr("Enter name of data set: ")),
    entryDsname,
    sticky = "w"
  )
  tkgrid(dsnameFrame, sticky = "w")


  tkgrid(label_rcmdr(worksheetFrame, text = "Sheet: "),
         getFrame(worksheet_box),
         sticky = "w", pady = c(5, 5))

  tkgrid(worksheetFrame, sticky = "e")


  tkgrid(variableNamesCheckBox,
         labelRcmdr(checkBoxFrame,
                    text = gettextRcmdr("Variable names in first row of spreadsheet")),
    sticky = "w"
  )
  tkgrid(rowNamesCheckBox,
         labelRcmdr(checkBoxFrame,
                    text = gettextRcmdr("Row names in first column of spreadsheet")),
    sticky = "w"
  )
  tkgrid(stringsAsFactorsCheckBox, labelRcmdr(checkBoxFrame,
    text = gettextRcmdr("Convert character data to factors")
  ),
  sticky = "w"
  )
  tkgrid(labelRcmdr(missingFrame, text = gettextRcmdr("Missing data indicator:")),
    missingEntry,
    sticky = "w"
  )
  tkgrid(checkBoxFrame, sticky = "w")
  tkgrid(missingFrame, sticky = "w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(focus = entryDsname)
}
