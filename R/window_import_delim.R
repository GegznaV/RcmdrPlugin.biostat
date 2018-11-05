#' ===========================================================================
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Based on function from "Rcmdr"

window_import_delim <- function() {
    initializeDialog(title =
                         gettextRcmdr("Read Text Data From File, Clipboard, or URL"))
    optionsFrame <- tkframe(top)
    dsname <- tclVar(unique_df_name("new_dataset", all_numbered = TRUE))
    entryDsname <- ttkentry(optionsFrame, width = "20", textvariable = dsname)
    radioButtons(optionsFrame, "location",
                 buttons = c(
                     "local",
                     "clipboard", "url"
                 ),
                 labels = gettextRcmdr(c(
                     "Local file system",
                     "Clipboard", "Internet URL"
                 )),
                 title = gettextRcmdr("Location of Data File"))
    headerVariable <- tclVar("1")
    headerCheckBox <- ttkcheckbutton(optionsFrame, variable = headerVariable)
    radioButtons(optionsFrame, "delimiter",
                 buttons = c("whitespace", "commas", "semicolons", "tabs"),
                 labels = gettextRcmdr(c(
                     "White space", "Commas [,]", "Semicolons [;]", "Tabs"
                 )),
                 title = gettextRcmdr("Field Separator"),
                 columns = 2
    )
    otherDelimiterFrame <- tkframe(optionsFrame)
    otherButton <- ttkradiobutton(otherDelimiterFrame,
                                  variable = delimiterVariable,
                                  value = "other", text = gettextRcmdr("Other")
    )
    otherVariable <- tclVar("")
    otherEntry <- ttkentry(otherDelimiterFrame,
                           width = "4",
                           textvariable = otherVariable
    )
    radioButtons(optionsFrame, "decimal",
                 title = gettextRcmdr("Decimal-Point Character"),
                 buttons = c("period","comma"),
                 labels = gettextRcmdr(c("Period [.]", "Comma [,]"))
    )
    missingVariable <- tclVar("NA")
    missingEntry <- ttkentry(optionsFrame,
                             width = "8",
                             textvariable = missingVariable)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        setBusyCursor()
        on.exit(setIdleCursor())
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall = readDataSet,
                           message = gettextRcmdr("You must enter a name for the data set."))
            return()
        }
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall = readDataSet,
                           message = paste("\"", dsnameValue, "\" ",
                                           gettextRcmdr("is not a valid name."),
                                           sep = ""
            ))
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue,
                                              gettextRcmdr("Data set")))) {
                readDataSet()
                return()
            }
        }

        location <- tclvalue(locationVariable)

        file <- if (location == "clipboard") {
            "clipboard"
        } else if (location == "local") {
            tclvalue(tkgetOpenFile(
                title = "Choose Text File to Import",
                filetypes = gettextRcmdr(
                "{\"Text Files\" {\".txt\" \".TXT\" \".dat\" \".DAT\" \".csv\" \".CSV\"}} {\"All Files\" {\"*\"}}")))
        } else {
            initializeDialog(subdialog, title = gettextRcmdr("Internet URL"))
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            onOKsub <- function() {
                closeDialog(subdialog)
            }
            urlFrame <- tkframe(subdialog)
            urlVar <- tclVar("")
            url <- ttkentry(urlFrame,
                            font = getRcmdr("logFont"),
                            width = "30", textvariable = urlVar
            )
            urlXscroll <- ttkscrollbar(urlFrame,
                                       orient = "horizontal",
                                       command = function(...) tkxview(url, ...)
            )
            tkconfigure(url, xscrollcommand = function(...) tkset(
                urlXscroll,
                ...
            ))
            .subexit <- function() tclvalue(urlVar) <- ""
            subOKCancelHelp()
            tkgrid(url, sticky = "w")
            tkgrid(urlXscroll, sticky = "ew")
            tkgrid(urlFrame, sticky = "nw")
            tkgrid(subButtonsFrame, sticky = "w")
            dialogSuffix(subdialog,
                         focus = url, onOK = onOKsub,
                         force.wait = TRUE
            )
            tclvalue(urlVar)
        }
        if (file == "") {
            if (getRcmdr("grab.focus")) {
                tkgrab.release(top)
            }
            tkdestroy(top)
            return()
        }
        head <- tclvalue(headerVariable) == "1"

        delimiter <- tclvalue(delimiterVariable)
        del <- switch(delimiter,
                      "whitespace" = "",
                      "commas"     = ",",
                      "semicolons" = ";",
                      "tabs"       = "\\t",
                      tclvalue(otherVariable)
                      )
        miss <- tclvalue(missingVariable)
        dec <-
            if (tclvalue(decimalVariable) == "period") {
                "."
            } else {
                ","
            }
        command <-
            str_glue('{dsnameValue} <- read.table("{file}", \n',
                     ' header = {head}, sep = "{del}",',
                     ' na.strings = "{miss}", dec = "{dec}",',
                     ' strip.white = TRUE)') %>%
            style_cmd()

        logger(command)
        result <- justDoIt(command)

        if (class(result)[1] != "try-error") {
            # gassign(dsnameValue, result)
            activeDataSet(dsnameValue)
        }
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "read.table")
    tkgrid(labelRcmdr(optionsFrame,
                      text = gettextRcmdr("Enter name for data set:")),
           entryDsname,
           sticky = "w"
    )
    tkgrid(labelRcmdr(optionsFrame, text = gettextRcmdr("Variable names in file:")),
           headerCheckBox,
           sticky = "w"
    )
    tkgrid(labelRcmdr(optionsFrame, text = gettextRcmdr("Missing data indicator:")),
           missingEntry,
           sticky = "w"
    )
    tkgrid(locationFrame, sticky = "w")
    tkgrid(otherButton, labelRcmdr(otherDelimiterFrame, text = gettextRcmdr("    Specify:")),
           otherEntry,
           sticky = "ew", padx = "3"
    )
    tkgrid(delimiterFrame, sticky = "nw", columnspan = 2)
    tkgrid(otherDelimiterFrame, sticky = "w")
    tkgrid(decimalFrame, sticky = "w")
    tkgrid(optionsFrame, sticky = "w")
    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix()
}
