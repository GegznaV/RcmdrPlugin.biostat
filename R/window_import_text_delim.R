#' ===========================================================================
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Based on function from "Rcmdr"
window_import_text_delim <- function() {
    window_import_text_delim0()
}



#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Based on function from "Rcmdr"

window_import_text_delim0 <- function(init_location = "local") {

    initializeDialog(title = gettext_bs("Read Text Data From File, Clipboard, or URL"))
    tk_title(top, text = gettext_bs("Import data from Text"))

    optionsFrame <- tkframe(top)
    dsname <- tclVar(unique_df_name("new_dataset", all_numbered = TRUE))
    entryDsname <- ttkentry(optionsFrame, width = "20", textvariable = dsname)
    radioButtons(
        optionsFrame,
        name = "location",
        initialValue = init_location,
        buttons = c(
            "local",
            "clipboard",
            "url"
        ),
        labels = gettext_bs(c(
            "Local file system",
            "Clipboard",
            "Internet URL"
        )),
        title = gettext_bs("Location of Data File"))
    headerVariable <- tclVar("1")
    headerCheckBox <- ttkcheckbutton(optionsFrame, variable = headerVariable)
    radioButtons(
        optionsFrame,
        name = "delimiter",
        initialValue = "commas",
        buttons = c("whitespace", "commas", "semicolons", "tabs"),
        labels = gettext_bs(c(
            "White space", "Commas [,]", "Semicolons [;]", "Tabs"
        )),
        title = gettext_bs("Field Separator"),
        columns = 2
    )
    otherDelimiterFrame <- tkframe(optionsFrame)
    otherButton <- ttkradiobutton(
        otherDelimiterFrame,
        variable = delimiterVariable,
        value = "other", text = gettext_bs("Other")
    )
    otherVariable <- tclVar("")
    otherEntry <- ttkentry(
        otherDelimiterFrame,
        width = "4",
        textvariable = otherVariable
    )
    radioButtons(
        optionsFrame, "decimal",
        title = gettext_bs("Decimal-Point Character"),
        buttons = c("period","comma"),
        labels = gettext_bs(c("Period [.]", "Comma [,]"))
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
            errorCondition(recall = window_import_text_delim,
                           message = gettext_bs("You must enter a name for the data set."))
            return()
        }
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall = window_import_text_delim,
                           message = paste0("\"", dsnameValue, "\" ",
                                           gettext_bs("is not a valid name.")
                           ))
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue,
                                              gettext_bs("Data set")))) {
                window_import_text_delim()
                return()
            }
        }

        location <- tclvalue(locationVariable)

        file <- if (location == "clipboard") {
            "clipboard"
        } else if (location == "local") {

            tclvalue(tkgetOpenFile(
                title = "Choose Text File to Import",
                filetypes = gettext_bs(
                    "{{Text Files} {.csv .dat .txt}}
                    {{All Files} *}")))

        } else {

            initializeDialog(subdialog, title = gettext_bs("Internet URL"))
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            onOKsub <- function() {
                closeDialog(subdialog)
            }
            urlFrame <- tkframe(subdialog)
            urlVar <- tclVar("")
            url <- ttkentry(urlFrame,
                            font = getRcmdr("logFont"),
                            width = "30",
                            textvariable = urlVar
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
            if (tclvalue(decimalVariable) == "period") {"."} else {","}

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
                      text = gettext_bs("Enter name for data set:")),
           entryDsname,
           sticky = "w"
    )
    tkgrid(labelRcmdr(optionsFrame, text = gettext_bs("Variable names in file:")),
           headerCheckBox,
           sticky = "w"
    )
    tkgrid(labelRcmdr(optionsFrame, text = gettext_bs("Missing data indicator:")),
           missingEntry,
           sticky = "w"
    )
    tkgrid(locationFrame, sticky = "w")
    tkgrid(otherButton, labelRcmdr(otherDelimiterFrame, text = gettext_bs("    Specify:")),
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
