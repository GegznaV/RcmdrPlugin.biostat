
#' @rdname Helper-functions
#' @export
#' @keywords internal
command_dataset_button_rcmdr <- function() {

        tkconfigure(getRcmdr("dataSetLabel"),
                    # foreground = "darkred",
                    image = "::image::dataIcon",
                    compound = "left",
                    command  = selectActiveDataSet)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
command_dataset_button_biostat <- function() {
    tkconfigure(getRcmdr("dataSetLabel"),
                # foreground = "darkred",
                image = "::image::bs_dataset",
                compound = "left",
                command = window_dataset_select)
}


#' @rdname Helper-functions
#' @export
#' @keywords internal
# Function copied from Rcmdr
selectActiveDataSet <- function() {
    dataSets <- listDataSets()
    .activeDataSet <- ActiveDataSet()
    if ((length(dataSets) == 1) && !is.null(.activeDataSet)) {
        Message(
            message = gettextRcmdr("There is only one dataset in memory."),
            type = "warning"
        )
        tkfocus(CommanderWindow())
        return()
    }
    if (length(dataSets) == 0) {
        Message(
            message = gettextRcmdr("There are no data sets from which to choose."),
            type = "error"
        )
        tkfocus(CommanderWindow())
        return()
    }
    initializeDialog(title = gettextRcmdr("Select Data Set"))
    dataSetsBox <- variableListBox(top, dataSets,
                                   title = gettextRcmdr("Data Sets (pick one)"),
                                   initialSelection = if (is.null(.activeDataSet)) NULL else which(.activeDataSet == dataSets) - 1
    )
    onOK <- function() {
        selection <- getSelection(dataSetsBox)
        closeDialog()
        setBusyCursor()
        on.exit(setIdleCursor())
        activeDataSet(selection)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp()
    tkgrid(getFrame(dataSetsBox), sticky = "nw")
    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix()
}