
#' @rdname Helper-functions
#' @export
#' @keywords internal
command_buttons_mode_rcmdr <- function() {

    tkconfigure(getRcmdr("dataSetLabel"),
                # foreground = "darkred",
                image = "::image::dataIcon",
                compound = "left",
                command  = selectActiveDataSet)

    tkconfigure(getRcmdr("modelLabel"),
                # foreground = "darkred",
                image = "::image::modelIcon",
                compound = "left",
                command  = selectActiveModel)

}

#' @rdname Helper-functions
#' @export
#' @keywords internal
command_buttons_mode_biostat <- function() {
    tkconfigure(getRcmdr("dataSetLabel"),
                # foreground = "darkred",
                image = "::image::bs_dataset",
                compound = "left",
                command = window_dataset_select)

    tkconfigure(getRcmdr("modelLabel"),
                # foreground = "darkred",
                image = "::image::bs_dataset",
                compound = "left",
                command = window_model_select)
}

# ============================================================================
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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
# Function copied from Rcmdr
selectActiveModel <- function() {
    models <- listAllModels()
    .activeModel <- ActiveModel()
    if ((length(models) == 1) && !is.null(.activeModel)) {
        Message(
            message = gettextRcmdr("There is only one model in memory."),
            type = "warning"
        )
        tkfocus(CommanderWindow())
        return()
    }
    if (length(models) == 0) {
        Message(
            message = gettextRcmdr("There are no models from which to choose."),
            type = "error"
        )
        tkfocus(CommanderWindow())
        return()
    }
    initializeDialog(title = gettextRcmdr("Select Model"))
    .activeDataSet <- ActiveDataSet()
    initial <- if (is.null(.activeModel)) NULL else which(.activeModel == models) - 1
    modelsBox <- variableListBox(top, models,
                                 title = gettextRcmdr("Models (pick one)"),
                                 initialSelection = initial
    )
    onOK <- function() {
        model <- getSelection(modelsBox)
        closeDialog()
        if (length(model) == 0) {
            tkfocus(CommanderWindow())
            return()
        }
        dataSet <- as.character(get(model)$call$data)
        if (length(dataSet) == 0) {
            errorCondition(message = gettextRcmdr("There is no dataset associated with this model."))
            return()
        }
        dataSets <- listDataSets()
        if (!is.element(dataSet, dataSets)) {
            errorCondition(message = sprintf(gettextRcmdr("The dataset associated with this model, %s, is not in memory."), dataSet))
            return()
        }
        if (is.null(.activeDataSet) || (dataSet != .activeDataSet)) activeDataSet(dataSet)
        putRcmdr("modelWithSubset", "subset" %in% names(get(model)$call))
        activeModel(model)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp()
    nameFrame <- tkframe(top)
    tkgrid(labelRcmdr(nameFrame, fg = getRcmdr("title.color"), font = "RcmdrTitleFont", text = gettextRcmdr("Current Model: ")),
           labelRcmdr(nameFrame, text = tclvalue(getRcmdr("modelName"))),
           sticky = "w"
    )
    tkgrid(nameFrame, sticky = "w", columnspan = "2")
    tkgrid(getFrame(modelsBox), columnspan = "2", sticky = "w")
    tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
    dialogSuffix()
}