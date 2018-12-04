#' @rdname Menu-window-functions
#' @export
#' @keywords internal


window_model_select <- function() {
    models <- listAllModels()
    .activeModel <- ActiveModel()

    # if ((length(models) == 1) && !is.null(.activeModel)) {
    #     Message(message = gettextRcmdr("There is only one model in memory."),
    #             type = "warning")
    #     tkfocus(CommanderWindow())
    #     return()
    # }

    if (length(models) == 0) {
        Message(
            message = gettextRcmdr("There are no models from which to choose."),
            type = "error"
        )
        tkfocus(CommanderWindow())
        return()
    }

    initializeDialog(title = gettextRcmdr("Select Model"))
    tk_title(top, "Select a model")

    .ds <- ActiveDataSet()

    initial <-
        if (is.null(.activeModel)) {
            NULL
        } else {
            which(.activeModel == models) - 1
        }

    modelsBox <- variableListBox2(
        top,
        models,
        listHeight       = 8,
        listWidth        = c(26, Inf),
        title = gettextRcmdr("Models (pick one)"),
        initialSelection = initial)

    # ========================================================================
    onOK <- function() {

        model <- getSelection(modelsBox)

        closeDialog()

        if (length(model) == 0) {
            tkfocus(CommanderWindow())
            return()
        }

        dataSet <- as.character(get(model)$call$data)
        if (length(dataSet) == 0) {
            errorCondition(message = gettextRcmdr(
                "There is no dataset associated with this model."))
            return()
        }

        dataSets <- listDataSets()
        if (!is.element(dataSet, dataSets)) {
            errorCondition(
                message = sprintf(
                    gettextRcmdr(
                        "The dataset associated with this model, %s, is not in memory."
                    ),
                    dataSet
                ))
            return()
        }

        if (is.null(.ds) || (dataSet != .ds))
            activeDataSet(dataSet)

        putRcmdr("modelWithSubset", "subset" %in% names(get(model)$call))
        activeModel(model)
        tkfocus(CommanderWindow())
    }

    OKCancelHelp()
    tkgrid(getFrame(modelsBox),
           columnspan = "2",
           sticky = "w")

    nameFrame <- tkframe(top)
    tkgrid(
        labelRcmdr(
            nameFrame,
            fg = getRcmdr("title.color"),
            font = "RcmdrTitleFont",
            text = gettextRcmdr("Current Model: ")
        ),
        labelRcmdr(nameFrame, text = tclvalue(getRcmdr("modelName"))),
        sticky = "w"
    )

    tkgrid(nameFrame, sticky = "w", pady = c(8, 2))

    tkgrid(buttonsFrame, sticky = "w")

    dialogSuffix()
}
