#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_do_summary <- function() {

    cur_env <- environment()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Default values ---------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    defaults <- list(
        initial.y_var = NULL,
        initial.gr_var = NULL,
        initial.digits = "NA"
    )

    dialog.values <- getDialog("window_do_summary", defaults)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Dialog elements --------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tabs =      c("dataTab", "optionsTab")
    tab_names = c(" Data ",  " Options ")

    initializeDialog(title = gettextRcmdr("Summarize variables"),
                     use.tabs = TRUE, tabs = tabs)

    # posthocFrame <- tkframe(posthocTab)
    # plotsFrame <- tkframe(plotsTab)

    # ** Data tab ------------------------------------------------------------
    # . Variable selection -----------------------------------------------------

    dataFrame <- tkframe(dataTab)
    yBox <- variableListBox2(
        dataFrame,
        Numeric(),
        selectmode = "multiple",
        listHeight = 6,
        title = gettextRcmdr("Variable to summarize \n(pick one or several)"),
        initialSelection = varPosn(dialog.values$initial.y_var, "numeric")
    )

    groupBox <- variableListBox2(
        dataFrame,
        selectmode = "multiple",
        Factors(),
        listHeight = 6,
        title = gettextRcmdr("Group variable \n(pick one, several or none)"),
        initialSelection = varPosn(dialog.values$initial.gr_var, "factor"))


    tkgrid(
        getFrame(yBox),
        labelRcmdr(dataFrame, text = "        "), # Vertical space
        getFrame(groupBox),
        sticky = "nw", pady = c(5, 5)
    )
    tkgrid(dataFrame, sticky = "w")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ** Main tab ------------------------------------------------------------
    # . Main test & model name textbox ---------------------------------------

    main_top_frame <- tkframe(optionsTab)

    labelText <- tclVar("Select the test") ### [!!!] Initial value

    # Choose model name ------------------------------------------------------
    UpdateModelNumber()

    modelName  <- tclVar(paste0(activeDataSet(),"_summary_", getRcmdr("modelNumber")))
    model_boxlFrame <- tkframe(main_top_frame)
    model <- ttkentry(model_boxlFrame, width = "20", textvariable = modelName)

    tkgrid(labelRcmdr(model_boxlFrame,
                      text = gettextRcmdr("Enter name for summary: "),
                      fg = Rcmdr::getRcmdr("title.color")),   sticky = "w")

    tkgrid(model, sticky = "ew")

    tkgrid(model_boxlFrame, sticky = "nw")




    digitsVar <- tclVar(defaults$initial.digits)

    digitsVarFrame <- tkframe(main_top_frame)
    digitsBox      <- ttkentry(digitsVarFrame, width = "20", textvariable = digitsVar)

    tkgrid(labelRcmdr(digitsVarFrame,
                      text = gettextRcmdr("Decimal digits to round to:\n(either integer or NA)"),
                      fg = Rcmdr::getRcmdr("title.color")),   sticky = "w")

    tkgrid(digitsBox, sticky = "ew")
    tkgrid(digitsVarFrame, sticky = "nw")


    tkgrid(main_top_frame, sticky = "nw")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {

        gr_var <- getSelection(groupBox)
        y_var  <- getSelection(yBox)
        digits <- suppressWarnings(tclvalue_int(digitsVar))
        modelValue <- trim.blanks(tclvalue(modelName))

        if (!is.valid.name(modelValue)) {
            UpdateModelNumber(-1)
            errorCondition(recall = window_do_summary,
                           message = sprintf(gettextRcmdr("\"%s\" is not a valid name."),
                                             modelValue))
            return()
        }

        if (is.element(modelValue, list_summaries_Models())) {
            if ("no" == tclvalue(checkReplace(modelValue,
                                              type = gettextRcmdr("Model")))) {
                UpdateModelNumber(-1)
                tkdestroy(top)
                window_do_summary()
                return()
            }
        }

        closeDialog()



        if (length(y_var) == 0) {
            errorCondition(
                recall = window_do_summary,
                message = gettextRcmdr("You must select a variable to summarize.")
            )
            return()
        }


        putDialog("window_do_summary",
                  list(initial.y_var  = y_var,
                       initial.gr_var = gr_var,
                       initial.digits = NA
                  )
        )

        # calculations -------------------------------------------------------
        .activeDataSet <- ActiveDataSet()
        Library("BioStat")

        if (length(y_var) > 1) {
            y_var <- paste0(y_var, collapse = " + ")
        }
        # For many groups ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(gr_var) > 1) {
            gr_var <- paste0(gr_var, collapse = " + ")
        }

        if (length(gr_var) == 0) {
            formula = glue("~{y_var}")

        } else {
            formula = glue("{y_var} ~ {gr_var}")

        }

        command <- glue("{modelValue} <- do_summary({formula}, data = {.activeDataSet})\n",
                        "print({modelValue}, digits = {digits})")

        doItAndPrint(command)

        # Post calculations --------------------------------------------------
        # activeModel(modelValue)
        # putRcmdr("modelWithSubset", FALSE)

        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ** Footer ------------------------------------------------------------------
    OKCancelHelp(
        helpSubject = "do_summary",
        model = TRUE,
        reset = "window_do_summary",
        apply = "window_do_summary"
    )
    # tkgrid(buttonsFrame, sticky = "w")

    dialogSuffix(use.tabs = TRUE, grid.buttons = TRUE,
                 tabs = tabs,
                 tab.names = tab_names)
}



# ==============================================================================

