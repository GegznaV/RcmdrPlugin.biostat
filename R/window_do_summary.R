#' @rdname Menu-window-functions
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
        initial.digits = "NA",
        initial.keep_model = FALSE
    )

    dialog.values <- getDialog("window_do_summary", defaults)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Dialog elements --------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tabs =      c("dataTab", "optionsTab")
    tab_names = c(" Data ",  " Options ")

    initializeDialog(title = gettextRcmdr("Do numerical summaries"),
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


    checkBoxes_cmd(model_boxlFrame,
                   # ttk = TRUE,
                   frame = "keep_model_Frame",
                   # title = "Plot options",
                   boxes = c("keep_model"),
                   initialValues = c(dialog.values$initial.keep_model),
                   labels = gettextRcmdr(
                       c("Keep summary as dataset")
                   ),
                   commands = list("keep_model" = function(){})
    )


    tkgrid(labelRcmdr(model_boxlFrame,
                      text = gettextRcmdr("Enter name for summary: "),
                      fg = Rcmdr::getRcmdr("title.color")),   sticky = "w")

    tkgrid(model, sticky = "ew")
    tkgrid(keep_model_Frame, sticky = "ew")

    tkgrid(model_boxlFrame, sticky = "nw")




    digitsVar <- tclVar(dialog.values$initial.digits)

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
        names(digits) <- NULL
        model_name_Value <- trim.blanks(tclvalue(modelName))
        keep_model <- tclvalue_lgl(keep_modelVariable)


        if (!is.valid.name(model_name_Value)) {
            UpdateModelNumber(-1)
            errorCondition(recall = window_do_summary,
                           message = sprintf(gettextRcmdr("\"%s\" is not a valid name."),
                                             model_name_Value))
            return()
        }

        if (is.element(model_name_Value, list_summaries_Models())) {
            if ("no" == tclvalue(checkReplace(model_name_Value,
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
                       initial.digits = as.character(digits),
                       initial.keep_model = keep_model
                  )
        )

        # calculations -------------------------------------------------------
        .activeDataSet <- ActiveDataSet()
        Library("biostat")

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


        if (keep_model) {
            keep_model_command <- ""

        } else {
            UpdateModelNumber(-1)
            keep_model_command <- glue("remove({model_name_Value})")
        }

        command <- style_cmd(glue(
            "{model_name_Value} <- do_summary({formula}, ",
            "data = {.activeDataSet})\n",
            "print({model_name_Value}, digits = {digits})\n",
            keep_model_command))

        doItAndPrint(command)

        # Post calculations --------------------------------------------------
        # activeModel(model_name_Value)
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

