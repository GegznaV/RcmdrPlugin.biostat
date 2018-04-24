# TODO:
# 1. Add radiobuttons instead of "Summary as data frame" checkbox:
#   values: Print summary as
#     - "n-way table"
#     - "n-way table (flat)"     (ftable)
#     - "n-way table for report" (pander)
#     - "data frame"
#     - "data frame for report" (pander)
# 2. Create count variable/environment for each type of summary separately.
# 3. Add counts variable name entrance field.
#    Default to "n", "nn", "nnn", etc. Now default is "Freq"
# 4. Radiobuttons:
#    Keep summary object as:
#        1. data frame;
#        2. n-way table;
#        3. print results and remove the object
# 5. Inactivate name field if "Keep summary" is unchecked.
# 6. Create separate window to plot the results. There may be a separate
#    checkbox that automatically opens necessary plotting window:
#    - plot as bar plot;
#    - plot as mosaic plot;
# 7. Possibility to sort variables.
# 8. Add "vcd::assocstats()"

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_summary_count <- function() {
    # cur_env <- environment()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Default values ---------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        initial_y_var = NULL,
        # initial_gr_var = NULL,
        # initial_digits = "NA",
        initial_as_df = TRUE,
        initial_keep_model = FALSE
    )

    dialog_values <- getDialog("window_summary_count", defaults)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Dialog elements --------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # tabs =      c("dataTab", "optionsTab")
    # tab_names = c(" Data ",  " Options ")

    initializeDialog(title = gettextRcmdr("Frequency table (for categorical data)"))

    # posthocFrame <- tkframe(posthocTab)
    # plotsFrame   <- tkframe(plotsTab)

    main_frame <- tkframe(top)

    # ** Data tab ------------------------------------------------------------
    # . Variable selection -----------------------------------------------------


    dataFrame <- tkframe(main_frame)
    yBox <- variableListBox2(
        dataFrame,
        Factors(),
        selectmode = "multiple",
        listHeight = 7,
        title = gettextRcmdr("Variables\n(pick one or several)"),
        initialSelection = varPosn(dialog_values$initial_y_var, "numeric")
    )

    # groupBox <- variableListBox2(
    #     dataFrame,
    #     selectmode = "multiple",
    #     Factors(),
    #     listHeight = 6,
    #     title = gettextRcmdr("Group variable \n(pick one, several or none)"),
    #     initialSelection = varPosn(dialog_values$initial_gr_var, "factor"))
    #
    # tkgrid(
    #     getFrame(yBox),
    #     labelRcmdr(dataFrame, text = "        "), # Vertical space
    #     getFrame(groupBox),
    #     sticky = "nw", pady = c(5, 5)
    # )
    tkgrid(
        getFrame(yBox),
        sticky = "nw", pady = c(5, 5), padx = c(0, 10)
    )


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ** Main tab ------------------------------------------------------------
    # . Main test & model name textbox ---------------------------------------

    main_top_frame <- tkframe(main_frame)

    # Choose model name ------------------------------------------------------
    UpdateModelNumber()

    modelName <- tclVar(paste0(activeDataSet(),"_freq_table_", getRcmdr("modelNumber")))
    model_boxlFrame <- tkframe(main_top_frame)
    model <- ttkentry(model_boxlFrame, width = "20", textvariable = modelName)


    bs_check_boxes(model_boxlFrame,
                   # ttk = TRUE,
                   frame = "keep_model_Frame",
                   # title = "Plot options",
                   boxes = c("as_df", "keep_model"),
                   initialValues = c(dialog_values$initial_as_df,
                                     dialog_values$initial_keep_model),
                   labels = gettextRcmdr(
                       c("Summary as data frame", "Keep summary")
                   ),
                   commands = list("as_df" = function(){},
                                   "keep_model" = function(){})
    )

    tkgrid(keep_model_Frame, sticky = "ew")
    tkgrid(labelRcmdr(model_boxlFrame,
                      text = gettextRcmdr("Enter name for summary: "),
                      fg = Rcmdr::getRcmdr("title.color")),   sticky = "w")

    tkgrid(model, sticky = "ew")

    # tkgrid(as_df_Frame, sticky = "ew")

    tkgrid(model_boxlFrame, sticky = "nw")


    # digitsVar <- tclVar(dialog_values$initial_digits)
    #
    # digitsVarFrame <- tkframe(main_top_frame)
    # digitsBox      <- ttkentry(digitsVarFrame, width = "20", textvariable = digitsVar)
    #
    # tkgrid(labelRcmdr(digitsVarFrame,
    #                   text = gettextRcmdr("Decimal digits to round to:\n(either integer or NA)"),
    #                   fg = Rcmdr::getRcmdr("title.color")),   sticky = "w")
    #
    # tkgrid(digitsBox, sticky = "ew")
    # tkgrid(digitsVarFrame, sticky = "nw")


    # tkgrid(main_top_frame, sticky = "nw")

    tkgrid(dataFrame, main_top_frame, columnspan = 2, sticky = "sw")
    tkgrid(main_frame, sticky = "w")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {

        # gr_var         <- getSelection(groupBox)
        y_var            <- getSelection(yBox)
        # digits         <- suppressWarnings(tclvalue_int(digitsVar))

        model_name_Value <- trim.blanks(tclvalue(modelName))
        keep_model       <- tclvalue_lgl(keep_modelVariable)
        as_df            <- tclvalue_lgl(as_dfVariable)

        if (!is.valid.name(model_name_Value)) {
            UpdateModelNumber(-1)
            errorCondition(recall = window_summary_count,
                           message = sprintf(gettextRcmdr("\"%s\" is not a valid name."),
                                             model_name_Value))
            return()
        }

        if (is.element(model_name_Value, list_summaries_Models())) {
            if ("no" == tclvalue(checkReplace(model_name_Value,
                                              type = gettextRcmdr("Model")))) {
                UpdateModelNumber(-1)
                tkdestroy(top)
                window_summary_count()
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if (length(y_var) == 0) {
            errorCondition(
                recall = window_summary_count,
                message = gettextRcmdr("You must select a variable to summarize.")
            )
            return()
        }

        putDialog("window_summary_count",
                  list(initial_y_var  = y_var,
                       # initial_gr_var = gr_var,
                       # initial_digits = as.character(digits),
                       initial_as_df = as_df,
                       initial_keep_model = keep_model
                  )
        )

        # calculations -------------------------------------------------------
        .activeDataSet <- ActiveDataSet()
        Library(c("tidyverse", "biostat"))

        if (length(y_var) > 1) {
            y_var <- paste0(y_var, collapse = ", ")
        }
        # For many groups ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # if (length(gr_var) > 1) {
        #     gr_var <- paste0(gr_var, collapse = " + ")
        # }

        # if (length(gr_var) == 0) {
        #     formula = glue("~{y_var}")
        #
        # } else {
        #     formula = glue("{y_var} ~ {gr_var}")
        #
        # }


        if (keep_model) {
            keep_model_command <- ""

        } else {
            UpdateModelNumber(-1)
            keep_model_command <- glue("remove({model_name_Value})")
        }

        if (as_df == TRUE) {
            as_df_command <- ' %>% \n as.data.frame(responseName = "Freq")\n'

        } else {
            as_df_command <- "\n"
        }

        command <- style_cmd(glue(
            "{model_name_Value} <- {.activeDataSet} %>% \n",
            'with(table({y_var}, useNA = "ifany"))',
            as_df_command,
            "print({model_name_Value})\n",
            keep_model_command))

        doItAndPrint(command)

        # Post calculations --------------------------------------------------
        # activeModel(model_name_Value)
        # putRcmdr("modelWithSubset", FALSE)

        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ** Footer ------------------------------------------------------------------
    # OKCancelHelp()
    OKCancelHelp(
        helpSubject = "do_summary",
        model = TRUE,
        reset = "window_summary_count",
        apply = "window_summary_count"
    )
    tkgrid(buttonsFrame, sticky = "ew", columnspan = 2, pady = c(10, 0))

    dialogSuffix(rows = 4,
                 columns = 2,
                 preventGrabFocus = TRUE)
    # dialogSuffix(rows = 1,
    #              columns = 2,
    #              preventGrabFocus = TRUE)
}
# ==============================================================================

