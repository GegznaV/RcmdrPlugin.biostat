# TODO:
#
# 1. Add checkbox "All variables" (for y_var)
# 2. Add checkboxes to choose/deselect summaries:
#       Include these summaries:
#       n missing unique
#       mean trimmed_mean sd variance cv
#       median mad IQR range
#       min q.05 q.25 q.75 q.95 max
#       skewness kurtosis
#       se
#
# 3. Add posibibity to use parameter to trim mean (percentage of trimmed points),
#  choose skewness (type of skewness), etc.

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_summary_skim <- function() {
    cur_env <- environment()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Default values ---------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        initial.y_var      = NULL,
        initial.gr_var     = NULL,
        # initial.digits     = "NA",
        initial.keep_model = FALSE
    )

    dialog_values <- getDialog("window_summary_skim", defaults)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Dialog elements --------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tabs =      c("dataTab", "optionsTab")
    tab_names = c(" Data ",  " Options ")

    initializeDialog(title = gettextRcmdr("Skim: Summarize Variables")
                     # , use.tabs = TRUE, tabs = tabs
                     )

    # posthocFrame <- tkframe(posthocTab)
    # plotsFrame <- tkframe(plotsTab)

    # ** Data tab ------------------------------------------------------------
    # . Variable selection -----------------------------------------------------

    dataTab <- tkframe(top)

    dataFrame <- tkframe(dataTab)
    yBox <- variableListBox2(
        dataFrame,
        Numeric(),
        selectmode = "multiple",
        listHeight = 8,
        title = gettextRcmdr("Variable(s) to summarize \n(pick one, several or none)"),
        # initialSelection = varPosn(dialog_values$initial.y_var, "numeric")
        initialSelection = var_pos_n(dialog_values$initial.y_var, "numeric")
    )

    groupBox <- variableListBox2(
        dataFrame,
        selectmode = "multiple",
        Variables(), # Factors(),
        listHeight = 8,
        title = gettextRcmdr("Grouping variable(s) \n(pick one, several or none)"),
        initialSelection = var_pos_n(dialog_values$initial.gr_var))
        # initialSelection = varPosn(dialog_values$initial.gr_var, "factor"))

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

    optionsTab <- tkframe(top)

    main_top_frame <- tkframe(optionsTab)

    labelText <- tclVar("Select the test") ### [!!!] Initial value

    # Choose model name ------------------------------------------------------
    # UpdateModelNumber()

    modelName  <- tclVar(unique_obj_names(suffix = "_summary", all_numbered = TRUE))
    model_box_frame <- tkframe(main_top_frame)
    model_name_box <- ttkentry(model_box_frame, width = "30", textvariable = modelName)

    keep_model_frame <- tkframe(model_box_frame)
    bs_check_boxes(keep_model_frame,
                   # ttk = TRUE,
                   frame = "keep_model_inner_frame",
                   # title = "Plot options",
                   boxes = c("keep_model"),
                   initialValues = c(dialog_values$initial.keep_model),
                   labels = gettextRcmdr(
                       c("Keep summary in R memory")
                   ),
                   commands = list("keep_model" = function(){})
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(dataTab, sticky = "w")
    tkgrid(optionsTab, pady = c(10, 0), sticky = "w")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(labelRcmdr(model_box_frame,
                      text = gettextRcmdr("Enter name for summary table: "),
                      fg = Rcmdr::getRcmdr("title.color")),   sticky = "w")

    tkgrid(model_name_box, keep_model_frame, sticky = "ew")
    tkgrid(keep_model_inner_frame, padx = c(10, 0))
    tkgrid(model_box_frame, sticky = "nw")

    # digitsVar <- tclVar(dialog_values$initial.digits)

    # digitsVarFrame <- tkframe(main_top_frame)
    # digitsBox      <- ttkentry(digitsVarFrame, width = "30", textvariable = digitsVar)

    # tkgrid(labelRcmdr(digitsVarFrame,
    #                   text = gettextRcmdr("Number of decimal digits to print:\n(either integer or NA)"),
    #                   fg = Rcmdr::getRcmdr("title.color")),
    #        sticky = "w",
    #        pady = c(10, 0))
    #
    # tkgrid(digitsBox, sticky = "ew")
    # tkgrid(digitsVarFrame, sticky = "nw")

    tkgrid(main_top_frame, sticky = "nw")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {


        gr_var <- getSelection(groupBox)
        y_var  <- getSelection(yBox)
        # digits <- suppressWarnings(tclvalue_int(digitsVar))
        # names(digits) <- NULL
        model_name_Value <- trim.blanks(tclvalue(modelName))
        keep_model <- tclvalue_lgl(keep_modelVariable)


        closeDialog()


        if (!is.valid.name(model_name_Value)) {
            # UpdateModelNumber(-1)
            errorCondition(recall = window_summary_skim,
                           message = sprintf(gettextRcmdr("\"%s\" is not a valid name."),
                                             model_name_Value))
            return()
        }

        if (is.element(model_name_Value, list_summaries_Models())) {
            if ("no" == tclvalue(checkReplace(model_name_Value,
                                              type = gettextRcmdr("Model")))) {
                # UpdateModelNumber(-1)
                tkdestroy(top)
                window_summary_skim()
                return()
            }
        }


        # if (length(y_var) == 0) {
        #     errorCondition(
        #         recall = window_summary_skim,
        #         message = gettextRcmdr("You must select a variable to summarize.")
        #     )
        #     return()
        # }

        putDialog("window_summary_skim",
                  list(initial.y_var  = y_var,
                       initial.gr_var = gr_var,
                       # initial.digits = as.character(digits),
                       initial.keep_model = keep_model
                  )
        )

        # calculations -------------------------------------------------------
        .activeDataSet <- ActiveDataSet()
        Library("skimr")

        if (length(y_var) > 1) {
            y_var <- paste0(y_var, collapse = ", ")

        } else if (length(y_var) == 0) {
            y_var <- ""
        }

        # For many groups ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(gr_var) > 1) {
            gr_var <- paste0(gr_var, collapse = ", ")

        }

        if (length(gr_var) == 0) {
            grouping <- ""

        } else {
            grouping <- glue("\n group_by({gr_var}) %>% \n")

        }


        if (keep_model) {
            keep_model_command <- ""

        } else {
            # UpdateModelNumber(-1)
            keep_model_command <- glue("remove({model_name_Value})")
        }

        command <- style_cmd(glue(
            "skimr::skim_with(numeric = list(hist = NULL)) \n",
            "{model_name_Value} <- \n",
            "{.activeDataSet} %>% {grouping} skimr::skim({y_var})\n",
            "summary({model_name_Value})\n",
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
    OKCancelHelp(
        helpSubject = "skim",
        helpPackage = "skimr",
        model = TRUE,
        reset = "window_summary_skim",
        apply = "window_summary_skim"
    )
    tkgrid(buttonsFrame, sticky = "w")

    dialogSuffix(
        # use.tabs = TRUE, grid.buttons = TRUE
        # , tabs = tabs,
        # tab.names = tab_names
                 )
}
# ==============================================================================

