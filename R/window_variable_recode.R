# Based on function from EZR

# TODO:
#
# 1. Add interactivity for double click.
#    a.  It should add all unique values to recodes window as
#        "old_val_1" = ""
#        "old_val_2" = ""
#        "old_val_3" = ""
#        ...
#
#    b. Change variable name to the name of selected variable +
#       tkfocus to variable name window.
#
# 2. Add menu for arguments .missing and .default.
#        # Add options if explicit level is needed:
#        .default = NULL
#        .missing = NULL
#
# 3. Add examples in the free area of the window.
#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_variable_recode <- function() {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_EZR("Recode Variable Values"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dataSet <- activeDataSet()
    defaults <-
        list(
            initial_make_factor = 1,
            initial_factor_type = "nominal",
            initial_variables = NULL,
            initial_name = "variable",
            initial_recode_directives = ""
        )

    dialog_values <- getDialog("window_variable_recode", defaults)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)

    variablesBox <-
        variableListBox2(
            listHeight = 7,
            upper_frame,
            Variables(),
            # selectmode = "multiple",
            title = gettext_EZR("Variable to recode \n(pick one, double-click)"),
            initialSelection = varPosn(dialog_values$initial_variables, "all")
        )


    # ------------------------------------------------------------------------
    recodesFrame <- tkframe(upper_frame)
    recodes <-
        tktext(
            recodesFrame,
            bg = "white",
            font = getRcmdr("logFont"),
            height = "7",
            width = "40",
            wrap = "none")

    recodesXscroll <-
        ttkscrollbar(
            recodesFrame,
            orient = "horizontal",
            command = function(...) tkxview(recodes, ...)
        )
    recodesYscroll <-
        ttkscrollbar(
            recodesFrame,
            command = function(...) tkyview(recodes, ...)
        )
    tkconfigure(
        recodes,
        xscrollcommand = function(...) tkset(recodesXscroll, ...)
    )
    tkconfigure(
        recodes,
        yscrollcommand = function(...) tkset(recodesYscroll, ...)
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkinsert(recodes, "1.0", dialog_values$initial_recode_directives)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    variablesFrame <- tkframe(top)
    newVariableName <- tclVar(dialog_values$initial_name)
    newVariable <-
        ttkentry(variablesFrame,
                 width = "23",
                 textvariable = newVariableName)


    # ------------------------------------------------------------------------
    factor_frame <- tkframe(top)

    factor_checkbox_frame <- tkframe(factor_frame)

    make_factorVariable <- tclVar(dialog_values$initial_make_factor)
    make_factorCheckBox <- ttkcheckbutton(factor_checkbox_frame,
                                          variable = make_factorVariable)

    radioButtons_horizontal(factor_frame,
                            name = "factor_type",
                            # title = gettext_Bio("Use functions: "),
                            # title.color = getRcmdr("title.color"),
                            buttons = c("nominal", "ordinal"),
                            values  = c("nominal", "ordinal"),
                            initialValue = dialog_values$initial_factor_type,
                            labels =  gettext_Bio(c("Nominal", "Ordinal")))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        # make_factor <- tclvalue(make_factorVariable) == "1"
        make_factor <- tclvalue_lgl(make_factorVariable)
        factor_type <- tclvalue(factor_typeVariable)

        # Read recode directives
        save_recodes <- trim.blanks(tclvalue(tkget(recodes, "1.0", "end")))

        # Format code into one row
        recode_directives <-
            stringr::str_replace_all(save_recodes,  c("(\n){2,}" = "\n",
                                                      "\n$"      = "",
                                                      "\n( )*"   = ", ",
                   # Remove tailing commas and spaces
                                                      "[, ]*$"   = ""))

        # str_detect(recode_directives,
        #            "[\\p{Alphabetic}\\p{Mark}\\p{Decimal_Number}]")

        # Is empty? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (recode_directives == "") {
            errorCondition(
                recall = window_variable_recode,
                message = gettext_EZR("No recode directives specified.")
            )
            return()
        }
        # Has single quotes `'`? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # if (0 != length(grep("'", recode_directives))) {
        #     errorCondition(
        #         recall = window_variable_recode,
        #         message = gettext_EZR("Use only double-quotes (\" \") in recode directives"))
        #     return()
        # }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # recode_directives <- strsplit(recode_directives, ",")[[1]]
        # recode_directives <- paste(sapply(recode_directives, process_recode),
        #                            collapse = ",")
        #
        # recode_directives <- sub(" *; *$", "", recode_directives)


        closeDialog()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        variables <- getSelection(variablesBox)

        # If no variable is selected
        if (length(variables) == 0) {
            errorCondition(
                recall = window_variable_recode,
                message = gettext_EZR("You must select a variable.")
            )
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        name <- trim.blanks(tclvalue(newVariableName))

        # check variable name for validity
        if (!is.valid.name(name)) {
            errorCondition(
                recall = window_variable_recode,
                message = glue::glue('"{name}"',
                                     gettext_EZR("is not a valid name."))
            )
            return()
        }

        if (name %in% Variables()) {
            if ("no" == tclvalue(checkReplace(name))) {
                window_variable_recode()
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_variable_recode",
                  list(
                      initial_make_factor = make_factor,
                      initial_variables = variables,
                      initial_name = name,
                      initial_recode_directives = save_recodes,
                      initial_factor_type = factor_type
                  )
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if (make_factor) {
            # Is a factor
            recode_fun <- "dplyr::recode_factor"
            ordered_factor <- switch(factor_type,
                                     "nominal" = "",
                                     "ordinal" = ".ordered = FALSE")
        } else {
            # Not a factor
            recode_fun <- "dplyr::recode"
            ordered_factor <- ""
        }

        command <- glue::glue(
            "## ", gettext_EZR("Recode variable values"), "\n\n",
            "{dataSet} <- {dataSet} %>% \n",
            "   dplyr::mutate({name} = {recode_fun}({variables}, ",
            " {recode_directives}{ordered_factor}))"
        ) %>%
            style_cmd()

        result <- doItAndPrint(command)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error")
            activeDataSet(dataSet,
                          flushModel = FALSE,
                          flushDialogMemory = FALSE)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
    }
    # ========================================================================
    OKCancelHelp(helpSubject = "dplyr::recode",
                 reset = "window_variable_recode",
                 apply = "window_variable_recode")


    tkgrid(upper_frame, sticky = "nw")
    tkgrid(getFrame(variablesBox),
           labelRcmdr(upper_frame, text = "  "),
           recodesFrame, sticky = "nw")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # tkgrid()

    tkgrid(labelRcmdr(
        variablesFrame,
        fg = getRcmdr("title.color"),
        text = gettext_EZR("Name for recoded variable: ")),
        sticky = "w",
        pady = c(2, 0))
    tkgrid(newVariable, sticky = "w")

    tkgrid(factor_checkbox_frame, pady = c(10, 0))

    tkgrid(make_factorCheckBox,
           labelRcmdr(factor_checkbox_frame,
                      text = gettext_EZR("Convert to factor:  ")),
           sticky = "w",
           pady = c(5, 0),
           columnspan = 2)

    tkgrid(factor_typeFrame, sticky = "w")

    tkgrid(
        labelRcmdr(
            recodesFrame,
            text = gettext_EZR(
                "Enter recode directives\n(one directive per row or comma separated)"),
            fg = getRcmdr("title.color"),
            font = "RcmdrTitleFont"
        ),
        sticky = "w"
    )

    tkgrid(recodes, recodesYscroll, sticky = "nw")
    tkgrid(recodesXscroll)

    tkgrid(variablesFrame, sticky = "w")
    tkgrid(factor_frame, sticky = "w")
    tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
    tkgrid.configure(recodesXscroll, sticky = "ew")
    tkgrid.configure(recodesYscroll, sticky = "ns")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dialogSuffix(bindReturn = FALSE)
}
