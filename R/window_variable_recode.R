# Based on function from EZR

# TODO:
#
# 1. Change new variable name and add _1, _2, _3, etc. if variable with that
#    name exists in active dataframe
#
#
# DONE:
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
# Helper functions

recode_values_template <- function(x) {

    if (is.character(x)) {
        x <- forcats::as_factor(x)
    }

    unique_values <-
        if (is.factor(x)) {
            levels(x)

        } else {
            sort(unique(x))
        }

    rez <-
        glue::glue('"{unique_values}" = ""') %>%
        paste(collapse = "\n") %>%
        paste('\n',
              '\n.default = ""',
              '\n.missing = ""')
    rez
    # writeLines(rez)
}

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
            # initial_make_factor = 1,
            initial_recode_into = "nominal",
            initial_variables = NULL,
            initial_name = "recoded_variable",
            initial_recode_directives = ""
        )

    dialog_values <- getDialog("window_variable_recode", defaults)



    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    variable_doubleclick <- function() {
        active_variable <- getSelection(variablesBox)
        var_val         <- eval_glue("{activeDataSet()}${active_variable}")

        # Change contents of "recodes" box
        tkdelete(recodes, "1.0", "end")
        tkinsert(recodes, "1.0", recode_values_template(var_val))

        # Change new variable name
        tclvalue(newVariableName) <- glue("{active_variable}_recoded")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)

    variablesBox <-
        variableListBox2(
            listHeight = 7,
            upper_frame,
            Variables(),
            onDoubleClick_fun = variable_doubleclick,
            # selectmode = "multiple",
            title = gettext_EZR("Variable to recode \n(pick one, double-click)"),
            initialSelection = varPosn(dialog_values$initial_variables, "all")
        )

    # ------------------------------------------------------------------------
    recodesFrame <- tkframe(upper_frame)
    recodes <-
        tktext(
            recodesFrame,
            bg     = "white",
            font   = getRcmdr("logFont"),
            height = "7",
            width  = "40",
            wrap   = "none")

    recodesXscroll <-
        ttkscrollbar(
            recodesFrame,
            orient  = "horizontal",
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
    lower_frame <- tkframe(top)
    lower_options_frame <- tkframe(lower_frame)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    variablesFrame  <- tkframe(lower_options_frame)
    newVariableName <- tclVar(dialog_values$initial_name)
    newVariable <-
        ttkentry(variablesFrame,
                 width = "23",
                 textvariable = newVariableName)
    # ------------------------------------------------------------------------
    variable_type_frame <- tkframe(lower_options_frame)

    # factor_checkbox_frame <- tkframe(variable_type_frame)

    # make_factorVariable   <- tclVar(dialog_values$initial_make_factor)
    # make_factorCheckBox   <- ttkcheckbutton(factor_checkbox_frame,
    #                                         variable = make_factorVariable)

    Rcmdr::radioButtons(variable_type_frame,
                            name          = "recode_into",
                            # title       = gettext_Bio("Use functions: "),
                            # title.color = getRcmdr("title.color"),
                            buttons       = c("nominal", "ordinal", "other"),
                            values        = c("nominal", "ordinal", "other"),
                            initialValue  = dialog_values$initial_recode_into,
                            labels        = gettext_Bio(c("Nominal factor",
                                                          "Ordinal factor",
                                                          "Do not convert")))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        # make_factor <- tclvalue(make_factorVariable) == "1"
        # make_factor <- tclvalue_lgl(make_factorVariable)
        recode_into <- tclvalue(recode_intoVariable)
        variables   <- getSelection(variablesBox)
        name        <- trim.blanks(tclvalue(newVariableName))


        # Read recode directives
        save_recodes <- trim.blanks(tclvalue(tkget(recodes, "1.0", "end")))

        # Format code into one row
        recode_directives <-
            stringr::str_replace_all(save_recodes,
                                     # Remove intact rows
                                     c('".*" = ""' = "",
                                       '\\.default = (NULL|"")' = "",
                                       '\\.missing = (NULL|"")' = "",
                                       # Remove leading commas and spaces
                                       "[, ]+(\n)"    = "\\1",
                                       # Remove emplty lines
                                       "(\n){2,}"  = "\n",
                                       # Remove tailing new line
                                       "\n$"       = "",
                                       # Remove leading spaces in each non-first row
                                       "\n( )*"    = ", ",
                                       # Remove leading spaces and commas in
                                       # the first row
                                       "^[, ]*"    = "",
                                       # Remove tailing commas and spaces
                                       "[, ]*$"    = "")
                   )
        # str_detect(recode_directives,
        #            "[\\p{Alphabetic}\\p{Mark}\\p{Decimal_Number}]")

        # Is empty? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (recode_directives == "") {
            errorCondition(
                recall  = window_variable_recode,
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
        # variables
        # If no variable is selected
        if (length(variables) == 0) {
            errorCondition(
                recall  = window_variable_recode,
                message = gettext_EZR("You must select a variable.")
            )
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # name

        # check variable name for validity
        if (!is.valid.name(name)) {
            errorCondition(
                recall  = window_variable_recode,
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
                      # initial_make_factor       = make_factor,
                      initial_variables         = variables,
                      initial_name              = name,
                      initial_recode_directives = save_recodes,
                      initial_recode_into       = recode_into
                  )
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        switch(recode_into,
               "nominal" = {
                   recode_fun     <- "dplyr::recode_factor"
                   ordered_factor <- ""
               },
               "ordinal" = {
                   recode_fun     <- "dplyr::recode_factor"
                   ordered_factor <- ", .ordered = TRUE" # .ordered = FALSE ???
               },
               "other" = {
                   recode_fun     <- "dplyr::recode"
                   ordered_factor <- ""
               })

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
    OKCancelHelp(helpSubject = "recode_factor",
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

    # tkgrid(factor_checkbox_frame, pady = c(10, 0))

    # tkgrid(make_factorCheckBox,
    #        labelRcmdr(factor_checkbox_frame,
    #                   text = gettext_EZR("Convert to:  ")),
    #        sticky = "w",
    #        pady = c(5, 0),
    #        columnspan = 2)
    tkgrid(
        labelRcmdr(variable_type_frame,
                   text = gettext_EZR("Convert variable into:  "),
                   fg = getRcmdr("title.color")),
        sticky = "w",
        pady = c(5, 0)
    )

    tkgrid(recode_intoFrame, sticky = "w")

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

    tkgrid(lower_frame,         sticky = "w")
    tkgrid(variablesFrame,      sticky = "w")
    tkgrid(variable_type_frame, sticky = "w")

    # ==========================================================================
    examples_frame <- tkframe(lower_frame)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid_text <- function(text = "", frame = examples_frame, fg = "black",
                            sticky = "w", padx = 10, pady = 0, ...) {
        tkgrid(labelRcmdr(frame, text = gettext_EZR(text), fg = fg),
               sticky = sticky, padx = padx, pady = pady, ...)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid_text("Examples of recode directives:", fg = getRcmdr("title.color"))
    tkgrid_text(paste(
        '      Example 1:       "old value 1" = "new value 1"\n',
                           # '\t\t"old value 3" = "new value 3"\n',
                           '\t\t"old value 2" = "new value 2"'))

    tkgrid_text('      Example 2:       "old 1" = "new 1", "old 2" = "new 2"\n')
    tkgrid_text('      .default - a default value for all non-specified values.')
    tkgrid_text('      .missing - a new explicit value for missing (NA) values.')
    # if (!is.null(incorrect_expr_msg)) {
    #     tkgrid_text(incorrect_expr_msg, fg = "darkred", sticky = "e",
    #                 pady = c(5, 0))
    # }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkgrid(lower_options_frame, examples_frame,
           sticky = "nw",
           columnspan = 2)
    # ==========================================================================


    tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
    tkgrid.configure(recodesXscroll, sticky = "ew")
    tkgrid.configure(recodesYscroll, sticky = "ns")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dialogSuffix(bindReturn = FALSE)
}
