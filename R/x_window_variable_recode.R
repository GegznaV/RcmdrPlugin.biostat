# From EZR
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
gettext_EZR <- function(...) {
    gettext(domain = "R-RcmdrPlugin.EZR", ...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_variable_recode <- function() {

    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    processRecode <- function(recode) {
        parts <- strsplit(recode, "=")[[1]]
        if (length(grep(",", parts[1])) > 0) {
            paste("c(", parts[1], ") = ", parts[2], sep = "")
        } else {
            paste(parts, collapse = "=")
        }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    dataSet <- activeDataSet()
    defaults <-
        list(
            initial.asFactor = 1,
            initial.variables = NULL,
            initial.name = "variable",
            initial.recode.directives = ""
        )

    dialog.values <- getDialog("window_variable_recode", defaults)
    initializeDialog(title = gettext_EZR("Recode Variable Values"))
    variablesBox <-
        variableListBox(
            top,
            Variables(),
            selectmode = "multiple",
            title = gettext_EZR("Variables to recode \n(pick one or more)"),
            initialSelection = varPosn(dialog.values$initial.variables, "all")
        )
    variablesFrame <- tkframe(top)
    newVariableName <- tclVar(dialog.values$initial.name)
    newVariable <-
        ttkentry(variablesFrame,
                 width = "20",
                 textvariable = newVariableName)
    recodesFrame <- tkframe(top)
    recodes <-
        tktext(
            recodesFrame,
            bg = "white",
            font = getRcmdr("logFont"),
            height = "7",
            width = "40",
            wrap = "none"
        )
    recodesXscroll <-
        ttkscrollbar(
            recodesFrame,
            orient = "horizontal",
            command = function(...)
                tkxview(recodes, ...)
        )
    recodesYscroll <-
        ttkscrollbar(
            recodesFrame,
            command = function(...)
                tkyview(recodes,
                        ...)
        )
    tkconfigure(
        recodes,
        xscrollcommand = function(...)
            tkset(recodesXscroll,
                  ...)
    )
    tkconfigure(
        recodes,
        yscrollcommand = function(...)
            tkset(recodesYscroll, ...)
    )
    tkinsert(recodes, "1.0", dialog.values$initial.recode.directives)
    asFactorFrame <- tkframe(top)
    asFactorVariable <- tclVar(dialog.values$initial.asFactor)
    asFactorCheckBox <-
        ttkcheckbutton(asFactorFrame, variable = asFactorVariable)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        logger(paste0("# ", gettext_EZR("Recode variable values")))

        asFactor <- tclvalue(asFactorVariable) == "1"
        save.recodes <- trim.blanks(tclvalue(tkget(recodes, "1.0", "end")))

        recode.directives <- gsub("\n", "; ", save.recodes)
        check.empty <- gsub(";", "", gsub(" ", "", recode.directives))

        if ("" == check.empty) {
            errorCondition(
                recall = window_variable_recode,
                message = gettext_EZR("No recode directives specified.")
            )
            return()
        }
        if (0 != length(grep("'", recode.directives))) {
            errorCondition(
                recall = window_variable_recode,
                message = gettext_EZR("Use only double-quotes (\" \") in recode directives"))
            return()
        }
        recode.directives <- strsplit(recode.directives, ";")[[1]]
        recode.directives <- paste(sapply(recode.directives,
                                          processRecode), collapse = ";")
        recode.directives <- sub(" *; *$", "", recode.directives)
        variables <- getSelection(variablesBox)
        closeDialog()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(variables) == 0) {
            errorCondition(
                recall = window_variable_recode,
                message = gettext_EZR("You must select a variable.")
            )
            return()
        }
        multiple <- if (length(variables) > 1) {TRUE} else {FALSE}

        name <- trim.blanks(tclvalue(newVariableName))

        #        save.recodes <- gsub("; ", "\\\n", trim.blanks(recode.directives))
        putDialog("window_variable_recode",
            list(
                initial.asFactor = asFactor,
                initial.variables = variables,
                initial.name = name,
                initial.recode.directives = save.recodes
            )
        )
        command <- paste0(dataSet, " <- within(", dataSet, ", {")

        nvar <- length(variables)
        for (i in 1:nvar) {
            variable <- variables[nvar - i + 1]
            newVar <- if (multiple) {paste0(name, variable)} else {name}
            if (!is.valid.name(newVar)) {
                errorCondition(
                    recall = window_variable_recode,
                    message = glue::glue('"{newVar}"',
                                         gettext_EZR("is not a valid name."))
                )
                return()
            }
            if (is.element(newVar, Variables())) {
                if ("no" == tclvalue(checkReplace(newVar))) {
                    window_variable_recode()
                    return()
                }
            }
            command <-
                glue::glue("{command}\n",
                       "{newVar} <- Recode({variable}, '{recode.directives}', ",
                       "as.factor.result = {asFactor})"
                )
        }
        command <- glue::glue("{command} \n})")

        result <- doItAndPrint(command)

        if (class(result)[1] != "try-error")
            activeDataSet(dataSet,
                          flushModel = FALSE,
                          flushDialogMemory = FALSE)
        #     else{
        #       if (getRcmdr("use.markdown")) removeLastRmdBlock()
        #       if (getRcmdr("use.knitr")) removeLastRnwBlock()
        #    }
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "window_variable_recode",
                 reset = "window_variable_recode",
                 apply = "window_variable_recode")

    tkgrid(getFrame(variablesBox), sticky = "nw")
    tkgrid(labelRcmdr(variablesFrame, text = ""))
    tkgrid(labelRcmdr(
        variablesFrame,
        text = gettext_EZR("New variable name or prefix for multiple recodes: ")
    ),
    newVariable,
    sticky = "w")
    tkgrid(asFactorCheckBox,
           labelRcmdr(
               asFactorFrame,
               text = gettext_EZR("Make (each) new variable a factor")),
           sticky = "w")
    tkgrid(labelRcmdr(asFactorFrame, text = ""))
    tkgrid(
        labelRcmdr(
            recodesFrame,
            text = gettext_EZR("Enter recode directives"),
            fg = getRcmdr("title.color"),
            font = "RcmdrTitleFont"
        ),
        sticky = "w"
    )

    tkgrid(recodes, recodesYscroll, sticky = "nw")
    tkgrid(recodesXscroll)
    tkgrid(variablesFrame, sticky = "w")
    tkgrid(asFactorFrame, sticky = "w")
    tkgrid(recodesFrame, sticky = "w")
    tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
    tkgrid.configure(recodesXscroll, sticky = "ew")
    tkgrid.configure(recodesYscroll, sticky = "ns")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dialogSuffix(bindReturn = FALSE)
}
