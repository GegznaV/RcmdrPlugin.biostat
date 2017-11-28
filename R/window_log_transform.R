# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Rcmdr window for log transformation
#'
#' @export
#' @keywords internal
#' @family transformations
#'
window_log_transform <- function() {
    initializeDialog(title = gettext_Bio("Logarithmic transformation"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_Frame <- tkframe(top)

    variableBox <-
        variableListBox2(upper_Frame,
                         Numeric(),
                         selectmode = "multiple",
                         title = gettext_Bio("Variables (pick one or more)"),
                         listHeight = 4
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    base_outter_Frame <- tkframe(upper_Frame)
    radioButtons(base_outter_Frame,
                 name = "base",
                 title = gettext_Bio("Base of logarithm"),
                 buttons = c("common", "natural", "binary"),
                 values = c("10", "exp(1)", "2"),
                 labels =  gettext_Bio(
                     c("Common (base = 10)",
                       "Natural (base = e)",
                       "Binary (base = 2)"))
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    prefix      <- tclVar(gettext_Bio("<automatic prefix>"))
    prefixField <- ttkentry(top, width = "20", textvariable = prefix)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        variables <- getSelection(variableBox)

        closeDialog()

        if (length(variables) == 0) {
            errorCondition(recall = window_log_transform,
                           message = gettext_Bio("You must select a variable."))
            return()
        }

        prefix <- trim.blanks(tclvalue(prefix))
        base   <- as.character(tclvalue(baseVariable))

        .activeDataSet <- ActiveDataSet()

        new_names <-
            if (prefix == gettext_Bio("<automatic prefix>")) {
                paste0("log_", variables)

            } else if (length(variables) == 1) {
                prefix

            } else {
                paste0(prefix, variables)
            }

        # Chech for errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        for (i in seq_along(variables)) {

            if (!is.valid.name(new_names[i])) {
                errorCondition(
                    recall = window_log_transform,
                    message = paste(new_names[i], gettext_Bio("is not a valid name."))
                )
                return()
            }
            if (is.element(new_names[i], Variables())) {
                if ("no" == tclvalue(checkReplace(new_names[i]))) {
                    window_log_transform()
                    return()
                }
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command <- paste0(c("\n",
            glue("{.activeDataSet} <- within({.activeDataSet}, {{ "),
            # Next line will be repeated many times:
            glue("   {new_names} <- log({variables}, base = {base}) "),
            "})\n"
        ),
        collapse = "\n")

        result <- justDoIt(command)

        if (class(result)[1] !=  "try-error")
            activeDataSet(.activeDataSet, flushModel = FALSE)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        msg <- glue("#---  ", gettext_Bio("Logarithmic transformation"), "  ---#\n\n",
                    "# ", gettext_Bio("New variable(s):"), " \n",
                    paste("#   ", new_names, collapse = "\n"))

        logger(paste0(msg, command, collapse = "\n"))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
    } # [end: onOK] ----------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "log")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(baseFrame, padx = c(15, 5))
    tkgrid(getFrame(variableBox), base_outter_Frame, sticky = "nw")
    tkgrid(upper_Frame)
    tkgrid(labelRcmdr(top,
                      text = gettext_Bio("New variable name or prefix for multiple variables:"),
                      fg = getRcmdr("title.color")),
           sticky = "w",
           pady = c(10, 0))

    tkgrid(prefixField, sticky = "ew", columnspan = 2)
    tkgrid(buttonsFrame, sticky = "ew", columnspan = 2)

    dialogSuffix(rows = 4,
                 columns = 2,
                 preventGrabFocus = TRUE)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
