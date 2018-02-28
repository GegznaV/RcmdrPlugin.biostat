# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Rcmdr window for z transformation
#'
#' @export
#' @keywords internal
#' @family transformations
#'
window_z_transform <- function() {
    initializeDialog(title = gettext_Bio("Z transformation (standardization)"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    variableBox <-
        variableListBox(top,
                        Numeric(),
                        selectmode = "multiple",
                        title = gettext_Bio("Variables (pick one or more)"),
                        listHeight = 5
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    prefix      <- tclVar(gettext_Bio('<automatic prefix>'))
    prefixField <- ttkentry(top,
                            width = "25",
                            textvariable = prefix)
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
        # base   <- as.character(tclvalue(baseVariable))

        .activeDataSet <- ActiveDataSet()

        new_names <-
            if (prefix == gettext_Bio('<automatic prefix>')) {
                paste0("z_", variables)

            } else if (length(variables) == 1) {
                prefix

            } else {
                paste0(make.names(prefix), variables)
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
                            glue("   {new_names} <- biostat::scale_vector({variables}) "),
                            "})\n"
        ),
        collapse = "\n")

        result <- justDoIt(command)

        if (class(result)[1] !=  "try-error")
            activeDataSet(.activeDataSet, flushModel = FALSE)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        msg <- glue("#---  ", gettext_Bio("Z transformation"), "  ---#\n\n",
                    "# ", gettext_Bio("New variable(s):"), " \n",
                    paste("#   ", new_names, collapse = "\n"))

        logger(paste0(msg, command, collapse = "\n"))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
    } # [end: onOK]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "scale_vector")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(getFrame(variableBox), sticky = "nw")

    tkgrid(labelRcmdr(top,
                      text = gettext_Bio("New variable name or prefix for multiple variables:"),
                      fg = getRcmdr("title.color")),
           sticky = "w",
           pady = c(10, 0))

    tkgrid(prefixField, sticky = "ew")
    tkgrid(buttonsFrame, sticky = "w", columnspan = 2)

    dialogSuffix(rows = 4,
                 columns = 2,
                 preventGrabFocus = TRUE)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
