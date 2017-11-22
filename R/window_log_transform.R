# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Rcmdr window for log transformation
#'
#' @export
#' @keywords internal
#' @family transformations
#'
window_log_transform <- function() {
    initializeDialog(title = get_BioStat_text("Logarithmic transformation"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    variableBox <-
        variableListBox(top,
                        Numeric(),
                        selectmode = "multiple",
                        title = get_BioStat_text("Variables (pick one or more)"),
                        listHeight = 5
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    radioButtons(name = "base",
                 buttons = c("common", "natural", "binary"),
                 values = c("10", "exp(1)", "2"),

                 labels =  get_BioStat_text(c("Common logarithm (base = 10)",
                                      "Natural logarithm (base = e)",
                                      "Binary logarithm (base = 2)")),
                 title = get_BioStat_text("Base of logarithmic transformation")
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    prefix      <- tclVar(get_BioStat_text("<automatic prefix>"))
    prefixField <- ttkentry(top, width = "20", textvariable = prefix)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        variables <- getSelection(variableBox)

        closeDialog()

        if (length(variables) == 0) {
            errorCondition(recall = window_log_transform,
                           message = get_BioStat_text("You must select a variable."))
            return()
        }

        prefix <- trim.blanks(tclvalue(prefix))
        base   <- as.character(tclvalue(baseVariable))

        .activeDataSet <- ActiveDataSet()

        new_names <-
            if (prefix == get_BioStat_text("<automatic prefix>")) {
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
                    message = paste(new_names[i], get_BioStat_text("is not a valid name."))
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
            glue("   {new_names} <- log({variables}, base = {base}) "),
            "})\n"
        ),
        collapse = "\n")

        result <- justDoIt(command)

        if (class(result)[1] !=  "try-error")
            activeDataSet(.activeDataSet, flushModel = FALSE)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        msg <- glue("#---  ", get_BioStat_text("Logarithmic transformation"), "  ---#\n\n",
                    "# ", get_BioStat_text("New variable(s):"), " \n",
                    paste("#   ", new_names, collapse = "\n"))

        logger(paste0(msg, command, collapse = "\n"))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
    } # [end: onOK]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "log")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(getFrame(variableBox), baseFrame, sticky = "nw")

    tkgrid(
        labelRcmdr(top,
                   text = get_BioStat_text("New variable name or prefix for multiple variables:")
        ),
        prefixField, sticky = "w")

    tkgrid(buttonsFrame, sticky = "w", columnspan = 2)

    dialogSuffix(rows = 4,
                 columns = 2,
                 preventGrabFocus = TRUE)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
