# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Rcmdr window for z transformation (does not work)
#'
#' @export
#' @keywords internal
#' @family transformations
#'
window_z_transform_2 <- function() {
    initializeDialog(title = get_EZR_text("Z transformation"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    variableBox <-
        variableListBox(top,
                        Numeric(),
                        selectmode = "multiple",
                        title = get_EZR_text("Variables (pick one or more)"),
                        listHeight = 5
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # [!!!]
    # radioButtons(name = "transformation_type",
    #              title = get_EZR_text("---",
    #              buttons = c("A", "B"),
    #              values  = c("", ", center = median, scale = IQR"),
    #              labels =  get_EZR_text(c("z-transformation",
    #                                   "center = median, scale = IQR")))
    # )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    prefix      <- tclVar(get_EZR_text("<automatic prefix>"))
    prefixField <- ttkentry(top, width = "20", textvariable = prefix)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        variables <- getSelection(variableBox)

        closeDialog()

        if (length(variables) == 0) {
            errorCondition(recall = window_log_transform,
                           message = get_EZR_text("You must select a variable."))
            return()
        }

        prefix <- trim.blanks(tclvalue(prefix))
        # type   <- as.character(tclvalue(transformation_typeVariable))

        .activeDataSet <- ActiveDataSet()

        new_names <-
            if (prefix == get_EZR_text("<automatic prefix>")) {
                paste0("z_", variables)

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
                    message = paste(new_names[i], get_EZR_text("is not a valid name."))
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
            glue("   {new_names} <- BioStat::scale_vector({variables}) "),
            "})\n"
        ),
        collapse = "\n")

        result <- justDoIt(command)

        if (class(result)[1] !=  "try-error")
            activeDataSet(.activeDataSet, flushModel = FALSE)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        msg <- glue("#---  ", get_EZR_text("Z transformation"), "  ---#\n\n",
                     "# ",
                     get_EZR_text("New variable(s):"), " \n",
                     paste("#   ", new_names, collapse = "\n"))

        logger(paste0(msg, command, collapse = "\n"))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
    } # [end: onOK]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "scale_vector")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(getFrame(variableBox), baseFrame, sticky = "nw")

    tkgrid(
        labelRcmdr(top,
                   text = get_EZR_text("New variable name or prefix for multiple variables:")
        ),
        prefixField, sticky = "w")

    # [!!!]
    tkgrid(buttonsFrame, sticky = "w", columnspan = 2)

    dialogSuffix(rows = 4,
                 columns = 1,
                 preventGrabFocus = TRUE)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
