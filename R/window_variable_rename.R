# TODO:
# 1. Simplify the code.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_variable_rename <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_Bio("Rename variables"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    variables_frame <- tkframe(top)
    variableBox <-
        variableListBox2(
            variables_frame,
            Variables(),
            title = gettext_Bio("Variables\n(pick one or more)"),
            selectmode = "multiple",
            initialSelection = NULL,
            listHeight = 10
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        old_names <- getSelection(variableBox)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        n_old_names <- length(old_names)

        if (n_old_names < 1) {
            errorCondition(
                recall = window_variable_rename,
                message = gettext_Bio("No variables selected.")
            )
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        .activeDataSet <- ActiveDataSet()
        unordered_names <- names(get(.activeDataSet))
        which_variables <- match(old_names, unordered_names)

        # Subdialog ----------------------------------------------------------
        initializeDialog(subdialog,  title = gettext_Bio("Change variable names"))
        new_names <- rep("", n_old_names)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        onOKsub <- function() {
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            closeDialog(subdialog)
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            for (i in 1:n_old_names) {
                new_names[i] <- eval_glue("tclvalue(newName{i})")
            }
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # If empty
            if (any(new_names == "")) {
                errorCondition(
                    recall = window_variable_rename,
                    message = gettext_Bio("A variable name is empty.")
                )
                return()
            }
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            test.names <- new_names == make.names(new_names)
            if (!all(test.names)) {
                errorCondition(recall = window_variable_rename,
                               message = paste(
                                   gettext_Bio("The following variable names are not valid:\n"),
                                   paste(new_names[!test.names], collapse = ", ")))
                return()
            }
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Check duplicated names
            all_names <- names(get(.activeDataSet))
            all_names[which_variables] <- new_names
            if (any(duplicated(all_names))) {
                errorCondition(
                    recall = window_variable_rename,
                    message = gettext_Bio("Variable names are not unique")
                )
                return()
            }
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            Library("dplyr")

            command <-
                glue("## Rename variables\n",
                     '# `new_name` = `old_name`\n\n',
                     "{.activeDataSet} <- {.activeDataSet} %>% \n",
                     'dplyr::rename(',
                     paste(glue('`{new_names}` = `{old_names}`'), collapse = ", \n"),
                     ')') %>%
                style_cmd()

            result <- justDoIt(command)
            logger(command)

            if (class(result)[1] !=  "try-error")
                activeDataSet(.activeDataSet, flushModel = FALSE)

            tkfocus(CommanderWindow())
        }
        # Create menus -------------------------------------------------------
        subOKCancelHelp()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkgrid(
            labelRcmdr(
                subdialog,
                text = gettext_Bio("Old Name   "),
                fg = "blue"
            ),
            labelRcmdr(
                subdialog,
                text = gettext_Bio("New name   "),
                fg = "blue"
            ),
            sticky = "w",
            pady = c(10, 15)
        )

        for (i in 1:n_old_names) {
            valVar <- paste0("newName", i)
            assign(valVar, tclVar(""))
            assign(paste0("entry", i), ttkentry(subdialog,
                                                width = "20",
                                                textvariable = get(valVar)
                )
            )
            tkgrid(labelRcmdr(subdialog, text = old_names[i]),
                   get(paste0("entry", i)),
                   sticky = "w")
        }
        tkgrid(subButtonsFrame,
               sticky = "e",
               columnspan = 2)

        dialogSuffix(
            subdialog,
            rows = n_old_names + 2,
            columns = 2,
            focus = entry1,
            onOK = onOKsub,
            force.wait = TRUE
        )
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")
    tkgrid(label_rcmdr(
        top,
        text = gettextRcmdr("Rename variables (columns)"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9))

    OKCancelHelp(helpSubject = "rename")
    tkgrid(variables_frame, sticky = "w", columnspan = 2)
    tkgrid(getFrame(variableBox), sticky = "nwe")
    tkgrid(buttonsFrame, sticky = "we")
    dialogSuffix(rows = 2, columns = 1)
}
