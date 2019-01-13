# TODO:
# 1. Add possibility to reorder variables
# 2. Check if `incorrect_cond_msg` argument is needed.
# 3. Create blue title for the window.
# 4. Buttons with options for dataset name: the asme name as original; new name

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Correctly initializes window `window_variable_select()`
window_variable_select0  <- function(variables) {
    window_variable_select()
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# new_dsname (character) - data frame name
# incorrect_cond_msg (character) - Message for incorrect expression.
window_variable_select <- function(new_dsname = NULL, incorrect_cond_msg = NULL) {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Select/Remove variables from data set"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)

    var_select_box <- variableListBox2(
        upper_frame,
        Variables(),
        title = gettext_bs("Select / Include \n(pick one or more)"),
        selectmode = "multiple",
        initialSelection = NA,
        listHeight = 8
    )

    var_delete_box <- variableListBox2(
        upper_frame,
        Variables(),
        title = gettext_bs("Remove \n(pick one or more)"),
        selectmode = "multiple",
        initialSelection = NA,
        listHeight = 8
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    lower_frame <- tkframe(top)

    if (is.null(new_dsname))  new_dsname <- unique_df_name(active_dataset())

    new_dsname_variable <- tclVar(new_dsname)
    new_dsname_field    <- ttkentry(lower_frame,
                                    width = "50",
                                    textvariable = new_dsname_variable)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        new_dsname <- trim.blanks(tclvalue(new_dsname_variable))
        var_delete <- getSelection(var_delete_box)
        var_select <- getSelection(var_select_box)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if ((length(var_select) + length(var_delete)) == 0) {
            errorCondition(
                recall = window_variable_select,
                message = gettext_bs("You must select one or more variables.")
            )
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Check if dataset name already exists ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if (new_dsname != active_dataset()) {

            if (new_dsname %in% listDataSets()) {
                if ("no" == tclvalue(checkReplace(
                    new_dsname,
                    gettext_bs("Data set")))) {
                    window_variable_select(
                        new_dsname = new_dsname,
                        incorrect_cond_msg =
                            glue('Chose other name than "{new_dsname}".'))
                    return()
                }
            }

        } else {
            response <-
                tk_messageBox(
                    # parent = top,
                    parent = CommanderWindow(),
                    caption = "Modify Dataset",
                    message =
                        sprintf(
                            gettext_bs(str_c(
                                "Variable(s):\n",
                                "Explicitly selected to include: %d\n",
                                "Explicitly selected to remove: %d\n",
                                "Do you agree to modify your dataset?")),
                            length(var_select), length(var_delete)
                        ),
                    icon    = "warning",
                    type    = "yesno",
                    default = "no"
                )

            if (response == "no") {
                onCancel()
                return()
            }

        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("dplyr")

        to_select <-
            if (length(var_select) > 0) {
                stringr::str_c("`", var_select, "`", collapse = ", ")
            } else {
                NULL
            }

        to_reorder <-
            if (
                # vars_reorder
                FALSE) {
                "everything()"
            } else {
                NULL
            }


        to_delete <-
            if (length(var_delete) > 0) {
                stringr::str_c("-", "`", var_delete, "`", collapse = ", ")
            } else {
                NULL
            }

        variables <- stringr::str_c(to_select, to_reorder, to_delete, sep = ", ")

        command <- glue("## Select, reorder, remove variables \n",
                        "{new_dsname} <- {active_dataset()} %>% \n",
                        "dplyr::select({variables})") %>%
            style_cmd()

        result <- doItAndPrint(command)

        if (class(result)[1] !=  "try-error") {
            # Change active dataset
            active_dataset(new_dsname, flushModel = FALSE)
        }

        tkfocus(CommanderWindow())

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command_dataset_refresh()
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "select", helpPackage = "dplyr")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(
        bs_label(top,
                    # fg = getRcmdr("title.color"),
                    text = gettext_bs("Choose either variables to include, or variables to remove. ")),
        pady = c(0, 10))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame, columnspan = 3, sticky = "n")
    tkgrid(getFrame(var_select_box),
           bs_label(upper_frame, text = "       "),
           getFrame(var_delete_box))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(lower_frame, sticky = "n", columnspan = 3)

    tkgrid(
        bs_label(lower_frame,
                    fg = getRcmdr("title.color"),
                    text = gettext_bs("Name for modified dataset \n(with selected and without removed variables) ")),
        pady = c(15, 0),
        sticky = "nw")

    tkgrid(
        new_dsname_field,
        pady = c(0, 0),
        sticky = "nw")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix(rows = 2, columns = 1)
}
