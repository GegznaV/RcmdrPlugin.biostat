# TODO:
# 1. Add possibility to reorder variables
# 2. Check if `incorrect_cond_msg` argument is needed.
# 3. Create blue title for the window.

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Correctly initializes window `window_variable_delete()`
window_variable_delete0  <- function(variables) {
    window_variable_delete()
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# new_dsname (character) - data frame name
# incorrect_cond_msg (character) - Message for incorrect expression.
window_variable_delete <- function(new_dsname = NULL,
                                   incorrect_cond_msg = NULL) {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_Bio("Delete/Select variables from data set"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)


    var_delete_box <- variableListBox2(
        upper_frame,
        Variables(),
        title = gettext_Bio("Delete \n(pick one or more)"),
        selectmode = "multiple",
        initialSelection = NULL,
        listHeight = 8
    )

    var_select_box <- variableListBox2(
        upper_frame,
        Variables(),
        title = gettext_Bio("Include \n(pick one or more)"),
        selectmode = "multiple",
        initialSelection = NULL,
        listHeight = 8
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    lower_frame <- tkframe(top)

    if (is.null(new_dsname))  new_dsname <- activeDataSet()

    new_dsname_variable <- tclVar(new_dsname)
    new_dsname_field    <- ttkentry(lower_frame,
                                    width = "46",
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
                recall = window_variable_delete,
                message = gettext_Bio("You must select one or more variables.")
            )
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Check if dataset name already exists ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if (new_dsname != activeDataSet()) {

            if (new_dsname %in% listDataSets()) {
                if ("no" == tclvalue(checkReplace(new_dsname,
                                                  gettext_EZR("Data set")))) {
                    window_variable_delete(new_dsname     = new_dsname,
                                       incorrect_cond_msg =
                                           glue('Chose other name than "{new_dsname}".'))
                    return()
                }
            }

        } else {
            response <- tclvalue(
                RcmdrTkmessageBox(
                    message =
                        sprintf(
                            gettext_Bio("Variable(s):\nExplicitly selected to delete: %d\nExplicitly selected to include: %d\nPlease confirm."),
                            length(var_delete), length(var_select)
                        ),
                    icon    = "warning",
                    type    = "okcancel",
                    default = "cancel"
                )
            )
            if (response == "cancel") {
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

        command <- glue("## Delete, select, reorder variables \n",
                        "{new_dsname} <- {activeDataSet()} %>% \n",
                        "dplyr::select({variables})") %>%
            style_cmd()

        result <- doItAndPrint(command)

        if (class(result)[1] !=  "try-error") {
            # Change active dataset
            activeDataSet(new_dsname, flushModel = FALSE)
        }

        tkfocus(CommanderWindow())

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command_dataset_refresh()
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "select", helpPackage = "dplyr")
    tkgrid(
        labelRcmdr(top,
                   # fg = getRcmdr("title.color"),
                   text = gettext_EZR("Choose either variables to delete, \nor variables to include. ")),
        pady = c(0, 10))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame, sticky = "new", columnspan = 2)
    tkgrid(getFrame(var_delete_box), getFrame(var_select_box), sticky = "nw")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(lower_frame, sticky = "new", columnspan = 2)

    tkgrid(
        labelRcmdr(lower_frame,
                   fg = getRcmdr("title.color"),
                   text = gettext_EZR("Name for dataset with included/deleted variables: ")),
        pady = c(15, 0),
        sticky = "nw")

    tkgrid(
        new_dsname_field,
        pady = c(0, 0),
        sticky = "nw")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix(rows = 2, columns = 1)
}
