#' command_new_dataset
#'
#' Code of this function is taken from `Rcmdr` package and slightly modified.
#'
#' @export
#' @keywords internal
window_dataset_rename <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Window to choose dataset's name

    initializeDialog(title = gettextRcmdr("Rename The Active Dataset"))
    dsname <- tclVar(glue::glue("{activeDataSet()}_renamed"))
    entryDsname <- ttkentry(top, width = "40", textvariable = dsname)

    onOK <- function() {
        dsnameValue <- trim.blanks(tclvalue(dsname))

        # Checks if no name is entered
        if (dsnameValue == "") {
            errorCondition(
                recall = window_dataset_rename,
                message = gettextRcmdr("You must enter the name of the dataset."))
            return()
        }

        # Check validity of the entered name
        if (!is.valid.name(dsnameValue)) {
            errorCondition(
                recall = window_dataset_rename,
                message = glue::glue('"{dsnameValue}" ',
                                     gettextRcmdr("is not a valid name for a dataset."))
            )
            return()
        }

        # Check if a dataset with the same name exists in the workspace
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Dataset")))) {
                window_dataset_rename()
                return()
            }
        }
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Edit window

        ds_to_rename <- activeDataSet()


        command1 <- glue::glue(
            "# Rename the dataset \n",
            "{dsnameValue} <- {ds_to_rename}")
        justDoIt(command1)
        activeDataSet(dsnameValue)

        command2 <- glue::glue("\nremove({ds_to_rename})")
        justDoIt(command2)



        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        logger(paste(command1, command2, sep = "; "))
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp()

    tkgrid(labelRcmdr(top,
                      fg = getRcmdr("title.color"),
                      text = gettextRcmdr("Dataset to rename: ")),
           labelRcmdr(top,
                      text = activeDataSet()),

           sticky = "w", pady = c(5, 10))

    tkgrid(labelRcmdr(top,
                      fg = getRcmdr("title.color"),
                      text = gettextRcmdr("Enter a new name:  ")),
           entryDsname,
           sticky = "e", pady = c(0, 10))


    tkgrid(buttonsFrame, columnspan = "2", sticky = "ew")
    tkgrid.configure(entryDsname, sticky = "w")
    dialogSuffix(focus = entryDsname)
}