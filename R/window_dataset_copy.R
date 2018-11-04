#' command_new_dataset
#'
#' Code of this function is taken from `Rcmdr` package and slightly modified.
#'
#' @export
#' @keywords internal
window_dataset_copy <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Window to choose dataset's name

    initializeDialog(title = gettextRcmdr("Make a Copy of The Active Dataset"))
    dsname <- tclVar(str_glue("{activeDataSet()}_copy"))
    entryDsname <- ttkentry(top, width = "40", textvariable = dsname)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        dsnameValue <- trim.blanks(tclvalue(dsname))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Checks if no name is entered
        if (dsnameValue == "") {
            errorCondition(
                recall = window_dataset_copy,
                message = gettextRcmdr("You must enter the name of the dataset."))
            return()
        }

        # Check validity of the entered name
        if (!is.valid.name(dsnameValue)) {
            errorCondition(
                recall = window_dataset_copy,
                message = str_glue('"{dsnameValue}" ',
                                   gettextRcmdr("is not a valid name for a dataset."))
            )
            return()
        }

        # Check if a dataset with the same name exists in the workspace
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Dataset")))) {
                window_dataset_copy()
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Edit window

        ds_to_copy <- activeDataSet()


        command1 <- str_glue(
            "## Make a copy of the dataset \n",
            "{dsnameValue} <- {ds_to_copy}")
        doItAndPrint(command1)
        activeDataSet(dsnameValue)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")
    tkgrid(label_rcmdr(
        top,
        text = gettextRcmdr("Make a copy of the dataset"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9), columnspan = 2)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    OKCancelHelp()

    tkgrid(labelRcmdr(top,
                      fg = getRcmdr("title.color"),
                      text = gettextRcmdr("Dataset to copy: ")),
           labelRcmdr(top,
                      text = activeDataSet()),

           sticky = "w", pady = c(5, 10))

    tkgrid(labelRcmdr(top,
                      fg = getRcmdr("title.color"),
                      text = gettextRcmdr("Enter a name for the copy:   ")),
           entryDsname,
           sticky = "e", pady = c(0, 10))

    tkgrid(buttonsFrame, columnspan = "2", sticky = "ew")
    tkgrid.configure(entryDsname, sticky = "w")
    dialogSuffix(focus = entryDsname)
}