#' command_new_dataset
#'
#' Code of this function is taken from `Rcmdr` package and slightly modified.
#'
#' @export
#' @keywords internal
window_dataset_new_rcmdr <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Window to choose dataset's name

    initializeDialog(title = gettextRcmdr("Create a New Dataset"))
    dsname <- tclVar("new_dataset")
    entryDsname <- ttkentry(top, width = "30", textvariable = dsname)

    onOK <- function() {
        dsnameValue <- trim.blanks(tclvalue(dsname))

        # Checks if no name is entered
        if (dsnameValue == "") {
            errorCondition(
                recall = window_new_dataset_rcmdr,
                message = gettextRcmdr("You must enter the name of the dataset."))
            return()
        }

        # Check validity of the entered name
        if (!is.valid.name(dsnameValue)) {
            errorCondition(
                recall = window_new_dataset_rcmdr,
                message = glue::glue('"{dsnameValue}" ',
                                     gettextRcmdr("is not a valid name for a dataset."))
            )
            return()
        }

        # Check if a dataset with the same name exists in the workspace
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Dataset")))) {
                window_new_dataset_rcmdr()
                return()
            }
        }
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Edit window
        command <- glue::glue("Rcmdr::editDataset(dsname = '{dsnameValue}')")
        result <- justDoIt(command)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            if (!getRcmdr("dataset.modified")) return()
            .data <- try(get(dsnameValue, envir = .GlobalEnv), silent = TRUE)
            if (nrow(.data) == 0) {
                errorCondition(recall = window_new_dataset_rcmdr,
                               message = gettextRcmdr("empty data set."))
                return()
            }
            tempdir <- tempdir()
            tempdir <- gsub("\\\\", "/", tempdir)
            savefile <- paste0(tempdir, "/", dsnameValue)
            save(".data", file = savefile)

            if (getRcmdr("use.markdown")) {
                removeNullRmdBlocks()
                enterMarkdown(paste0('load("', savefile, '")'))
            }
            if (getRcmdr("use.knitr")) {
                removeNullRnwBlocks()
                enterKnitr(paste0('load("', savefile, '")'))
            }
        }
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "editDataset")
    tkgrid(labelRcmdr(top,
                      text = gettextRcmdr("Enter dataset's name:   ")),
           entryDsname,
           pady = c(5, 5),
           sticky = "e")

    tkgrid(buttonsFrame, columnspan = "2", sticky = "ew")
    tkgrid.configure(entryDsname, sticky = "w")
    dialogSuffix(focus = entryDsname)
}