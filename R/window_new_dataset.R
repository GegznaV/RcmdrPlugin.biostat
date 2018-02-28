
#' command_new_dataset
#'
#' This function is taken from `Rcmdr` package and slightly modified.
#' @export
#' @keywords internal
window_new_dataset_rcmdr <- function() {
    initializeDialog(title = gettextRcmdr("New Data Set"))
    dsname <- tclVar("Dataset")
    entryDsname <- ttkentry(top, width = "20", textvariable = dsname)
    onOK <- function() {
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(
                recall = newDataSet,
                message = gettextRcmdr("You must enter the name of a data set."))
            return()
        }
        if (!is.valid.name(dsnameValue)) {
            errorCondition(
                recall = newDataSet,
                message = glue::glue('"{dsnameValue}" ',
                                     gettextRcmdr("is not a valid name."))
            )
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))) {
                newDataSet()
                return()
            }
        }
        closeDialog()
        command <- glue::glue("Rcmdr::editDataset(dsname = '{dsnameValue}')")
        result <- justDoIt(command)

        if (class(result)[1] != "try-error") {
            if (!getRcmdr("dataset.modified")) return()
            .data <- try(get(dsnameValue, envir = .GlobalEnv), silent = TRUE)
            if (nrow(.data) == 0) {
                errorCondition(recall = newDataSet,
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
    OKCancelHelp(helpSubject = "editDataset")
    tkgrid(labelRcmdr(top,
                      text = gettextRcmdr("Enter name for data set:")),
           entryDsname,
           sticky = "e")

    tkgrid(buttonsFrame, columnspan = "2", sticky = "w")
    tkgrid.configure(entryDsname, sticky = "w")
    dialogSuffix(focus = entryDsname)
}