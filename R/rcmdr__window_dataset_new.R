#' command_new_dataset
#'
#' Code of this function is taken from `Rcmdr` package and slightly modified.
#'
#' @export
#' @keywords internal
window_dataset_new_rcmdr <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Window to choose dataset's name

  initializeDialog(title = gettext_bs("Create New Dataset"))
  dsname <- tclVar(unique_df_name("dataset", all_numbered = TRUE))
  entryDsname <- ttkentry(top, width = "30", textvariable = dsname)

  onOK <- function() {
    dsnameValue <- trim.blanks(tclvalue(dsname))

    # Checks if no name is entered
    if (dsnameValue == "") {
      errorCondition(
        recall = window_new_dataset_rcmdr,
        message = gettext_bs("You must enter the name of the dataset."))
      return()
    }

    # Check validity of the entered name
    if (!is.valid.name(dsnameValue)) {
      errorCondition(
        recall = window_new_dataset_rcmdr,
        message = str_glue('"{dsnameValue}" ',
          gettext_bs("is not a valid name for a dataset."))
      )
      return()
    }

    # Check if a dataset with the same name exists in the workspace
    if (is.element(dsnameValue, listDataSets())) {
      if ("no" == tclvalue(checkReplace(dsnameValue, gettext_bs("Dataset")))) {
        window_new_dataset_rcmdr()
        return()
      }
    }
    closeDialog()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Edit window
    command <- str_glue("Rcmdr::editDataset(dsname = '{dsnameValue}')")
    result <- justDoIt(command)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (inherits(result, "try-error")) {
      if (!getRcmdr("dataset.modified")) return()
      .data <- try(get(dsnameValue, envir = .GlobalEnv), silent = TRUE)
      if (nrow(.data) == 0) {
        errorCondition(recall = window_new_dataset_rcmdr,
          message = gettext_bs("empty data set."))
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
  ok_cancel_help(helpSubject = "editDataset")
  tkgrid(labelRcmdr(
    top,
    text = gettext_bs("Enter dataset's name:   ")),
  entryDsname,
  pady = c(5, 5),
  sticky = "e")

  tkgrid(buttonsFrame, columnspan = "2", sticky = "ew")
  tkgrid.configure(entryDsname, sticky = "w")
  dialogSuffix(focus = entryDsname)
}
