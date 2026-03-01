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
  entry_dsname <- ttkentry(top, width = "30", textvariable = dsname)

  onOK <- function() {
    dsname_value <- trim.blanks(tclvalue(dsname))

    # Checks if no name is entered
    if (dsname_value == "") {
      errorCondition(
        recall = window_dataset_new_rcmdr,
        message = gettext_bs("You must enter the name of the dataset."))
      return()
    }

    # Check validity of the entered name
    if (!is.valid.name(dsname_value)) {
      errorCondition(
        recall = window_dataset_new_rcmdr,
        message = str_glue('"{dsname_value}" ',
          gettext_bs("is not a valid name for a dataset."))
      )
      return()
    }

    # Check if a dataset with the same name exists in the workspace
    if (is.element(dsname_value, listDataSets())) {
      if ("no" == tclvalue(checkReplace(dsname_value, gettext_bs("Dataset")))) {
        window_dataset_new_rcmdr()
        return()
      }
    }
    closeDialog()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Edit window
    command <- str_glue("Rcmdr::editDataset(dsname = '{dsname_value}')")
    result <- justDoIt(command)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (inherits(result, "try-error")) {
      if (!getRcmdr("dataset.modified")) return()
      .data <- try(get(dsname_value, envir = .GlobalEnv), silent = TRUE)
      if (nrow(.data) == 0) {
        errorCondition(recall = window_dataset_new_rcmdr,
          message = gettext_bs("empty data set."))
        return()
      }
      tempdir <- tempdir()
      tempdir <- gsub("\\\\", "/", tempdir)
      savefile <- paste0(tempdir, "/", dsname_value)
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
  entry_dsname,
  pady = c(5, 5),
  sticky = "e")

  tkgrid(buttons_frame, columnspan = "2", sticky = "ew")
  tkgrid.configure(entry_dsname, sticky = "w")
  dialogSuffix(focus = entry_dsname)
}
