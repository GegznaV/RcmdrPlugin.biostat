#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Imported from Rcmdr
active_dataset <- function(dsname, flushModel = TRUE, flushDialogMemory = TRUE) {
  .ds <- ActiveDataSet()

  if (missing(dsname)) {
    if (is.null(.ds)) {
      Message(message = gettextRcmdr("There is no active data set."), type = "error")
      return(FALSE)
    } else {
      return(.ds)
    }
  }

  if (!is.data.frame(ds <- get(dsname, envir = .GlobalEnv))) {
    if (!exists.method("as.data.frame", ds, default = FALSE)) {
      Message(message = paste0(
        dsname, gettextRcmdr(" is not a data frame and cannot be attached.")),
        type = "error")
      tkfocus(CommanderWindow())
      return()
    }

    command <- paste0(dsname, " <- as.data.frame(", dsname, ")")
    justDoIt(command)
    logger(command)

    Message(
      message = paste0(dsname, gettextRcmdr(" has been coerced to a data frame")),
      type = "warning"
    )
  }
  varnames <- names(get(dsname, envir = .GlobalEnv))
  newnames <- make.names(varnames)  # <---------------------------------- ???
  badnames <- varnames != newnames

  if (any(badnames)) {
    command <- paste0("names(", dsname, ") <- make.names(names(", dsname, "))")
    doItAndPrint(command)
  }

  if (!is.null(.ds) && getRcmdr("attach.data.set") && (length(grep(.ds, search())) != 0)) {
    detach(pos = match(.ds, search()))
    logger(paste0("detach(", .ds, ")"))
  }

  if (flushModel) {
    putRcmdr(".activeModel", NULL)
    RcmdrTclSet("modelName", gettextRcmdr("<No active model>"))
    tkconfigure(getRcmdr("modelLabel"), foreground = "red")
  }

  if (flushDialogMemory) putRcmdr("dialog.values", list())

  ActiveDataSet(dsname)

  nrow <- nrow(get(dsname, envir = .GlobalEnv))
  ncol <- ncol(get(dsname, envir = .GlobalEnv))
  putRcmdr("nrow", nrow)
  putRcmdr("ncol", ncol)

  Message(sprintf(
    gettextRcmdr("The dataset %s has %d rows and %d columns."), dsname, nrow, ncol),
    type = "note")

  if (any(badnames)) {
    Message(message = paste(
      dsname,
      gettextRcmdr(" contains non-standard variable names:\n"),
      paste0(varnames[badnames], collapse = ", "),
      gettextRcmdr("\nThese have been changed to:\n"),
      paste(newnames[badnames], collapse = ", ")),
      type = "warning")
  }

  RcmdrTclSet("dataSetName", paste(" ", dsname, " "))
  tkconfigure(getRcmdr("dataSetLabel"), foreground = "blue")
  activateMenus()
  dsname
}