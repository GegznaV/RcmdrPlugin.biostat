#  ===========================================================================
# TODO:
# 1) [+] Add dialogue with warning message if to proceed;
# 2) [ ] Fix issue related to Relative/Absolute path.
# 3) [ ] In the warning message, truncate very long list of variable names,
#       which will be ovewritten,
# 4)  [ ] In the warning message, add number of objects, that will be replaced.
#
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_import_rdata <- function() {
  initial_dir <- getwd()

  file <- tclvalue(tkgetOpenFile(
    parent = CommanderWindow(),
    title = "Choose R-data File to Import",
    initialdir  = initial_dir,
    multiple = FALSE,
    filetypes = gettext_bs("{ {R-data Files} {.RData .Rda} } { {All Files} * }")))

  if (file == "") {
    tkfocus(CommanderWindow())
    return()
  }

  # Check names in RData file --------------------------------------------------
  rdata_names       <- load(file, envir = new.env())
  current_obj_names <- objects(envir = .GlobalEnv, all.names = TRUE)
  overwrite_names   <- rdata_names[rdata_names %in% current_obj_names]


  if (length(overwrite_names) > 0) {
    o_names  <- str_c(overwrite_names, collapse = ", ")
    warn_msg <- str_c(
      "In your R workspace, the following objects \n",
      "already exist and will be overwritten: \n\n",
      o_names,
      "\n\nDo you want to proceed?"
    )

    choice <- tk_messageBox(
      parent = CommanderWindow(), type = "yesno", default = "no",
      message = warn_msg, icon = "warning",
      caption = "Import and Overwrite"
    )

    if (choice == "no") {
      Message("Data import was canceled.", type = "warn")
      return()
    }
  }

  # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cursor_set_busy()
  on.exit(cursor_set_idle())

  # Change these lines [!!!]: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  make_relative <- TRUE

  if (make_relative) {
    file <- make_relative_path(file)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  command <- str_glue(
    "## Import data from R-data file\n",
    'load("{file}")'
  )

  dsname <- justDoIt(command)
  logger(command)

  # --------------------------------------------------------------------------
  # if (class(dsname)[1] != "try-error") {
  #     Message("...", type = "error")
  #     return()
  # }

  if (length(dsname) == 1) {
    active_dataset(dsname)

  } else {
    Message(
      message = paste(
        gettext_bs("More than one object was imported. Object names:\n"),
        paste(dsname, collapse = ", ")
      ),
      type = "note")
  }

  if (length(overwrite_names) > 0) {
    warn_msg2 <- str_c(
      "\nThe following objects were overwritten: \n",
      o_names
    )
    Message(warn_msg2, type = "warning")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkfocus(CommanderWindow())
}
