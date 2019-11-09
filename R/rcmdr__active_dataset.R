#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Imported from Rcmdr
#
# Modified version of function `activeDataSet` from package Rcmdr 2.5-1
active_dataset <- function(dsname, flushModel = TRUE, flushDialogMemory = TRUE) {
  .ds <- active_dataset_0()

  if (missing(dsname)) {
    if (is.null(.ds)) {

      msg_1 <- gettext_bs("There is no active data set.")
      Message(message = msg_1, type = "error")
      ans <- tk_messageBox(
        parent  = CommanderWindow(),
        caption = 'No Active Dataset',
        message = msg_1,
        icon    = "error",
        type    = "ok")

      return(FALSE)
    } else {
      return(.ds)
    }
  }

  if (!is.data.frame(ds <- get(dsname, envir = .GlobalEnv))) {
    if (!exists.method("as.data.frame", ds, default = FALSE)) {
      Message(message = paste0(
        dsname, gettext_bs(" is not a data frame and cannot be attached.")),
        type = "error")
      tkfocus(CommanderWindow())
      return()
    }

    command <- str_glue("{dsname} <- as.data.frame({dsname})")
    justDoIt(command)
    logger(command)

    Message(
      message = str_glue(gettext_bs("Dataset `{dsname}` has been coerced to a data frame.")),
      type = "warning"
    )
  }

  varnames <- names(get(dsname, envir = .GlobalEnv))
  newnames <- make.names(varnames)  # TODO avoid make.names <------------- ???
  badnames <- varnames != newnames

  if (any(badnames)) {

    old_bad_names  <- paste0(varnames[badnames], collapse = ", ")
    new_good_names <- paste0(newnames[badnames], collapse = ", ")

    warn_msg <- str_glue(
      "Dataset `{dsname}`",
      gettext_bs(" contains non-standard variable names:\n\n"),
      stringr::str_trunc(old_bad_names, 245),
      gettext_bs("\n\nR Commander may not work properly, if these names are not corrected. "),
      gettext_bs("Do you AGREE to change the names into correct ones:\n\n"),
      stringr::str_trunc(new_good_names, 245),
    )

    ans <- tk_messageBox(
      parent  = CommanderWindow(),
      caption = 'Column Names Need Correction',
      message = warn_msg,
      icon    = "warning",
      type    = "yesno",
      default = "yes")

    if (ans == "yes") {
      command <- str_glue("## Make syntactically correct variable names\n",
                          "names({dsname}) <- make.names(names({dsname}))")
      doItAndPrint(command)
    }
  }

  if (!is.null(.ds) && getRcmdr("attach.data.set") && (length(grep(.ds, search())) != 0)) {
    detach(pos = match(.ds, search()))
    logger(str_glue("detach({.ds})"))
  }

  if (flushModel) {
    putRcmdr(".activeModel", NULL)
    RcmdrTclSet("modelName", gettext_bs("<No active model>"))
    tkconfigure(getRcmdr("modelLabel"), foreground = "red")
  }

  if (flushDialogMemory) putRcmdr("dialog.values", list())

  active_dataset_0(dsname)

  nrow <- nrow(get(dsname, envir = .GlobalEnv))
  ncol <- ncol(get(dsname, envir = .GlobalEnv))
  putRcmdr("nrow", nrow)
  putRcmdr("ncol", ncol)

  Message(sprintf(
    gettext_bs("The dataset `%s` has %d rows and %d columns."), dsname, nrow, ncol),
    type = "note")

  if (any(badnames)) {
    if (ans == "yes") {
      Message(message = "Some variable names were corrected.", type = "warning")

    } else {
      Message(
        message = paste0("Some variable names are not standard and this ",
                         "may cause issues working in R Commander."),
        type = "warning")
    }
  }

  RcmdrTclSet("dataSetName", paste(" ", dsname, " "))
  tkconfigure(getRcmdr("dataSetLabel"), foreground = "blue")

  activate_menus()

  dsname
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified version of function `ActiveDataSet` from package Rcmdr 2.5-1
active_dataset_0 <- function(name) {
  if (missing(name)) {
    temp <- getRcmdr(".activeDataSet")
    if (is.null(temp)) {
      return(NULL)

    } else if (!exists(temp) || !is.data.frame(get(temp, envir = .GlobalEnv))) {
        Message(sprintf(
          gettextRcmdr("the dataset %s is no longer available"),
          temp
        ), type = "error")

        putRcmdr(".activeDataSet", NULL)
        Variables(NULL)
        Numeric(NULL)
        Factors(NULL)
        TwoLevelFactors(NULL)
        RcmdrTclSet("dataSetName", gettextRcmdr("<No active dataset>"))
        putRcmdr(".activeModel", NULL)
        putRcmdr("nrow", NULL)
        putRcmdr("ncol", NULL)
        RcmdrTclSet("modelName", gettextRcmdr("<No active model>"))
        tkconfigure(getRcmdr("dataSetLabel"), foreground = "red")
        tkconfigure(getRcmdr("modelLabel"),   foreground = "red")
        # activateMenus()
        activate_menus()
        if (getRcmdr("suppress.menus") && RExcelSupported()) return(NULL)
      }
    return(temp)


  } else {
    putRcmdr(".activeDataSet", name)

    if (!is.null(name)) {
      Variables(listVariables(name))
      Numeric(list_numeric(name))
      Factors(list_factors(name))
      TwoLevelFactors(list_two_level_factors(name))
      open.showData.windows <- getRcmdr("open.showData.windows")
      if (!is.null(open.showData.windows) && name %in% names(open.showData.windows)) {
        ID <- open.showData.windows[[name]]$ID
        posn <- as.numeric(c(
          tclvalue(.Tcl(paste("winfo x", ID))),
          tclvalue(.Tcl(paste("winfo y", ID)))
        ))
        posn <- paste("+", paste(posn, collapse = "+"), sep = "")
        tkdestroy(open.showData.windows[[name]])
        suppress <-
          if (getRcmdr("suppress.X11.warnings")) ", suppress.X11.warnings=FALSE" else ""
        view.height <-
          max(as.numeric(getRcmdr("output.height")) +
              as.numeric(getRcmdr("log.height")), 10)
        command <- paste0(
          "showData(", name, ", placement='", posn,
          "', font=getRcmdr('logFont'), maxwidth=",
          getRcmdr("log.width"), ", maxheight=", view.height, suppress, ")"
        )
        window <- justDoIt(command)
        open.showData.windows[[active_dataset_0()]] <- window
        putRcmdr("open.showData.windows", open.showData.windows)
      }

    } else {
      Variables(NULL)
      Numeric(NULL)
      Factors(NULL)
      TwoLevelFactors(NULL)
      RcmdrTclSet("dataSetName", gettextRcmdr("<No active dataset>"))
      putRcmdr(".activeModel", NULL)
      putRcmdr("nrow", NULL)
      putRcmdr("ncol", NULL)
      RcmdrTclSet("modelName", gettextRcmdr("<No active model>"))
      tkconfigure(getRcmdr("dataSetLabel"), foreground = "red")
      tkconfigure(getRcmdr("modelLabel"),   foreground = "red")
      # activateMenus()
      activate_menus()
      if (getRcmdr("suppress.menus") && RExcelSupported()) return(NULL)
    }
  }
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list_numeric <- function(dataSet = active_dataset_0()) {
  if (missing(dataSet)) {
    Numeric()
  }
  else {
    variables <- listVariables(dataSet)
    variables[sapply(variables, function(.x) {
      .v <- eval_text(safe_names(.x), envir = get(dataSet, envir = .GlobalEnv))
      is.numeric(.v)
      })]
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list_factors <- function(dataSet = active_dataset_0()) {
  if (missing(dataSet)) {
    Factors()
  }
  else {
    variables <- listVariables(dataSet)
    variables[sapply(variables, function(.x) {
      .v <- eval_text(safe_names(.x), envir = get(dataSet, envir = .GlobalEnv))
      is.factor(.v) || is.logical(.v) || is.character(.v)
    })]
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list_two_level_factors <- function(dataSet = active_dataset_0()) {
  if (missing(dataSet)) {
    TwoLevelFactors()
  }
  else {
    factors <- list_factors(dataSet)
    if (length(factors) == 0) return(NULL)
    factors[sapply(factors, function(.x) {
      .v <- eval_text(safe_names(.x), envir = get(dataSet, envir = .GlobalEnv))
      # NOTE: length(levels(factor(.v))) == 2  #  is faster than:
      # length(levels(.v)) == 2 || length(na.omit(unique(.v))) == 2
      length(levels(factor(.v))) == 2
    })]
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
