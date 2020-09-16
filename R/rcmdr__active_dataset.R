# TODO: Check if code `putRcmdr("bs_dataset_and_col_names", ds_names_current)`
#       is in all places it should be in.

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Imported from Rcmdr
#
# Modified version of function `activeDataSet` from package Rcmdr 2.5-1.
# Updated according to Rcmdr 2.7-0
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
    doItAndPrint(command)

    Message(
      message = str_glue(gettext_bs("Dataset `{dsname}` has been coerced to a data frame.")),
      type = "warning"
    )
  }

  varnames <- names(get(dsname, envir = .GlobalEnv))
  newnames <- make.names(varnames)  # FIXME avoid make.names <------------- ???
  badnames <- varnames != newnames

  # To prevent repeated messages for the same data
  ds_names_current  <- c(dsname, varnames)
  ds_names_previous <- getRcmdr("bs_dataset_and_col_names", fail = FALSE)
  putRcmdr("bs_dataset_and_col_names", ds_names_current)

  suggest_fixing_names <-
    any(badnames) && !identical(ds_names_previous, ds_names_current)

  if (suggest_fixing_names) {

    str_l <- 30  # Default length of a bad name (will be padded to this length)
    n_max <- 8   # Number or names to display

    old_bad_names  <- safe_names(varnames[badnames])
    new_good_names <-            newnames[badnames]

    n <- length(old_bad_names)
    if (n > n_max) {l <- n_max} else {l <- n}

    old_bad_names  <- old_bad_names[1:l]
    new_good_names <- new_good_names[1:l]

    space <- ifelse(str_length(old_bad_names) < (str_l - 2), "\t", "   ")

    str_fixed <- str_c(str_pad(old_bad_names, str_l, "right"), space, "->   ",
      new_good_names, collapse = "\n")

    warn_msg <- gettext_bs(str_glue(
      "Dataset `{dsname}` contains {n} non-standard variable name(s). ",
      "Due tho this fact, some functions in R Commander may fail. ",
      "It is recommended to use syntactically correct name(s), e.g.:\n\n",
      "{str_fixed} \n\n",
      "Do you agree to fix the name(s) now?",
    ))

    ans <- tk_messageBox(
      parent  = CommanderWindow(),
      caption = 'Column Names Need Correction',
      message = warn_msg,
      icon    = "warning",
      type    = "yesno",
      default = "yes")

    if (ans == "yes") {
      command <- str_glue(
        "## Make syntactically correct variable names\n",
        # "{dsname} <- janitor::clean_names({dsname})"
        "names({dsname}) <- make.names(names({dsname}))"
      )
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

  if (suggest_fixing_names) {
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

      putRcmdr("bs_dataset_and_col_names", NULL)
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
      if (packageVersion("Rcmdr") >= "2.7.0") {
        DiscreteNumeric(list_discrete_numeric(name))
        Character(list_character(name))
      }

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
      putRcmdr("bs_dataset_and_col_names", NULL)
      Variables(NULL)
      Numeric(NULL)
      Factors(NULL)
      TwoLevelFactors(NULL)
      if (packageVersion("Rcmdr") >= "2.7.0") {
        DiscreteNumeric(NULL)
        Character(NULL)
      }
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
# Based on Rcmdr v2.5-1
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
# Based on Rcmdr v2.7-0
list_character <- function(dataSet = active_dataset_0()) {
  if (missing(dataSet)) {
    Character()
  }
  else {
    variables <- listVariables(dataSet)
    variables[sapply(variables, function(.x) {
      .v <- eval_text(safe_names(.x), envir = get(dataSet, envir = .GlobalEnv))
      is.character(.v)
    })]
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Based on Rcmdr v2.5-1
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
# Based on Rcmdr v2.5-1
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
# Based on Rcmdr v2.7-0
list_discrete_numeric <- function(dataSet = active_dataset_0()) {
  if (missing(dataSet)) {
    DiscreteNumeric()
  }
  else {
    threshold <- getRcmdr("discreteness.threshold")
    if (threshold <= 0) {
      n <- getRcmdr("nrow")
      if (is.null(n)) {
        n <- nrow(get(dataSet, envir = .GlobalEnv))
      }
      threshold <- min(round(2 * sqrt(n)), round(10 * log10(n)), 100)
    }
    variables <- list_numeric()
    if (length(variables) == 0) {return(NULL)}
    variables[sapply(variables, function(.x) {
      length(
        unique(
          eval_text(safe_names(.x), envir = get(dataSet, envir = .GlobalEnv))
        )
      ) <= threshold
    })]
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
