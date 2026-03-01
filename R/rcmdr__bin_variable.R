
#' Bin a numeric variable into intervals (Rcmdr-based)
#'
#' Based on Rcmdr function `binVariable()`. Authors: Dan Putler, J. Fox.
#'
#' @keywords internal
window_bin_variable <- function() {
  defaults <- list(initial.levels = "ranges", initial.bins = "3", initial.var_name = NULL,
    initial.new_var = "variable", initial.method = "intervals")
  dialog_values <- getDialog("window_bin_variable", defaults)
  env <- environment()
  initializeDialog(title = gettextRcmdr("Bin a Numeric Variable [Rcmdr]"))
  variable_frame <- tkframe(top)
  variable_box <- variableListBox(variable_frame, Numeric(),
    title = gettextRcmdr("Variable to bin (pick one)"),
    initialSelection = varPosn(dialog_values$initial.var_name, "numeric"))
  new_variable_frame <- tkframe(variable_frame)
  new_variable_name <- tclVar(dialog_values$initial.new_var)
  new_variable <- ttkentry(new_variable_frame, width = "18", textvariable = new_variable_name)
  bins_frame <- tkframe(top)
  bins_variable <- tclVar(dialog_values$initial.bins)
  slider <- tkscale(bins_frame, from = 2, to = 30, showvalue = TRUE, length = 200,
    variable = bins_variable, resolution = 1, orient = "horizontal")
  options_frame <- tkframe(top)
  radioButtons(options_frame, name = "levels",
    buttons = c("specify", "numbers", "ranges"),
    labels = gettextRcmdr(c("Specify names", "Numbers", "Ranges")),
    title = gettextRcmdr("Level Names"),
    initialValue = dialog_values$initial.levels)

  radioButtons(options_frame, name = "method",
    buttons = c("intervals", "proportions", "natural"),
    labels = gettextRcmdr(c("Equal-width bins", "Equal-count bins", "Natural breaks\n(from K-means clustering)")),
    title = gettextRcmdr("Binning Method"),
    initialValue = dialog_values$initial.method)

  onOK <- function() {
    levels <- tclvalue(levelsVariable)
    bins <- as.numeric(tclvalue(bins_variable))
    var_name <- getSelection(variable_box)
    closeDialog()
    if (length(var_name) == 0) {
      errorCondition(recall = window_bin_variable, message = gettextRcmdr("You must select a variable."))
      return()
    }
    new_var <- tclvalue(new_variable_name)
    if (is.element(new_var, Variables())) {
      if ("no" == tclvalue(checkReplace(new_var))) {
        window_bin_variable()
        return()
      }
    }
    if (!is.valid.name(new_var)) {
      errorCondition(message = paste("\"", new_var, "\" ",
        gettextRcmdr("is not a valid name."), sep = ""),
      recall = window_bin_variable)
      return()
    }
    method <- tclvalue(methodVariable)
    putDialog ("window_bin_variable", list (initial.levels = levels, initial.bins = bins, initial.var_name = var_name,
      initial.new_var = new_var, initial.method = method))
    if (levels == "specify") {
      initializeDialog(subdialog, title = gettextRcmdr("Bin Names"))
      onOKsub <- function() {
        closeDialog(subdialog)
        level <- character(bins)
        for (i in 1:bins) {
          level[i] <- eval(parse(text = paste("tclvalue(levelName",
            i, ")", sep = "")))
        }
        if (length(unique(level)) != length(level)) {
          errorCondition(window = subdialog, message = gettextRcmdr("Level names must be unique."),
            recall = onOK)
          return()
        }
        assign("levelNames", level, envir = env)
      }
      subOKCancelHelp()
      tkgrid(labelRcmdr(subdialog, text = gettextRcmdr("Bin"),
        fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), labelRcmdr(subdialog, text = gettextRcmdr("Name"),
        fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
      for (i in 1:bins) {
        val_var <- paste("levelName", i, sep = "")
        assign(val_var, tclVar(i))
        assign(paste("entry", i, sep = ""), ttkentry(subdialog,
          width = "20", textvariable = get(val_var)))
        tkgrid(labelRcmdr(subdialog, text = as.character(i)),
          get(paste("entry", i, sep = "")), sticky = "w")
      }
      tkgrid(subButtonsFrame, sticky = "w", columnspan = 2)
      dialogSuffix(subdialog, focus = entry1, bindReturn = FALSE, force.wait = TRUE)
    }
    labels <- if (levels == "numbers")
      "FALSE"
    else if (levels == "ranges")
      "NULL"
    else {
      if (!exists("levelNames")) {
        onCancel()
        window_bin_variable()
        return()
      }
      paste("c('", paste(levelNames, collapse = "','"),
        "')", sep = "")
    }
    .ds <- ActiveDataSet()
    command <- paste(.ds, "$", new_var, " <- ",
      "with(", .ds, ", RcmdrMisc::binVariable(", var_name, ", bins=",
      bins, ", method=", "'", method, "', labels=", labels,
      "))", sep = "")
    logger(command)
    result <- justDoIt(command)
    if (!inherits(result, "try-error"))
      activeDataSet(.ds, flushModel = FALSE,
        flushDialogMemory = FALSE)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "binVariable", reset = "window_bin_variable")
  tkgrid(labelRcmdr(new_variable_frame, text = gettextRcmdr("New variable name"),
    fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(new_variable, sticky = "w")
  tkgrid(getFrame(variable_box), labelRcmdr(variable_frame, text = "    "),
    new_variable_frame, sticky = "nw")
  tkgrid(variable_frame, sticky = "w")
  tkgrid(labelRcmdr(bins_frame, text = gettextRcmdr("Number of bins:")),
    slider, sticky = "s")
  tkgrid(bins_frame, sticky = "w")
  tkgrid(levelsFrame, labelRcmdr(options_frame, text = "    "),
    methodFrame, sticky = "nw")
  tkgrid(options_frame, sticky = "w")
  tkgrid(buttons_frame, sticky = "w")
  dialogSuffix()
}
