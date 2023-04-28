
# Author: Dan Putler
# (revision by J. Fox, 2 Feb 05)
#
# Function is based on Rcmdr function binVariable()
# (revision by V. Gegzna, 2019-12-17)
window_bin_variable <- function() {
  defaults <- list(initial.levels = "ranges", initial.bins = "3", initial.varName = NULL,
    initial.newVar = "variable", initial.method = "intervals")
  dialog.values <- getDialog("window_bin_variable", defaults)
  env <- environment()
  initializeDialog(title = gettextRcmdr("Bin a Numeric Variable [Rcmdr]"))
  variableFrame <- tkframe(top)
  variableBox <- variableListBox(variableFrame, Numeric(),
    title = gettextRcmdr("Variable to bin (pick one)"),
    initialSelection = varPosn(dialog.values$initial.varName, "numeric"))
  newVariableFrame <- tkframe(variableFrame)
  newVariableName <- tclVar(dialog.values$initial.newVar)
  newVariable <- ttkentry(newVariableFrame, width = "18", textvariable = newVariableName)
  binsFrame <- tkframe(top)
  binsVariable <- tclVar(dialog.values$initial.bins)
  slider <- tkscale(binsFrame, from = 2, to = 30, showvalue = TRUE, length = 200,
    variable = binsVariable, resolution = 1, orient = "horizontal")
  optionsFrame <- tkframe(top)
  radioButtons(optionsFrame, name = "levels",
    buttons = c("specify", "numbers", "ranges"),
    labels = gettextRcmdr(c("Specify names", "Numbers", "Ranges")),
    title = gettextRcmdr("Level Names"),
    initialValue = dialog.values$initial.levels)

  radioButtons(optionsFrame, name = "method",
    buttons = c("intervals", "proportions", "natural"),
    labels = gettextRcmdr(c("Equal-width bins", "Equal-count bins", "Natural breaks\n(from K-means clustering)")),
    title = gettextRcmdr("Binning Method"),
    initialValue = dialog.values$initial.method)

  onOK <- function() {
    levels <- tclvalue(levelsVariable)
    bins <- as.numeric(tclvalue(binsVariable))
    varName <- getSelection(variableBox)
    closeDialog()
    if (length(varName) == 0) {
      errorCondition(recall = window_bin_variable, message = gettextRcmdr("You must select a variable."))
      return()
    }
    newVar <- tclvalue(newVariableName)
    if (is.element(newVar, Variables())) {
      if ("no" == tclvalue(checkReplace(newVar))) {
        window_bin_variable()
        return()
      }
    }
    if (!is.valid.name(newVar)) {
      errorCondition(message = paste("\"", newVar, "\" ",
        gettextRcmdr("is not a valid name."), sep = ""),
      recall = window_bin_variable)
      return()
    }
    method <- tclvalue(methodVariable)
    putDialog ("window_bin_variable", list (initial.levels = levels, initial.bins = bins, initial.varName = varName,
      initial.newVar = newVar, initial.method = method))
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
        valVar <- paste("levelName", i, sep = "")
        assign(valVar, tclVar(i))
        assign(paste("entry", i, sep = ""), ttkentry(subdialog,
          width = "20", textvariable = get(valVar)))
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
    command <- paste(.ds, "$", newVar, " <- ",
      "with(", .ds, ", RcmdrMisc::binVariable(", varName, ", bins=",
      bins, ", method=", "'", method, "', labels=", labels,
      "))", sep = "")
    logger(command)
    result <- justDoIt(command)
    if (class(result)[1] != "try-error")
      activeDataSet(.ds, flushModel = FALSE,
        flushDialogMemory = FALSE)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "binVariable", reset = "window_bin_variable")
  tkgrid(labelRcmdr(newVariableFrame, text = gettextRcmdr("New variable name"),
    fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(newVariable, sticky = "w")
  tkgrid(getFrame(variableBox), labelRcmdr(variableFrame, text = "    "),
    newVariableFrame, sticky = "nw")
  tkgrid(variableFrame, sticky = "w")
  tkgrid(labelRcmdr(binsFrame, text = gettextRcmdr("Number of bins:")),
    slider, sticky = "s")
  tkgrid(binsFrame, sticky = "w")
  tkgrid(levelsFrame, labelRcmdr(optionsFrame, text = "    "),
    methodFrame, sticky = "nw")
  tkgrid(optionsFrame, sticky = "w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix()
}
