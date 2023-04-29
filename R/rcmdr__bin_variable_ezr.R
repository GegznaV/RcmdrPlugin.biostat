# Functions are based on RcmdrPlugin.EZR functions
# window_bin_variable_manual() and window_bin_variable_manual2()
# (revision by V. Gegzna, 2019-12-17)
window_bin_variable_manual <- function() {
  initializeDialog(title = gettext(domain = "R-RcmdrPlugin.biostat",
    "Bin numeric variable with specified threshold [EZR]"))
  dataSet <- activeDataSet()
  variablesBox <- variableListBox(top, Numeric(), title = gettext(domain = "R-RcmdrPlugin.biostat", "Select one numeric variable"), listHeight = 15)
  newVariableName <- tclVar("")
  newVariableNameEntry <- ttkentry(top, width = "20", textvariable = newVariableName)
  threshold <- tclVar("")
  thresholdEntry <- ttkentry(top, width = "20", textvariable = threshold)
  radioButtons(name = "grouping", buttons = c("equalgreater", "greater"), values = c(">=", ">"),
    labels = gettext(domain = "R-RcmdrPlugin.biostat", c(">= (equal to or greater than)", "> (greater than)")), title = gettext(domain = "R-RcmdrPlugin.biostat", "Threshold"))
  onOK <- function() {
    logger(paste("## ", gettext(domain = "R-RcmdrPlugin.biostat",
      "Bin numeric variable with specified threshold"), sep = ""))
    var <- trim.blanks(getSelection(variablesBox))
    if (length(var) == 0) {
      errorCondition(recall = window_bin_variable_manual, message = gettext(domain = "R-RcmdrPlugin.biostat", "You must select a variable."))
      return()
    }
    newVar <- trim.blanks(tclvalue(newVariableName))
    if (!is.valid.name(newVar)) {
      errorCondition(recall = window_bin_variable_manual,
        message = paste('"', newVar, '" ', gettext(domain = "R-RcmdrPlugin.biostat", "is not a valid name."), sep = ""))
      return()
    }
    threshold <- tclvalue(threshold)
    if (length(threshold) == 0) {
      errorCondition(recall = window_bin_variable_manual, message = gettext(domain = "R-RcmdrPlugin.biostat", "Input threshold to bin a numeric variable."))
      return()
    }
    grouping <- as.character(tclvalue(groupingVariable))
    closeDialog()
    command <-  paste(dataSet, "$", newVar, " <- ifelse(", dataSet, "$", var, grouping, threshold, ", 1 , 0)", sep = "")
    logger(command)
    result <- justDoIt(command)
    if (class(result)[1] !=  "try-error") activeDataSet(dataSet, flushModel = FALSE)
    # 		logger(paste("#", gettext(domain="R-RcmdrPlugin.biostat","New variable"), " ", newVar, " ", gettext(domain="R-RcmdrPlugin.biostat","was made."), "(", threshold, gettext(domain="R-RcmdrPlugin.biostat","<=:1, >:0"), sep="") )
    logger(paste("#", gettext(domain = "R-RcmdrPlugin.biostat", "New variable"), " ", newVar, " ", gettext(domain = "R-RcmdrPlugin.biostat", "was made."), sep = ""))
    doItAndPrint(paste("table(", dataSet, "$", newVar, ", exclude=NULL)", sep = ""))
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "ifelse")
  tkgrid(getFrame(variablesBox), sticky = "nw")
  tkgrid(tklabel(top, text = gettext(domain = "R-RcmdrPlugin.biostat", "New variable name")), newVariableNameEntry, sticky = "w")
  tkgrid(tklabel(top, text = gettext(domain = "R-RcmdrPlugin.biostat", "Threshold to bin a numeric variable.")), thresholdEntry, sticky = "w")
  tkgrid.configure(newVariableNameEntry, sticky = "w")
  tkgrid.configure(thresholdEntry, sticky = "w")
  tkgrid(groupingFrame, sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
  dialogSuffix(rows = 3, columns = 2)
}


window_bin_variable_manual2 <- function() {
  initializeDialog(title = gettext(domain = "R-RcmdrPlugin.biostat",
    "Bin numeric variable to more than 2 groups with specified thresholds [EZR]"))
  dataSet <- activeDataSet()
  variablesBox <- variableListBox(top, Numeric(), title = gettext(domain = "R-RcmdrPlugin.biostat", "Select one numeric variable"), listHeight = 15)

  thresholdFrame <- tkframe(top)
  newVariableNameFrame <- tkframe(thresholdFrame)
  newVariableNameVariable <- tclVar("")
  newVariableNameField <- ttkentry(thresholdFrame, width = "20", textvariable = newVariableNameVariable)
  threshold1Frame <- tkframe(thresholdFrame)
  threshold1Variable <- tclVar("")
  threshold1Field <- ttkentry(thresholdFrame, width = "10", textvariable = threshold1Variable)
  threshold2Frame <- tkframe(thresholdFrame)
  threshold2Variable <- tclVar("")
  threshold2Field <- ttkentry(thresholdFrame, width = "10", textvariable = threshold2Variable)
  threshold3Frame <- tkframe(thresholdFrame)
  threshold3Variable <- tclVar("")
  threshold3Field <- ttkentry(thresholdFrame, width = "10", textvariable = threshold3Variable)
  threshold4Frame <- tkframe(thresholdFrame)
  threshold4Variable <- tclVar("")
  threshold4Field <- ttkentry(thresholdFrame, width = "10", textvariable = threshold4Variable)
  threshold5Frame <- tkframe(thresholdFrame)
  threshold5Variable <- tclVar("")
  threshold5Field <- ttkentry(thresholdFrame, width = "10", textvariable = threshold5Variable)
  levelname1Frame <- tkframe(thresholdFrame)
  levelname1Variable <- tclVar("<no group>")
  levelname1Field <- ttkentry(thresholdFrame, width = "20", textvariable = levelname1Variable)
  levelname2Frame <- tkframe(thresholdFrame)
  levelname2Variable <- tclVar("<no group>")
  levelname2Field <- ttkentry(thresholdFrame, width = "20", textvariable = levelname2Variable)
  levelname3Frame <- tkframe(thresholdFrame)
  levelname3Variable <- tclVar("<no group>")
  levelname3Field <- ttkentry(thresholdFrame, width = "20", textvariable = levelname3Variable)
  levelname4Frame <- tkframe(thresholdFrame)
  levelname4Variable <- tclVar("<no group>")
  levelname4Field <- ttkentry(thresholdFrame, width = "20", textvariable = levelname4Variable)
  levelname5Frame <- tkframe(thresholdFrame)
  levelname5Variable <- tclVar("<no group>")
  levelname5Field <- ttkentry(thresholdFrame, width = "20", textvariable = levelname5Variable)
  levelname6Frame <- tkframe(thresholdFrame)
  levelname6Variable <- tclVar("<no group>")
  levelname6Field <- ttkentry(thresholdFrame, width = "20", textvariable = levelname6Variable)
  radioButtons(name = "grouping", buttons = c("equalgreater", "greater"), values = c(">=", ">"),
    labels = gettext(domain = "R-RcmdrPlugin.biostat", c(">= (equal to or greater than)", "> (greater than)")), title = gettext(domain = "R-RcmdrPlugin.biostat", "Threshold"))

  onOK <- function() {
    logger(paste("## ", gettext(domain = "R-RcmdrPlugin.biostat", "Bin numeric variable to more than 2 groups with specified thresholds"), sep = ""))
    var <- trim.blanks(getSelection(variablesBox))
    if (length(var) == 0) {
      errorCondition(recall = window_bin_variable_manual2, message = gettext(domain = "R-RcmdrPlugin.biostat", "You must select a variable."))
      return()
    }
    newVar <- trim.blanks(tclvalue(newVariableNameVariable))
    if (!is.valid.name(newVar)) {
      errorCondition(recall = window_bin_variable_manual2,
        message = paste('"', newVar, '" ', gettext(domain = "R-RcmdrPlugin.biostat", "is not a valid name."), sep = ""))
      return()
    }
    threshold1 <- tclvalue(threshold1Variable)
    threshold2 <- tclvalue(threshold2Variable)
    threshold3 <- tclvalue(threshold3Variable)
    threshold4 <- tclvalue(threshold4Variable)
    threshold5 <- tclvalue(threshold5Variable)
    levelname1 <- tclvalue(levelname1Variable)
    levelname2 <- tclvalue(levelname2Variable)
    levelname3 <- tclvalue(levelname3Variable)
    levelname4 <- tclvalue(levelname4Variable)
    levelname5 <- tclvalue(levelname5Variable)
    levelname6 <- tclvalue(levelname6Variable)
    grouping <- as.character(tclvalue(groupingVariable))
    if (grouping == ">=") {
      right <- ", right=FALSE)"
    } else {
      right <- ", right=TRUE)"
    }
    levels <- 0
    breaks <- ", breaks=c(-Inf, "
    labels <- ", labels=c("
    if (levelname1 == "<no group>") {
      errorCondition(recall = window_bin_variable_manual2, message = gettext(domain = "R-RcmdrPlugin.biostat", "Input at least two groups."))
      return()
    } else {
      levels <- levels + 1
      labels <- paste(labels, '"', levelname1, '"', sep = "")
    }
    if (levelname2 != "<no group>") {
      if (length(threshold1) == 0) {
        errorCondition(recall = window_bin_variable_manual2, message = gettext(domain = "R-RcmdrPlugin.biostat", "Input threshold to bin a numeric variable."))
        return()
      }
      levels <- levels + 1
      breaks <- paste(breaks, threshold1, sep = "")
      labels <- paste(labels, ', "', levelname2, '"', sep = "")
    }
    if (levelname3 != "<no group>") {
      if (length(threshold2) == 0) {
        errorCondition(recall = window_bin_variable_manual2, message = gettext(domain = "R-RcmdrPlugin.biostat", "Input threshold to bin a numeric variable."))
        return()
      }
      levels <- levels + 1
      breaks <- paste(breaks, ", ", threshold2, sep = "")
      labels <- paste(labels, ', "', levelname3, '"', sep = "")
    }
    if (levelname4 != "<no group>") {
      if (length(threshold3) == 0) {
        errorCondition(recall = window_bin_variable_manual2, message = gettext(domain = "R-RcmdrPlugin.biostat", "Input threshold to bin a numeric variable."))
        return()
      }
      levels <- levels + 1
      breaks <- paste(breaks, ", ", threshold3, sep = "")
      labels <- paste(labels, ', "', levelname4, '"', sep = "")
    }
    if (levelname5 != "<no group>") {
      if (length(threshold4) == 0) {
        errorCondition(recall = window_bin_variable_manual2, message = gettext(domain = "R-RcmdrPlugin.biostat", "Input threshold to bin a numeric variable."))
        return()
      }
      levels <- levels + 1
      breaks <- paste(breaks, ", ", threshold4, sep = "")
      labels <- paste(labels, ', "', levelname5, '"', sep = "")
    }
    if (levelname6 != "<no group>") {
      if (length(threshold5) == 0) {
        errorCondition(recall = window_bin_variable_manual2, message = gettext(domain = "R-RcmdrPlugin.biostat", "Input threshold to bin a numeric variable."))
        return()
      }
      levels <- levels + 1
      breaks <- paste(breaks, ", ", threshold5, sep = "")
      labels <- paste(labels, ', "', levelname6, '"', sep = "")
    }
    if (levels < 2) {
      errorCondition(recall = window_bin_variable_manual2, message = gettext(domain = "R-RcmdrPlugin.biostat", "Input at least two groups."))
      return()
    }
    breaks <- paste(breaks, ", Inf)", sep = "")
    labels <- paste(labels, ")", sep = "")
    closeDialog()
    command <-  paste(dataSet, "$", newVar, " <- cut(", dataSet, "$", var, breaks, labels, right, sep = "")
    logger(command)
    result <- justDoIt(command)
    if (class(result)[1] !=  "try-error") activeDataSet(dataSet, flushModel = FALSE)
    logger(paste("#", gettext(domain = "R-RcmdrPlugin.biostat", "New variable"), " ", newVar, " ", gettext(domain = "R-RcmdrPlugin.biostat", "was made."), sep = ""))
    doItAndPrint(paste("table(", dataSet, "$", newVar, ", exclude=NULL)", sep = ""))
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "ifelse")
  tkgrid(getFrame(variablesBox), sticky = "nw")
  tkgrid(labelRcmdr(newVariableNameFrame, text = gettext(domain = "R-RcmdrPlugin.biostat", "New variable name:")), newVariableNameField, sticky = "w")
  tkgrid(newVariableNameFrame, labelRcmdr(thresholdFrame, text = "  "), sticky = "w")

  tkgrid(labelRcmdr(thresholdFrame, text = gettext(domain = "R-RcmdrPlugin.biostat", "Input thresholds and level names."), fg = "blue"), sticky = "w")

  tkgrid(labelRcmdr(levelname1Frame, text = paste(gettext(domain = "R-RcmdrPlugin.biostat", "Level group name"), " 1:", sep = "")), levelname1Field, sticky = "w")
  tkgrid(levelname1Frame, labelRcmdr(thresholdFrame, text = "  "), sticky = "w")
  tkgrid(labelRcmdr(levelname2Frame, text = paste(gettext(domain = "R-RcmdrPlugin.biostat", "Level group name"), " 2:", sep = "")), levelname2Field, sticky = "w")
  tkgrid(labelRcmdr(threshold1Frame, text = paste(gettext(domain = "R-RcmdrPlugin.biostat", "Threshold"), " 1:", sep = "")), threshold1Field, sticky = "w")
  tkgrid(levelname2Frame, labelRcmdr(thresholdFrame, text = "  "), threshold1Frame, sticky = "w")
  tkgrid(labelRcmdr(levelname3Frame, text = paste(gettext(domain = "R-RcmdrPlugin.biostat", "Level group name"), " 3:", sep = "")), levelname3Field, sticky = "w")
  tkgrid(labelRcmdr(threshold2Frame, text = paste(gettext(domain = "R-RcmdrPlugin.biostat", "Threshold"), " 2:", sep = "")), threshold2Field, sticky = "w")
  tkgrid(levelname3Frame, labelRcmdr(thresholdFrame, text = "  "), threshold2Frame, sticky = "w")
  tkgrid(labelRcmdr(levelname4Frame, text = paste(gettext(domain = "R-RcmdrPlugin.biostat", "Level group name"), " 4:", sep = "")), levelname4Field, sticky = "w")
  tkgrid(labelRcmdr(threshold3Frame, text = paste(gettext(domain = "R-RcmdrPlugin.biostat", "Threshold"), " 3:", sep = "")), threshold3Field, sticky = "w")
  tkgrid(levelname4Frame, labelRcmdr(thresholdFrame, text = "  "), threshold3Frame, sticky = "w")
  tkgrid(labelRcmdr(levelname5Frame, text = paste(gettext(domain = "R-RcmdrPlugin.biostat", "Level group name"), " 5:", sep = "")), levelname5Field, sticky = "w")
  tkgrid(labelRcmdr(threshold4Frame, text = paste(gettext(domain = "R-RcmdrPlugin.biostat", "Threshold"), " 4:", sep = "")), threshold4Field, sticky = "w")
  tkgrid(levelname5Frame, labelRcmdr(thresholdFrame, text = "  "), threshold4Frame, sticky = "w")
  tkgrid(labelRcmdr(levelname6Frame, text = paste(gettext(domain = "R-RcmdrPlugin.biostat", "Level group name"), " 6:", sep = "")), levelname6Field, sticky = "w")
  tkgrid(labelRcmdr(threshold5Frame, text = paste(gettext(domain = "R-RcmdrPlugin.biostat", "Threshold"), " 5:", sep = "")), threshold5Field, sticky = "w")
  tkgrid(levelname6Frame, labelRcmdr(thresholdFrame, text = "  "), threshold5Frame, sticky = "w")
  tkgrid(thresholdFrame, sticky = "w")
  tkgrid(groupingFrame, sticky = "nw")

  tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
  dialogSuffix(rows = 3, columns = 2)
}
