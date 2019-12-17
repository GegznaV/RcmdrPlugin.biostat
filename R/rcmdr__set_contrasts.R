# Function is based on Rcmdr function setContrasts()
# TODO: adapt funtion to the coding conventions of "biostat" plugin

window_set_contrasts <- function() {
  .ds <- ActiveDataSet()

  initializeDialog(title = gettextRcmdr("Set Contrasts for Factor"))
  variableBox <- variableListBox(top, Factors(), title = gettextRcmdr("Factor (pick one)"))
  radioButtons(
    name = "contrasts", buttons = c("treatment", "sum", "helmert", "poly", "specify"),
    values = c("contr.Treatment", "contr.Sum", "contr.helmert", "contr.poly", "specify"),
    labels = gettextRcmdr(c(
      "Treatment (dummy) contrasts",
      "Sum (deviation) contrasts",
      "Helmert contrasts",
      "Polynomial contrasts",
      "Other (specify)"
    )), title = gettextRcmdr("Contrasts"),
    columns = 2
  )
  onOK <- function() {
    variable <- getSelection(variableBox)

    closeDialog()

    if (length(variable) == 0) {
      errorCondition(recall = window_set_contrasts,
        message = gettextRcmdr("You must select a variable."))
      return()
    }
    variable <- safe_names(variable)

    contrasts <- tclvalue(contrastsVariable)
    if (contrasts != "specify") {
      command <- str_glue(
        "## Define contrasts for a factor ({variable}) \n",
        'contrasts({.ds}${variable}) <- "{contrasts}"'
        )
      result <- doItAndPrint(command)
      if (class(result)[1] != "try-error") activeDataSet(.ds)
      tkfocus(CommanderWindow())

    } else {
      initializeDialog(subdialog, title = gettextRcmdr("Specify Contrasts"))
      tkgrid(labelRcmdr(subdialog, text = gettextRcmdr("Enter Contrast Coefficients"),
        fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
      env <- environment()
      tableFrame <- tkframe(subdialog)
      row.names <- str_glue_eval("levels({.ds}${variable})")
      row.names <- substring(paste(abbreviate(row.names, 12), "            "), 1, 12)
      nrows <- length(row.names)
      ncols <- nrows - 1
      make.col.names <- paste0("labelRcmdr(tableFrame, text='", gettextRcmdr("Contrast Name:"), "')")
      for (j in 1:ncols) {
        varname <- paste(".col.", j, sep = "")
        assign(varname, tclVar(str_glue(".{j}")), envir = env)
        make.col.names <-
          str_glue("{make.col.names}, ttkentry(tableFrame, width = '12', textvariable = {varname})")
      }
      str_glue_eval("tkgrid({make.col.names}, sticky = 'w')", envir_eval = env)
      for (i in 1:nrows) {
        make.row <- str_glue("labelRcmdr(tableFrame, text = '{row.names[i]}')")
        for (j in 1:ncols) {
          varname <- str_glue(".tab.{i}.{j}")
          assign(varname, tclVar("0"), envir = env)
          make.row <-
            str_glue("{make.row}, ttkentry(tableFrame, width = '5', textvariable = {varname})")
        }
        str_glue_eval("tkgrid({make.row}, sticky = 'w')", envir_eval = env)
      }
      tkgrid(tableFrame, sticky = "w")

      onOKsub <- function() {
        closeDialog(subdialog)
        cell <- 0
        values <- rep(NA, nrows * ncols)
        for (j in 1:ncols) {
          for (i in 1:nrows) {
            cell <- cell + 1
            varname <- str_glue(".tab.{i}.{j}")
            values[cell] <- as.numeric(str_glue_eval("tclvalue({varname})"))
          }
        }

        values <- na.omit(values)
        if (length(values) != nrows * ncols) {
          errorCondition(subdialog,
            recall = window_set_contrasts,
            message = sprintf(gettextRcmdr(
              "Number of valid entries in contrast matrix(%d)\nnot equal to number of levels (%d) * number of contrasts (%d)."
            ), length(values), nrows, ncols)
          )
          return()
        }

        if (qr(matrix(values, nrows, ncols))$rank < ncols) {
          errorCondition(subdialog, recall = window_set_contrasts,
            message = gettextRcmdr("Contrast matrix is not of full column rank"))
          return()
        }

        contrast.names <- rep("", ncols)
        for (j in 1:ncols) {
          varname <- str_glue(".col.{j}")
          contrast.names[j] <- str_glue_eval("tclvalue({varname})")
        }
        if (length(unique(contrast.names)) < ncols) {
          errorCondition(subdialog, recall = window_set_contrasts, message = gettextRcmdr("Contrast names must be unique"))
          return()
        }

        all_values <- paste(values, collapse = ", ")
        all_contrasts <- paste("'", contrast.names, "'", sep = "", collapse = ", ")

        command <- str_glue(
          "## Define contrasts for a factor ('{variable}') \n",
          ".Contrasts <- matrix(c({all_values}), {nrows}, {ncols}) \n",
          "colnames(.Contrasts) <- c({all_contrasts}) \n",
          "contrasts({.ds}${variable}) <- .Contrasts \n",
          "remove(.Contrasts)"
          )

        result <- doItAndPrint(command)

        if (class(result)[1] != "try-error") {
          activeDataSet(.ds, flushModel = FALSE, flushDialogMemory = FALSE)

        } else {
          remove(.Contrasts, envir = .GlobalEnv)

        }
        tkfocus(CommanderWindow())
      }
      subOKCancelHelp(helpSubject = "contrasts")

      tkgrid(tableFrame, sticky = "w")
      tkgrid(labelRcmdr(subdialog, text = ""))
      tkgrid(subButtonsFrame, sticky = "w")
      dialogSuffix(subdialog, focus = subdialog, force.wait = TRUE)
    }
  }
  ok_cancel_help(helpSubject = "contrasts")
  tkgrid(getFrame(variableBox), sticky = "nw")
  tkgrid(contrastsFrame, sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix()
}
