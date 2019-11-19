# TODO:
# 1. Simplify the code.
# 2. Update the structure of code for the window.
# 3. Make more informative messages for variable checking procedures.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_variable_rename <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onOK <- function() {
    old_names <- getSelection(var_y_box)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    n_old_names <- length(old_names)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (variable_is_not_selected(old_names, parent = top)) {return()}


    # if (n_old_names < 1) {
    #
    #     errorCondition(
    #         recall = window_variable_rename,
    #         message = gettext_bs("No variables selected.")
    #     )
    #     return()
    # }

    closeDialog()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    .ds <- active_dataset_0()
    unordered_names <- names(get(.ds))
    which_variables <- match(old_names, unordered_names)

    # Subdialog ----------------------------------------------------------
    initializeDialog(subdialog, title = gettext_bs("Change variable names"))
    new_names <- rep("", n_old_names)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOKsub <- function() {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for (i in 1:n_old_names) {
        new_names[i] <- str_glue_eval("tclvalue(newName{i})")
      }
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      closeDialog(subdialog)
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # TODO: [???] warn if any of the names is empty,
      # but allow to continue, if user wants.
      # If the new name is empty, the old name will not be changed.

      # If empty
      if (any(new_names == "")) {
        errorCondition(
          recall = window_variable_rename,
          message = gettext_bs("A variable name is empty.")
        )
        return()
      }
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # TODO: [???] warn if any of the names is incorrect,
      # but allow to continue, if user wants.

      test.names <- new_names == make.names(new_names)

      if (!all(test.names)) {
        errorCondition(recall = window_variable_rename,
          message = paste(
            gettext_bs("The following variable names are not valid:\n"),
            paste(new_names[!test.names], collapse = ", ")))
        return()
      }

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # TODO: [???] show which names are not unique after renaming.

      # Check duplicated names
      all_names <- names(get(.ds))
      all_names[which_variables] <- new_names

      if (any(duplicated(all_names))) {
        errorCondition(
          recall = window_variable_rename,
          message = gettext_bs("Variable names are not unique")
        )
        return()
      }
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      Library("dplyr")

      old_names <- safe_names(old_names)
      new_names <- safe_names(new_names)

      renaming_directives <-
        str_c(str_glue('{new_names} = {old_names}'), collapse = ", \n")

      command <-
        str_glue("## Rename variables\n",
          '# new_name = old_name\n\n',
          "{.ds} <- {.ds} %>% \n",
          'dplyr::rename({renaming_directives})') %>%
        style_cmd()

      result <- justDoIt(command)
      logger(command)

      if (class(result)[1] !=  "try-error")
        active_dataset(.ds, flushModel = FALSE)

      tkfocus(CommanderWindow())
    }
    # Create menus -------------------------------------------------------
    subOKCancelHelp()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(
      tk_label_blue(subdialog, text = gettext_bs("Old Name   ")),
      tk_label_blue(subdialog, text = gettext_bs("New name   ")),
      sticky = "w",
      pady = c(10, 15)
    )

    for (i in 1:n_old_names) {
      valVar <- paste0("newName", i)
      assign(valVar, tclVar(""))
      assign(x = paste0("entry", i),
        value =  ttkentry(subdialog, width = "20", textvariable = get(valVar)
        ))

      tkgrid(labelRcmdr(subdialog, text = old_names[i]),
        get(paste0("entry", i)),
        sticky = "w")
    }

    tkgrid(subButtonsFrame, sticky = "e", columnspan = 2)

    dialogSuffix(
      subdialog,
      rows = n_old_names + 2,
      columns = 2,
      focus = entry1,
      onOK = onOKsub,
      force.wait = TRUE
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  initializeDialog(title = gettext_bs("Rename variables (columns)"))
  tk_title(top, "Rename variables (columns)")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  variables_frame <- tkframe(top)
  var_y_box <-
    bs_listbox(
      parent       = variables_frame,
      title        = gettext_bs("Variables\n(pick one or more)"),
      values       = variables_all(),
      selectmode   = "multiple",
      height       = 10,
      width        = 30,
      on_keyboard  = "select",
      tip          = str_c(
        "Select variables to rename and push 'OK' button.\n\n",
        "Hold 'Ctrl' key and left-click mouse to either \n",
        "select several objects or deselect one.\n",
        "Use letters on the keyboard to navigate quicker.")
    )

  tkgrid(var_y_box$frame, sticky = "w", padx = c(10, 0))


  info_1 <- tk_label(top, text = "Select no more than 30 variables to rename at once.")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ok_cancel_help(helpSubject = "rename", helpPackage = "dplyr")
  tkgrid(variables_frame, columnspan = 2)
  tkgrid(info_1)
  tkgrid(buttonsFrame, sticky = "we")
  dialogSuffix(rows = 2, columns = 1)
}
