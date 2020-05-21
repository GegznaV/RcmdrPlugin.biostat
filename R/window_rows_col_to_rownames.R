#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Correctly initializes window `window_rows_col_to_rownames()`
window_rows_col_to_rownames0  <- function() {
  window_rows_col_to_rownames()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# new_dsname (character) - data frame name
# init_conditions (character) - conditions to be evaluated to select rows
# incorrect_cond_msg (character) - Message for incorrect expression.
window_rows_col_to_rownames <- function(new_dsname = NULL,
  init_conditions = NULL,
  incorrect_cond_msg = NULL) {

  # Dialog -----------------------------------------------------------------

  initializeDialog(title = gettext_bs("Set Row Names"))
  tk_title(top, gettext_bs("Move Column Values to Row Names"), columnspan = 2)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  upper_frame <- tkframe(top)
  y_var_box <-
    bs_listbox(
      parent = upper_frame,
      title  = gettext_bs("Variables with unique values\n(select one)"),
      values = variables_with_unique_values(),
      height = 7
    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onOK <- function() {
    col_name <- getSelection(y_var_box)

    .ds <- active_dataset_0()

    closeDialog()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Library("tibble")
    command <- str_glue(
      "## ", gettext_bs("Move column values to row names"), "\n",
      '{.ds} <- tibble::column_to_rownames({.ds}, var = "{col_name}")'
    ) %>%
      style_cmd()

    result <- doItAndPrint(command)
    command_dataset_refresh()
    tkfocus(CommanderWindow())
  }

  # ========================================================================
  ok_cancel_help(helpSubject = "column_to_rownames", helpPackage = "tibble")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(upper_frame,
    columnspan = 2
    # , sticky = "nw"
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(getFrame(y_var_box),
    # sticky = "nw",
    columnspan = 2)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(buttonsFrame, sticky = "ew", columnspan = 2)
  dialogSuffix(rows = 3, columns = 2)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
