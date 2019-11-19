# TODO:
#
# 1. Check code for bugs.
# 2. Add ability to choose to either update dataset or to create new.
# 3. Add ability to use several variables for sorting.
#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_arrange <- function() {
  win_title <- gettext_bs("Arrange Rows")
  initializeDialog(title = win_title)
  tk_title(top, win_title)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  upper_frame <- tkframe(top)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  var_y_box <-
    bs_listbox(
      parent     = upper_frame,
      values     = variables_all(),
      selectmode = "single",
      title      = gettext_bs("Variable for sorting"),
      height     = 8
    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  optionsFrame <- tkframe(upper_frame)

  Rcmdr::radioButtons(
    optionsFrame,
    name    = "decreasing",
    buttons = gettext_bs(c("Ascending", "Descending")),
    values  = c("FALSE", "TRUE"),
    labels  = gettext_bs(c("Ascending", "Descending")),
    title   = gettext_bs("Sorting order")
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onOK <- function() {
    y          <- get_selection(var_y_box)
    decreasing <- as.logical(tclvalue(decreasingVariable))
    .ds        <- active_dataset()
    new_dsname <- active_dataset()  # TODO: [???] should be a separate window

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (variable_is_not_selected(y, "variable", parent = top)) {
      return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    y <- safe_names(y)

    variables <-
      if (decreasing) {
        str_glue("desc({y})")

      } else {
        y
      }

    command <- str_glue(
      "## Sort rows \n",
      "{new_dsname} <- {.ds} %>% \n",
      "dplyr::arrange({variables})")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Library("dplyr")
    result <- justDoIt(command)

    if (class(result)[1] != "try-error") {
      logger(style_cmd(command))

      active_dataset(new_dsname, flushModel = FALSE)

      # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      closeDialog()

    } else {
      logger_error(command, error_msg = result)
      show_code_evaluation_error_message()
      return()
    }

    tkfocus(CommanderWindow())
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Announce about the success to run the function `onOk()`
    TRUE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ok_cancel_help(helpSubject = "arrange", helpPackage = "dplyr")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(upper_frame, sticky = "new")
  tkgrid(var_y_box$frame, optionsFrame, sticky = "new", columnspan = 2)
  tkgrid(decreasingFrame, sticky = "nw")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
  dialogSuffix(rows = 6, columns = 1)
}
