# TODO:
#
# 1. Simplify code.
# 2. Use "forcats" functions where possible.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_factor_lvls_drop <- function() {

  win_title <- gettext_bs("Drop Unused Factor Levels")
  initializeDialog(title = win_title)
  tk_title(top, win_title)

  .ds <- active_dataset()

  all_factors_variable <- tclVar("0")
  all_frame <- tkframe(top)
  all_factors_checkbox <- ttkcheckbutton(
    all_frame,
    variable = all_factors_variable
  )

  variables_box <-
    bs_listbox(
      parent     = top,
      values     = variables_fct(),
      title      = gettext_bs("Factors(s) to drop levels \n(pick one or more)"),
      selectmode = "multiple",
      height     = 6
    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onOK <- function() {
    # logger(paste(
    #     "#####",
    #     gettext_bs("Drop unused factor levels"),
    #     "#####",
    #     sep = ""
    # ))
    all <- tclvalue(all_factors_variable)
    variables <- get_selection(variables_box)
    closeDialog()
    if (all == 0 && length(variables) == 0) {
      errorCondition(
        recall = window_factor_lvls_drop,
        message = gettext_bs("You must select one or more variables.")
      )
      return()
    }
    response <-
      tk_messageBox(
        # parent = top,
        caption = "Drop Unused Levels",
        message = gettext_bs("Unused factor levels will be dropped.\nDo you agree?"),
        icon = "warning",
        type = "yesno",
        default = "no"
      )

    if (response != "yes") {
      onCancel()
      return()
    }

    if (all == 1)
      command <- str_glue("{.ds} <- droplevels({.ds})")
    else {
      command <- ""
      for (variable in variables) {
        command <-
          paste0(command, str_glue("{.ds}${variable} <- droplevels({.ds}${variable})\n"))
      }
    }
    doItAndPrint(command)
    active_dataset(.ds,
      flushModel = FALSE,
      flushDialogMemory = FALSE)
    tkfocus(CommanderWindow())
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ok_cancel_help(helpSubject = "droplevels")

  tkgrid(variables_box$frame, sticky = "nw")

  tkgrid(all_factors_checkbox,
    labelRcmdr(
      all_frame,
      text = gettext_bs("All factor variables")
    ),
    sticky = "w", pady = c(2, 0))
  tkgrid(allFrame, sticky = "ew")

  tkgrid(buttons_frame, sticky = "w")
  dialogSuffix()
}
