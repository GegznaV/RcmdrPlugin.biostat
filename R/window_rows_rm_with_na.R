# TODO: add option to remove rows with all NA in selected variables
#
# library(tidyverse)
# df = tibble::tribble(
#   ~x, ~y, ~z, ~q,
#   1,   1 , 1,  1,
#   2,   2,  2, NA,
#   3,   3, NA, NA,
#   4,  NA, NA, NA
# )
# df %>% filter_all(any_vars(!is.na(.))) # remove empty rows
# df %>% filter_at(vars(q, z), any_vars(!is.na(.))) # remove rows with all NA in selected vars


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_rm_with_na <- function() {

  # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cmd_activation <- function() {
    val_scope <- get_values(f1_b1)
    val_which <- get_values(f1_b2)
    combo_val <- str_c(val_scope, val_which, sep = ".")

    bottom_label <- switch(
      combo_val,
      "search_all.any_na"      = label_all_vars_any_na,
      "search_all.all_na"      = label_all_vars_all_na,
      "search_selected.any_na" = label_selected_vars_any_na,
      "search_selected.all_na" = label_selected_vars_all_na,
      "\n\n"
    )

    tkconfigure(label_bottom, text = bottom_label)

    switch(
      val_scope,
      "search_all"      = tk_disable(var_y_box),
      "search_selected" = tk_normalize(var_y_box)
    )
  }
  # Function onOK ----------------------------------------------------------
  onOK <- function() {
    # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cursor_set_busy(top)
    on.exit(cursor_set_idle(top))

    # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    scope    <- get_values(f1_b1)
    rm_if    <- get_values(f1_b2)
    new_name <- get_values(name_box)
    vars_y   <- get_selection(var_y_box)

    combo_val <- str_c(scope, rm_if, sep = ".")

    # Reset widget properties before checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # tkconfigure(name_entry, foreground = "black")

    # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (scope == "search_selected") {
      if (variable_is_not_selected(vars_y, "variable")) {return()}
    }

    if (is_empty_name(new_name))            {return()}
    if (is_not_valid_name(new_name))        {return()}
    if (forbid_to_replace_object(new_name)) {return()}
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    putDialog("window_rows_rm_with_na", list(
      scope          = scope,
      rm_if          = rm_if,
      var_y          = vars_y
    ))

    # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    vars_y_txt <- str_c(safe_names(vars_y), collapse = ", ")
    prepare_lab <-
      . %>%
      str_wrap(width = 40) %>%
      str_replace("\n", "\n## ")

    command <-
      switch(
        combo_val,

        "search_all.any_na" = {
          str_glue(
            "## {prepare_lab(label_all_vars_any_na)}\n",
            "{new_name} <- {.ds} %>% tidyr::drop_na()"
          )},

        "search_all.all_na" = {
          str_glue(
            "## {prepare_lab(label_all_vars_all_na)}\n",
            "{new_name} <- {.ds} %>% \n",
            "filter_all(any_vars(!is.na(.))) "
          )},

        "search_selected.any_na" = {
          str_glue(
            "## {prepare_lab(label_selected_vars_any_na)}\n",
            "{new_name} <- {.ds} %>% \n ",
            "tidyr::drop_na({vars_y_txt})"
          )},

        "search_selected.all_na" = {
          str_glue(
            "## {prepare_lab(label_selected_vars_all_na)}\n",
            "{new_name} <- {.ds} %>% \n ",
            "filter_at(vars({vars_y_txt}), any_vars(!is.na(.)))"
          )}
      )

    # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Library("tidyverse")

    result <- justDoIt(command)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (class(result)[1] != "try-error") {
      logger(style_cmd(command))
      active_dataset(new_name)

      # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      closeDialog()


    } else {
      logger_error(command, error_msg = result)
      show_code_evaluation_error_message()
      return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkfocus(CommanderWindow())
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Announce about the success to run the function `onOk()`
    TRUE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  }

  # Initial values ---------------------------------------------------------

  # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  .ds    <- active_dataset()
  fg_col <- Rcmdr::getRcmdr("title.color")

  # label_bottom_text_all <-
  label_all_vars_any_na <-
    "Remove the rows that contain at least one missing (NA) value.\n"

  label_all_vars_all_na <-
    "Remove empty rows.\n"

  # label_bottom_text_selected <- str_c(
  label_selected_vars_any_na <- str_c(
    "Remove the rows that contain at least one missing (NA) value \n",
    "in the selected variables."
  )

  label_selected_vars_all_na <- str_c(
    "Remove the rows that contain all missing (NA) values \n",
    "in the selected variables."
  )

  # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  initializeDialog(title = gettext_bs("Remove Rows with Missing Values"))

  tk_title(top, "Remove Rows Containing Missing Values") # Title ~~~~~~~~~~~~~~

  # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  defaults <- list(
    position       = "first",
    scope          = "search_all",
    rm_if          = "any_na",
    var_y          = NULL
  )
  initial <- getDialog("window_rows_rm_with_na", defaults)

  # Widgets ----------------------------------------------------------------
  main_frame <- tkframe(top)

  left_frame  <- tkframe(main_frame)
  right_frame <- tkframe(main_frame)

  tkgrid(main_frame, sticky = "nsw")
  tkgrid(left_frame, right_frame, sticky = "nsw")

  # Listbox
  var_y_box <-
    bs_listbox(
      right_frame,
      values      = variables_all(),
      value       = initial$var_y,
      title       = title_var_n,
      selectmode  = "extended",
      on_keyboard = "scroll",
      height      = 8,
      width       = c(26, Inf),
      tip         = tip_multiple_ctrl_letters
    )

  tkgrid(var_y_box$frame, padx = c(5, 0))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  f1_b1 <- bs_radiobuttons(
    parent          = left_frame,
    title           = "Take into account:",
    layout          = "horizontal",
    buttons         = c("search_all"    , "search_selected"),
    labels          = c("All variables" , "Selected variables"),
    value           = initial$scope,
    default_command = cmd_activation
  )

  f1_b2 <- bs_radiobuttons(
    parent          = left_frame,
    title           = "Remove rows with:",
    buttons         = c("all_na", "any_na"),
    labels          = c("All missing values", "At least one missing value"),
    value           = initial$rm_if ,
    default_command = cmd_activation
  )

  tkgrid(f1_b1$frame, sticky = "nw")
  tkgrid(f1_b2$frame, sticky = "nw", pady = c(5, 0))

  # Name
  init_name <-
    str_c(.ds, "_rm_na_rows") %>%
    str_trunc(50, ellipsis = "") %>%
    unique_obj_names()

  name_box <- bs_entry(
    left_frame,
    width = 32,
    value = init_name,
    label = "New dataset's name:",
    label_position = "above"
  )

  tkgrid(name_box$frame, sticky = "ws", pady = c(16, 0))


  # Information

  label_bottom <- tk_label(top, text = "\n\n", fg = "darkred")
  tkgrid(label_bottom, pady = c(10, 0), sticky = "ws")



  # Help menus -------------------------------------------------------------
  help_menu <- function() {

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = "Function `drop_na`",
      command  = open_help("drop_na", package = "tidyr"))

    tkadd(menu_main, "command",
      label    = "Function `is.na`",
      command  = open_help("is.na", package = "base"))

    tkadd(menu_main, "separator")

    tkadd(menu_main, "command",
      label    = "Function `filter_all`",
      command  = open_help("filter_all", package = "dplyr"))

    tkadd(menu_main, "command",
      label    = "Function `filter_at`",
      command  = open_help("filter_at", package = "dplyr"))

    tkadd(menu_main, "command",
      label    = "Function `any_vars`",
      command  = open_help("any_vars", package = "dplyr"))

    tkadd(menu_main, "command",
      label    = "Function `vars`",
      command  = open_help("vars", package = "dplyr"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }
  # Finalize ---------------------------------------------------------------
  ok_cancel_help(
    on_help = help_menu,
    reset = "window_rows_rm_with_na()"
  )

  tkgrid(buttonsFrame, sticky = "ew")
  dialogSuffix()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cmd_activation()
}
