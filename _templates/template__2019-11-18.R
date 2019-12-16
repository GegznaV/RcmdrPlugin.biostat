# FIXME: This is a primary version. Not a real template.


# ___ Main function ___ ======================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_tmp <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  defaults <-
    list(
      selected_variable = "{none}"
    )

  initial <- getDialog("window_tmp", defaults)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  set_info_1 <- function(value = defaults$defaults, color = "darkred") {

    set_values(f1_var_selected_1, value)
    tkconfigure(f1_lab_selected_1, foreground = color)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_info_1 <- function() {

    values <- get_values(f1_var_selected_1)
    if (str_detect(values, fixed("{"))) {
      return(NULL)

    } else {
      values
    }
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  update_info_1 <- function() {

    values <- get_selected_1()
    if (is.null(values)) {
      set_info_1()
    } else {
      set_info_1(values, color = "darkgreen")
    }
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  update_box_1 <- function() {

    values <- variables_fct()
    if (length(values) > 0) {
      tk_normalize(f2_box_1)
      set_values(f2_box_1, values)

    } else {
      set_values(f2_box_1, "(no factor variables)")
      tk_disable(f2_box_1)

      update_box_2()
    }
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  update_box_2 <- function() {

    update_info_1()

    variable <- get_info_1()

    if (is.null(variable)) {
      set_values(f2_box_2, "(no values)")
      tk_disable(f2_box_2)
      return()
    }

    variable <- get_active_ds()[[variable]]
    values <- levels(variable)

    if (length(values) > 0) {
      tk_normalize(f2_box_2)
      set_values(f2_box_2, values)

    } else {
      set_values(f2_box_2, "(no values)")
      tk_disable(f2_box_2)
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_selected_1 <- function() {
    # Return either string or NULL
    value <- get_selection(f2_box_1)
    if (length(value) == 0) {
      value <- NULL
    }
    value
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_selected_2 <- function() {
    # Return either string or NULL
    value <- get_selection(f2_box_2)
    if (length(value) == 0) {
      value <- NULL
    }
    value
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  context_f2_box_1_left <- function() {

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

    pkg <- get_selected_1()
    .ds <- get_selected_2()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.null(.ds)) {
      return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = str_glue("Load dataset '{.ds}'"),
      compound = "left",
      image    = "::image::bs_load_pkg",
      command  = onOK
    )

    tkadd(menu_main, "command",
      label    = "Load dataset and close window",
      compound = "left",
      image    = "::image::bs_load_pkg_c",
      command  = function() {
        onOK()
        close_dialog()
      })

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = str_glue("Documentation on '{.ds}'"),
      compound = "left",
      image    = "::image::bs_help",
      command  = open_help(.ds, package = pkg)
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  context_f2_box_1_right <- function() {

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

    .ds <- get_selected_2()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.null(.ds)) {
      return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = str_glue("Documentation on '{.ds}'"),
      compound = "left",
      image    = "::image::bs_help",
      command  = open_help_selected_2
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  refresh_window <- function() {
    update_box_1()
    update_box_2()
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  onOK <- function() {
    return()
    # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cursor_set_busy(top)
    on.exit(cursor_set_idle(top))

    # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    selected_1 <- tclvalue(f1_var_selected_1)
    selected_2  <- tclvalue(f1_var_selected_2)

    if (str_detect(selected_2, fixed("{"))) {
      selected_2 <- NULL
    }

    if (str_detect(selected_1, fixed("{"))) {
      selected_1 <- NULL
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (variable_is_not_selected(selected_2, "dataset", parent = top)) {
      return()
    }
    if (forbid_to_replace_object(selected_2, parent = top)) {return()}
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    putDialog("window_tmp", list(
      which_packages  = get_selection(f2_pkg_opts)
    ))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    command_load <-
      if (is.null(selected_1)) {
        str_glue('data("{selected_2}")')
      } else {
        str_glue('data("{selected_2}", package = "{selected_1}")')
      }

    command <- str_glue(
      "## Load data from R package \n",
      "{command_load}"
    )

    # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    result <- justDoIt(command)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (class(result)[1] != "try-error") {
      logger(style_cmd(command))

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

  # Title ------------------------------------------------------------------
  title_text <- gettext_bs("Window Title")
  initializeDialog(title = title_text)
  tk_title(top, title_text)

  # cursor_set_busy(top)
  # on.exit(cursor_set_idle(top))

  # Widgets ------------------------------------------------------------------

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # F1: Indicators what is selected ------------------------------------------
  f1 <- tkframe(top)

  f1_lab_selected_1_0 <- tk_label(f1, text = "Selected variable: ")
  f1_var_selected_1   <- tclVar(defaults$selected_variable)
  f1_lab_selected_1   <- tk_label_red(f1, textvariable = f1_var_selected_1)

  tkgrid(f1, sticky = "w")
  tkgrid(f1_lab_selected_1_0, f1_lab_selected_1, pady = c(0, 0), sticky = "w")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # F2: Selection boxes ------------------------------------------------------
  f2 <- tkframe(top)

  f2_box_1 <-
    bs_listbox(
      parent = f2,
      height = 7,
      width  = 25,
      values = variables_fct(),
      on_double_click = update_box_2,
      title = gettext_bs("Variable (select one)")
      # use_filter = TRUE,
      # filter_label = "Filter variables"
    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_box_2 <-
    bs_listbox(
      parent = f2,
      height = 7,
      width  = 28,
      values = "",
      on_double_click = context_f2_box_1_left,
      on_click_3      = context_f2_box_1_right,
      title = gettext_bs("Levels (reorder)"),
      bind_row_swap = TRUE,
      selectmode = "multiple"
      # use_filter = TRUE,
      # filter_label = "Filter variables"
    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  f2_but_set_2 <- tkframe(f2)

  f2_but_2_1 <- tk2button(
    parent  = f2_but_set_2,
    image   = "::image::bs_go_first",
    command =  do_nothing,
    tip = "Tip 1"
  )

  f2_but_2_2 <- tk2button(
    f2_but_set_2,
    image = "::image::bs_go_prev",
    command = do_nothing,
    tip = "Tip 2"
  )

  f2_but_2_3 <- tk2button(
    f2_but_set_2,
    image = "::image::bs_delete",
    command = do_nothing,
    tip = "Tip 3"
  )

  f2_but_2_4 <- tk2button(
    f2_but_set_2,
    image = "::image::bs_go_next",
    command = update_box_2,
    tip = "Tip 4"
  )

  f2_but_2_5 <- tk2button(
    f2_but_set_2,
    image = "::image::bs_go_last",
    command = do_nothing,
    tip = "Tip 5"
  )

  tkgrid(f2_but_2_1)
  tkgrid(f2_but_2_2)
  tkgrid(f2_but_2_3)
  tkgrid(f2_but_2_4)
  tkgrid(f2_but_2_5)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  f2_but_set_1 <- tkframe(f2)

  f2_but_1_1 <- tk2button(
    f2_but_set_1,
    image = "::image::bs_go_top",
    command = function() {
      move_selected_row_in_box(f2_box_2, move_to = "top")
    },
    tip = "Move selected line \nto the top."
  )

  f2_but_1_2 <- tk2button(
    f2_but_set_1,
    image = "::image::bs_go_up",
    command = function() {
      move_selected_row_in_box(f2_box_2, move_to = "-1")
    },
    tip = "Move selected line \nup by 1 position."
  )

  f2_but_1_3 <- tk2button(
    f2_but_set_1,
    image = "::image::bs_refresh",
    # command = select_dataset,
    tip = "Reset."
  )

  f2_but_1_4 <- tk2button(
    f2_but_set_1,
    image = "::image::bs_go_down",
    command = function() {
      move_selected_row_in_box(f2_box_2, move_to = "+1")
    },
    tip = "Move selected line \nup by 1 position."
  )

  f2_but_1_5 <- tk2button(
    f2_but_set_1,
    image = "::image::bs_go_bottom",
    command = function() {
      move_selected_row_in_box(f2_box_2, move_to = "end")
    },
    tip = "Move selected line \nto the bottom."
  )

  tkgrid(f2_but_1_1)
  tkgrid(f2_but_1_2)
  tkgrid(f2_but_1_3)
  tkgrid(f2_but_1_4)
  tkgrid(f2_but_1_5)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f2, sticky = "nw")

  tkgrid(
    f2_box_1$frame,
    f2_but_set_2,
    f2_box_2$frame,
    f2_but_set_1,
    sticky = "n"
  )
  tkgrid.configure(f2_but_set_2, sticky = "s")
  tkgrid.configure(f2_but_set_1, sticky = "s")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Help menus ---------------------------------------------------------------
  help_menu <- function() {

    .ds <- get_selected_2()
    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = "Function `fct_relevel()`",
      command  = open_help("fct_relevel", package = "forcats")
    )

    tkadd(menu_main, "command",
      label    = "Function `fct_reorder()`",
      command  = open_help("fct_reorder", package = "forcats")
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ok_cancel_help(
    on_help = help_menu,
    close_on_ok = TRUE,
    apply = "window_tmp()",
    apply_label = "Apply"
  )
  # ======================================================================~~~~
  tkgrid(buttonsFrame, sticky = "ew")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dialogSuffix(bindReturn = FALSE)

  refresh_window()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

