
# ___ Main function ___ ======================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @keywords internal
window_fct_relevel <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  defaults <-
    list(
      selected_variable = "{none}",
      seed = as.integer(Sys.time())
    )

  initial <- getDialog("window_fct_relevel", defaults)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_seed <- function() {
    defaults$seed
  }

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

      update_box_levels()
    }
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  update_box_levels <- function() {

    update_info_1()

    variable_name <- get_info_1()

    if (is.null(variable_name)) {
      set_values(f2_box_levels, "(no values)")
      tk_disable(f2_box_levels)
      return()
    }

    f4$variable$update()

    variable <- get_active_ds()[[variable_name]]

    variable <- switch(
      get_sorting_type(),

      "Manual"            = ,
      "Manually"          = ,
      "Original order"    = ,
      "In original order" = variable,

      "Alphabetic order" = ,
      "In alphabetic order" = {
        forcats::fct_relabel(variable, sort)
      },

      "By first appearance" = {
        forcats::fct_inorder(variable)
      },

      "In frequency" = {
        forcats::fct_infreq(variable)
      },

      "Numeric order (Roman numbers)"    = ,
      "In numeric order (Roman numbers)" = {
        forcats::fct_relabel(
          variable, gtools::mixedsort, numeric.type = "roman"
        )
      },

      "Numeric order"    = ,
      "In numeric order" = {
        forcats::fct_relabel(
          variable, gtools::mixedsort, numeric.type = "decimal"
        )
        # forcats::fct_inseq(variable) # fct_inseq() has bugs
      },

      "Random order"    = ,
      "In random order" = {
        # set.seed(get_seed())
        forcats::fct_shuffle(variable)
      },

      "Reversed order"          = ,
      "Reversed original order" = ,
      "In reversed order"       = {
        forcats::fct_rev(variable)
      },

      # "By sorting along another variable" = {
      #     forcats::fct_reorder(variable, variable2, .fun = mean, na.rm = TRUE,
      #       .desc = FALSE)
      #   },
      variable
    )

    values <- levels(variable)

    if (length(values) > 0) {
      tk_normalize(f2_box_levels)
      set_values(f2_box_levels, values)

    } else {
      set_values(f2_box_levels, "(no values)")
      tk_disable(f2_box_levels)
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
  get_selected_levels <- function() {
    # Return either string or NULL
    value <- get_selection(f2_box_levels)
    if (length(value) == 0) {
      value <- NULL
    }
    value
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_sorting_type <- function() {
    get_selection(f3_combo_1)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  is_manual_sorting <- function() {
    isTRUE(get_sorting_type() %in% c(
      "Manually", "Manual", "In original order", "Original order"))
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  activate_arrow_buttons <- function() {

    objs <- list(f2_box_levels, f2_but_1_1, f2_but_1_2, f2_but_1_4, f2_but_1_5)

    if (is_manual_sorting()) {
      purrr::walk(objs, tk_normalize)

    } else {
      purrr::walk(objs, tk_disable)

    }
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  context_f2_box_1_left <- function() {

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

    pkg <- get_selected_1()
    .ds <- get_selected_levels()
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

    .ds <- get_selected_levels()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.null(.ds)) {
      return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = str_glue("Documentation on '{.ds}'"),
      compound = "left",
      image    = "::image::bs_help",
      command  = open_help_selected_levels
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  refresh_window <- function() {
    update_box_1()
    update_box_levels()
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  onOK <- function() {
    return()
    # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cursor_set_busy(top)
    on.exit(cursor_set_idle(top))

    # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    selected_1 <- tclvalue(f1_var_selected_1)
    selected_levels  <- tclvalue(f1_var_selected_levels)

    if (str_detect(selected_levels, fixed("{"))) {
      selected_levels <- NULL
    }

    if (str_detect(selected_1, fixed("{"))) {
      selected_1 <- NULL
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (variable_is_not_selected(selected_levels, "dataset", parent = top)) {
      return()
    }
    if (forbid_to_replace_object(selected_levels, parent = top)) {return()}
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    putDialog("window_fct_relevel", list(
      which_packages  = get_selection(f2_pkg_opts)
    ))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    command_load <-
      if (is.null(selected_1)) {
        str_glue('data("{selected_levels}")')
      } else {
        str_glue('data("{selected_levels}", package = "{selected_1}")')
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
  title_text <- gettext_bs("Reorder Factor Levels")
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
      on_double_click = update_box_levels,
      title = gettext_bs("Variable (select one)")
      # use_filter = TRUE,
      # filter_label = "Filter variables"
    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_box_levels <-
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

  # f2_but_2_1 <- tk2button(
  #   parent  = f2_but_set_2,
  #   image   = "::image::bs_go_first",
  #   command =  do_nothing,
  #   tip = "Tip 1"
  # )
  #
  # f2_but_2_2 <- tk2button(
  #   f2_but_set_2,
  #   image = "::image::bs_go_prev",
  #   command = do_nothing,
  #   tip = "Tip 2"
  # )
  #
  # f2_but_2_3 <- tk2button(
  #   f2_but_set_2,
  #   image = "::image::bs_delete",
  #   command = do_nothing,
  #   tip = "Tip 3"
  # )

  f2_but_2_4 <- tk2button(
    f2_but_set_2,
    image = "::image::bs_go_next",
    command = update_box_levels,
    tip = "Tip 4"
  )

  # f2_but_2_5 <- tk2button(
  #   f2_but_set_2,
  #   image = "::image::bs_go_last",
  #   command = do_nothing,
  #   tip = "Tip 5"
  # )

  # tkgrid(f2_but_2_1)
  # tkgrid(f2_but_2_2)
  # tkgrid(f2_but_2_3)
  tkgrid(f2_but_2_4)
  # tkgrid(f2_but_2_5)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  f2_but_set_1 <- tkframe(f2)

  f2_but_1_1 <- tk2button(
    f2_but_set_1,
    image = "::image::bs_go_top",
    command = function() {
      move_selected_row_in_box(f2_box_levels, move_to = "top")
    },
    tip = "Move selected lines \nto the top.\nCtrl + Up"
  )

  f2_but_1_2 <- tk2button(
    f2_but_set_1,
    image = "::image::bs_go_up",
    command = function() {
      move_selected_row_in_box(f2_box_levels, move_to = "-1")
    },
    tip = "Move selected lines \nup by 1 position.\nAlt + Up"
  )

  # f2_but_1_3 <- tk2button(
  #   f2_but_set_1,
  #   image = "::image::bs_refresh_r",
  #   # command = select_dataset,
  #   tip = "Reset."
  # )

  f2_but_1_4 <- tk2button(
    f2_but_set_1,
    image = "::image::bs_go_down",
    command = function() {
      move_selected_row_in_box(f2_box_levels, move_to = "+1")
    },
    tip = "Move selected lines \nup by 1 position.\nAlt + Down"
  )

  f2_but_1_5 <- tk2button(
    f2_but_set_1,
    image = "::image::bs_go_bottom",
    command = function() {
      move_selected_row_in_box(f2_box_levels, move_to = "end")
    },
    tip = "Move selected lines \nto the bottom.\nCtrl + Down"
  )

  tkgrid(f2_but_1_1)
  tkgrid(f2_but_1_2)
  # tkgrid(f2_but_1_3)
  tkgrid(f2_but_1_4)
  tkgrid(f2_but_1_5)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f2, sticky = "nw")

  tkgrid(
    f2_box_1$frame,
    f2_but_set_2,
    f2_box_levels$frame,
    f2_but_set_1,
    sticky = "n"
  )
  tkgrid.configure(f2_but_set_2, sticky = "", padx = c(5, 5), pady = c(25, 0))
  tkgrid.configure(f2_but_set_1, sticky = "s")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # F3 -----------------------------------------------------------------------
  f3 <- tkframe(top)

  f3_combo_1 <-
    bs_combobox(
      parent = f3,
      label = "List levels in this order",
      label_position = "above",
      selection = 1,
      width = 30,

      on_select = update_box_levels,

      values = c(
        # "Manually",
        "Original order",
        "Alphabetic order",
        "By first appearance",
        "In frequency",
        "Numeric order",
        "Numeric order (Roman numbers)",
        "Random order",
        "Reversed original order",
        # "In original order",
        # "By first appearance",
        # "In alphabetic order",
        # "In frequency",
        # "In numeric order (Roman numbers)",
        # "In numeric order",
        # "In random order",
        # "In reversed order",
        # "By sorting along another variable",
        NULL
      ))

  tkgrid(f3, sticky = "w")
  tkgrid(f3_combo_1$frame)

  # F4 -----------------------------------------------------------------------
  f4 <- bs_names_ds_var(get_var_name_fun = get_selected_1)
  tkgrid(f4$frame, sticky = "w", pady = c(10, 0))
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Help menus ---------------------------------------------------------------
  help_menu <- function() {

    .ds <- get_selected_levels()
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
    apply = "window_fct_relevel()",
    apply_label = "Apply"
  )
  # ======================================================================~~~~
  tkgrid(buttonsFrame, sticky = "ew")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dialogSuffix(bindReturn = FALSE)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  refresh_window()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



