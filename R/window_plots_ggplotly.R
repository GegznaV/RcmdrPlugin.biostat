#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_plots_ggplotly <- function() {
  # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  activate_options <- function() {
    source <- get_values(f1_source_of_plot)

    # Important for the first time
    if (gg_lastplot_exists()) {
      tk_normalize(f1_source_of_plot, "last_gg")

    } else {
      tk_disable(f1_source_of_plot, "last_gg")
      # Deselect disabled value
      if (source == "last_gg") {
        set_values(f1_source_of_plot, "obj_gg")
      }
    }

    if (gg_objects_exist()) {
      tk_normalize(f1_source_of_plot, "obj_gg")

    } else {
      tk_disable(f1_source_of_plot, "obj_gg")
      # Deselect disabled value
      if (source == "obj_gg") {
        set_values(f1_source_of_plot, "last_gg")
      }
    }

    switch(source,
      "obj_gg" = {
        tk_normalize(f1_gg_obj_name_box)
        set_values(
          f1_gg_obj_name_box,
          list_objects_of_class("gg", envir = .GlobalEnv)
        )
        set_selection(f1_gg_obj_name_box, 1)
      },
      # Default:
      tk_disable(f1_gg_obj_name_box)
    )
  }

  # Function onOK ----------------------------------------------------------
  onOK <- function() {
    # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cursor_set_busy(top)
    on.exit(cursor_set_idle(top))

    # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    source <- get_values(f1_source_of_plot)
    gg_obj <- get_selection(f1_gg_obj_name_box)

    # Reset widget properties before checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (variable_is_not_selected(source, "source of plot", parent = top)) {
      return()
    }

    if (source == "obj_gg") {
      if (variable_is_not_selected(gg_obj, "'ggplot' object", parent = top)) {
        return()
      }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    putDialog("window_plots_ggplotly", list(
      source = source
    ))

    # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    gg_obj <- safe_names(gg_obj)

    command <-
      if (source == "obj_gg") {
        str_glue(
          "## Print a ggplot as an interactive plot.\n",
          'plotly::ggplotly({gg_obj})')
      } else {
        str_c(
          "## Prnt the last ggplot as an interactive plot.\n",
          'plotly::ggplotly()')
      }

    # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Library("plotly")

    # doItAndPrint(command)
    result <- justDoIt(command)

    # result <- try_command(command)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (class(result)[1] != "try-error") {
      logger(style_cmd(command))
      # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      closeDialog()

    } else {
      logger_error(command, error_msg = result)
      show_code_evaluation_error_message(parent = top)
      return()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    command_dataset_refresh()
    tkfocus(CommanderWindow())
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Announce about the success to run the function `onOk()`
    TRUE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  }

  # Initial values ---------------------------------------------------------

  # Initialize dialog window and title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  dialogue_title <- gettext_bs("Make Interactive Plot")
  initializeDialog(title = dialogue_title)
  tk_title(top, dialogue_title)

  # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  defaults <- list(
    source = if (gg_objects_exist()) {"obj_gg"} else {"last_gg"}
  )
  initial <- getDialog("window_plots_ggplotly", defaults)

  # Widgets ----------------------------------------------------------------

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f1 <- tkframe(top)

  f1_source_of_plot <-
    bs_radiobuttons(
      parent = f1,
      buttons = c(
        obj_gg  = "'ggplot2' object",
        last_gg = "Last ggplot2 plot"
      ),
      value           = initial$source,
      title           = "Source of plot",
      default_command = activate_options,
      tips            = list(
        obj_gg     = str_c(
          "A 'ggplot2' object saved in 'R' workspace.\n",
          "This option is inactive if no 'ggplot2' \n",
          "objects are present."),
        last_gg    = str_c(
          "The last created 'ggplot2' plot.\n",
          "This option is inactive if no `ggplot`\n",
          "was created in this session.")
      ),       # named list of strings
      default_tip     = "",
      border          = FALSE,
      sticky_buttons  = "w",
      sticky_title    = "w"
    )


  f1_gg_obj_name_box <-
    bs_listbox(
      parent = f1,
      values = list_objects_of_class("gg", envir = .GlobalEnv),
      title  = "List of ggplot2 objects:",
      width  = 25,
      height = 7
    )

  # Layout
  tkgrid(f1, sticky = "N")
  tkgrid(f1_source_of_plot$frame, f1_gg_obj_name_box$frame, sticky = "NW")
  tkgrid.configure(f1_source_of_plot$frame, sticky = "NW", padx = c(0, 90))


  # Help menus ---------------------------------------------------------------
  help_menu <- function() {

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = "Function `ggplotly()`",
      command  = open_help("ggplotly", package = "plotly"))

    tkadd(menu_main, "command",
      label    = "Function `last_plot()`",
      command  = open_help("last_plot", package = "ggplot2"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }
  # Finalize -----------------------------------------------------------------

  # Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--
  ok_cancel_help(
    on_help = help_menu,
    apply = "window_plots_ggplotly()"
  )

  tkgrid(buttonsFrame, sticky = "ew")
  dialogSuffix()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  activate_options()
}
