# Create a window:
# radiobuttons: "all", "fivenum", "common", "custom"

# If "custom":
# "mean", "sd", "min", "q1", "med", "q3", "max", "mad", "iqr", "cv",
# "skewness", "se.skewness", "kurtosis", "n.valid",  "pct.valid"

# TODO:
# ungroup
#

# "all"
# "fivenum"
# "common"
# "custom"

# "mean" = "Mean",
# "sd"   = "Standard deviation (SD)",
# "min"  = "Minimum value (min)",
# "q1"   = "Q1",
# "med"  = "Median",
# "q3"   = "Q3",
# "max"  = "Maximum value (max)",
# "mad"  = "Median absolute deviation (MAD)",
# "iqr"  = "Interquartile range (IQR)",
# "cv"   = "Cefficient of variation",
# "skewness"    = "Skewness",
# "se.skewness" = "Standard error of skewness",
# "kurtosis"    = "Kurtosis",
# "n.valid"     = "Number of valid cases",
# "pct.valid"   = "Percentage of valid cases"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_summary_descr <- function() {
  # SummaryTools
  .ds <- active_dataset_0()

  # If any numeric variables exist
  ds_numeric <-
    purrr::map_lgl(
      str_glue_eval("{.ds}", envir_eval = .GlobalEnv),
      ~ is.numeric(.)
    )

  if (any(ds_numeric)) {
    Library("tidyverse")
    Library("summarytools")

    command <- str_glue(
      .trim = FALSE,
      "## The summary of numeric variables\n",
      "{.ds} %>% ",
      # "  group_by() %>%",
      "  select_if(is.numeric) %>%",
      "  descr(round.digits = 2)"
    )
    doItAndPrint(style_cmd(command))

  } else {
    doItAndPrint("## No numeric variables found")
  }
}




# #' @rdname Menu-window-functions
# #' @export
# #' @keywords internal
#
# window_xxx <- function() {
#   # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   # ...
#
#   # Function onOK ----------------------------------------------------------
#   # Function onOK ----------------------------------------------------------
#   onOK <- function() {
#     # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     cursor_set_busy(top)
#     on.exit(cursor_set_idle(top))
#
#     # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     by_group  <- get_values(f1_widget_y_gr$checkbox)
#     y_var     <- get_selection(f1_widget_y_gr$y)
#     gr_var    <- get_selection(f1_widget_y_gr$gr)
#
#     # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     if (variable_is_not_selected(y_var, "response variable", parent = top)) {
#       return()
#     }
#
#     if (isTRUE(by_group)) {
#       if (variable_is_not_selected(gr_var, "explanatory variable",
#         parent = top, article = "an")) {
#         return()
#       }
#     }
#
#     # if (is_empty_name(new_name, parent = top))              {return()}
#     # if (is_not_valid_name(new_name, parent = top))          {return()}
#     # if (forbid_to_replace_variable(new_name, parent = top)) {return()}
#
#
#     all_selected <- c(y_var, gr_var)
#     non_standard <- all_selected != make.names(all_selected)
#
#     if (by_group && any(non_standard)) {
#       # RcmdrTkmessageBox(popup_msg, icon = "error", title = title, type = "ok")
#       tk_messageBox(
#         # parent = top,
#         message = str_c(collapse = "\n",
#           "The calculations may fail due to non-standard variable names: \n",
#           all_selected[non_standard]
#         ),
#         caption = "Non-standard Variable Names",
#         type = "ok",
#         icon = "warning")
#     }
#
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     putDialog("window_summary_desc", list(
#       # Variables
#       y_var     = y_var,
#       gr_var    = gr_var,
#       by_group  = by_group,
#
#       keep_results  = keep_results,
#       force_options = force_options,
#       # conf_level   = 0.95,    # Not implemented
#
#       # Numeric output
#       print_num  = print_num,
#       digits_per = digits_per,
#       digits_num = digits_num,
#       scipen     = scipen,
#       big_mark   = get_big_mark_label(big_mark),
#
#       # Plots
#       use_plot         = use_plot,
#       new_plots_window = new_plots_window,
#       labels_direction = get_lab_direction_label(las)
#     ))
#
#     # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#     y_var  <- safe_names(y_var)
#     gr_var <- safe_names(gr_var)
#
#     rez <- unique_obj_names("desc_summary", all_numbered = TRUE)
#
#     if (isTRUE(print_num)) {
#       print_code <- str_glue("print({rez}, plotit = FALSE) \n", .trim = FALSE)
#
#       # Ensure required DescTools options are set properly
#       if (!isTRUE(biostat_env$desctools_opts_are_set)) {
#         force_options <- TRUE
#         biostat_env$desctools_opts_are_set <- TRUE
#       }
#
#       opts_code <-
#         get_desctools_opts_str(
#           big_mark = big_mark,
#           num_digits = digits_num,
#           per_digits = digits_per,
#           scipen = scipen,
#           .force = force_options)
#
#     } else {
#       print_code <- ""
#       opts_code  <- ""
#     }
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     if (isTRUE(use_plot)) {
#
#       if (new_plots_window == TRUE) {
#         open_new_plots_window()
#       }
#
#       plot_code <-
#         if (las == par("las")) {
#           str_glue("plot({rez}) \n", .trim = FALSE)
#
#         } else {
#           str_glue(
#             .trim = FALSE,
#             "withr::with_par(list(las = {las}), plot({rez})) \n")
#         }
#
#
#     } else {
#       plot_code <- ""
#     }
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     opts <- ""
#     if (isTRUE(by_group)) {
#       variables <- str_glue("{y_var} ~ {gr_var}")
#
#     } else {
#       variables <- str_glue("{y_var}")
#       if (any(variables %in% variables_fct() &
#           !variables %in% variables_fct_like_2_lvls())) {
#         opts <- ', ord = "level"'
#         # TODO: add possibility to use other options of "ord":
#         # "level" by factor levels
#         # "name"  alphabetical order
#         # "asc"   by frequencies ascending
#         # "desc"  by frequencies descending
#       }
#     }
#
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     if (isTRUE(keep_results)) {
#       rm_code <- ""
#
#     } else {
#       rm_code <- str_glue("remove({rez}) \n")
#
#     }
#
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     command <- str_glue(
#       .trim = FALSE,
#       "{opts_code}",
#       "## Summary of variables\n",
#       "{rez} <- \n   with({.ds}, DescTools::Desc({variables}{opts})) \n",
#       "{print_code}",
#       "{plot_code}",
#       "{rm_code}")
#
#     # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Library("DescTools")
#
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     # Checks for syntax errors
#     result <- try_command(command)
#
#     if (class(result)[1] == "try-error") {
#       logger_error(command, error_msg = as.character(result))
#       show_code_evaluation_error_message(parent = top, result$message)
#       return()
#     }
#
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     # Checks for code evaluation errors
#
#     result <- doItAndPrint(style_cmd(command))
#
#     if (class(result)[1] == "try-error") {
#
#       logger_error(command, error_msg = result)
#       show_code_evaluation_error_message(parent = top, result)
#       return()
#     }
#
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     tkfocus(CommanderWindow())
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     # Announce about the success to run the function `onOk()`
#     TRUE
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   }
#
#   # Initial values ---------------------------------------------------------
#
#   # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   .ds <- active_dataset() # active_dataset_0()
#
#   # Initialize dialog window and title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   dialogue_title <- gettext_bs("Summaries for Numeric Variables")
#   initializeDialog(title = dialogue_title)
#   tk_title(top, dialogue_title)
#
#   # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   defaults <- list(
#     position = "first"
#   )
#   initial <- getDialog("window_xxx", defaults)
#
#
#   # ... Widgets ============================================================
#   # Widgets ----------------------------------------------------------------
#
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   upper_frame <- tkframe(top)
#
#   # Layout
#   tkgrid(upper_frame, pady = c(0, 5))
#
#   # * Y and Groups box =====================================================
#
#   defaults <- list(
#     var_y      = NULL,
#     var_gr     = NULL,
#     use_groups = "0"
#   )
#
#   f1_widget_y_gr <- bs_listbox_y_gr(
#     parent         = top,
#     y_title        = title_var_1,
#     y_var_type     = "num",
#     y_initial      = initial$var_y,
#     y_select_mode  = "single",
#
#     gr_title       = title_gr_0_n,
#     gr_var_type    = "fct_like",
#     gr_initial     = initial$var_gr,
#     gr_select_mode = "multiple",
#
#     ch_initial     = initial$use_groups
#   )
#
#   # Text entry box
#   name_entry <- bs_entry(
#     parent = upper_frame,
#     width = 3,
#     value = 2,
#     label = "Round:",
#     label_position = "left",
#     validate = "focus",
#     validatecommand = validate_pos_int
#   )
#
#
#   f1_but_1 <- bs_radiobuttons(
#     parent         = upper_frame,
#     buttons = c(
#       col_vars   = "Variables",
#       col_stats  = "Statistics"
#     ),
#     # value           = buttons[1],
#     title           = "Columns of summary contain:",
#     commands        = list(),       # named list of functions
#     default_command = do_nothing,
#     tips            = list(),       # named list of strings
#     default_tip     = "",
#     border          = FALSE,
#     layout          = "horizontal",
#     sticky_buttons  = "w",
#     sticky_title    = "w"
#   )
#   tkgrid(f1_but_1$frame)
#
#   tkgrid(name_entry$frame, sticky = "w")
#
#
#   # Help menus -------------------------------------------------------------
#   help_menu <- function() {
#
#     menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)
#
#     menu_qq   <- tk2menu(menu_main, tearoff = FALSE)
#     menu_test <- tk2menu(menu_main, tearoff = FALSE)
#
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     tkadd(menu_main, "cascade",
#       label    = "Normality tests",
#       menu     = menu_test)
#
#     tkadd(menu_test, "command",
#       label    = "xxx",
#       command  = open_help("xxx", package = "xxx"))
#
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     tkadd(menu_main, "cascade",
#       label    = "xxx-B",
#       menu     = menu_qq)
#
#     tkadd(menu_qq, "command",
#       label    = "xxx",
#       command  = open_help("xxx", package = "xxx"))
#
#     tkadd(menu_qq, "separator")
#
#     tkadd(menu_qq, "command",
#       label    = "Package 'xxx'",
#       command  = open_online_fun("https://   xxx"))
#
#
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     tkpopup(menu_main,
#       tkwinfo("pointerx", top),
#       tkwinfo("pointery", top))
#   }
#   # Finalize ---------------------------------------------------------------
#
#   # Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   ok_cancel_help(
#     close_on_ok = TRUE,
#     on_help = help_menu,
#     reset_location = TRUE,
#     reset = "window_test_normality",
#     apply = "window_test_normality"
#   )
#
#   tkgrid(buttonsFrame, sticky = "ew")
#   dialogSuffix()
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#   # Interactive bindings ---------------------------------------------------
#
#   # Add interactivity for `fname_frame` and `fname_label`
#   # tkbind(file_label,     "<ButtonPress-1>", on_click)
#   # tkbind(fname_frame,    "<ButtonPress-1>", on_click)
#   # tkbind(fname_label,    "<ButtonPress-1>", on_click)
#   #
#   # tkbind(fname_frame, "<Enter>",
#   #        function() tkconfigure(fname_label, foreground = "blue"))
#   # tkbind(fname_frame, "<Leave>",
#   #        function() tkconfigure(fname_label, foreground = "black"))
#   # # tkconfigure(file_label,     cursor = "hand2")
#   # tkconfigure(fname_frame,    cursor = "hand2")
#   # tkconfigure(button_ch_file, cursor = "hand2")
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
# }
