# TODO:
# - add tips.
# - enable "groups in color" only if groups are selected (???)
# - Context menu for dataset name:
#       + to update and make unique name
#       + to correct the name (apply clean_str())
# - Add context menus to other entry fields:
#       + e.g., to reset to default value.
#
# TODO: simulate normal data of current group's sample size several (e.g. 8, 15,
#       or 24) times and plot on 3x3, 4x4, 5x5 grid with data of investigated
#       group to compare its normality. The normality tests could also be
#       performed.


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_test_normality <- function() {
  # Functions --------------------------------------------------------------

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  activate_tests <- function() {

    if (get_values(f2_num_enable)) {
      tkgrid(f2_num_sub)

    } else {
      tkgrid.remove(f2_num_sub)
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  activate_results_name <- function() {
    if (get_values(f2_keep_results, "keep_results")) {
      tkgrid(f2_results_name$frame)

    } else {
      tkgrid.remove(f2_results_name$frame)
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  activate_round_p <- function() {

    if (get_values(f2_as_markdown, "as_markdown")) {
      tkgrid(f2_round_p$frame)

    } else {
      tkgrid.remove(f2_round_p$frame)
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  activate_pearson <- function() {

    if (get_test_function() == "pearson.test") {
      tkgrid(f2_pearson_opts$frame)

    } else {
      tkgrid.remove(f2_pearson_opts$frame)
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  activate_plots <- function() {

    if (get_values(f2_plot_enable)) {
      tkgrid(f2_plot_sub)

    } else {
      tkgrid.remove(f2_plot_sub)
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  activate_band <- function() {

    if (get_values(f2_plot_opts, "qq_band")) {
      tkgrid(f2_band$frame)

    } else {
      tkgrid.remove(f2_band$frame)
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  activate_band_options <- function() {

    if (get_bandtype_function() == "boot") {
      tkgrid(f2_band_boot$frame)

    } else {
      tkgrid.remove(f2_band_boot$frame)
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  activate_all <- function() {
    activate_tests()
    activate_results_name()
    activate_round_p()
    activate_pearson()
    activate_plots()
    activate_band()
    activate_band_options()
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_test_list <- function() {
    tibble::tribble(
      ~name,                                              ~fun,
      gettext_bs("Shapiro-Wilk test"),                    "shapiro.test",
      gettext_bs("Anderson-Darling test"),                "ad.test",
      gettext_bs("Cramer-von Mises test"),                "cvm.test",
      gettext_bs("Lilliefors (Kolmogorov-Smirnov) test"), "lillie.test",
      gettext_bs("Shapiro-Francia test"),                 "sf.test",
      gettext_bs("Pearson chi-square test"),              "pearson.test")
  }

  get_test_function <- function() {
    res   <- get_selection(f2_test_name)
    tests <- get_test_list()
    tests[tests$name == res, ]$fun
  }

  get_test_name <- function(fun = get_test_function()) {
    tests <- get_test_list()
    tests[tests$fun == fun, ]$name
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_bandtype_list <- function() {
    tibble::tribble(
      ~name,                                     ~fun,
      gettext_bs("Point-wise (pointwise)"),      "pointwise",
      gettext_bs("Parametric bootstrap (boot)"), "boot",
      gettext_bs("Kolmogorov-Smirnov (ks)"),     "ks",
      gettext_bs("Tail-sensitive (ts)"),         "ts")
  }

  get_bandtype_function <- function() {
    name  <- get_selection(f2_band)
    bands <- get_bandtype_list()
    bands[bands$name == name, ]$fun
  }

  get_bandtype_name <- function(fun = get_bandtype_function()) {
    bands <- get_bandtype_list()
    bands[bands$fun == fun, ]$name
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onOK <- function() {
    # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cursor_set_busy(top)
    on.exit(cursor_set_idle(top))

    # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    y_var                <- get_selection(f1_widget_y_gr$y)
    gr_var               <- get_selection(f1_widget_y_gr$gr)
    by_group             <- get_values(f1_widget_y_gr$checkbox)

    use_test             <- get_values(f2_num_enable)
    test_function        <- get_test_function()
    as_markdown          <- get_values(f2_as_markdown, "as_markdown")
    keep_results         <- get_values(f2_keep_results, "keep_results")
    results_name         <- get_values(f2_results_name)
    digits_p             <- get_values(f2_round_p)
    bins                 <- get_values(f2_pearson_opts)
    test_simplify        <- get_values(f2_test_simplify, "test_simplify")

    use_plot             <- get_values(f2_plot_enable)
    new_plots_window     <- get_values(f2_plot_opts, "new_plots_window")
    plot_in_colors       <- get_values(f2_plot_opts, "plot_in_colors")
    qq_detrend           <- get_values(f2_plot_opts, "qq_detrend")
    qq_line              <- get_values(f2_plot_opts, "qq_line")
    qq_band              <- get_values(f2_plot_opts, "qq_band")
    qq_bandtype_function <- get_bandtype_function()
    bootstrap_n          <- get_values(f2_band_boot)
    conf_level           <- get_values(f2_conf)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    warn <- options(warn = -1)

    nbins <- as.numeric(bins)
    bootstrap_n <- as.integer(bootstrap_n)
    conf_level  <- as.numeric(conf_level)

    options(warn)

    # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if ((!use_test) && (!use_plot)) {
      show_error_messages(
        str_c(
          "You should either perform a normality test\n",
          "or draw a QQ plot, or do both options."),
        title = "Select What to Do",
        parent = top)

      return()
    }

    # Checking - test ----------------------------------------------------
    if (use_test) {

      if (variable_is_not_selected(y_var, "variable to test", parent = top)) {
        return()
      }

      if (variable_is_not_selected(test_function, "normality test", parent = top)) {
        return()
      }

      if (test_function == "pearson.test") {
        # Chi-square bins ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (bins != bins_auto && (is.na(nbins) || nbins < 4)) {
          show_error_messages(
            "Number of bins for chi-square test \nmust be 4 or more.",
            title = "Too Few Bins Selected",
            parent = top)
          return()
        }
      }

      if (keep_results) {

        if (is_empty_name(results_name, "dataset name", parent = top)) {
          return()
        }

        if (is_not_valid_name(results_name, parent = top)) {
          return()
        }

        if (forbid_to_replace_object(results_name, parent = top)) {
          return()
        }
      }
    }

    # Checking - plot ----------------------------------------------------
    if (use_plot) {

      if (qq_band) {

        if (variable_is_not_selected(
          qq_bandtype_function,
          "QQ line confidece band type",
          parent = top)) {
          return()
        }

        if (qq_bandtype_function == "boot") {
          if (!checkmate::test_integerish(bootstrap_n)) {
            show_error_messages(
              str_c(
                "The number of bootstrap replicates must be a whole number.\n",
                "Please, correct the number."),
              title = "Incorrect Number of Bootstrap Replicates",
              parent = top)

            return()
          }
        }

        if (!dplyr::between(conf_level, 0, 1)) {
          show_error_messages(
            str_c(
              "Confidence level must be a number between 0 and 1.\n",
              "Usually, 0.90, 0.95 or 0.99."),
            title = "Incorrect Cnfidence Level",
            parent = top)

          return()
        }

      }
    }

    # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    putDialog(
      "window_test_normality",
      list(
        by_group         = by_group,
        y_var            = y_var,
        gr_var           = gr_var,

        use_test         = use_test,
        test_name        = get_test_name(test_function),
        keep_results     = keep_results,
        as_markdown      = as_markdown,
        digits_p         = digits_p,
        bins             = bins,

        use_plot         = use_plot,
        new_plots_window = new_plots_window,
        plot_in_colors   = plot_in_colors,
        qq_detrend       = qq_detrend,
        qq_line          = qq_line,
        qq_band          = qq_band,
        bootstrap_n      = bootstrap_n,
        conf_level       = conf_level,
        qq_band_type     = get_bandtype_name(qq_bandtype_function)
      )
    )

    # Construct commands -------------------------------------------------

    Library("tidyverse")

    y_var  <- safe_names(y_var)
    gr_var <- safe_names(gr_var)


    # For many groups ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (by_group && length(gr_var) > 0) {
      gr_var_str  <- paste0(gr_var, collapse = ", ")
      gr_var_plot <- paste0(gr_var, collapse = " + ")

    } else {
      gr_var_str  <- ""
      gr_var_plot <- ""
    }

    # Commands - plot ----------------------------------------------------

    if (use_plot == TRUE) {

      if (qq_detrend) {

        y_label <- "Distance between empirical \\n quantiles and QQ line"
        title_detrend  <- " (detrended)"
        detrend_code   <- "detrend = TRUE"
        qq_points_code <- "qqplotr::stat_qq_point(detrend = TRUE) + "

      } else {

        y_label <- "Empirical quantiles"
        title_detrend <- ""
        detrend_code <- NULL
        qq_points_code <- "qqplotr::stat_qq_point() + "
      }

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # If plot by group
      use_color_code <- 'color = "grey50"'
      use_fill_code  <- ""
      gr_legend_code <- NULL
      facet_code     <- ""

      if (by_group) {
        facet_code     <- str_glue('facet_wrap(~{gr_var_plot}, scales = "free") + ')
        gr_legend_code <- str_glue('fill = "{gr_var_plot}", \n color = "{gr_var_plot}"')

        if (plot_in_colors) {
          if (length(gr_var) > 1) {
            # Several grouping variables
            use_fill_code <- str_glue(
              .trim = FALSE,
              ',\n fill = interaction({gr_var_str}, sep = "|")')

            use_color_code <- str_glue(
              .trim = FALSE,
              '\n mapping = aes(color = interaction({gr_var_str}, sep = "|"))')

          } else if (length(gr_var) == 1) {
            # One grouping variable
            use_fill_code  <- str_glue(", fill = {gr_var_str}")
            use_color_code <- str_glue("mapping = aes(color = {gr_var_str})")
          }
        }
      }

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      if (qq_band) {
        bootstrap_n_code <- NULL
        title_boot       <- ""
        conf_band_name <-
          str_remove(get_bandtype_name(qq_bandtype_function), " \\(.*?\\)")
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if (qq_bandtype_function == "boot") {
          bootstrap_n_code <- str_glue("B = {bootstrap_n}", .trim = FALSE)
          title_boot       <- str_glue(" ({bootstrap_n} rep.)")
        }

        band_arg_code <- str_glue(str_c(
          detrend_code,
          "alpha = 0.3",
          "conf = {conf_level}",
          '\n bandType = "{qq_bandtype_function}"',
          bootstrap_n_code,
          sep = ", "))

        qq_band_code <- str_glue("qqplotr::stat_qq_band(\n{band_arg_code}) + ")

        labs_subtitle <- str_glue(
          'subtitle = "Band: {conf_band_name}{title_boot}, ',
          'confidence level: {conf_level}"')

      } else {
        qq_band_code <- ""
        labs_subtitle <- NULL
      }

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (qq_line) {
        line_arg_code <- str_c(detrend_code,
          {
            use_color_code
          },
          sep = ", ")
        qq_line_code <- str_glue("qqplotr::stat_qq_line({line_arg_code}) + ")
      } else {
        qq_line_code <- ""
      }

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      labs_x     <- 'x = "Theoretical quantiles"'
      labs_y     <- str_glue('y = "{y_label}"')
      labs_title <- str_glue('title = "Normal QQ plot{title_detrend} of {y_var}"')

      lab_args_code <- str_c(
        labs_x, labs_y, gr_legend_code, labs_title, labs_subtitle,
        sep = ",\n        "
      )

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      command_plot <-
        str_glue(
          .sep = "\n",
          .trim = FALSE,
          "## Normal QQ plot",
          "ggplot({.ds}, aes(sample = {y_var}{use_fill_code})) + ",
          "    {qq_band_code}",
          "    {qq_line_code}",
          "    {qq_points_code}",
          "    {facet_code}",
          "    labs({lab_args_code}) + ",
          "    theme_bw()") %>%
        str_replace_all("((\n)?    \n( )*?\n|\n\n|\n    \n)", "\n")

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      Library("qqplotr")

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (new_plots_window == TRUE) {
        open_new_plots_window()
      }

    } else {
      command_plot <- NULL
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Commands - test ----------------------------------------------------

    if (use_test) {

      chi_sq_params <-
        if (test_function != "pearson.test" || bins == bins_auto) {
          ""
        } else {
          str_glue("n.classes = ", bins)
        }


      if (test_simplify) {
        empty_htest_obj <- ""
        new_line <- ""
        perform_test_code <- str_glue("{test_function}({chi_sq_params})")

      } else {
        # FIXME: name "htest_na" should not overwrite existing variables. Check
        #        its uniqueness.

        empty_htest_obj <-
          'htest_na <- structure(list(statistic = NA), class = "htest") \n'
        new_line <- "\n"
        perform_test_code <-
          str_glue(.trim = FALSE,
            "\n possibly({test_function}, otherwise = htest_na)({chi_sq_params})")
        # perform_test_code <- str_glue("safely({test_function})({chi_sq_params}) %>% .$result")
      }


      single_test_code <-
        str_glue(".${y_var} %>% {perform_test_code} %>% broom::tidy()")
      # str_glue("(.) %>% pull({y_var}) %>% {perform_test_code} %>% broom::tidy()")
      # str_glue(".${y_var} %>% {test_function}({chi_sq_params}) %>% broom::tidy()")
      # str_glue("broom::tidy({test_function}(.${y_var}{chi_sq_params}))")

      accu <- str_c("0.",  str_dup(0, times = as.integer(digits_p) - 1), "1")

      main_test_code <-
        if (by_group) {
          str_glue(
            "group_by({gr_var_str}) %>% \n",
            "group_modify({new_line} ~ {single_test_code})")
        } else {
          single_test_code
        }

      print_results_code <-
        if (as_markdown) {
          str_glue(
            " %>% \n",
            "  mutate(p.value = scales::pvalue(p.value, accuracy = {accu})) %>% \n",
            '  knitr::kable(digits = {digits_p}, format = "pandoc")'
          )

        } else {
          ""
        }

      # Keep rezults ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      keep_results_str <-
        if (keep_results) {
          ""
        } else {
          "\n remove({results_name})"
        }

      # Test results ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Command
      command_do_test <- str_glue(
        "## Notmality test \n {empty_htest_obj}",
        "{results_name} <- \n {.ds} %>%\n",
        "    {main_test_code} \n\n",

        "## Print the results of the notmality test \n",
        "{results_name} ",
        print_results_code, "\n",
        keep_results_str)

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (test_function != "shapiro.test") {
        Library("nortest")
      }
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    } else {
      command_do_test <- NULL
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    command <- str_c(command_plot, command_do_test, sep = "\n\n")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Checks for syntax errors
    result <- try_command(command)

    if (class(result)[1] == "try-error") {
      logger_error(command, error_msg = as.character(result))
      show_code_evaluation_error_message(parent = top,
        add_msg = result$message)
      # ??? `result$message` or just `result`
      return()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Checks for code evaluation errors
    result <- doItAndPrint(style_cmd(command))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (class(result)[1] == "try-error") {

      logger_error(command, error_msg = result)
      show_code_evaluation_error_message(
        parent = top,
        add_msg = as.character(result),
        add_note = "\nNOTE: normaity is tested in each group separately.")
      return()

    } else {
      # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # close_dialog()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # command_dataset_refresh()
    tkfocus(CommanderWindow())
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Announce about the success to run the function `onOk()`
    TRUE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  }

  # Initialize -------------------------------------------------------------
  .ds   <- active_dataset_0()
  nrows <- getRcmdr("nrow") # nrows in active dataset
  bins_auto <- gettext("<auto>")

  initialize_dialog(title = gettext_bs("Test Normality by Group"))
  tk_title(top, text = "Normality Tests and Normal QQ Plots")

  defaults <- list(
    # Variables
    y_var     = NULL,
    gr_var    = NULL,
    by_group  = FALSE,

    # Normality test
    use_test  = TRUE,
    test_name =
    # if (nrows <= 5000) {
      gettext_bs("Shapiro-Wilk test")
    # } else {
    #     gettext_bs("Anderson-Darling test")
    # }
    ,
    bins = bins_auto,

    keep_results     = FALSE,
    as_markdown      = TRUE,
    digits_p         = "3",
    bins             = bins_auto,

    test_simplify    = FALSE,

    # QQ plot
    use_plot         = TRUE,
    new_plots_window = is_plot_in_separate_window(),
    plot_in_colors   = TRUE,
    qq_detrend       = FALSE,
    qq_line          = TRUE,

    qq_band          = TRUE,
    qq_band_type     = gettext_bs("Point-wise (pointwise)"),
    bootstrap_n      = 999,
    conf_level       = 0.95
  )

  initial <- getDialog("window_test_normality", defaults)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f0 <- tkframe(top)

  # F1 ---------------------------------------------------------------------
  f1 <- tkframe(f0)

  f1_widget_y_gr <- bs_listbox_y_gr(
    parent         = f1,
    y_title        = gettext_bs("Variable to test\n(pick one)"),
    y_var_type     = "num",
    y_initial      = initial$y_var,
    y_select_mode  = "single",

    gr_title       = gettext_bs("Groups variable\n(pick one, several or none)"),
    gr_var_type    = "fct_like",
    gr_initial     = initial$gr_var,
    gr_select_mode = "multiple",

    ch_initial     = initial$by_group
  )

  # F2 ---------------------------------------------------------------------
  f2 <- tkframe(f0)

  # F2 test ----------------------------------------------------------------
  f2_num <- tk2labelframe(f2, text = "Test options")

  f2_num_enable <- bs_checkboxes(
    parent   = f2_num,
    boxes    = "do_test",
    labels   = gettext_bs("Perform normality test"),
    values   = initial$use_test,
    commands = list("do_test"  = activate_tests)
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_num_sub <- tk2frame(f2_num)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_as_markdown <- bs_checkboxes(
    parent   = f2_num_sub,
    border   = FALSE,
    boxes    = "as_markdown",
    labels   = gettext_bs("Print as Markdown table"),
    commands = list(as_markdown  = activate_round_p),
    values   = initial$as_markdown,
    default_tip = str_c(
      "The way how the results should be printed: \n",
      "1. [selected] As Markdown table. \n",
      "2. [unselected] As tibble.")
  )


  f2_round_p <- bs_radiobuttons(
    parent  = f2_num_sub,
    title   = "Round p-values to decimal digits: ",
    buttons = c("2" = "2", "3" = "3", "4" = "4", "5" = "5", "10" = "more"),
    layout  = "horizontal",
    value   = initial$digits_p,
    default_tip = "Rounding for p value and test statistic."
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  f2_keep_results <- bs_checkboxes(
    parent   = f2_num_sub,
    border   = FALSE,
    boxes    = "keep_results",
    labels   = gettext_bs("Keep test results in R memory"),
    commands = list(keep_results = activate_results_name),
    values   = initial$keep_results,
    default_tip = str_c(
      "Keep the results as data frame if you \n",
      "want to use them later (e.g., to plot)")
  )

  f2_results_name <- bs_entry(
    parent = f2_num_sub,
    value  = unique_obj_names("normality", all_numbered = TRUE),
    width  = 33,
    label  = gettext_bs("Dataset name:"),
    label_position = "above",
    validate = "focus",
    validatecommand = validate_var_name_string,
    invalidcommand  = make_red_text
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_test_name <- bs_combobox(
    parent = f2_num_sub,
    width = 30,
    # label = "Test:",
    # label_position = "above",
    values = c(
      # if (nrows <= 5000)
      gettext_bs("Shapiro-Wilk test"),
      gettext_bs("Anderson-Darling test"),
      gettext_bs("Cramer-von Mises test"),
      gettext_bs("Lilliefors (Kolmogorov-Smirnov) test"),
      # if (nrows <= 5000)
      gettext_bs("Shapiro-Francia test"),
      gettext_bs("Pearson chi-square test")
    ),
    tip = "Name of normality test",
    value = initial$test_name,
    on_select = activate_pearson
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  validate_pearson_bins <- function(P, W) {
    # P - value
    res <- is_pos_integer_str(P) || str_detect(P, bins_auto)

    if (res == TRUE) {
      tkconfigure(W, foreground = "black")
      return(tcl("expr", "TRUE"))
    } else {
      return(tcl("expr", "FALSE"))
    }
  }

  f2_pearson_opts <- bs_entry(
    parent = f2_num_sub,
    value  = initial$bins,
    width  = "8",
    label  = gettext_bs("Number of bins for\nPearson chi-square"),
    validate = "focus",
    validatecommand = validate_pearson_bins,
    invalidcommand  = make_red_text_reset_val(to = bins_auto)
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_test_simplify <- bs_checkboxes(
    parent   = f2_num_sub,
    border   = FALSE,
    boxes    = "test_simplify",
    labels   = gettext_bs("Simplify code"),
    tips     = list(test_simplify = str_c(
      "Which version of the code shoulde be used: \n",
      "1. [unselected]: a more robust and more universal code. \n",
      "2. [selected]: a less complex and easier-to-understand code.")
    ),
    values   = initial$test_simplify
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # F2 plot ----------------------------------------------------------------

  f2_plot <- tk2labelframe(f2, text = "Plot options")

  f2_plot_enable <- bs_checkboxes(
    parent   = f2_plot,
    boxes    = "use_plot",
    labels   = gettext_bs("Draw normal QQ plot"),
    values   = initial$use_plot,
    commands = list("use_plot" = activate_plots)
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_plot_sub <- tk2frame(f2_plot)

  f2_plot_opts <- bs_checkboxes(
    parent = f2_plot_sub,
    border = FALSE,
    boxes  = c(
      "new_plots_window",
      "plot_in_colors",
      "qq_detrend",
      "qq_line",
      "qq_band"
    ),
    values = c(
      initial$new_plots_window,
      initial$plot_in_colors,
      initial$qq_detrend,
      initial$qq_line,
      initial$qq_band
    ),
    labels = gettext_bs(
      c(
        "Create new window for plots",
        "Use colors for groups",
        "Detrend",
        "Add reference line",
        "Add confidence band")
    ),
    commands = list(
      qq_band = activate_band
    )
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_band <- bs_combobox(
    parent = f2_plot_sub,
    width  = 25,
    label  = "Type of confidence band:",
    label_position = "above",
    values = gettext_bs(c(
      "Point-wise (pointwise)",
      "Tail-sensitive (ts)",
      "Kolmogorov-Smirnov (ks)",
      "Parametric bootstrap (boot)")),
    value = initial$qq_band_type,
    tip = "Type of confidence band for qq-line",
    on_select = activate_band_options)

  f2_band_boot <-
    bs_entry(
      f2_band$frame,
      width   = 6,
      value   = initial$bootstrap_n,
      justify = "right",
      label   = "Number of boot-\nstrap replicates",
      tip     = str_c(
        "Positive integer. Usually number between \n",
        "1000 and 10 000. Larger numbers result   \n",
        "in longer calculations."),
      validate = "key",
      validatecommand = validate_pos_int,
      invalidcommand  = make_red_text
    )

  f2_conf <-
    bs_entry(
      f2_band$frame,
      width   = 6,
      value   = initial$conf_level,
      justify = "center",
      label   = "Confidence level",
      tip     = str_c(
        "Number between 0 and 1.            \n",
        "Usually 0.90, 0.95 or 0.99.        \n\n",
        "Let 'signif' be significance level \n",
        "and 'conf' be confidence level.    \n",
        "Then: signif = 1 - conf"
      ),
      validate = "key",
      validatecommand = validate_num_0_1,
      invalidcommand  = make_red_text)

  # Layout -----------------------------------------------------------------
  tkgrid(f0)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f1, sticky = "nwe", padx = c(0, 4))
  tkgrid(f1_widget_y_gr$frame, sticky = "nwe", padx = c(10, 0))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f2, pady = c(5, 0),  sticky = "")
  tkgrid(f2_num, f2_plot,     sticky = "nse", padx = c(0, 5))

  # Numerical options
  tkgrid(f2_num_enable$frame, sticky = "nwe", padx = c(5, 70))
  tkgrid(f2_num_sub)
  tkgrid(f2_test_name$frame,                  padx = 5, pady = c(3, 5))
  tkgrid(f2_pearson_opts$frame, sticky = "nse", padx = c(8, 5), pady = c(0, 2))

  tkgrid(f2_as_markdown$frame, sticky = "w",  padx = c(5, 0))
  tkgrid(f2_round_p$frame,     sticky = "w",  padx = 5, pady = c(0, 5))
  tkgrid(f2_round_p$frame_obj, sticky = "w")

  tkgrid(f2_keep_results$frame, sticky = "w",  padx = c(5, 0), pady = c(0, 0))
  tkgrid(f2_results_name$frame, sticky = "w",  padx = c(5, 0), pady = c(0, 5))

  tkgrid(f2_test_simplify$frame, sticky = "w",  padx = c(5, 5))


  # Plot
  tkgrid(f2_plot_enable$frame, sticky = "nwe", padx = c(5, 43))
  tkgrid(f2_plot_sub,          sticky = "nwe")
  tkgrid(f2_plot_opts$frame,   sticky = "nwe", padx = c(5, 0))
  tkgrid(f2_band$frame,                        padx = 5, pady = 5)
  tkgrid(f2_band_boot$frame,   sticky = "e",   padx = 0, pady = c(2, 0))
  tkgrid(f2_conf$frame,        sticky = "e",   padx = 0, pady = 4)

  # Help menus -------------------------------------------------------------
  help_menu <- function() {

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

    menu_qq <- tk2menu(menu_main, tearoff = FALSE)
    menu_test <- tk2menu(menu_main, tearoff = FALSE)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "cascade",
      label    = "Normality tests",
      menu     = menu_test)

    tkadd(menu_test, "command",
      label    = "Shapiro-Wilk test for normality",
      command  = open_help("shapiro.test", package = "stats"))

    tkadd(menu_test, "command",
      label    = "Anderson-Darling test for normality",
      command  = open_help("ad.test", package = "nortest"))

    tkadd(menu_test, "command",
      label    = "Cramer-von Mises test for normality",
      command  = open_help("cvm.test", package = "nortest"))

    tkadd(menu_test, "command",
      label    = "Shapiro-Francia test for normality",
      command  = open_help("sf.test", package = "nortest"))

    tkadd(menu_test, "command",
      label    = "Pearson chi-square test for normality",
      command  = open_help("pearson.test", package = "nortest"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_test, "separator")

    tkadd(menu_test, "command",
      label    = "Function 'possibly' (package 'purrr')",
      command  = open_help("possibly", package = "purrr"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "cascade",
      label    = "Quantile comparison (QQ) plot",
      menu     = menu_qq)

    tkadd(menu_qq, "command",
      label    = "QQ points",
      command  = open_help("stat_qq_point", package = "qqplotr"))

    tkadd(menu_qq, "command",
      label    = "QQ line",
      command  = open_help("stat_qq_line", package = "qqplotr"))

    tkadd(menu_qq, "command",
      label    = "QQ band",
      command  = open_help("stat_qq_band", package = "qqplotr"))

    tkadd(menu_qq, "separator")

    tkadd(menu_qq, "command",
      label    = "Package 'qqplotr'",
      command  = open_online_fun("https://aloy.github.io/qqplotr/articles/introduction.html"))

    tkadd(menu_qq, "command",
      label    = "QQ plot explained (YouTube)",
      command  = open_online_fun("https://www.youtube.com/watch?v=okjYjClSjOg#t=9"))

    tkadd(menu_qq, "command",
      label    = "QQ plot interpretation (Stack Overflow forum)",
      command  = open_online_fun("https://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }

  # Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ok_cancel_help(
    close_on_ok = TRUE,
    on_help = help_menu,
    reset_location = TRUE,
    reset = "window_test_normality",
    apply = "window_test_normality")

  tkgrid(buttonsFrame, sticky = "ew")
  dialogSuffix()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  activate_all()
}
