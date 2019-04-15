# TODO:
# - write onOK function.
# - add tips.
# - enable "groups in color" only if groups are selected (???)
#

# `qqplotr` Documentation: https://aloy.github.io/qqplotr/


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
            ~name,                                             ~fun,
            gettext_bs("Shapiro-Wilk test")                    , "shapiro.test",
            gettext_bs("Anderson-Darling test")                , "ad.test",
            gettext_bs("Cramer-von Mises test")                , "cvm.test",
            gettext_bs("Lilliefors (Kolmogorov-Smirnov) test") , "lillie.test",
            gettext_bs("Shapiro-Francia test")                 , "sf.test",
            gettext_bs("Pearson chi-square test")              , "pearson.test")
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
            gettext_bs("Point-wise (pointwise)")     , "pointwise",
            gettext_bs("Parametric bootstrap (boot)"), "boot",
            gettext_bs("Kolmogorov-Smirnov (ks)")    , "ks",
            gettext_bs("Tail-sensitive (ts)")        , "ts")
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


        # Reset widget properties before checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # tkconfigure(name_entry, foreground = "black")

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
                by_group         = by_group         ,
                y_var            = y_var            ,
                gr_var           = gr_var           ,

                use_test         = use_test         ,
                test_name        = get_test_name(test_function),
                keep_results     = keep_results     ,
                as_markdown      = as_markdown      ,
                digits_p         = digits_p         ,
                bins             = bins             ,

                use_plot         = use_plot         ,
                new_plots_window = new_plots_window ,
                plot_in_colors   = plot_in_colors   ,
                qq_detrend       = qq_detrend       ,
                qq_line          = qq_line          ,
                qq_band          = qq_band          ,
                bootstrap_n      = bootstrap_n      ,
                conf_level       = conf_level       ,
                qq_band_type     = get_bandtype_name(qq_bandtype_function)
            )
        )

        # Construct commands -------------------------------------------------

        Library("tidyverse")

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

                y_label <- "Distance between empirical quantiles\\n and QQ line"
                title_0 <- " (detrended)"
                detrend_code <- "detrend = TRUE"

                qq_points_code <- '    qqplotr::stat_qq_point(detrend = TRUE) + \n'

            } else {

                y_label <- "Empirical quantiles"
                title_0 <- ""
                detrend_code <- NULL

                qq_points_code <- '    qqplotr::stat_qq_point() + \n'

            }
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # If plot by group
            use_color_code   <- 'color = "grey50"'
            use_fill_code    <- ""
            fill_legend_code <- ""
            facet_code       <- ""

            if (by_group) {
                facet_code       <- str_glue('facet_wrap(~{gr_var_plot}, scales = "free") + ')
                fill_legend_code <- 'fill = "Groups", \n color = "Groups",  \n'

                if (plot_in_colors) {
                    if (length(gr_var) > 1) {
                        # Several grouping variables
                        use_fill_code <- str_c(",\n", str_glue(
                            'fill = interaction({gr_var_str}, sep = "|")'))

                        use_color_code <- str_c("\n", str_glue(
                            'mapping = aes(color = interaction({gr_var_str}, sep = "|"))'))

                    } else if (length(gr_var) == 1) {
                        # One grouping variable
                        use_fill_code  <- str_glue(', fill = {gr_var_str}')
                        use_color_code <- str_glue('mapping = aes(color = {gr_var_str})')
                    }
                }
            }

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if (qq_band) {



                    if (qq_bandtype_function == "boot") {
                        bootstrap_n_code <- str_glue("B = {bootstrap_n}")
                        title_boot       <- str_glue("({bootstrap_n} rep.)")

                    } else {
                        bootstrap_n_code <- NULL
                        title_boot       <- ""
                    }

                band_arg_code <- str_glue(str_c(
                    detrend_code,
                    "alpha = 0.3",
                    'bandType = "{qq_bandtype_function}"',
                    bootstrap_n_code,
                    "conf = {conf_level}",
                    sep = ", "))

                qq_band_code <- str_glue('    qqplotr::stat_qq_band({band_arg_code}) + ')
            } else {
                qq_band_code <- ""
            }

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if (qq_line) {
                line_arg_code <- str_c(detrend_code, {use_color_code}, sep = ", ")
                qq_line_code <- str_glue('    qqplotr::stat_qq_line({line_arg_code}) + ')
            } else {
                qq_line_code <- ""
            }


            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            conf_band_name <-
                str_remove(get_bandtype_name(qq_bandtype_function), " \\(.*?\\)")

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            command_plot <-
                str_glue(
                    .sep = "\n",
                    "## Normal QQ plot",
                    'ggplot({.ds}, aes(sample = {y_var}{use_fill_code})) + ',
                    '    {qq_band_code}',
                    '    {qq_line_code}',
                    '    {qq_points_code}',
                    '    {facet_code}',

                    '    labs(x = "Theoretical quantiles", ',
                    '        y = "{y_label}",',
                    '        {fill_legend_code}',
                    '        title = "Normal QQ plot{title_0} of {y_var}",    ',
                    '        subtitle = "Confidence level: {conf_level}, band: {conf_band_name}{title_boot}") + ',
                    '    theme_bw()') %>%
                str_replace_all("((\n)?\n( )*?\n)", "\n")


            Library("qqplotr")

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

            single_test_code <-
                str_glue(".${y_var} %>% {test_function}({chi_sq_params}) %>% broom::tidy()")
            # str_glue("broom::tidy({test_function}(.${y_var}{chi_sq_params}))")

            main_test_code <-
                if (by_group) {
                    str_glue(
                        "group_by({gr_var_str}) %>% \n",
                        "group_map( ~ {single_test_code})")
                } else {
                    single_test_code
                }

            print_results_code <-
                if (as_markdown) {
                    str_glue(' %>% \n  knitr::kable(digits = {digits_p}, format = "pandoc")')

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
                "## Notmality test \n",
                "{results_name} <- \n {.ds} %>%\n",
                "    {main_test_code} \n\n",

                "## Print results \n",
                "{results_name} ",
                print_results_code, "\n",
                keep_results_str)


            Library("nortest")

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
            show_code_evaluation_error_message(parent = top)
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Checks for code evaluation errors
        result <- doItAndPrint(style_cmd(command))

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] == "try-error") {

            logger_error(command, error_msg = result)
            show_code_evaluation_error_message(parent = top)
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
            if (nrows <= 5000) {
                gettext_bs("Shapiro-Wilk test")
            } else {
                gettext_bs("Anderson-Darling test")
            },
        bins = bins_auto,

        keep_results     = FALSE,
        as_markdown      = TRUE,
        digits_p         = "3",
        bins             = bins_auto,

        # QQ plot
        use_plot         = TRUE,
        new_plots_window = TRUE,
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

    # f2_num_opts <- bs_checkboxes(
    #     parent = f2_num_sub,
    #     border = FALSE,
    #     boxes = c("as_markdown", "keep_results"),
    #     labels = gettext_bs(c(
    #         "Print as Markdown table",
    #         "Keep test results in R memory"
    #     )),
    #     commands = list(
    #         keep_results = activate_results_name,
    #         as_markdown  = activate_round_p
    #         ),
    #     values = c(
    #         initial$as_markdown,
    #         initial$keep_results
    #     )
    # )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2_as_markdown <- bs_checkboxes(
        parent   = f2_num_sub,
        border   = FALSE,
        boxes    = "as_markdown",
        labels   = gettext_bs("Print as Markdown table"),
        commands = list(as_markdown  = activate_round_p),
        values   = initial$as_markdown
    )

    f2_round_p <- bs_radiobuttons(
        parent  = f2_num_sub,
        title   = "Round p-values to decimal digits: ",
        buttons = c("2" = "2", "3" = "3", "4" = "4", "5" = "5", "8" = "more"),
        layout  = "horizontal",
        value   = initial$digits_p
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    f2_keep_results <- bs_checkboxes(
        parent   = f2_num_sub,
        border   = FALSE,
        boxes    = "keep_results",
        labels   = gettext_bs("Keep test results in R memory"),
        commands = list(keep_results = activate_results_name),
        values   = initial$keep_results
    )

    f2_results_name <- bs_entry(
        parent = f2_num_sub,
        value  = unique_obj_names("normality"),
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
            if (nrows <= 5000) gettext_bs("Shapiro-Wilk test"),
            gettext_bs("Anderson-Darling test"),
            gettext_bs("Cramer-von Mises test"),
            gettext_bs("Lilliefors (Kolmogorov-Smirnov) test"),
            if (nrows <= 5000) gettext_bs("Shapiro-Francia test"),
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
        boxes  = c("new_plots_window",
                   "plot_in_colors",
                   "qq_detrend",
                   "qq_line",
                   "qq_band"),
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
            "Parametric bootstrap (boot)",
            "Kolmogorov-Smirnov (ks)",
            "Tail-sensitive (ts)")),
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
                "Number between 0 and 1.         \n",
                "Usually 0.90, 0.95 or 0.99.      \n\n",
                "If 'signif' be significance level, \n",
                "and 'conf' be confidence level.   \n",
                "Then signif = 1 - conf."
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
    tkgrid(f2_pearson_opts$frame,sticky = "nse",padx = c(8, 5), pady = c(0, 2))

    tkgrid(f2_as_markdown$frame, sticky = "w",  padx = c(5, 0))
    tkgrid(f2_round_p$frame,     sticky = "w",  padx = 5, pady = c(0, 5))
    tkgrid(f2_round_p$frame_obj, sticky = "w")

    tkgrid(f2_keep_results$frame,sticky = "w",  padx = c(5, 0), pady = c(0, 0))
    tkgrid(f2_results_name$frame,sticky = "w",  padx = c(5, 0), pady = c(0, 5))


    # Plot
    tkgrid(f2_plot_enable$frame, sticky = "nwe", padx = c(5, 43))
    tkgrid(f2_plot_sub,          sticky = "nwe")
    tkgrid(f2_plot_opts$frame,   sticky = "nwe", padx = c(5, 0))
    tkgrid(f2_band$frame,                        padx = 5, pady = 5)
    tkgrid(f2_band_boot$frame,   sticky = "e",   padx = 0, pady = c(2, 0))
    tkgrid(f2_conf$frame,        sticky = "e",   padx = 0, pady = 4)

    # Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(
        close_on_ok = TRUE,
        helpSubject = "shapiro.test",
        # helpPackage = "stats",
        reset = "window_test_normality",
        apply = "window_test_normality")

    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    activate_all()
}

