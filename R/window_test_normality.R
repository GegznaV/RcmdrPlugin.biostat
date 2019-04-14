# TODO:
# - write onOK function.
# - add tips.
# - enable "groups in color" only if groups are selected (???)

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
        if (get_values(f2_num_opts, "keep_results")) {
            tkgrid(f2_results_name$frame)

        } else {
            tkgrid.remove(f2_results_name$frame)
        }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    activate_pearson <- function() {

        if (get_selection(f2_test_name) == gettext_bs("Pearson chi-square test")) {
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

        if (get_band_type() == "boot") {
            tkgrid(f2_band_boot$frame)

        } else {
            tkgrid.remove(f2_band_boot$frame)
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    activate_all <- function() {
        activate_tests()
        activate_results_name()
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
        # Get values ---------------------------------------------------------

        by_group             <- get_values(f1_widget_y_gr$checkbox)
        y_var                <- get_selection(f1_widget_y_gr$y)
        gr_var               <- get_selection(f1_widget_y_gr$gr)

        use_test             <- get_values(f2_num_enable)
        test_function        <- get_test_function()
        as_markdown          <- get_values(f2_num_opts, "as_markdown")
        keep_results         <- get_values(f2_num_opts, "keep_results")
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

        # positive integer in range 1000 - 1E4
        bootstrap_n      <- as.integer(get_values(f2_band_boot))

        # between 0 - 1
        conf_level       <- as.numeric(get_values(f2_conf))

        # Chi-square bins ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        warn  <- options(warn = -1)
        nbins <- as.numeric(bins)
        options(warn)

        if (bins != bins_auto && (is.na(nbins) || nbins < 4)) {
            errorCondition(
                recall = window_test_normality,
                message = gettext_bs("Number of bins must be a number >= 4")
            )
            return()
        }

        chi_sq_params <-
            if (test_function != "pearson.test" || bins == bins_auto) {
                ""
            } else {
                str_glue(",\n n.classes = ", bins)
            }

        if (length(y_var) == 0) {
            errorCondition(recall = window_test_normality,
                           message = gettext_bs("You must select a variable."))
            return()
        }

        # putDialog ----------------------------------------------------------
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


        closeDialog()

        # Do analysis --------------------------------------------------------
        Library("tidyverse")
        # Library("biostat")
        Library("nortest")
        Library("qqplotr")


        # For many groups ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(gr_var) > 0) {
            # gr_var <- paste0(gr_var, collapse = " + ")
            gr_var_str  <- paste0(gr_var, collapse = ", ")
            gr_var_plot <- paste0(gr_var, collapse = " + ") # ??? <----- -----

        } else {
            gr_var_str  <- ""
            gr_var_plot <- ""
        }

        by_gr_str <-
            if (length(gr_var) == 0) {
                ""

            } else {
                str_glue("group_by({gr_var_str}) %>%\n")
            }

        # Round ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        round_str <- if (digits_p > 0) {
            str_glue(
                .sep = "\n",
                '    mutate(',
                '        statistic = formatC(statistic, digits = 3, format = "f"),',
                '        p.value   = formatC(p.value,   digits = {digits_p}, format = "f"),',
                '    ) %>% '
            )
        } else {
            ""
        }


        # --------------------------------------------------------------------
        print_as_report <-
            if (as_markdown == TRUE) {
                Library("pander")
                '\n pander::pander(style = "simple")'

            } else {
                # " %>% \n    print({print_opt})\n"
                "\n print.data.frame()"
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
        command <- str_glue(
            "{results_name} <- {.ds} %>%\n",
            "    {by_gr_str}",
            "    do(broom::tidy({test_function}(.${y_var}{chi_sq_params})))\n\n",

            "{results_name} %>% \n",
            round_str,
            print_as_report,
            keep_results_str)


        doItAndPrint(style_cmd(command))


        # plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        # library(ggplot2)
        # library(qqplotr)
        # data("barley", package = "lattice")
        #
        # qqplot_1 <-
        #     ggplot(data = barley, aes(sample = yield)) +
        #     stat_qq_band(mapping = aes(fill = site), alpha = 0.5) +
        #     stat_qq_line(color = "darkred", alpha = 0.8) +
        #     stat_qq_point() +
        #     facet_wrap(~site, scales = "free") +
        #     theme_bw() +
        #     labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
        #
        # qqplot_1

        if (use_plot == TRUE) {

            if (new_plots_window == TRUE) {
                open_new_plots_window()
            }

            if (length(gr_var) == 0) {
                command2 <- str_glue(
                    'biostat::qq_plot(~{y_var}, ',
                    'data = {.ds}, use_colors = {plot_in_colors})')

            } else {
                command2 <- str_glue(
                    'biostat::qq_plot({y_var} ~ {gr_var}, ',
                    'data = {.ds}, use_colors = {plot_in_colors})')
            }

            # Paketo `qqplotr` dokumentacija: https://aloy.github.io/qqplotr/
            ggplot({.ds}, aes(sample = {y_var})) +
                stat_qq_band(detrend = T) +
                stat_qq_line(detrend = T) +
                stat_qq_point(detrend = T) +
                facet_wrap(~{gr_var}, scales = "free") +
                labs(x = "Theoretical quantiles",
                     y = "Empirical quantiles",
                     title = "Normal QQ plot",
                     subtitle = "confidence level: {conf_level}, band: {conf_band}")

            # ggplot(data = smp, mapping = aes(sample = norm)) +
            #     stat_qq_band(detrend = TRUE) +
            #     stat_qq_line(detrend = TRUE) +
            #     stat_qq_point(detrend = TRUE)

            # [VG] ???    <----  ------
            # ...


            doItAndPrint(command2)
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
        # onOK [end] ---------------------------------------------------------
    }

    # Initialize -------------------------------------------------------------
    .ds   <- active_dataset_0()
    nrows <- getRcmdr("nrow") # nrows in active dataset
    bins_auto <- gettext("<auto>")

    initializeDialog(title = gettext_bs("Test Normality by Group"))
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
        as_markdown      = FALSE,
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
        y_initial      = initial$var_y,
        y_select_mode  = "single",

        gr_title       = gettext_bs("Groups variable\n(pick one, several or none)"),
        gr_var_type    = "fct_like",
        gr_initial     = initial$var_gr,
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

    f2_num_opts <- bs_checkboxes(
        parent = f2_num_sub,
        border = FALSE,
        boxes = c("as_markdown", "keep_results"),
        labels = gettext_bs(c(
            "Print as Markdown table",
            "Keep test results in R memory"
            )),
        commands = list(keep_results = activate_results_name),
        values = c(
            initial$as_markdown,
            initial$keep_results
        )
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
    f2_round_p <- bs_radiobuttons(
        parent  = f2_num_sub,
        title   = "Round p-values to decimal digits: ",
        buttons = c("2" = "2", "3" = "3", "4" = "4", "5" = "5", "0" = "more"),
        layout  = "horizontal",
        value   = initial$digits_p
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
                "Positive integer. Usually",
                "between 1000 and 10 000.")
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
                "Usually 0.90, 0.95 or 0.99      \n\n",
                "If signif - significance level, \n",
                "conf - confidence level, then   \n",
                "signif = 1 - conf"
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
    tkgrid(f2_pearson_opts$frame,
           sticky = "nse", padx = c(8, 5), pady = c(0, 0))
    tkgrid(f2_num_opts$frame,    sticky = "w",  padx = c(5, 0))

    tkgrid(f2_results_name$frame,sticky = "w",  padx = c(5, 0), pady = c(0, 0))
    tkgrid(f2_round_p$frame,     sticky = "w",  padx = 5, pady = c(5, 0))
    tkgrid(f2_round_p$frame_obj, sticky = "w")


    # Plot
    tkgrid(f2_plot_enable$frame, sticky = "nwe", padx = c(5, 43))
    tkgrid(f2_plot_sub,          sticky = "nwe")
    tkgrid(f2_plot_opts$frame,   sticky = "nwe", padx = c(5, 0))
    tkgrid(f2_band$frame,                        padx = 5, pady = 5)
    tkgrid(f2_band_boot$frame,   sticky = "e",   padx = 0, pady = c(2, 0))
    tkgrid(f2_conf$frame,        sticky = "e",   padx = 0, pady = 4)

    # Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(
        helpSubject = "shapiro.test",
        # helpPackage = "stats",
        reset = "window_test_normality",
        apply = "window_test_normality")

    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    activate_all()
}

