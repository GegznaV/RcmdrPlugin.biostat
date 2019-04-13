# TODO:
# - write onOK function.
# - add tips.
# - enable "groups in color" only if groups are selected (???)
# - Read and save defaults/initial values correctly.

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
    activate_pearson <- function() {

        if (get_selection(f2_test_box) == gettext_bs("Pearson chi-square")) {
            tkgrid(f2_pearson_opts$frame)

        } else {
            tkgrid.remove(f2_pearson_opts$frame)
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
        activate_pearson()
        activate_plots()
        activate_band()
        activate_band_options()

    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_test_function <- function() {
        res <- get_selection(f2_test_box)

        str_glue_eval(
            'switch(
                res,
                "{gettext_bs("Shapiro-Wilk")}"                    = "shapiro.test",
                "{gettext_bs("Anderson-Darling")}"                = "ad.test",
                "{gettext_bs("Cramer-von Mises")}"                = "cvm.test",
                "{gettext_bs("Lilliefors (Kolmogorov-Smirnov)")}" = "lillie.test",
                "{gettext_bs("Shapiro-Francia")}"                 = "sf.test",
                "{gettext_bs("Pearson chi-square")}"              = "pearson.test",
                stop("unknown value in `f2_test_box`")
            )')
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_band_type <- function() {
        res <- get_selection(f2_band)

        str_glue_eval(
            'switch(
                res,
                "{gettext_bs("Point-wise (pointwise)")}"       = "pointwise",
                "{gettext_bs("Parametric bootstrap (boot)")}"  = "boot",
                "{gettext_bs("Kolmogorov-Smirnov (ks)")}"      = "ks",
                "{gettext_bs("Tail-sensitive (ts)")}"          = "ts",
                stop("unknown value in `f2_band`:", res)
            )'
        )
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        # Get values ---------------------------------------------------------

        by_group         <- get_values(f1_widget_y_gr$checkbox)
        y_var            <- get_selection(f1_widget_y_gr$y)
        gr_var           <- get_selection(f1_widget_y_gr$gr)

        use_test         <- get_values(f2_num_enable)
        test             <- get_test_function()
        keep_results     <- get_values(f2_num_opts, "keep_results")
        as_markdown      <- get_values(f2_num_opts, "as_markdown")
        digits_p         <- get_values(f2_round_p)
        bins             <- get_values(f2_pearson_opts)

        add_plot         <- get_values(f2_plot_enable)
        new_plots_window <- get_values(f2_plot_opts, "new_plots_window")
        plot_in_colors   <- get_values(f2_plot_opts, "plot_in_colors")
        qq_detrend       <- get_values(f2_plot_opts, "qq_detrend")
        qq_line          <- get_values(f2_plot_opts, "qq_line")
        qq_band          <- get_values(f2_plot_opts, "qq_band")
        qq_band_type     <- get_band_type()
        bootstrap_n      <- as.integer(get_values(f2_band_boot))  # positive integer in range 1000 - 1E4
        conf_level       <- as.numeric(get_values(f2_conf))       # between 0 - 1


        # Chi-square bins ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        warn  <- options(warn = -1)
        nbins <- as.numeric(bins)
        options(warn)

        if (bins != gettext_bs("<auto>") &&
            (is.na(nbins) || nbins < 4)) {
            errorCondition(
                recall = window_test_normality,
                message = gettext_bs("Number of bins must be a number >= 4")
            )
            return()
        }

        chi_sq_params <-
            if (test != "pearson.test" || bins == gettext_bs("<auto>")) {
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
                # y_var            = y_var,
                # gr_var           = gr_var,
                # by_group         = by_group,
                # test             = test,
                # bins             = bins,
                # add_plot         = add_plot,
                # plot_in_colors   = plot_in_colors,
                # new_plots_window = new_plots_window,
                # as_markdown      = as_markdown,
                # keep_results     = keep_results,
                # digits_p         = digits_p
                by_group         = by_group         ,
                y_var            = y_var            ,
                gr_var           = gr_var           ,
                use_test         = use_test         ,
                test             = test             ,
                keep_results     = keep_results     ,
                as_markdown      = as_markdown      ,
                digits_p         = digits_p         ,
                bins             = bins             ,
                add_plot         = add_plot         ,
                new_plots_window = new_plots_window ,
                plot_in_colors   = plot_in_colors   ,
                qq_detrend       = qq_detrend       ,
                qq_line          = qq_line          ,
                qq_band          = qq_band          ,
                qq_band_type     = qq_band_type     ,
                bootstrap_n      = bootstrap_n      ,
                conf_level       = conf_level
            )
        )


        closeDialog()

        # Do analysis --------------------------------------------------------
        Library("tidyverse")
        # Library("biostat")
        Library("nortest")
        Library("qqplotr")


        test_obj <- unique_obj_names("shapiro_test_results")

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
                "\n remove({test_obj})"
            }

        # Test results ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Command
        command <- str_glue(
            "{test_obj} <- {.ds} %>%\n",
            "    {by_gr_str}",
            "    do(broom::tidy({test}(.${y_var}{chi_sq_params})))\n\n",

            "{test_obj} %>% \n",
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

        if (add_plot == TRUE) {

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

    initializeDialog(title = gettext_bs("Test Normality by Group"))
    tk_title(top, text = "Normality Tests and Normal QQ Plots")

    defaults <- list(
        # Variables
        y_var            = NULL,
        gr_var           = NULL,
        by_group         = FALSE,

        # Normality test
        use_test          = TRUE,
        test             =
            if (nrows <= 5000) {
                "Shapiro-Wilk"
            } else {
                "Anderson-Darling"
            },
        bins             = gettext_bs("<auto>"),

        keep_results     = FALSE,
        as_markdown      = FALSE,
        digits_p         = "3",

        # QQ plot
        add_plot         = TRUE,
        new_plots_window = TRUE,
        plot_in_colors   = TRUE,
        qq_detrend       = FALSE,
        qq_line          = TRUE,
        qq_band          = TRUE,

        band_type = 1,

        #
        confidence_level = 0.95


    )

    initial <- getDialog("window_test_normality", defaults)



    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Variables

    # upper_frame <- labeled_frame(top, "Select variables")
    # upper_frame <- tkframe(top)

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
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # F2 test ----------------------------------------------------------------

    f2_num <- tk2labelframe(f2, text = "Test options")

    f2_num_enable <- bs_checkboxes(
        parent   = f2_num,
        boxes    = "do_test",
        labels   = gettext_bs("Perform normality test"),
        values   = initial$use_test,
        commands = list("do_test"  = activate_tests)
    )

    f2_num_sub <- tk2frame(f2_num)

    f2_num_opts <- bs_checkboxes(
        parent = f2_num_sub,
        # title = "Numerical output options",
        border = FALSE,
        boxes = c("keep_results", "as_markdown"),
        labels = gettext_bs(c(
            "Keep test results in R memory",
            "Print as Markdown table")),
        values = c(
            initial$keep_results,
            initial$as_markdown
        )
    )

    f2_test_box <- bs_combobox(
        parent = f2_num_sub,
        width = 29,
        label = "Test:",
        label_position = "above",
        values = c(
            if (nrows <= 5000) gettext_bs("Shapiro-Wilk"),
            gettext_bs("Anderson-Darling"),
            gettext_bs("Cramer-von Mises"),
            gettext_bs("Lilliefors (Kolmogorov-Smirnov)"),
            if (nrows <= 5000) gettext_bs("Shapiro-Francia"),
            gettext_bs("Pearson chi-square")
        ),
        value = initial$test,
        on_select = activate_pearson
    )

    f2_pearson_opts <- bs_entry(
        parent = f2_num_sub,
        value  = gettext_bs("<auto>"),
        width  = "8",
        label  = gettext_bs("Number of bins for\nPearson chi-square"),
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

    # F2 plot ----------------------------------------------------------------

    f2_plot <- tk2labelframe(f2, text = "Plot options")

    f2_plot_enable <- bs_checkboxes(
        parent   = f2_plot,
        boxes    = "add_plot",
        labels   = gettext_bs("Draw normal QQ plot"),
        values   = initial$add_plot,
        commands = list("add_plot" = activate_plots)
    )

    f2_plot_sub <- tk2frame(f2_plot)

    f2_plot_opts <- bs_checkboxes(
        parent = f2_plot_sub,
        # title  = "Plot options",
        border = FALSE,
        boxes  = c("new_plots_window", "plot_in_colors", "qq_detrend", "qq_line", "qq_band"),
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
        parent    = f2_plot_sub,
        label_position = "above",
        label = "Type of confidence band:",
        values    = gettext_bs(c(
            "Point-wise (pointwise)",
            "Parametric bootstrap (boot)",
            "Kolmogorov-Smirnov (ks)",
            "Tail-sensitive (ts)")),
        width     = 25,
        selection = 1,
        on_select = activate_band_options)

    f2_band_boot <-
        bs_entry(
            f2_band$frame, width = 6,
            value = 999,
            justify = "right",
            label = "Number of boot-\nstrap replicates",
            tip = str_c(
                "Positive integer. Usually",
                "between 1000 and 10 000.")
        )

    f2_conf <-
        bs_entry(
            f2_band$frame, width = 6,
            value = "0.95",
            justify = "center",
            label = "Confidence level",
            tip = str_c(
                "Number between 0 and 1.       \n\n",
                "If signif - significance level, \n",
                "conf - confidence level, then   \n",
                "signif = 1 - conf"
            ))

    # bandType = "boot"; B = ...
    # conf =


    # Layout -----------------------------------------------------------------

    tkgrid(f0)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f1, sticky = "nwe", padx = c(0, 4))
    tkgrid(f1_widget_y_gr$frame, sticky = "nwe", padx = c(10, 0))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f2, pady = c(5, 0), sticky = "")
    tkgrid(f2_num, f2_plot, padx = c(0, 5), sticky = "nse")

    # Numerical options
    tkgrid(f2_num_enable$frame, padx = c(5, 60), sticky = "nwe")
    tkgrid(f2_num_sub)
    tkgrid(f2_num_opts$frame,   padx = c(5, 0), sticky = "w")
    tkgrid(f2_round_p$frame,    sticky = "w", padx = 5, pady = c(5, 0))
    tkgrid(f2_round_p$frame_obj, sticky = "w")
    tkgrid(f2_test_box$frame,   padx = 5, pady = c(3, 5))
    tkgrid(f2_pearson_opts$frame,
           sticky = "nse", padx = c(8, 5), pady = c(0, 0))

    # Plot
    tkgrid(f2_plot_enable$frame, sticky = "nwe", padx = c(5, 43))
    tkgrid(f2_plot_sub,          sticky = "nwe")
    tkgrid(f2_plot_opts$frame,   padx = c(5, 0), sticky = "nwe")
    tkgrid(f2_band$frame,                    padx = 5, pady = 5)
    tkgrid(f2_band_boot$frame, sticky = "e", padx = 0, pady = c(2, 0))
    tkgrid(f2_conf$frame,      sticky = "e", padx = 0, pady = 4)


    # Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(helpSubject = "shapiro.test",
                   # helpPackage = "stats",
                   reset = "window_test_normality",
                   apply = "window_test_normality")

    tkgrid(buttonsFrame, sticky = "ew")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    activate_all()

}


