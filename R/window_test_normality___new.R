# cmd_onClick_by_group_checkbox <- function() {
#     if (tclvalue_lgl(by_groupVariable) == FALSE) {
#
#         # Clear factor variable box
#         for (sel in seq_along(gr_var_box$varlist) - 1)
#             tkselection.clear(gr_var_box$listbox, sel)
#         tkconfigure(by_groupCheckBox, state = "disabled")
#
#     } else {
#         # Box is checked only if groups in gr_var_box
#         # are selected
#         tclvalue(by_groupVariable) <- "0"
#     }
# }



#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_test_normality <- function() {
    # Functions --------------------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_plot_activation <- function() {
        if (tclvalue_lgl(add_plotVariable) ) {
            tk_normalize(plot_in_colorsCheckBox)
            tk_normalize(new_plots_windowCheckBox)

        } else {
            tk_disable(plot_in_colorsCheckBox)
            tk_disable(new_plots_windowCheckBox)
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_test_activation <- function() {
        if (tclvalue_lgl(do_testVariable)) {

            tk_normalize(keep_resultsCheckBox)
            tk_normalize(as_markdownCheckBox)

            tk_normalize(d2Button)
            tk_normalize(d3Button)
            tk_normalize(d4Button)
            tk_normalize(d5Button)
            tk_normalize(dmoreButton)

            # if (nrows <= 5000) {
            #     tk_normalize(shapiro.testButton)
            #     tk_normalize(sf.testButton)
            # } else {
            #     # The test are not applicalble, if n > 5000
            #     tk_disable(shapiro.testButton)
            #     tk_disable(sf.testButton)
            # }
            #
            # tk_normalize(ad.testButton)
            # tk_normalize(cvm.testButton)
            # tk_normalize(lillie.testButton)
            # tk_normalize(pearson.testButton)

            tk_activate(binsField)


        } else {

            tk_disable(keep_resultsCheckBox)
            tk_disable(as_markdownCheckBox)

            tk_disable(d2Button)
            tk_disable(d3Button)
            tk_disable(d4Button)
            tk_disable(d5Button)
            tk_disable(dmoreButton)


            # tk_disable(shapiro.testButton)
            # tk_disable(ad.testButton)
            # tk_disable(cvm.testButton)
            # tk_disable(lillie.testButton)
            # tk_disable(sf.testButton)
            # tk_disable(pearson.testButton)

            tk_disable(binsField)

        }
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
    onOK <- function() {
        # Get values ---------------------------------------------------------

        by_group   <- get_values(f1_widget_y_gr$checkbox)
        var_y      <- get_selection(f1_widget_y_gr$y)
        var_gr     <- get_selection(f1_widget_y_gr$gr)



        by_group         <- tclvalue(by_groupVariable)
        gr_var           <- getSelection(gr_var_box)
        y_var            <- getSelection(y_var_box)
        test             <- get_test_function()
        keep_results     <- tclvalue_lgl(keep_resultsVariable)
        digits_p         <- tclvalue_int(digits_pVariable)
        add_plot         <- tclvalue_lgl(add_plotVariable)
        plot_in_colors   <- tclvalue_lgl(plot_in_colorsVariable)
        as_markdown      <- tclvalue_lgl(as_markdownVariable)
        new_plots_window <- tclvalue_lgl(new_plots_windowVariable)
        bins             <- tclvalue(binsVariable)

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

        # putDialog ----------------------------------------------------------
        putDialog(
            "window_test_normality",
            list(y_var            = y_var,
                 gr_var           = gr_var,
                 by_group         = by_group,
                 test             = test,
                 bins             = bins,
                 add_plot         = add_plot,
                 plot_in_colors   = plot_in_colors,
                 new_plots_window = new_plots_window,
                 as_markdown      = as_markdown,
                 keep_results     = keep_results,
                 digits_p         = digits_p
            )
        )

        if (length(y_var) == 0) {
            errorCondition(recall = window_test_normality,
                           message = gettext_bs("You must select a variable."))
            return()
        }

        closeDialog()

        # Do analysis --------------------------------------------------------
        Library("tidyverse")
        # Library("biostat")
        Library("nortest")


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
                stat_qq_band() +
                stat_qq_line() +
                stat_qq_point() +
                facet_wrap(~{gr_var}, scales = "free") +
                labs(x = "Theoretical quantiles", y = "Empirical quantiles")


            # [VG] ??? --------------------------------------------  <-----
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
        plot_in_colors   = TRUE,
        new_plots_window = TRUE,
        band_type = 1,

        #
        confidence_level = 0.95


    )

    initial <- getDialog("window_test_normality", defaults)



    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Variables

    # upper_frame <- labeled_frame(top, "Select variables")
    # upper_frame <- tkframe(top)

    # F1 ---------------------------------------------------------------------
    f1 <- tkframe(top)

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
    f2 <- tkframe(top)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2_num <- tkframe(f2)

    f2_num_opts <- bs_checkboxes(
        parent = f2_num,
        title = "Numerical output options",
        border = TRUE,
        boxes = c("do_test",
                  "keep_results",
                  "as_markdown"),
        labels = gettext_bs(c(
            "Perform normality test",
            "Keep test results in R memory",
            "Print as Markdown table")),
        values = c(
            initial$use_test,
            initial$keep_results,
            initial$as_markdown
        ),
        commands = list(do_test = cmd_test_activation)
    )




    f2_test_box <- bs_combobox(
        f2_num_opts$frame,
        width = 29,
        values = c(
            if (nrows <= 5000) gettext_bs("Shapiro-Wilk"),
            gettext_bs("Anderson-Darling"),
            gettext_bs("Cramer-von Mises"),
            gettext_bs("Lilliefors (Kolmogorov-Smirnov)"),
            if (nrows <= 5000) gettext_bs("Shapiro-Francia"),
            gettext_bs("Pearson chi-square")
        ),
        value = initial$test
    )

    # binsFrame    <- tkframe(f2_num)
    # binsVariable <- tclVar(initial$bins)
    # binsField    <- ttkentry(binsFrame, width = "8", textvariable = binsVariable)
    #
    # tkgrid(
    #     labelRcmdr(binsFrame,
    #                text = gettext_bs("Number of bins for\nPearson chi-square")),
    #     binsField,
    #     padx = 3,
    #     sticky = "sw"
    # )
    # tkgrid(binsFrame, sticky = "nse", padx = c(8, 8), pady = c(0, 0))


    f2_pearson_opts <- bs_entry(
        parent = f2_num_opts$frame,
        value = gettext_bs("<auto>"),
        width  = "8",
        label  = gettext_bs("Number of bins for\nPearson chi-square"),
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2_round_p <- bs_radiobuttons(
        parent  = f2_num_opts$frame,
        title   = "Round p-values to decimal digits: ",
        buttons = c("2" = "2", "3" = "3", "4" = "4", "5" = "5", "more" = "0"),
        layout  = "horizontal",
        value   = initial$digits_p
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2_plot <- tkframe(f2)

    f2_plot_opts <- bs_checkboxes(
        parent = f2_plot,
        title  = "Plot options",
        border = TRUE,
        boxes  = c("add_plot", "plot_in_colors", "new_plots_window"),
        values = c(
            initial$add_plot,
            initial$plot_in_colors,
            initial$new_plots_window
        ),
        labels = gettext_bs(
            c(  "Normal QQ-plot",
                "Groups in color",
                "Plot in a new window")
        ),
        commands = list("add_plot" = cmd_plot_activation)
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2_band_lab <- bs_label_b(f2_plot_opts$frame, text = "bandType")

    f2_band <- bs_combobox(
        parent    = f2_plot_opts$frame,
        label_position = "above",
        label = "Type of confidence band:",
        values    = c("Point-wise (pointwise)",
                      "Parametric bootstrap (boot)",
                      "Kolmogorov-Smirnov (ks)",
                      "Tail-sensitive (ts)"),
        width     = 25,
        selection = 1)

    f2_band_boot <-
        bs_entry(
            f2_plot_opts$frame, width = 5, value = 999,
            label = "Number of bootstrap\nreplicates")

    f2_conf <-
        bs_entry(
            f2_plot_opts$frame, width = 5, value = "0.95",
            label = "Confidence level",
            tip = str_c("sig. = 1 - conf. \n",
                        "sig. - significance level \n",
                        "conf. - confidence level"))

    # bandType = "boot"; B = ...
    # conf =


    # Layout -----------------------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f1, sticky = "nwe", padx = c(0, 4)) #~~~~~~~~~~~~~~~~~

    tkgrid(f1_widget_y_gr$frame, sticky = "nwe", padx = c(10, 0))
    # tkgrid(getFrame(gr_var_box), sticky = "nsw", padx = c(20, 0))
    # tkgrid(by_group_Frame,       sticky = "sw",  padx = c(20, 0), pady = c(0, 5))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkgrid(f2, pady = c(5, 0), sticky = "we") #~~~~~~~~~~~~~~~~~~~~
    tkgrid(f2_num, f2_plot, padx = c(0, 5), sticky = "nse")
    # choose_test_Frame,

    # Numerical options
    # tkgrid(f2_num, padx = c(5, 0), sticky = "ns")
    # tkgrid(digits_pFrame, sticky = "swe")

    # # Choose test
    # tkgrid(choose_test_inner_Frame,   padx = c(0, 0), sticky = "nswe")
    # tkgrid(testFrame, sticky = "swe", padx = c(8, 8))
    # tkgrid(binsFrame, sticky = "nse", padx = c(8, 8), pady = c(0, 0))

    # f2_plot


    tkgrid(f2_num_opts$frame, sticky = "w", padx = c(5, 0))

    tkgrid(f2_test_box$frame, pady = 2)
    tkgrid(f2_pearson_opts$frame,
           # padx = 3, sticky = "sw",
           sticky = "nse", padx = c(8, 8), pady = c(0, 0))
    tkgrid(f2_round_p$frame)


    tkgrid(f2_plot_opts$frame, padx = c(5, 0), sticky = "nwe")
    tkgrid(f2_band_lab)
    tkgrid(f2_band$frame)
    tkgrid(f2_band_boot$frame)
    tkgrid(f2_conf$frame)

    # Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(helpSubject = "shapiro.test",
                   # helpPackage = "stats",
                   reset = "window_test_normality",
                   apply = "window_test_normality")

    tkgrid(buttonsFrame, sticky = "ew")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Activate cmd_... functions
    # eval_text(stringr::str_c(ls(pattern = "^cmd_"), "();", collapse = ""))
    #
    cmd_plot_activation()
    cmd_test_activation()
}


