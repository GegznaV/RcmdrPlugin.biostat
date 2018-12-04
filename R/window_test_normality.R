#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_test_normality <- function() {
    # Initialize -------------------------------------------------------------
    .ds <- ActiveDataSet()

    nrows <- getRcmdr("nrow") # nrows in active dataset
    defaults <- list(
        initial_y_var            = NULL,
        initial_gr_var           = NULL,
        initial_by_group         = FALSE,
        initial_do_test          = TRUE,
        initial_test             = if (nrows <= 5000) "shapiro.test" else "ad.test",
        initial_bins             = gettext_bs("<auto>"),
        initial_add_plot         = FALSE,
        initial_plot_in_colors   = TRUE,
        initial_new_plots_window = TRUE,
        initial_keep_results     = FALSE,
        initial_as_markdown      = FALSE,
        initial_digits_p         = "3"
    )

    dialog_values <- getDialog("window_test_normality", defaults)

    initializeDialog(title = gettext_bs("Test Normality by Group"))
    tk_title(top, text = "Normality tests and normal QQ plots")


    # Callback  functions-----------------------------------------------------

    cmd_onClick_by_group_checkbox <- function(){
        if (tclvalue_lgl(by_groupVariable) == FALSE) {

            # Clear factor variable box
            for (sel in seq_along(gr_var_Box$varlist) - 1)
                tkselection.clear(gr_var_Box$listbox, sel)
            tkconfigure(by_groupCheckBox, state = "disabled")

        } else {
            # Box is checked only if groups in gr_var_Box
            # are selected
            tclvalue(by_groupVariable) <- "0"
        }
    }

    cmd_onRelease_gr_var_Box <- function() {
        # On mouse relese select/deselect checkbox
        if (length(getSelection(gr_var_Box)) == 0) {
            tclvalue(by_groupVariable) <- "0"
            tkconfigure(by_groupCheckBox, state = "disabled")

        } else {
            tclvalue(by_groupVariable) <- "1"
            tkconfigure(by_groupCheckBox, state = "active")
        }
    }

    cmd_plot_activation <- function() {
        if (tclvalue_lgl(add_plotVariable) ) {
            tk_normalize(plot_in_colorsCheckBox)
            tk_normalize(new_plots_windowCheckBox)

        } else {
            tk_disable(plot_in_colorsCheckBox)
            tk_disable(new_plots_windowCheckBox)
        }
    }

    cmd_test_activation <- function() {
        if (tclvalue_lgl(do_testVariable)) {

            tk_normalize(keep_resultsCheckBox)
            tk_normalize(as_markdownCheckBox)

            tk_normalize(d2Button)
            tk_normalize(d3Button)
            tk_normalize(d4Button)
            tk_normalize(d5Button)
            tk_normalize(dmoreButton)

            if (nrows <= 5000) {
                tk_normalize(shapiro.testButton)
                tk_normalize(sf.testButton)
            } else {
                # The test are not applicalble, if n > 5000
                tk_disable(shapiro.testButton)
                tk_disable(sf.testButton)
            }

            tk_normalize(ad.testButton)
            tk_normalize(cvm.testButton)
            tk_normalize(lillie.testButton)
            tk_normalize(pearson.testButton)

            tk_activate(binsField)


        } else {

            tk_disable(keep_resultsCheckBox)
            tk_disable(as_markdownCheckBox)

            tk_disable(d2Button)
            tk_disable(d3Button)
            tk_disable(d4Button)
            tk_disable(d5Button)
            tk_disable(dmoreButton)


            tk_disable(shapiro.testButton)
            tk_disable(ad.testButton)
            tk_disable(cvm.testButton)
            tk_disable(lillie.testButton)
            tk_disable(sf.testButton)
            tk_disable(pearson.testButton)

            tk_disable(binsField)

        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Variables

    # upper_frame <- labeled_frame(top, "Select variables")
    upper_frame <- tkframe(top)

    y_var_Box <- bs_listbox(
        parent_window = upper_frame,
        title = gettext_bs("Variable to test\n(pick one)"),
        height =  6,
        variable_list = variables_num(),
        initial_selection = varPosn(dialog_values$initial_y_var, "numeric")
    )

    gr_var_Frame <- tkframe(upper_frame)

    gr_var_Box <- bs_listbox(
        gr_var_Frame,
        title = gettext_bs("Groups variable\n(pick one, several or none)"),
        select_mode = "multiple",
        variable_list = variables_fct_like(),
        height =  5,
        initial_selection =  varPosn(dialog_values$initial_gr_var, "factor"),
        on_release = cmd_onRelease_gr_var_Box)

    bs_check_boxes(
        gr_var_Frame,
        frame = "by_group_Frame",
        boxes = c("by_group"),
        commands = list("by_group" = cmd_onClick_by_group_checkbox),
        initialValues = c(dialog_values$initial_by_group),
        # initialValues = (length(getSelection(gr_var_Box)) != 0),
        labels = gettext_bs(c("Test by group"))
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    middle_frame <- tkframe(top)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    numerical_options_frame <- tkframe(middle_frame)


    bs_check_boxes(
        numerical_options_frame,
        title = "Numerical output options",
        ttk   = TRUE,
        frame = "numerical_middle_frame",
        boxes = c("do_test",
                  "keep_results",
                  "as_markdown"),
        initialValues = c(
            dialog_values$initial_do_test,
            dialog_values$initial_keep_results,
            dialog_values$initial_as_markdown
        ),
        labels = gettext_bs(c(
            "Normality test",
            "Keep results in R memory",
            "Print as Markdown table")),
        commands = list(do_test = cmd_test_activation)
    )

    radioButtons_horizontal(
        numerical_middle_frame,
        # title = "Decimal digits to round p-values to: ",
        title = "Round p-values to decimal digits: ",
        # right.buttons = FALSE,
        name = "digits_p",
        # sticky_buttons = "w",
        buttons = c("d2", "d3", "d4",  "d5", "dmore"),
        values =  c("2",  "3",  "4",   "5",  "0"),
        labels =  c("2  ","3  ","4  ", "5 ", "more"),
        initialValue = dialog_values$initial_digits_p
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Choose test

    choose_test_Frame <- labeled_frame(
        middle_frame,
        gettext_bs("Normality test"))

    choose_test_inner_Frame <- tkframe(choose_test_Frame)

    Rcmdr::radioButtons(
        choose_test_inner_Frame,
        name = "test",
        buttons = c(if (nrows <= 5000) "shapiro.test",
                    "ad.test",
                    "cvm.test",
                    "lillie.test",
                    if (nrows <= 5000) "sf.test",
                    "pearson.test"
        ),
        labels = c(
            if (nrows <= 5000) gettext_bs("Shapiro-Wilk"),
            gettext_bs("Anderson-Darling"),
            gettext_bs("Cramer-von Mises"),
            gettext_bs("Lilliefors (Kolmogorov-Smirnov)"),
            if (nrows <= 5000) gettext_bs("Shapiro-Francia"),
            gettext_bs("Pearson chi-square")
        ),
        initialValue = dialog_values$initial_test
    )

    binsVariable <- tclVar(dialog_values$initial_bins)
    binsFrame    <- tkframe(choose_test_inner_Frame)
    binsField    <- ttkentry(binsFrame, width = "8", textvariable = binsVariable)

    tkgrid(
        labelRcmdr(binsFrame,
                   text = gettext_bs("Number of bins for\nPearson chi-square")),
        binsField,
        padx = 3,
        sticky = "sw"
    )


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    plot_options_frame <- tkframe(middle_frame)

    bs_check_boxes(
        plot_options_frame,
        title = "Plot options",
        ttk   = TRUE,
        frame = "plot_middle_frame",
        boxes = c("add_plot", "plot_in_colors", "new_plots_window"),
        initialValues = c(
            dialog_values$initial_add_plot,
            dialog_values$initial_plot_in_colors,
            dialog_values$initial_new_plots_window
        ),
        labels = gettext_bs(
            c(  "Normal QQ-plot",
                "Groups in color",
                "Plot in a new window")
        ),
        commands = list("add_plot" = cmd_plot_activation)
    )

    cbox_1_lab <- bs_label_b(plot_middle_frame, text = "bandType")

    cbox_1 <- bs_combobox(
        parent_window = plot_middle_frame,
        variable_list = c("< default >", "pointwise", "boot", "ks", "ts"))

    # bandType = "boot"; B = ...
    # conf =

    tkgrid(cbox_1_lab)
    tkgrid(cbox_1$frame)

    # Functions --------------------------------------------------------------
    onOK <- function() {
        # Get values ---------------------------------------------------------
        by_group         <- tclvalue(by_groupVariable)
        gr_var           <- getSelection(gr_var_Box)
        y_var            <- getSelection(y_var_Box)
        test             <- tclvalue(testVariable)
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
            list(initial_y_var            = y_var,
                 initial_gr_var           = gr_var,
                 initial_by_group         = by_group,
                 initial_test             = test,
                 initial_bins             = bins,
                 initial_add_plot         = add_plot,
                 initial_plot_in_colors   = plot_in_colors,
                 initial_new_plots_window = new_plots_window,
                 initial_as_markdown       = as_markdown,
                 initial_keep_results      = keep_results,
                 initial_digits_p         = digits_p
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

        if (add_plot == TRUE) {

            if (new_plots_window == TRUE) {
                open_new_plots_window()
            }

            if (length(gr_var) == 0) {
                command2 <- glue(
                    'biostat::qq_plot(~{y_var}, ',
                    'data = {.ds}, use_colors = {plot_in_colors})')

            } else {
                command2 <- glue(
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

    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")

    # Layout -----------------------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame, sticky = "nwe", padx = c(0, 4)) #~~~~~~~~~~~~~~~~~

    tkgrid(getFrame(y_var_Box), gr_var_Frame, sticky = "nwe", padx = c(10, 0))
    tkgrid(getFrame(gr_var_Box), sticky = "nsw", padx = c(20, 0))
    tkgrid(by_group_Frame,       sticky = "sw",  padx = c(20, 0), pady = c(0, 5))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkgrid(middle_frame, pady = c(5, 0), sticky = "we") #~~~~~~~~~~~~~~~~~~~~
    tkgrid(numerical_options_frame,
           choose_test_Frame,
           plot_options_frame, padx = c(0, 5), sticky = "nse")

    # Numerical options
    tkgrid(numerical_middle_frame, padx = c(5, 0), sticky = "ns")
    tkgrid(digits_pFrame, sticky = "swe")

    # Choose test
    tkgrid(choose_test_inner_Frame,   padx = c(0, 0), sticky = "nswe")
    tkgrid(testFrame, sticky = "swe", padx = c(8, 8))
    tkgrid(binsFrame, sticky = "nse", padx = c(8, 8), pady = c(0, 0))

    # plot_options_frame
    tkgrid(plot_middle_frame, padx = c(5, 0), sticky = "nwe")

    # Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "shapiro.test",
                 # helpPackage = "stats",
                 reset = "window_test_normality",
                 apply = "window_test_normality")

    tkgrid(buttonsFrame, sticky = "ew")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Activate cmd_... functions
    # eval_(stringr::str_c(ls(pattern = "^cmd_"), "();", collapse = ""))
    #
    # cmd_plot_activation()
    # cmd_test_activation()
}
