#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_test_normality <- function() {
    # Initialize -------------------------------------------------------------
    nrows <- getRcmdr("nrow") # nrows in active dataset
    defaults <- list(initial_y_var  = NULL,
                     initial_gr_var = NULL,
                     initial_by_group = FALSE,
                     initial_test = if (nrows <= 5000) "shapiro.test" else "ad.test",
                     initial_bins = gettext_bs("<auto>"),
                     initial_add_plot = FALSE,
                     initial_plot_in_colors  = TRUE,
                     initial_new_plots_window = TRUE,
                     initial_use_pander = FALSE,
                     initial_pval_legend = FALSE,
                     initial_digits_p = "3"
    )

    dialog_values <- getDialog("window_test_normality", defaults)

    initializeDialog(title = gettext_bs("Tests of Normality by Group"))


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

    cmd_onClick_add_plot_checkbox <- function() {
        if (tclvalue_lgl(add_plotVariable) ) {
            tkconfigure(plot_in_colorsCheckBox,   state = "active")
            tkconfigure(new_plots_windowCheckBox, state = "active")
        } else {
            tkconfigure(plot_in_colorsCheckBox,   state = "disabled")
            tkconfigure(new_plots_windowCheckBox, state = "disabled")
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Variables

    # variables_Frame <- labeled_frame(top, "Select variables")
    variables_Frame <- tkframe(top)

    y_var_Box <- variableListBox2(
        variables_Frame,
        listHeight =  6,
        Numeric(),
        title = gettext_bs("Variable to test\n(pick one)"),
        initialSelection = varPosn(dialog_values$initial_y_var, "numeric")
    )

    gr_var_Frame <- tkframe(variables_Frame)

    gr_var_Box <- variableListBox2(
        gr_var_Frame,
        selectmode = "multiple",
        Factors(),
        listHeight =  5,
        title = gettext_bs("Groups variable\n(pick one, several or none)"),
        initialSelection =  varPosn(dialog_values$initial_gr_var, "factor"),
        onRelease_fun = cmd_onRelease_gr_var_Box)

    bs_check_boxes(gr_var_Frame,
                   frame = "by_group_Frame",
                   boxes = c("by_group"),
                   commands = list("by_group" = cmd_onClick_by_group_checkbox),
                   initialValues = c(dialog_values$initial_by_group),
                   # initialValues = (length(getSelection(gr_var_Box)) != 0),
                   labels = gettext_bs(c("Test by group"))
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    options_Frame <- tkframe(top)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Choose test

    choose_test_Frame <- labeled_frame(options_Frame,
                                       gettext_bs("Normality test"))

    choose_test_inner_Frame <- tkframe(choose_test_Frame)
    Rcmdr::radioButtons(choose_test_inner_Frame,
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

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    options_right_Frame <- tkframe(options_Frame)

    bs_check_boxes(options_right_Frame,
                   ttk = TRUE,
                   frame = "plot_options_Frame",
                   title = "Plot options",
                   boxes = c("add_plot", "plot_in_colors", "new_plots_window"),
                   initialValues = c(
                       dialog_values$initial_add_plot,
                       dialog_values$initial_plot_in_colors,
                       dialog_values$initial_new_plots_window
                   ),
                   labels = gettext_bs(
                       c(  "Draw a normal qq-plot",
                           "Plot groups in color",
                           "New window for qq-plot")
                   ),
                   commands = list("add_plot" = cmd_onClick_add_plot_checkbox)
    )

    Rcmdr::checkBoxes(options_right_Frame,
               ttk = TRUE,
               frame = "numerical_options_Frame",
               title = "Numerical output options",
               boxes = c("use_pander",
                         "pval_legend"),
               initialValues = c(
                   dialog_values$initial_use_pander,
                   dialog_values$initial_pval_legend
               ),
               labels = gettext_bs(
                   # c("R Markdown compatible results",
                   c("Results as Markdown table",
                     "Legend for significance stars")
               )
    )

    radioButtons_horizontal(numerical_options_Frame,
                            # title = "Decimal digits to round p-values to: ",
                            title = "Round p-values to decimal digits: ",
                            # right.buttons = FALSE,
                            name = "digits_p",
                            # sticky_buttons = "w",
                            buttons = c("d2", "d3", "d4",  "d5", "dmore"),
                            values =  c("2",  "3",  "4",   "5", "0"),
                            labels =  c("2  ","3  ","4  ", "5 ", "more"),
                            initialValue = dialog_values$initial_digits_p
    )

    tkgrid(
        labelRcmdr(binsFrame,
                   text = gettext_bs("Number of bins for\nPearson chi-square")),
        binsField,
        padx = 3,
        sticky = "sw"
    )

    # Functions --------------------------------------------------------------
    onOK <- function() {
        # Get values ---------------------------------------------------------
        by_group         <- tclvalue(by_groupVariable)
        gr_var           <- getSelection(gr_var_Box)
        y_var            <- getSelection(y_var_Box)
        test             <- tclvalue(testVariable)
        pval_leg         <- tclvalue_lgl(pval_legendVariable)
        digits_p         <- tclvalue_int(digits_pVariable)
        add_plot         <- tclvalue_lgl(add_plotVariable)
        plot_in_colors   <- tclvalue_lgl(plot_in_colorsVariable)
        use_pander       <- tclvalue_lgl(use_panderVariable)
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
                glue(",\n{spaces(24)}n.classes = ", bins)
            }

        # putDialog ----------------------------------------------------------
        putDialog("window_test_normality",
                  list(initial_y_var  = y_var,
                       initial_gr_var = gr_var,
                       initial_by_group = by_group,
                       initial_test = test,
                       initial_bins = bins,
                       initial_add_plot = add_plot,
                       initial_plot_in_colors   = plot_in_colors,
                       initial_new_plots_window = new_plots_window,
                       initial_use_pander  = use_pander,
                       initial_pval_legend = pval_leg,
                       initial_digits_p = digits_p
                  )
        )

        if (length(y_var) == 0) {
            errorCondition(recall = window_test_normality,
                           message = gettext_bs("You must select a variable."))
            return()
        }

        closeDialog()

        # Do analysis --------------------------------------------------------
        # Library(c("tidyverse", "biostat", "nortest"))
        Library("tidyverse")
        Library("biostat")
        Library("nortest")

        # NA means no rounding
        digits_p <- if (digits_p > 0) digits_p else NA
        print_opt <- glue("digits_p = {digits_p}, legend = {pval_leg}")

        print_as_report <-
            if (use_pander == TRUE) {
                Library("pander")
                " %>% \n    pander::pander({print_opt})\n"

            } else {
                " %>% \n    print({print_opt})\n"
            }
        #
        # For many groups ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(gr_var) > 1) {
            gr_var <- paste0(gr_var, collapse = " + ")
        }

        # Test results ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if (length(gr_var) == 0) {
            command <- glue(
                'biostat::test_normality(~{y_var}, ',
                'data = {ActiveDataSet()},\n',
                '{spaces(24)}test = {test}{chi_sq_params})',
                print_as_report)

        } else {
            command <- glue(
                'biostat::test_normality({y_var} ~ {gr_var}, ',
                'data = {ActiveDataSet()},\n',
                '{spaces(24)}test = {test}{chi_sq_params})',
                print_as_report)
        }
        doItAndPrint(command)

        # plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if (add_plot == TRUE) {
            if (new_plots_window == TRUE) {
                open_new_plots_window()
            }

            if (length(gr_var) == 0) {
                command2 <- glue(
                    'biostat::qq_plot(~{y_var}, ',
                    'data = {ActiveDataSet()}, use_colors = {plot_in_colors})')

            } else {
                command2 <- glue(
                    'biostat::qq_plot({y_var} ~ {gr_var}, ',
                    'data = {ActiveDataSet()}, use_colors = {plot_in_colors})')
            }

            doItAndPrint(command2)
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
        # onOK [end] ---------------------------------------------------------
    }

    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")
    tkgrid(label_rcmdr(
        top,
        text = gettext_bs("Normality tests and normal QQ plots"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9))

    # Layout -----------------------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(variables_Frame, sticky = "nwe", padx = c(0, 4)) #~~~~~~~~~~~~~~~~~

    tkgrid(getFrame(y_var_Box), gr_var_Frame, sticky = "nwe", padx = c(10, 0))

    tkgrid(getFrame(gr_var_Box), sticky = "nsw", padx = c(20, 0))
    tkgrid(by_group_Frame,       sticky = "sw",  padx = c(20, 0), pady = c(0, 5))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(options_Frame, pady = c(5, 0), sticky = "we") #~~~~~~~~~~~~~~~~~~~~
    tkgrid(choose_test_Frame, options_right_Frame, padx = c(0, 5), sticky = "nse")
    # choose_test_Frame
    tkgrid(choose_test_inner_Frame,   padx = c(0, 43), sticky = "nswe")
    tkgrid(testFrame, sticky = "swe", padx = c(8, 8))
    tkgrid(binsFrame, sticky = "nse", padx = c(8, 8), pady = c(0, 0))
    # options_right_Frame
    tkgrid(plot_options_Frame,      padx = c(5, 0), sticky = "nwe")

    tkgrid(numerical_options_Frame, padx = c(5, 0), sticky = "swe")
    tkgrid(digits_pFrame, sticky = "swe")
    # Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "test_normality", helpPackage = "biostat",
                 reset = "window_test_normality",
                 apply = "window_test_normality")

    tkgrid(buttonsFrame, sticky = "w")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Activate cmd_... functions
    eval_(paste0(ls(pattern = "^cmd_"), "();", collapse = ""))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dialogSuffix()
}
