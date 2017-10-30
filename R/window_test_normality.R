#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_normality_test <- function() {

    nrows <- getRcmdr("nrow") # nrows in active dataset
    defaults <- list(initial_var = NULL,
                     initial_test = if (nrows <= 5000) "shapiro.test" else "ad.test",
                     initial_bins = gettextRcmdr("<auto>"),
                     initial_groups = NULL,
                     initial_add_plot = FALSE,
                     initial_plot_in_colors = TRUE,
                     initial_separate_window = FALSE,
                     initial_report_friendly = FALSE
    )

    dialog_values <- getDialog("window_normality_test", defaults)

    initializeDialog(title = gettextRcmdr("Test of Normality (BioStat)"))

    optionsFrame <- tkframe(top)
    radioButtons(optionsFrame,
                 name = "test",
                 buttons = c(if (nrows <= 5000) "shapiro.test",
                             "ad.test",
                             "cvm.test",
                             "lillie.test",
                             if (nrows <= 5000) "sf.test",
                             "pearson.test"
                 ),
                 labels = c(
                     if (nrows <= 5000) gettextRcmdr("Shapiro-Wilk"),
                     gettextRcmdr("Anderson-Darling"),
                     gettextRcmdr("Cramer-von Mises"),
                     gettextRcmdr("Lilliefors (Kolmogorov-Smirnov)"),
                     if (nrows <= 5000) gettextRcmdr("Shapiro-Francia"),
                     gettextRcmdr("Pearson chi-square")
                 ),
                 title = gettextRcmdr("Normality Test"),
                 initialValue = dialog_values$initial_test
    )
    binsFrame <- tkframe(optionsFrame)
    binsVariable <- tclVar(dialog_values$initial_bins)
    binsField <- ttkentry(binsFrame, width = "8", textvariable = binsVariable)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    variableBox <- variableListBox(
        top,
        Numeric(),
        title = gettextRcmdr("Variable to test\n(pick one)"),
        initialSelection = varPosn(dialog_values$initial_var, "numeric")
    )

    # vBox_frame <- tkframe(getFrame(variableBox))

    checkBoxes(
        frame = "checkBoxFrame",
        # frame = vBox_frame,
        title = "Options",
        boxes = c("add_plot", "plot_in_colors", "separate_window", "report_friendly"),
        initialValues = c(
            dialog_values$initial_add_plot,
            dialog_values$initial_plot_in_colors,
            dialog_values$initial_separate_window,
            dialog_values$initial_report_friendly
        ),
        labels = gettextRcmdr(
            c(
                "Draw a qq-plot",
                "Plot in color",
                "Separate window for each plot",
                "RMarkdown-friendly results"
            )
        )
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    groupsBox(
        recall = window_normality_test,
        label = gettextRcmdr("Test by:"),
        initialLabel = if (is.null(dialog_values$initial_groups)) {
            gettextRcmdr("Test by groups")
        } else {
            paste(gettextRcmdr("Test by:"),
                  paste0(dialog_values$initial_groups, collapse = " + "))

            # [!!!] Čia gali būti reikialinga str_ komanda,
            # kuri po tam tikro ilgio eiličių rašo "..."
        },
        initialGroup = dialog_values$initial_groups
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        var <- getSelection(variableBox)
        test <- tclvalue(testVariable)
        bins <- tclvalue(binsVariable)
        warn <- options(warn = -1)
        nbins <- as.numeric(bins)
        options(warn)
        if (bins != gettextRcmdr("<auto>") &&
            (is.na(nbins) || nbins < 4)) {
            errorCondition(
                recall = window_normality_test,
                message = gettextRcmdr("Number of bins must be a number >= 4")
            )
            return()
        }

        n.classes <-
            if (test != "pearson.test" || bins == gettextRcmdr("<auto>")) {
                ""
            } else {
                paste0(", n.classes = ", bins)
            }

        add_plot <- as.logical(as.integer(tclvalue(add_plotVariable)))
        plot_in_colors <- as.logical(as.integer(tclvalue(plot_in_colorsVariable)))
        report_friendly <- as.logical(as.integer(tclvalue(report_friendlyVariable)))
        separate_window <- as.logical(as.integer(tclvalue(separate_windowVariable)))

        putDialog("window_normality_test",
                  list(initial_var = var,
                       initial_test = test,
                       initial_bins = bins,
                       initial_groups = if(.groups == FALSE) NULL else .groups,
                       initial_add_plot = add_plot,
                       initial_plot_in_colors = plot_in_colors,
                       initial_separate_window = separate_window,
                       initial_report_friendly = report_friendly
                  )
        )

        if (length(var) == 0) {
            errorCondition(recall = window_normality_test,
                           message = gettextRcmdr("You must select a variable."))
            return()
        }

        closeDialog()

        Library("tidyverse")
        Library("BioStat")
        Library("nortest")

        print_as_report <-
            if (report_friendly == TRUE) {
                Library("pander")
                " %>% \n pander()\n"
            } else {
                ""
            }

        # For many groups ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(.groups) > 1) {
            .groups <- paste0(.groups, collapse = " + ")
        }

        # plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if (add_plot == TRUE) {
        # logger(paste("add_plot:", add_plot, class(add_plot)))
        # logger(paste("plot_in_colors:", plot_in_colors, class(add_plot)))

            if (separate_window == TRUE) {
                open_new_plots_window()
            }

            if (.groups == FALSE) {
                command2 <- glue::glue(
                    'BioStat::qq_plot(~{var}, ',
                    'data = {ActiveDataSet()}, use_colors = {plot_in_colors})')
            } else{
                command2 <- glue::glue(
                    'BioStat::qq_plot({var}~{.groups}, ',
                    'data = {ActiveDataSet()}, use_colors = {plot_in_colors})')
            }

            doItAndPrint(command2)
        }
        # Test results ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        if (.groups == FALSE) {
            command <- glue::glue(
                'BioStat::test_normality(~{var}, ',
                'data = {ActiveDataSet()},\n test = {test}{n.classes})',
                print_as_report)
        } else{
            command <- glue::glue(
                'BioStat::test_normality({var}~{.groups}, ',
                'data = {ActiveDataSet()},\n test = {test}{n.classes})',
                print_as_report)
        }
        doItAndPrint(command)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
    }

    OKCancelHelp(helpSubject = "normalityTest",
                 reset = "window_normality_test",
                 apply = "window_normality_test")

    tkgrid(getFrame(variableBox), sticky = "nw")
    tkgrid(checkBoxFrame, sticky = "ne")
    tkgrid(groupsFrame, sticky = "w", padx = 6)
    # tkgrid(vBox_frame, sticky = "nw")

    tkgrid(
        labelRcmdr(binsFrame, text = gettextRcmdr(
            "Number of bins\nfor Pearson chi-square")),
        binsField,
        padx = 3,
        sticky = "sw"
    )

    tkgrid(testFrame, binsFrame, sticky = "sw")
    tkgrid(optionsFrame, sticky = "sw")
    tkgrid(buttonsFrame, sticky = "w")

    dialogSuffix()
}
