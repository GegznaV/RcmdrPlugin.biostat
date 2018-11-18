#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_qqplot_biostat <- function() {

    nrows <- getRcmdr("nrow") # nrows in active dataset
    defaults <- list(initial_var = NULL,
                     initial_test = if (nrows <= 5000) "shapiro.test" else "ad.test",
                     initial_bins = gettext_bs("<auto>"),
                     initial_groups = NULL
    )

    dialog.values <- getDialog("window_qqplot_biostat", defaults)
    initializeDialog(title = gettext_bs("Test of Normality"))

    variableBox <- variableListBox(
        top,
        Numeric(),
        title = gettext_bs("Variable to test\n(pick one)"),
        initialSelection = varPosn(dialog.values$initial_var, "numeric")
    )

    optionsFrame <- tkframe(top)
    Rcmdr::radioButtons(optionsFrame,
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
                        title = gettext_bs("Normality Test"),
                        initialValue = dialog.values$initial_test
    )
    binsFrame <- tkframe(optionsFrame)
    binsVariable <- tclVar(dialog.values$initial_bins)
    binsField <- ttkentry(binsFrame, width = "8", textvariable = binsVariable)

    groupsBox(
        recall = window_qqplot_biostat,
        label = gettext_bs("Test by:"),
        initialLabel = if (is.null(dialog.values$initial_group)) {
            gettext_bs("Test by groups")
        } else {
            paste(gettext_bs("Test by:"),
                  paste0(dialog.values$initial_group, collapse = " + "))

            # [!!!] 2ia gali b8ti reikialinti str_ komanda,
            # kuri po tam tikro ilgio eiličių rašo "..."
        },
        initialGroup = dialog.values$initial_group
    )


    onOK <- function() {
        var <- getSelection(variableBox)
        test <- tclvalue(testVariable)
        bins <- tclvalue(binsVariable)
        warn <- options(warn = -1)
        nbins <- as.numeric(bins)
        options(warn)
        if (bins != gettext_bs("<auto>") &&
            (is.na(nbins) || nbins < 4)) {
            errorCondition(
                recall = window_qqplot_biostat,
                message = gettext_bs("Number of bins must be a number >= 4")
            )
            return()
        }

        n.classes <-
            if (test != "pearson.test" || bins == gettext_bs("<auto>")) {
                ""
            } else {
                paste0(", n.classes = ", bins)
            }

        putDialog("window_qqplot_biostat",
                  list(initial_var = var,
                       initial_test = test,
                       initial_bins = bins,
                       initial_groups = if(.groups == FALSE) NULL else .groups
                  )
        )
        if (length(var) == 0) {
            errorCondition(recall = window_qqplot_biostat,
                           message = gettext_bs("You must select a variable."))
            return()
        }

        closeDialog()

        Library("biostat")

        # For many groups ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(.groups) > 1) {
            .groups <- paste0(.groups, collapse = " + ")
        }

        # plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (.groups == FALSE) {
            command2 <- str_glue(
                'biostat::qq_plot(~{var}, ',
                'data = {ActiveDataSet()}, use_colors = TRUE)')
        } else{
            command2 <- str_glue(
                'biostat::qq_plot({var}~{.groups}, ',
                'data = {ActiveDataSet()}, use_colors = TRUE)')
        }

        doItAndPrint(command2)

        # Test results ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # if (.groups == FALSE) {
        #     command <- str_glue(
        #         'biostat::test_normality(~{var}, ',
        #         'data = {ActiveDataSet()}, test = {test}{n.classes})')
        # } else{
        #     command <- str_glue(
        #         'biostat::test_normality({var}~{.groups}, ',
        #         'data = {ActiveDataSet()}, test = {test}{n.classes})')
        # }
        #
        # # if (.groups == FALSE) {
        # #     command <- str_glue(
        # #         'normalityTest(~{var}, test = "{test}",',
        # #         ' data = {ActiveDataSet()}{n.classes})'                )
        # # } else{
        # #     command <- str_glue(
        # #         'normalityTest({var}~{.groups}, test = "{test}",',
        # #         ' data = {ActiveDataSet()}{n.classes})'                )
        # # }
        #
        # doItAndPrint(command)

        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "qq_plot",
                 reset = "window_qqplot_biostat",
                 apply = "window_qqplot_biostat")
    tkgrid(getFrame(variableBox), sticky = "nw")
    tkgrid(
        labelRcmdr(binsFrame, text = gettext_bs(
            "Number of bins\nfor Pearson chi-square"
        )),
        binsField,
        padx = 3,
        sticky = "sw"
    )
    tkgrid(testFrame, binsFrame, sticky = "sw")
    tkgrid(optionsFrame, sticky = "sw")
    tkgrid(groupsFrame, sticky = "w", padx = 6)
    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix()
}
