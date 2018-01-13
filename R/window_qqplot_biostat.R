#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_qqplot_biostat <- function() {

    nrows <- getRcmdr("nrow") # nrows in active dataset
    defaults <- list(initial_var = NULL,
                     initial_test = if (nrows <= 5000) "shapiro.test" else "ad.test",
                     initial_bins = gettextRcmdr("<auto>"),
                     initial_groups = NULL
    )

    dialog.values <- getDialog("window_qqplot_biostat", defaults)
    initializeDialog(title = gettextRcmdr("Test of Normality"))

    variableBox <- variableListBox(
        top,
        Numeric(),
        title = gettextRcmdr("Variable to test\n(pick one)"),
        initialSelection = varPosn(dialog.values$initial_var, "numeric")
    )

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
                 initialValue = dialog.values$initial_test
    )
    binsFrame <- tkframe(optionsFrame)
    binsVariable <- tclVar(dialog.values$initial_bins)
    binsField <- ttkentry(binsFrame, width = "8", textvariable = binsVariable)

    groupsBox(
        recall = window_qqplot_biostat,
        label = gettextRcmdr("Test by:"),
        initialLabel = if (is.null(dialog.values$initial_group)) {
            gettextRcmdr("Test by groups")
        } else {
            paste(gettextRcmdr("Test by:"),
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
        if (bins != gettextRcmdr("<auto>") &&
            (is.na(nbins) || nbins < 4)) {
            errorCondition(
                recall = window_qqplot_biostat,
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

        putDialog("window_qqplot_biostat",
                  list(initial_var = var,
                       initial_test = test,
                       initial_bins = bins,
                       initial_groups = if(.groups == FALSE) NULL else .groups
                  )
        )
        if (length(var) == 0) {
            errorCondition(recall = window_qqplot_biostat,
                           message = gettextRcmdr("You must select a variable."))
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
            command2 <- glue::glue(
                'biostat::qq_plot(~{var}, ',
                'data = {ActiveDataSet()}, use_colors = TRUE)')
        } else{
            command2 <- glue::glue(
                'biostat::qq_plot({var}~{.groups}, ',
                'data = {ActiveDataSet()}, use_colors = TRUE)')
        }

        doItAndPrint(command2)

        # Test results ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # if (.groups == FALSE) {
        #     command <- glue::glue(
        #         'biostat::test_normality(~{var}, ',
        #         'data = {ActiveDataSet()}, test = {test}{n.classes})')
        # } else{
        #     command <- glue::glue(
        #         'biostat::test_normality({var}~{.groups}, ',
        #         'data = {ActiveDataSet()}, test = {test}{n.classes})')
        # }
        #
        # # if (.groups == FALSE) {
        # #     command <- glue::glue(
        # #         'normalityTest(~{var}, test = "{test}",',
        # #         ' data = {ActiveDataSet()}{n.classes})'                )
        # # } else{
        # #     command <- glue::glue(
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
        labelRcmdr(binsFrame, text = gettextRcmdr(
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
