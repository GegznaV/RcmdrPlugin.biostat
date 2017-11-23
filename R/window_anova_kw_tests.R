#' @export
#' @keywords internal
window_anova_kw_tests <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Required packages ------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Library("multcomp")
    Library("abind")
    Library("BioStat")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Default values ---------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    defaults <- list(
            initial.group = NULL,
            initial.response = NULL,
            initial.alpha = 0.05,
            initial.pairwise = 0,
            initial.welch = 0
        )

    dialog.values <- getDialog("compare_centers", defaults)



    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Dialog elements --------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         tabs = c("dataTab", "mainTab",    "posthocTab",       "plotsTab")
    tab_names = c(" Data ", " Main test ", " Post-hoc analysis ", " Plots ")


    initializeDialog(title = gettextRcmdr("Compare centers of independent samples"),
                     use.tabs = TRUE, tabs = tabs)

    optionsFrame <- tkframe(posthocTab)
    optionsFrame <- tkframe(plotsTab)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ** Main tab ------------------------------------------------------------
    # . Main test & model name textbox ---------------------------------------

    main_top_frame <- tkframe(mainTab)

    # Choose test
    radioButtons(window = main_top_frame,
                 name = "main_test",
                 buttons = c("anova", "welch_anova", "kw_test"),
                 values = c("anova", "welch_anova", "kw_test"),

                 labels =  get_BioStat_text(c("ANOVA",
                                              "Welch ANOVA",
                                              "Kruskal-Wallis test")),
                 title = get_BioStat_text("Test")
    )

    # Choose model name
    UpdateModelNumber()

    modelName  <- tclVar(paste0("Model_", getRcmdr("modelNumber")))
    mode_boxlFrame <- tkframe(main_top_frame)
    model <- ttkentry(mode_boxlFrame, width = "20", textvariable = modelName)

    tkgrid(labelRcmdr(mode_boxlFrame,
                      text = gettextRcmdr("Enter name for model: "),
                      fg = Rcmdr::getRcmdr("title.color")),   sticky = "w")
    tkgrid(model, sticky = "ew")

    # Choose alpha

    # textEntryVarTcl <- tclVar(dialog.values$initial.alpha)
    textEntryVarTcl <- tclVar("0.05")
    text_alpha <- tk2entry(mode_boxlFrame,
                           width = 5,
                           textvariable = textEntryVarTcl)

    tkgrid(labelRcmdr(mode_boxlFrame,
               text = gettextRcmdr("Significance level (0-1):"),
               fg = Rcmdr::getRcmdr("title.color")),
           pady = c(5, 0),
           sticky = "nw")

    tkgrid(text_alpha, sticky = "nw")


    tkgrid(
        main_testFrame,
        mode_boxlFrame,
        pady = c(0, 5),
        padx = c(10, 5),
        sticky = "nw"
    )


    tkgrid(main_top_frame, sticky = "w")


    # . Variable selection -----------------------------------------------------

    dataFrame <- tkframe(dataTab)
    groupBox <- variableListBox(
        dataFrame,
        Factors(),
        listHeight = 5,
        title = gettextRcmdr("Group variable \n(pick one)"),
        initialSelection = varPosn(dialog.values$initial.group, "factor"))

    responseBox <- variableListBox(
        dataFrame,
        Numeric(),
        listHeight = 5,
        title = gettextRcmdr("Response variable \n(pick one)"),
        initialSelection = varPosn(dialog.values$initial.response, "numeric")
    )


    tkgrid(
        getFrame(responseBox),
        labelRcmdr(dataFrame, text = "        "), # Vertical space
        getFrame(groupBox),
        sticky = "nw", pady = c(5, 5)
    )
    tkgrid(dataFrame, sticky = "w")

    # ** Post-hoc tab -----------------------------------------------------------
    posthoc_test_panel <- ttk_labelframe(posthocTab, "Post-hoc test options")

    radioButtons(window = posthoc_test_panel,
                 name = "posthoc_test",
                 buttons = c("none", "tukey_test", "gh_test", "con_im_test"),
                 values  = c("none", "tukey_test", "gh_test", "con_im_test"),

                 labels =  get_BioStat_text(c("none",
                                              "Tukey HSD",
                                              "Games-Howell",
                                              "Conover-Iman")),
                 title = get_BioStat_text("Post-hoc test")
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pval_adjustment <- inputComboBox(
        posthoc_test_panel,
        variableList = p.adjust.methods,
        initialSelection = p.adjust.methods[1],
        title = get_BioStat_text("P value adjustment method")
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(posthoc_testFrame,         pady = c(0, 5), padx = c(5, 5), sticky = "nw")
    tkgrid(getFrame(pval_adjustment), pady = c(5, 5), padx = c(5, 5), sticky = "nw")
    tkgrid(posthoc_test_panel, sticky = "nw")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
    #

    # optionsFrame <- tkframe(dataTab)
    #
    # pairwiseVariable <- tclVar(dialog.values$initial.pairwise)
    # pairwiseCheckBox <- ttkcheckbutton(optionsFrame, variable = pairwiseVariable)
    #
    # welchVariable <- tclVar(dialog.values$initial.welch)
    # welchCheckBox <- ttkcheckbutton(optionsFrame, variable = welchVariable)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # onOK -------------------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        # modelValue <- trim.blanks(tclvalue(modelName))
        #
        # if (!is.valid.name(modelValue)) {
        #     UpdateModelNumber(-1)
        #     errorCondition(recall = window_compare_centers,
        #                    message = sprintf(gettextRcmdr("\"%s\" is not a valid name."),modelValue
        #                    ))
        #     return()
        # }
        # if (is.element(modelValue, listAOVModels())) {
        #     if ("no" == tclvalue(checkReplace(modelValue, type = gettextRcmdr("Model")))) {
        #         UpdateModelNumber(-1)
        #         tkdestroy(top)
        #         window_compare_centers()
        #         return()
        #     }
        # }
        # group <- getSelection(groupBox)
        # response <- getSelection(responseBox)
        closeDialog()


        # calculations -------------------------------------------------------

        # if (length(group) == 0) {
        #     errorCondition(
        #         recall = window_compare_centers,
        #         message = gettextRcmdr("You must select a groups factor.")
        #     )
        #     return()
        # }
        #
        # if (length(response) == 0) {
        #     errorCondition(
        #         recall = window_compare_centers,
        #         message = gettextRcmdr("You must select a response variable.")
        #     )
        #     return()
        # }
        # .activeDataSet <- ActiveDataSet()
        #
        # # command <- glue::glue("{modelValue} <- aov({response} ~ {group}, data = {.activeDataSet})")
        # # justDoIt(command)
        # # logger(command)
        #
        # doItAndPrint(glue::glue("{modelValue} <- aov({response} ~ {group}, data = {.activeDataSet})"))
        #
        # doItAndPrint(glue::glue("summary({modelValue})"))
        #
        # doItAndPrint(
        #     glue::glue('with({.activeDataSet}, ',
        #                '  numSummary({response}, groups = {group}, statistics = c("mean", "sd"))',
        #                ')', sep = " \n"))
        #
        # activeModel(modelValue)
        #
        #
        # putRcmdr("modelWithSubset", FALSE)
        # pairwise <- tclvalue(pairwiseVariable)
        #    welch <- tclvalue(welchVariable)
        #
        # putDialog("compare_centers",
        #           list(initial.group = group,
        #                initial.response = response,
        #                initial.pairwise = pairwise,
        #                initial.welch = welch
        #           )
        # )
        #
        # if (pairwise == 1) {
        #     if (eval_glue("length(levels({.activeDataSet}${group})) < 3"))
        #
        #         Message(message = gettextRcmdr(
        #                 "Factor has fewer than 3 levels; pairwise comparisons omitted."),
        #             type = "warning"
        #         )
        #     else {
        #         commands <- glue::glue(
        #             "local({{",
        #             '  .Pairs <- glht({modelValue}, linfct = mcp({group} = "Tukey"))',
        #             "  print(summary(.Pairs)) # pairwise tests",
        #             "  print(confint(.Pairs)) # confidence intervals",
        #             "  print(cld(.Pairs))     # compact letter display",
        #             "  old.oma <- par(oma = c(0, 5, 0, 0))",
        #             "  plot(confint(.Pairs))",
        #             "  par(old.oma)",
        #             "}})",
        #
        #             sep = " \n")
        #
        #         doItAndPrint(commands)
        #     }
        # }
        #
        # if (welch == 1) {
        #     command <- glue::glue(
        #         "oneway.test({response} ~ {group}, data = {.activeDataSet}) # Welch test")
        #     doItAndPrint(command)
        # }

        tkfocus(CommanderWindow())
    }


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    #
    #
    #
    #
    # tkgrid(main_testFrame, posthoc_testFrame, sticky = "ne")
    # tkgrid(test_options, sticky = "w")
    #
    # tkgrid(pairwiseCheckBox,
    #        labelRcmdr(optionsFrame, text = gettextRcmdr("Pairwise comparisons of means")),
    #        sticky = "w")
    #
    # tkgrid(welchCheckBox,
    #        labelRcmdr(
    #            optionsFrame,
    #            text = gettextRcmdr("Welch F-test not assuming equal variances")
    #        ),
    #        sticky = "w")
    #
    # tkgrid(optionsFrame, sticky = "w")


   # ** Footer ------------------------------------------------------------------


    OKCancelHelp(
        helpSubject = "anova",
        model = TRUE,
        reset = "compare_centers",
        apply = "compare_centers"
    )
    # tkgrid(buttonsFrame, sticky = "w")

    dialogSuffix(use.tabs = TRUE, grid.buttons = TRUE,
                 tabs = tabs,
                 tab.names = tab_names)
}