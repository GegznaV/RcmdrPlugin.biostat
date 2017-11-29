#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_anova_kw_mood_tests <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Required packages ------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Library("multcomp")
    Library("abind")
    Library("BioStat")

    cur_env <- environment()
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

    dialog.values <- getDialog("window_anova_kw_mood_tests", defaults)



    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Dialog elements --------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         tabs = c("dataTab", "mainTab",    "posthocTab",          "outputTab", "plotsTab")
    tab_names = c(" Data ", " Main test ", " Post-hoc analysis ", "Numerical output", "Plots ")

    initializeDialog(title = gettextRcmdr("Compare centers of independent samples"),
                     use.tabs = TRUE, tabs = tabs)

    # posthocFrame <- tkframe(posthocTab)
    # plotsFrame <- tkframe(plotsTab)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ** Main test tab ------------------------------------------------------------
    # . Main test & model name textbox ---------------------------------------

    main_top_frame <- tkframe(mainTab)

    labelText <- tclVar("...") ### [!!!] Initial value

    # Callback functions
    update_test_name <- function() {
        tclvalue(labelText) <-
            paste("Current test: ", tclvalue(main_testVariable))
    }


    ph_buttons <- tclVar("...")
    ph_values  <- tclVar("...")
    ph_labels  <- tclVar("...")


    update_posthoc_choices <- function() {
        switch(tclvalue(main_testVariable),
               "anova" = {
                   logger("ANOVA")
                   tclvalue(ph_buttons) = c("none", "tukey_test", "gh_test", "pw_stud_t_test", "pw_welch_t_test")
                   tclvalue(ph_values)  = c("none", "tukey_test", "gh_test", "pw_stud_t_test", "pw_welch_t_test" )
                   tclvalue(ph_labels)  = s2u(c("none", "Tukey HSD test", "Games-Howell test", "Pairwise Student t test", "Pairwise Welch t test"))
               },

               "welch_anova" = {
                   logger("Welch ANOVA")
                   tclvalue(ph_buttons) = c("none", "gh_test", "pw_welch_t_test")
                   tclvalue(ph_values)  = c("none", "gh_test", "pw_welch_t_test")
                   tclvalue(ph_labels)  = s2u(c("none", "Games-Howell test", "Pairwise Welch t"))
               },

               "kw_test" = {
                   logger("Kruskal-Wallis ANOVA")

                   tclvalue(ph_buttons) = c("none", "con_im_test", "dunn_test", "nemenyi", "pw_wicoxon_test")
                   tclvalue(ph_values)  = c("none", "con_im_test", "dunn_test", "nemenyi", "pw_wicoxon_test")
                   tclvalue(ph_labels)  = s2u(c("none", "Conover-Iman test", "Dunn test", "Nemenyi test", "Pairwise Wilcoxon test"))
               },

               "mood_test" = {
                   logger("Mood test")

                   tclvalue(ph_buttons) = c("none", "pw_median_test")
                   tclvalue(ph_values)  = c("none", "pw_median_test")
                   tclvalue(ph_labels)  = s2u(c("none", "Pairwise median test", "Dunn test", "Nemenyi test"))
               }

        )
               logger(paste(tclvalue_chr(ph_labels), collapse = ", "))

               tkdestroy(posthoc_test_panel)

        radiobuttons_env(window = posthoc_test_panel,
                            name = "posthoc_test",
                            buttons = tclvalue_chr(ph_buttons),
                            values  = tclvalue_chr(ph_values),

                            labels = gettext_Bio(u2s(tclvalue_chr(ph_labels))),
                            title = gettext_Bio("Post-hoc test"), env = parent.frame()
               )

        tkgrid(posthoc_testFrame,         pady = c(0, 5), padx = c(5, 5), sticky = "nw")
        tkgrid(getFrame(pval_adjustment), pady = c(5, 5), padx = c(5, 5), sticky = "nw")
        tkgrid(posthoc_test_panel, sticky = "nw")

    }

    # Main test --------------------------------------------------------------
    radioButtons(window = main_top_frame,
                 name = "main_test",
                 buttons = c("anova", "welch_anova", "kw_test", "mood_test"),
                 values =  c("anova", "welch_anova", "kw_test", "mood_test"),
                 # initialValue = NULL,

                 labels =  gettext_Bio(c("ANOVA",
                                         "Welch ANOVA",
                                         "Kruskal-Wallis test",
                                         "Mood's median test")),
                 title = gettext_Bio("Test"),
                 command = function() {
                     logger("ok")
                     update_test_name()
                     update_posthoc_choices()
                     if (!missing("posthoc_test_panel"))
                         tkdestroy(posthoc_test_panel)
                     create_posthoc_tab(env = cur_env)
                 }

    )

    # Choose model name ------------------------------------------------------
    UpdateModelNumber()

    modelName  <- tclVar(paste0("Model_", getRcmdr("modelNumber")))
    model_boxlFrame <- tkframe(main_top_frame)
    model <- ttkentry(model_boxlFrame, width = "20", textvariable = modelName)

    tkgrid(labelRcmdr(model_boxlFrame,
                      text = gettextRcmdr("Enter name for model: "),
                      fg = Rcmdr::getRcmdr("title.color")),   sticky = "w")

    tkgrid(model, sticky = "ew")

    # Choose alpha -----------------------------------------------------------

    # textEntryVarTcl <- tclVar(dialog.values$initial.alpha)
    textEntryVarTcl <- tclVar("0.05")
    text_alpha <- tk2entry(model_boxlFrame,
                           width = 5,
                           textvariable = textEntryVarTcl)

    tkgrid(labelRcmdr(model_boxlFrame,
               text = gettextRcmdr("Significance level (0-1):"),
               fg = Rcmdr::getRcmdr("title.color")),
           pady = c(5, 0),
           sticky = "nw")

    tkgrid(text_alpha, sticky = "nw")


    tkgrid(
        main_testFrame,
        model_boxlFrame,
        pady = c(0, 5),
        padx = c(10, 5),
        sticky = "nw"
    )

    tkgrid(main_top_frame, sticky = "w")

    # tkgrid(tk2label(mainTab, text = paste("Current test: ", test_name)),
    tkgrid(tk2label(mainTab, textvariable = labelText),
           pady = c(20, 0),
           sticky = "w")


    # ** Data tab ------------------------------------------------------------
    # . Variable selection -----------------------------------------------------

    dataFrame <- tkframe(dataTab)
    groupBox <- variableListBox2(
        dataFrame,
        Factors(),
        listHeight = 10,
        title = gettextRcmdr("Group variable \n(pick one)"),
        initialSelection = varPosn(dialog.values$initial.group, "factor"))

    responseBox <- variableListBox2(
        dataFrame,
        Numeric(),
        listHeight = 10,
        title = gettextRcmdr("Variable to test \n(pick one)"),
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
    create_posthoc_tab <- function(env = parent.frame()) {
        posthoc_test_panel <- labeled_frame(posthocTab, "Post-hoc test options")

        radioButtons(window = posthoc_test_panel,
                     name = "posthoc_test",
                     buttons = tclvalue_chr(ph_buttons),
                     values  = tclvalue_chr(ph_values),

                     labels = gettext_Bio(u2s(tclvalue_chr(ph_labels))),
                     title = gettext_Bio("Post-hoc test")
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        pval_adjustment <- inputComboBox(
            posthoc_test_panel,
            variableList = p.adjust.methods,
            initialSelection = p.adjust.methods[1],
            title = gettext_Bio("P value adjustment method")
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkgrid(posthoc_testFrame,         pady = c(0, 5), padx = c(5, 5), sticky = "nw")
        tkgrid(getFrame(pval_adjustment), pady = c(5, 5), padx = c(5, 5), sticky = "nw")
        tkgrid(posthoc_test_panel, sticky = "nw")

        assign("posthoc_test_panel", posthoc_test_panel, envir = env)
    }
    create_posthoc_tab(env = cur_env)
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
        #     errorCondition(recall = window_anova_kw_mood_tests,
        #                    message = sprintf(gettextRcmdr("\"%s\" is not a valid name."),modelValue
        #                    ))
        #     return()
        # }
        # if (is.element(modelValue, listAOVModels())) {
        #     if ("no" == tclvalue(checkReplace(modelValue, type = gettextRcmdr("Model")))) {
        #         UpdateModelNumber(-1)
        #         tkdestroy(top)
        #         window_anova_kw_mood_tests()
        #         return()
        #     }
        # }
        # group <- getSelection(groupBox)
        # response <- getSelection(responseBox)
        closeDialog()


        # calculations -------------------------------------------------------

        # if (length(group) == 0) {
        #     errorCondition(
        #         recall = window_anova_kw_mood_tests,
        #         message = gettextRcmdr("You must select a groups factor.")
        #     )
        #     return()
        # }
        #
        # if (length(response) == 0) {
        #     errorCondition(
        #         recall = window_anova_kw_mood_tests,
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
        # putDialog("window_anova_kw_mood_tests",
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
        reset = "window_anova_kw_mood_tests",
        apply = "window_anova_kw_mood_tests"
    )
    # tkgrid(buttonsFrame, sticky = "w")

    dialogSuffix(use.tabs = TRUE, grid.buttons = TRUE,
                 tabs = tabs,
                 tab.names = tab_names)
}



# ==============================================================================

