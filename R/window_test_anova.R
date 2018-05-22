




#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_test_anova <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Required packages ------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Library("multcomp")
    Library("abind")
    Library("biostat")

    cur_env <- environment()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Default values ---------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    defaults <- list(
        initial.response = NULL,
        initial.group    = NULL,
        initial.alpha    = 0.05,
        initial.pairwise = 0,
        initial.welch    = 0
    )

    dialog.values <- getDialog("window_anova_kw_mood_tests", defaults)



    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Dialog elements --------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tabs = c("dataTab", "mainTab",    "posthocTab",          "outputTab",        "plotsTab")
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
    Rcmdr::radioButtons(window = main_top_frame,
                        name = "main_test",
                        buttons = c("anova", "welch_anova", "kw_test", "mood_test"),
                        values  = c("anova", "welch_anova", "kw_test", "mood_test"),
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
        title = gettextRcmdr("Grouping variable \n(pick one)"),
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

        Rcmdr::radioButtons(window = posthoc_test_panel,
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

do_anova <- function(variables) {


    # y_var  <- "weight"
    # gr_var <- "group"
    # .activeDataSet <- "PlantGrowth"
    # model_name <- "model_1a"
    #
    # markdown_compatible <- FALSE

    # model_anova <- aov(weight ~ group, data = PlantGrowth)
    #
    # model_anova_summary <- summary(model_anova)
    #
    # pander::pander(model_anova_summary, missing = "")
    # print(model_anova_summary)
    #
    # autoplot(model_anova, which = 1:2, alpha = 0.6,
    # data = PlantGrowth, colour = "group")

    # model_to_print <- .activeDataSet
    #

    .activeDataSet <- activeDataSet()
    formula <- glue('{y_var} ~ {gr_var}, data = {.activeDataSet}')

    model_to_print <- switch(main_test,
                             "anova" = "anova_summary",
                             # otherwise
                             model_name)

    if (markdown_compatible) {
        Library("pander")
        if (is.na(pander::panderOptions("missing"))) {
            doItAndPrint('panderOptions("missing", "") \n')
        }
        print_fun <- "pander"
    } else {
        print_fun <- "print"
    }

    switch(main_test,
           "anova" = {
               glue('#  --- One-way ANOVA --- \n\n',
                    '{model_name} <- aov({formula}) \n',
                    'anova_summary <- summary({model_name}) \n',
                    '{print_fun}(anova_summary) \n',
                    'remove(anova_summary)')

               if (use_anova_diagnostics) {
                   # Diagnostics (ANOVA only)
                   # 1. Homogeneity of variances
                   # 2. Normality
                   Library(ggfortify)
                   open_new_plots_window()
                   glue('autoplot({model_name}, which = 1:2, alpha = 0.6,',
                        '{spaces(9)}data = {.activeDataSet}, colour = "{gr_var}")')
               }


           },
           wanova = {
               glue('#  --- Welch ANOVA --- \n\n',
                    '{model_name} <- oneway.test({formula}) \n',
                    '{print_fun}({model_name}) \n')
           },

           kw_test = {
               glue('#  --- Kruskal-Wallis test --- \n\n',
                    '{model_name} <- kruskal.test({formula}) \n',
                    '{print_fun}({model_name}) \n')
           },

           mood_test = {
               Library(RVAideMemoire)
               glue('#  --- Mood Median test --- \n\n',
                    '{model_name} <- mood.medtest({formula}) \n',
                    '{print_fun}({model_name}) \n')
           },

           stop("Unrecognized test")

    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Patikrinti, ar pagrindinio testo rezultatas statistiškai reikšmingas
    #
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    p_adj <- "holm"
    switch(posthoc_test,
           "tukey" = {
               Library(biostat)

               # Formula based results
               glue('# --- Post-hoc analysis: Tukey HSD test ---\n\n',
                    'posthoc_results <- posthoc_anova({formula}, \n',
                    '                                 method = "Tukey") \n')


               glue('# --- Post-hoc analysis: Games-Howell test ---\n\n',
                    'posthoc_results <- posthoc_anova({formula}, \n',
                    '                                 method = "Games-Howell") \n')


               glue('# --- Post-hoc analysis: Pairwise median test ---\n\n',
                    'posthoc_results <- pairwiseMedianTest({formula}, \n',
                    '                                      method = "{p_adj}") \n')

               Library("PMCMR")
               glue('# --- Post-hoc analysis: Conover-Iman test ---\n\n',
                    'posthoc_results <- ',
                    '    posthoc.kruskal.conover.test({formula}, \n',
                    '                                 p.adjust.method = "{p_adj}")')

               Library("PMCMR")
               glue('# --- Post-hoc analysis: Dunn test ---\n\n',
                    'posthoc_results <- ',
                    '    posthoc.kruskal.dunn.test({formula}, \n',
                    '                              p.adjust.method = "{p_adj}")')

               Library("PMCMR")
               glue('# --- Post-hoc analysis: Nemenyi test ---\n\n',
                    'posthoc_results <- ',
                    '    posthoc.kruskal.nemenyi.test({formula}, \n',
                    '                                 dist = "Chisquare")')



               # Non-formula based
               # Pairwise Student t test
               glue('# --- Post-hoc analysis: Pairwise Student t test ---\n\n',
                    'with({.activeDataSet}, \n',
                    '     pairwise.t.test({y_var}, {gr_var}, \n',
                    '                     p.adjust.method = "{p_adj}", \n',
                    '                     pool.sd = TRUE))')
               # Pairwise Welch t test
               glue('# --- Post-hoc analysis: Pairwise Welch t test ---\n\n',
                    'with({.activeDataSet}, \n',
                    '     pairwise.t.test({y_var}, {gr_var}, \n',
                    '                     p.adjust.method = "{p_adj}", \n',
                    '                     pool.sd = FALSE))')

               # Pairwise Wilcoxon t test
               glue('# --- Post-hoc analysis: Pairwise Mann-Whitney-Wilcoxon test ---\n\n',
                    'with({.activeDataSet}, \n',
                    '     pairwise.wilcox.test({y_var}, {gr_var}, \n',
                    '                          p.adjust.method = "{p_adj}"))')
           }

    )

    glue('cld_results <- make_cld(posthoc_results)',
         "\n\n",
         '{print_fun}(posthoc_results) \n',
         '{print_fun}(cld_results) \n')


    # Grafikas su cld žymėjimais
    plot_parameters <- ""
    glue('gg_boxplot_plus({formula},\n',
         '{spaces(16)}cld = cld_results{plot_parameters})\n')




}


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_test_anova2 <- function() {
    cur_env <- environment()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Default values ---------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        initial.y_var      = NULL,
        initial.gr_var     = NULL,
        initial.digits     = "NA",
        initial.keep_model = FALSE
    )

    dialog.values <- getDialog("window_test_anova2", defaults)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Dialog elements --------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tabs =      c("dataTab", "optionsTab")
    tab_names = c(" Data ",  " Options ")

    initializeDialog(title = gettextRcmdr("One-way ANOVA"),
                     use.tabs = TRUE, tabs = tabs)

    # posthocFrame <- tkframe(posthocTab)
    # plotsFrame <- tkframe(plotsTab)

    # ** Data tab ------------------------------------------------------------
    # . Variable selection -----------------------------------------------------

    dataFrame <- tkframe(dataTab)
    yBox <- variableListBox2(
        dataFrame,
        Numeric(),
        selectmode = "single",
        listHeight = 6,
        title = gettextRcmdr("Response variable \n(pick one)"),
        initialSelection = varPosn(dialog.values$initial.y_var, "numeric")
    )

    groupBox <- variableListBox2(
        dataFrame,
        selectmode = "single",
        variables_fct(),
        listHeight = 6,
        title = gettextRcmdr("Grouping variable \n(pick one)"),
        initialSelection = var_pos_n(dialog.values$initial.gr_var, "factor_strict"))

    tkgrid(
        getFrame(yBox),
        labelRcmdr(dataFrame, text = "        "), # Vertical space
        getFrame(groupBox),
        sticky = "nw", pady = c(5, 5)
    )
    tkgrid(dataFrame, sticky = "w")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ** Main tab ------------------------------------------------------------
    # . Main test & model name textbox ---------------------------------------

    main_top_frame <- tkframe(optionsTab)

    labelText <- tclVar("Select the test") ### [!!!] Initial value

    # Choose model name ------------------------------------------------------
    modelName  <- tclVar(unique_obj_names(suffix = "_anova", all_numbered = TRUE))
    model_boxlFrame <- tkframe(main_top_frame)
    model <- ttkentry(model_boxlFrame, width = "20", textvariable = modelName)

    bs_check_boxes(model_boxlFrame,
                   # ttk = TRUE,
                   frame = "keep_model_Frame",
                   # title = "Plot options",
                   boxes = c("keep_model"),
                   initialValues = c(dialog.values$initial.keep_model),
                   labels = gettextRcmdr(
                       c("Keep summary in R memory")
                   ),
                   commands = list("keep_model" = function(){})
    )


    tkgrid(labelRcmdr(model_boxlFrame,
                      text = gettextRcmdr("Enter name for summary: "),
                      fg = Rcmdr::getRcmdr("title.color")),   sticky = "w")

    tkgrid(model, sticky = "ew")
    tkgrid(keep_model_Frame, sticky = "ew")

    tkgrid(model_boxlFrame, sticky = "nw")




    digitsVar <- tclVar(dialog.values$initial.digits)

    digitsVarFrame <- tkframe(main_top_frame)
    digitsBox      <- ttkentry(digitsVarFrame, width = "20", textvariable = digitsVar)

    tkgrid(labelRcmdr(digitsVarFrame,
                      text = gettextRcmdr("Decimal digits to round to:\n(either integer or NA)"),
                      fg = Rcmdr::getRcmdr("title.color")),   sticky = "w")

    tkgrid(digitsBox, sticky = "ew")
    tkgrid(digitsVarFrame, sticky = "nw")


    tkgrid(main_top_frame, sticky = "nw")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {

        gr_var <- getSelection(groupBox)
        y_var  <- getSelection(yBox)
        digits <- suppressWarnings(tclvalue_int(digitsVar))
        names(digits) <- NULL
        model_name_Value <- trim.blanks(tclvalue(modelName))
        keep_model <- tclvalue_lgl(keep_modelVariable)


        if (!is.valid.name(model_name_Value)) {
            UpdateModelNumber(-1)
            errorCondition(recall = window_test_anova2,
                           message = sprintf(gettextRcmdr("\"%s\" is not a valid name."),
                                             model_name_Value))
            return()
        }

        if (is.element(model_name_Value, list_summaries_Models())) {
            if ("no" == tclvalue(checkReplace(model_name_Value,
                                              type = gettextRcmdr("Model")))) {
                UpdateModelNumber(-1)
                tkdestroy(top)
                window_test_anova2()
                return()
            }
        }

        closeDialog()

        if (length(y_var) == 0) {
            errorCondition(
                recall = window_test_anova2,
                message = gettextRcmdr("You must select a variable to summarize.")
            )
            return()
        }

        putDialog("window_test_anova2",
                  list(initial.y_var  = y_var,
                       initial.gr_var = gr_var,
                       initial.digits = as.character(digits),
                       initial.keep_model = keep_model
                  )
        )

        # calculations -------------------------------------------------------
        .activeDataSet <- ActiveDataSet()
        Library("biostat")

        if (length(y_var) > 1) {
            y_var <- paste0(y_var, collapse = " + ")
        }
        # For many groups ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(gr_var) > 1) {
            gr_var <- paste0(gr_var, collapse = " + ")
        }

        if (length(gr_var) == 0) {
            formula = glue("~{y_var}")

        } else {
            formula = glue("{y_var} ~ {gr_var}")

        }


        if (keep_model) {
            keep_model_command <- ""

        } else {
            UpdateModelNumber(-1)
            keep_model_command <- glue("remove({model_name_Value})")
        }

        command <- style_cmd(glue(
            "{model_name_Value} <- biostat::do_summary({formula}, ",
            "data = {.activeDataSet})\n",
            "print({model_name_Value}, digits = {digits})\n",
            keep_model_command))

        doItAndPrint(command)

        # Post calculations --------------------------------------------------
        # activeModel(model_name_Value)
        # putRcmdr("modelWithSubset", FALSE)

        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ** Footer ------------------------------------------------------------------
    OKCancelHelp(
        helpSubject = "do_summary",
        helpPackage = "biostat",
        model = TRUE,
        reset = "window_test_anova2",
        apply = "window_test_anova2"
    )
    # tkgrid(buttonsFrame, sticky = "w")

    dialogSuffix(use.tabs = TRUE, grid.buttons = TRUE,
                 tabs = tabs,
                 tab.names = tab_names)
}
# ==============================================================================



