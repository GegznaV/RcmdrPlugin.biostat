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

    dialog.values <- getDialog("window_anova_kw_tests", defaults)



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




    labelText <- tclVar("...") ### [!!!] Initial value

    update_test_name <- function() {
        tclvalue(labelText) <-
            paste("Current test: ", as.character(tclvalue(main_testVariable)))
    }

    # Choose test
    radioButtons(window = main_top_frame,
                 name = "main_test",
                 buttons = c("anova", "welch_anova", "kw_test"),
                 values = c("anova", "welch_anova", "kw_test"),
                 # initialValue = -1,

                 labels =  get_BioStat_text(c("ANOVA",
                                              "Welch ANOVA",
                                              "Kruskal-Wallis test")),
                 title = get_BioStat_text("Test"),
                 command = update_test_name
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

    # tkgrid(tk2label(mainTab, text = paste("Current test: ", test_name)),
    tkgrid(tk2label(mainTab,
                    textvariable = labelText),
           pady = c(20, 0),
           sticky = "w")


    # ** Data tab ------------------------------------------------------------
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
    posthoc_test_panel <- labeled_frame(posthocTab, "Post-hoc test options")

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
        #     errorCondition(recall = window_anova_kw_tests,
        #                    message = sprintf(gettextRcmdr("\"%s\" is not a valid name."),modelValue
        #                    ))
        #     return()
        # }
        # if (is.element(modelValue, listAOVModels())) {
        #     if ("no" == tclvalue(checkReplace(modelValue, type = gettextRcmdr("Model")))) {
        #         UpdateModelNumber(-1)
        #         tkdestroy(top)
        #         window_anova_kw_tests()
        #         return()
        #     }
        # }
        # group <- getSelection(groupBox)
        # response <- getSelection(responseBox)
        closeDialog()


        # calculations -------------------------------------------------------

        # if (length(group) == 0) {
        #     errorCondition(
        #         recall = window_anova_kw_tests,
        #         message = gettextRcmdr("You must select a groups factor.")
        #     )
        #     return()
        # }
        #
        # if (length(response) == 0) {
        #     errorCondition(
        #         recall = window_anova_kw_tests,
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
        # putDialog("window_anova_kw_tests",
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
        reset = "window_anova_kw_tests",
        apply = "window_anova_kw_tests"
    )
    # tkgrid(buttonsFrame, sticky = "w")

    dialogSuffix(use.tabs = TRUE, grid.buttons = TRUE,
                 tabs = tabs,
                 tab.names = tab_names)
}



# ==============================================================================


# ----------------------------------------------------------------------------
...ANOVA... <- function() {

    model_anova <- aov(weight ~ group, data = PlantGrowth)

    model_anova_summary <- summary(model_anova)
    pander::pander(model_anova_summary)
    print(model_anova_summary)


    # 1. Homogeneity of variances
    plot(model_anova, 1)

    # 2. Normality
    plot(model_anova, 2)

    library(ggplot2)
    library(ggfortify)

    old_parameters <- par(mfrow = c(1, 2))
    plot(model_anova, which = 1:2)
    par(old_parameters)

    autoplot(model_anova, which = 1:2)

    autoplot(model_anova, which = 1) + geom_point(aes(color = group))
}

# # 1. Homogeneity of variances
#
# The residuals versus fits plot can be used to check the homogeneity of variances.
#
# In the plot below, there is no evident relationships between residuals and fitted values (the mean of each groups), which is good. So, we can assume the homogeneity of variances.

# ----------------------------------------------------------------------------
...Welch_ANOVA... <- function() {

    model_wanova <- oneway.test(weight ~ group, data = PlantGrowth)
    pander::pander(model_wanova)
    print(model_wanova)
}
# ----------------------------------------------------------------------------
...Kruskal_Wallis_test... <- function() {

    model_kw_test <- kruskal.test(weight ~ group, data = PlantGrowth)

    pander::pander(model_kw_test)
    print(model_kw_test)
}

# ============================================================================

# ----------------------------------------------------------------------------
...ANOVA_post_hoc__tukey... <- function() {
    model_tukey <- posthoc_anova_tukey(weight ~ group, data = PlantGrowth)

    model_tukey

    cld_result <- make_cld(model_tukey)
    cld_result
    pander::pander(cld_result)


    library(tidyverse)

    # cld_max <- with(PlantGrowth, tapply(weight, group, function(x) 1.05 * max(x)))
    cld_y <- max(-PlantGrowth$weight * 0.95)
    cld_y <- min(-PlantGrowth$weight * 1.05)

    cld_y <- min(PlantGrowth$weight * 0.95)
    cld_y <- max(PlantGrowth$weight * 1.05)


    DATA <- mutate(PlantGrowth, group = fct_reorder(group, weight, fun = mean))

    ggplot(DATA, aes(x = group, y = weight, fill = group)) +
        geom_boxplot(width = .2) +
        geom_jitter(
            aes(x = as.numeric(group) + .3),
            alpha = 0.3,
            width = .1,
            shape = 21
        ) +
        geom_text(data = cld_result,
                  aes(x = Group, label = cld, y = cld_y),
                  fontface = "bold",
                  inherit.aes = FALSE) +
        ggtitle("Pair-wise comparisons")

        # +
        # ylim(56, 74)  # don't cut off the text I just added


}
# ----------------------------------------------------------------------------
...ANOVA_post_hoc__tukey_2... <- function() {

    library(multcomp)

    model_anova <- aov(weight ~ group, data = PlantGrowth)
    summary(model_anova)
    model_post_hoc <- glht(model_anova, linfct = mcp(group = "Tukey"))
    summary(model_post_hoc)
    cld(model_post_hoc) # kompaktiškas raidinis žymėjimas letter display

    summary(model_post_hoc) # poriniai testai
    confint(post_hoc_modelis) # skirtumų pasikliauties intervalai

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
...ANOVA_post_hoc__gh... <- function() {
    model_gh <- posthoc_anova_games_howell(weight ~ group, data = PlantGrowth)

   model_gh

   make_cld(model_gh)
}
# ----------------------------------------------------------------------------
...KW_post_hoc__Conover_Iman... <- function() {
    library(PMCMR)
    model_conover_iman <- posthoc.kruskal.conover.test(weight ~ group, data = PlantGrowth)

    model_conover_iman

    make_cld(model_conover_iman)
}
# ----------------------------------------------------------------------------
...fun... <- function(variables) {
    library("ggpubr")
    ggboxplot(PlantGrowth,
              x = "group",
              y = "weight",
              color = "group",
              palette = c("#00AFBB", "#E7B800", "#FC4E07"),
              order = c("ctrl", "trt1", "trt2"),
              ylab = "Weight",
              xlab = "Treatment")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
...Mood_median_test... <- function(variables) {
    library(RVAideMemoire)

    model_mood <- mood.medtest(weight ~ group, data = PlantGrowth)
    model_mood

    pander::pander(model_mood)
    class(model_mood)
}

...Mood_median_test__post_hoc... <- function(variables) {

        library(rcompanion)

    model_pw_mood = pairwiseMedianTest(weight ~ group, data = PlantGrowth,
                              method = "fdr")

    pander::pander(model_pw_mood)
    BioStat::make_cld(p.adjust ~ Comparison, model_pw_mood)


}

# RVAideMemoire::bootstrap()
# rcompanion::plotNormalHistogram()