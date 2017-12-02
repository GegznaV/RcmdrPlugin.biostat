#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_tmp <- function() {

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

    dialog.values <- getDialog("window_tmp", defaults)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Dialog elements --------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tabs = c("dataTab", "mainTab", "posthocTab", "outputTab", "plotsTab")
    tab_names = c(" Data ", " Main test ", " Post-hoc analysis ", "Numeric output", "Plots ")

    initializeDialog(title = gettextRcmdr("Compare centers of independent samples"),
                     use.tabs = TRUE, tabs = tabs)

    # posthocFrame <- tkframe(posthocTab)
    # plotsFrame <- tkframe(plotsTab)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ** Main tab ------------------------------------------------------------
    # . Main test & model name textbox ---------------------------------------

    main_top_frame <- tkframe(mainTab)

    labelText <- tclVar("Select the test") ### [!!!] Initial value

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
                     if ("posthoc_test_panel"  %in% names(parent.frame()))
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
           pady = c(5, 0), sticky = "nw")

    tkgrid(text_alpha, sticky = "nw")
    tkgrid(main_testFrame,model_boxlFrame,pady = c(0, 5),padx = c(10, 5),sticky = "nw")
    tkgrid(main_top_frame, sticky = "w")

    # tkgrid(tk2label(mainTab, text = paste("Current test: ", test_name)),
    tkgrid(tk2label(mainTab, textvariable = labelText),
           pady = c(20, 0),
           sticky = "w")

    # ** Post-hoc tab -----------------------------------------------------------
    create_posthoc_tab <- function(env = parent.frame()) {
        posthoc_test_panel <- labeled_frame(posthocTab, "Post-hoc test options")
        tmpFRAME <- tkframe(posthoc_test_panel)

        radioButtons(window = tmpFRAME,
                     name = "posthoc_test",
                     buttons = tclvalue_chr(ph_buttons),
                     values  = tclvalue_chr(ph_values),

                     labels = gettext_Bio(u2s(tclvalue_chr(ph_labels))),
                     title  = gettext_Bio("Post-hoc test")
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        pval_adjustment <- inputComboBox(
            posthoc_test_panel,
            variableList = p.adjust.methods,
            initialSelection = p.adjust.methods[1],
            title = gettext_Bio("P value adjustment method")
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkgrid(tmpFRAME, row = 20)
        tkgrid(posthoc_testFrame,         pady = c(0, 5), padx = c(5, 5), sticky = "nw")
        tkgrid(getFrame(pval_adjustment), pady = c(5, 5), padx = c(5, 5), sticky = "nw")
        tkgrid(posthoc_test_panel, sticky = "nw")

        assign("posthoc_test_panel", posthoc_test_panel, envir = env)
    }
    update_posthoc_choices()
    create_posthoc_tab(env = cur_env)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        closeDialog()
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

