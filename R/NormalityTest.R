NormalityTest <- function () {
    nrows <- getRcmdr("nrow")
    defaults <- list (initial.var = NULL, initial.test=if (nrows <= 5000) "shapiro.test" else "ad.test",
                      initial.bins = gettextRcmdr("<auto>"), initial.groups=NULL)
    dialog.values <- getDialog ("NormalityTest", defaults)
    initializeDialog(title = gettextRcmdr("Test of Normality"))
    variableBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"),
                                   initialSelection = varPosn (dialog.values$initial.var, "numeric"))
    optionsFrame <- tkframe(top)
    radioButtons(optionsFrame, name = "test",
                 buttons = c(if (nrows <= 5000) "shapiro.test", "ad.test", "cvm.test", "lillie.test",
                             if (nrows <= 5000) "sf.test", "pearson.test"),
                 labels = c(if (nrows <= 5000) gettextRcmdr("Shapiro-Wilk"),
                            gettextRcmdr("Anderson-Darling"),
                            gettextRcmdr("Cramer-von Mises"),
                            gettextRcmdr("Lilliefors (Kolmogorov-Smirnov)"),
                            if (nrows <= 5000) gettextRcmdr("Shapiro-Francia"),
                            gettextRcmdr("Pearson chi-square")),
                 title = gettextRcmdr("Normality Test"),
                 initialValue = dialog.values$initial.test)
    binsFrame <- tkframe(optionsFrame)
    binsVariable <- tclVar(dialog.values$initial.bins)
    binsField <- ttkentry(binsFrame, width = "8", textvariable = binsVariable)
    groupsBox(recall=NormalityTest, label=gettextRcmdr("Test by:"),
              initialLabel=if (is.null(dialog.values$initial.group)) gettextRcmdr("Test by groups")
              else paste(gettextRcmdr("Test by:"), dialog.values$initial.group),
              initialGroup=dialog.values$initial.group)
    onOK <- function() {
        var <- getSelection(variableBox)
        test <- tclvalue(testVariable)
        bins <- tclvalue(binsVariable)
        warn <- options(warn = -1)
        nbins <- as.numeric(bins)
        options(warn)
        if (bins != gettextRcmdr("<auto>") && (is.na(nbins) || nbins < 4)) {
            errorCondition(recall = NormalityTest, message = gettextRcmdr("Number of bins must be a number >= 4"))
            return()
        }
        n.classes <- if (test != "pearson.test" || bins == gettextRcmdr ("<auto>")) "" else paste0(", n.classes=", bins)
        putDialog ("NormalityTest", list (initial.var = var, initial.test = test, initial.bins=bins,
                                          initial.groups=if (.groups == FALSE) NULL else .groups))
        if (length(var) == 0) {
            errorCondition(recall = NormalityTest, message = gettextRcmdr("You must select a variable."))
            return()
        }
        closeDialog()
        if (.groups == FALSE){
            command <- paste0("normalityTest(~", var, ', test="', test, '", data=', ActiveDataSet(), n.classes, ")")
        }
        else{
            command <- paste0("normalityTest(", var, " ~ ", .groups, ', test="', test, '", data=', ActiveDataSet(), n.classes,  ")")
        }
        doItAndPrint(command)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "normalityTest", reset = "NormalityTest", apply = "NormalityTest")
    tkgrid(getFrame(variableBox), sticky = "nw")
    tkgrid(labelRcmdr(binsFrame, text=gettextRcmdr("Number of bins\nfor Pearson chi-square")),
           binsField, padx=3, sticky="sw")
    tkgrid(testFrame, binsFrame, sticky="sw")
    tkgrid(optionsFrame, sticky="sw")
    tkgrid(groupsFrame, sticky = "w", padx=6)
    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix()
}
