# TODO:
#
# 1. Simplify code.
# 2. Use "forcats" functions where possible.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_factor_lvls_drop <- function() {
    dataSet <- activeDataSet()
    initializeDialog(title = gettext_Bio("Drop Unused Factor Levels"))
    allfactorsVariable <- tclVar("0")
    allFrame <- tkframe(top)
    allfactorsCheckBox <-
        ttkcheckbutton(allFrame, variable = allfactorsVariable)
    variablesBox <- variableListBox2(
        top,
        Factors(),
        title = gettext_Bio("Factors(s) to drop levels \n(pick one or more)"),
        selectmode = "multiple",
        initialSelection = NULL,
        listHeight = 6
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        # logger(paste(
        #     "#####",
        #     gettext_Bio("Drop unused factor levels"),
        #     "#####",
        #     sep = ""
        # ))
        all <- tclvalue(allfactorsVariable)
        variables <- getSelection(variablesBox)
        closeDialog()
        if (all == 0 && length(variables) == 0) {
            errorCondition(
                recall = window_factor_lvls_drop,
                message = gettext_Bio("You must select one or more variables.")
            )
            return()
        }
        response <-
            tclvalue(
                RcmdrTkmessageBox(
                    message = gettext_Bio("Drop unused factor levels\nPlease confirm."),
                    icon = "warning",
                    type = "okcancel",
                    default = "cancel"
                )
            )
        if (response == "cancel") {
            onCancel()
            return()
        }
        if (all == 1)
            command <- paste0(dataSet, " <- droplevels(", dataSet, ")")
        else{
            command <- ""
            for (variable in variables) {
                command <-
                    paste0(command,dataSet,"$",variable," <- droplevels(",dataSet,"$",variable,")\n"
                    )
            }
        }
        doItAndPrint(command)
        activeDataSet(dataSet,
                      flushModel = FALSE,
                      flushDialogMemory = FALSE)
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")
    tkgrid(label_rcmdr(
        top,
        text = gettextRcmdr("Drop unused factor levels"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "droplevels")
    tkgrid(allfactorsCheckBox,
           labelRcmdr(
               allFrame,
               text = gettext_Bio("all factors")
           ),
           sticky = "w")
    tkgrid(allFrame, sticky = "w")
    tkgrid(labelRcmdr(
        top,
        text = gettext_Bio("OR"),
        fg = "red"
    ), sticky = "w")
    tkgrid(getFrame(variablesBox), sticky = "nw")
    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix()
}
