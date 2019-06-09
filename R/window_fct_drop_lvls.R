# TODO:
#
# 1. Simplify code.
# 2. Use "forcats" functions where possible.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_factor_lvls_drop <- function() {

    win_title <- gettext_bs("Drop Unused Factor Levels")
    initializeDialog(title = win_title)
    tk_title(top, win_title)

    .ds <- active_dataset()

    allfactorsVariable <- tclVar("0")
    allFrame <- tkframe(top)
    allfactorsCheckBox <- ttkcheckbutton(
        allFrame,
        variable = allfactorsVariable
    )

    variablesBox <-
        bs_listbox(
            parent     = top,
            values     = variables_fct(),
            title      = gettext_bs("Factors(s) to drop levels \n(pick one or more)"),
            selectmode = "multiple",
            height     = 6
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        # logger(paste(
        #     "#####",
        #     gettext_bs("Drop unused factor levels"),
        #     "#####",
        #     sep = ""
        # ))
        all <- tclvalue(allfactorsVariable)
        variables <- get_selection(variablesBox)
        closeDialog()
        if (all == 0 && length(variables) == 0) {
            errorCondition(
                recall = window_factor_lvls_drop,
                message = gettext_bs("You must select one or more variables.")
            )
            return()
        }
        response <-
            tk_messageBox(
                # parent = top,
                caption = "Drop Unused Levels",
                message = gettext_bs("Unused factor levels will be dropped.\nDo you agree?"),
                icon = "warning",
                type = "yesno",
                default = "no"
            )

        if (response != "yes") {
            onCancel()
            return()
        }

        if (all == 1)
            command <- paste0(.ds, " <- droplevels(", .ds, ")")
        else{
            command <- ""
            for (variable in variables) {
                command <-
                    paste0(command,.ds,"$",variable," <- droplevels(",.ds,"$",variable,")\n"
                    )
            }
        }
        doItAndPrint(command)
        active_dataset(.ds,
                       flushModel = FALSE,
                       flushDialogMemory = FALSE)
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(helpSubject = "droplevels")

    tkgrid(variablesBox$frame, sticky = "nw")

    tkgrid(allfactorsCheckBox,
           labelRcmdr(
               allFrame,
               text = gettext_bs("All factor variables")
           ),
           sticky = "w", pady = c(2, 0))
    tkgrid(allFrame, sticky = "ew")

    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix()
}
