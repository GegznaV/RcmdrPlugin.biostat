# TODO:
#
# 1. Simplify code.
# 2. Use "forcats" functions where possible.
# 3. Restrict method to factors only and not variables, that are not factors but
#   are treated as factors by  R Commander (i.e., characters and logicals):
#           [3] ERROR: no applicable method for 'droplevels' applied to
#                      an object of class "character".

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_fct_lvls_reorder_manual <- function() {
    initializeDialog(title = gettext_bs("Reorder factor levels"))
    variableBox <-
        variableListBox2(
            top,
            variables_fct(),
            title = gettext_bs("Factor (pick one)"),
            listHeight = 7
        )
    orderedFrame <- tkframe(top)
    orderedVariable <- tclVar("0")
    orderedCheckBox <-
        tkcheckbutton(orderedFrame, variable = orderedVariable)
    factorName <-
        tclVar(gettext_bs("<same as original>"))
    factorNameField <-
        ttkentry(top, width = "20", textvariable = factorName)
    onOK <- function() {
        # logger(paste(
        #     "#####",
        #     gettext_bs("Reorder Factor Levels"),
        #     "#####",
        #     sep = ""
        # ))
        variable <- getSelection(variableBox)
        closeDialog()
        if (length(variable) == 0) {
            errorCondition(
                recall = window_fct_lvls_reorder_manual,
                message = gettext_bs("You must select a variable.")
            )
            return()
        }
        name <- trim.blanks(tclvalue(factorName))
        if (name == gettext_bs("<same as original>"))
            name <- variable
        if (!is.valid.name(name)) {
            errorCondition(recall = window_fct_lvls_reorder_manual,
                           message = paste(
                               '"',
                               name,
                               '" ',
                               gettext_bs("is not a valid name."),
                               sep = ""
                           ))
            return()
        }
        if (is.element(name, Variables())) {
            if ("no" == tclvalue(checkReplace(name))) {
                window_fct_lvls_reorder_manual()
                return()
            }
        }
        .activeDataSet <- ActiveDataSet()
        old.levels <-
            eval(parse(
                text = paste("levels(", .activeDataSet, "$", variable, ")",
                             sep = "")
            ), envir = .GlobalEnv)
        nvalues <- length(old.levels)
        ordered <- tclvalue(orderedVariable)
        if (nvalues > 30) {
            errorCondition(recall = window_fct_lvls_reorder_manual,
                           message = sprintf(
                               gettext_bs("Number of levels (%d) too large."),
                               nvalues
                           ))
            return()
        }
        initializeDialog(subdialog,
                         title = gettext_bs("Reorder Levels"))
        order <- 1:nvalues
        onOKsub <- function() {
            closeDialog(subdialog)
            opt <- options(warn = -1)
            for (i in 1:nvalues) {
                order[i] <-
                    as.numeric(eval(parse(
                        text = paste("tclvalue(levelOrder", i, ")", sep = "")
                    )))
            }
            options(opt)
            if (any(sort(order) != 1:nvalues) || any(is.na(order))) {
                errorCondition(recall = window_fct_lvls_reorder_manual,
                               message = paste(
                                   gettext_bs("Order of levels must include all integers from 1 to "
                                   ),
                                   nvalues,
                                   sep = ""
                               ))
                return()
            }
            levels <- old.levels[order(order)]
            ordered <- if (ordered == "1")
                ", ordered = TRUE"
            else
                ""


            levels_ok <- stringr::str_c('"',{levels},'"', collapse = ", ")

            Library("forcats")
            command <-
                glue(
                    "{.activeDataSet} <- within({.activeDataSet}, {{",
                    "{name} <- factor({variable}, levels = c({levels_ok}){ordered})",
                    "}})"
                ) %>% style_cmd()

            result <- justDoIt(command)
            logger(command)

            if (class(result)[1] !=  "try-error")
                activeDataSet(.activeDataSet, flushModel = FALSE)
        }
        subOKCancelHelp()
        tkgrid(
            labelRcmdr(
                subdialog,
                text = gettext_bs("Old Levels"),
                fg = "blue"
            ),
            labelRcmdr(
                subdialog,
                text = gettext_bs("New order"),
                fg = "blue"
            ),
            sticky = "w"
        )
        for (i in 1:nvalues) {
            valVar <- paste("levelOrder", i, sep = "")
            assign(valVar, tclVar(i))
            assign(
                paste("entry", i, sep = ""),
                ttkentry(
                    subdialog,
                    width = "2",
                    textvariable = get(valVar)
                )
            )
            #                textvariable=eval(parse(text=valVar))))
            tkgrid(labelRcmdr(subdialog, text = old.levels[i]),
                   get(paste("entry", i, sep = "")),
                   sticky = "w")
            #            tkgrid(labelRcmdr(subdialog, text=old.levels[i]), eval(parse(text=paste("entry", i, sep=""))), sticky="w")
        }
        tkgrid(subButtonsFrame,
               sticky = "w",
               columnspan = 2)
        dialogSuffix(
            subdialog,
            focus = entry1,
            rows = nvalues + 1,
            columns = 2,
            force.wait = TRUE
        )
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")
    tkgrid(bs_label(
        top,
        text = gettext_bs("Reorder factor levels"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "factor")
    tkgrid(getFrame(variableBox), sticky = "nw")
    tkgrid(labelRcmdr(top, text = gettext_bs("Name for factor")), sticky =
               "w")
    tkgrid(factorNameField, sticky = "w")
    tkgrid(labelRcmdr(
        orderedFrame,
        text = gettext_bs("Make ordered factor")
    ),
    orderedCheckBox,
    sticky = "w")
    tkgrid(orderedFrame, sticky = "w")
    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix(rows = 5,
                 columns = 1,
                 preventGrabFocus = TRUE)
}
