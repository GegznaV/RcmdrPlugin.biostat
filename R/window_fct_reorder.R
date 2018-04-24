# TODO:
# 1. The window is not functional. Fix it.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_fct_reorder <- function() {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initial_new_name_variable <- tclVar(gettextRcmdr("New variable name goes here..."))
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_onClick_name <- function() {
        switch(tclvalue(which_nameVariable),
               "overwrite"  = tk_disable(new_name_field,
                                         textvariable = initial_new_name_variable),
               "create_new" = {

                   selected_var <- getSelection(variableBox)

                   init_name <-
                       if (length(selected_var) == 0) {
                           "new_variable"
                       } else {
                           str_c(selected_var, "_reordered")
                       }

                   tk_activate(new_name_field,
                               textvariable = tclVar(init_name))
                   })
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_onRelease_var_box <- function() {
        switch(tclvalue(which_nameVariable),
               "create_new" = {
                   value <- str_c(getSelection(variableBox), "_reordered")
                   tkconfigure(new_name_field, textvariable = tclVar(value))
                   })
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettextRcmdr("Reorder Factor Levels"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    variableBox <- variableListBox2(top,
                                    variableList = Factors(),
                                    onRelease_fun = cmd_onRelease_var_box,
                                    listHeight = 5,
                                    title = gettextRcmdr("Factor (pick one)"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    new_name_frame <- tkframe(top)

    Rcmdr::radioButtons(
        new_name_frame,
        "which_name",
        labels = gettextRcmdr(c(
            "Update selected variable", #  (overwrites)
            "Create a new variable:")),
        buttons = c("overwrite", "create_new"),
        command = cmd_onClick_name
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    new_name_variable <- initial_new_name_variable

    new_name_field <- ttkentry(new_name_frame,
                                width = "28",
                                textvariable = new_name_variable,
                                state = "disabled")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        variable             <- getSelection(variableBox)
        choose_new_name_type <- tclvalue(which_nameVariable)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(variable) == 0) {
            errorCondition(recall = window_fct_reorder,
                           message = gettextRcmdr("You must select a variable."))
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        switch(
            choose_new_name_type,
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            "overwrite" = {
                new_name <- variable
            },
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            "create_new" =  {
                new_name <- trim.blanks(tclvalue(new_name_variable))
                # Validate name
                if (!is.valid.name(new_name)) {
                    errorCondition(
                        recall = window_fct_reorder,
                        message = glue('"{new_name}" ',
                                       gettextRcmdr("is not a valid name."))
                    )
                    return()
                }
                # checkReplace
                if (is.element(new_name, Variables())) {
                    if ("no" == tclvalue(checkReplace(new_name))) {
                        window_fct_reorder()
                        return()
                    }

                }
            }
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        .activeDataSet <- ActiveDataSet()

        old_levels <- eval_glue("levels({.activeDataSet}${variable})",
                                envir = .GlobalEnv)

        nvalues <- length(old_levels)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (nvalues > 30) {
            errorCondition(recall = window_fct_reorder,
                           message = sprintf(
                               gettextRcmdr("Number of levels (%d) is too large.\nMust be no more than 30."),
                               nvalues
                           ))
            return()
        }
        # ====================================================================
        initializeDialog(subdialog, title = gettextRcmdr("Reorder Levels"))

        new_order <- rep_len(NA_integer_, nvalues)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        onOKsub <- function() {
            closeDialog(subdialog)
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            opt <- options(warn = -1)
            for (i in 1:nvalues) {
                new_order[i] <- eval_glue("tclvalue_int(levelOrder{i})")
            }
            options(opt)

            if (any(sort(new_order) != 1:nvalues) || any(is.na(new_order))) {
                errorCondition(
                    recall = window_fct_reorder,
                    message = paste0(
                        gettextRcmdr(
                            "Order of levels must include all integers from 1 to "
                        ), nvalues
                    )
                )
                return()
            }
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            reordered_levels <-
                paste(glue('"{old_levels[order(new_order)]}"'), collapse = ", ")


            Library("tidyverse")
            command <- style_cmd(glue(
                '## --- Change order of factor levels --- \n\n',

                '{.activeDataSet} <- {.activeDataSet} %>% dplyr::mutate(\n',
                '{new_name} = fct_relevel({variable}, {reordered_levels})\n',
                ')'
            ))

            result <- doItAndPrint(command)

            if (class(result)[1] !=  "try-error")
                activeDataSet(.activeDataSet,
                              flushModel = FALSE,
                              flushDialogMemory = FALSE)
             } ## END: onOKsub
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            subOKCancelHelp()
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            tkgrid(
                labelRcmdr(
                    subdialog,
                    text = gettextRcmdr("Old Levels"),
                    fg = getRcmdr("title.color"),
                    font = "RcmdrTitleFont"
                ),
                labelRcmdr(
                    subdialog,
                    text = gettextRcmdr("New order"),
                    fg = getRcmdr("title.color"),
                    font = "RcmdrTitleFont"
                ),
                sticky = "w"
            )

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        for (i in 1:nvalues) {
            valVar <- glue("levelOrder{i}")
            assign(valVar, tclVar(i))
            assign(glue("entry{i}"), ttkentry(subdialog,
                                              width = "2",
                                              textvariable = get(valVar))
            )
            tkgrid(labelRcmdr(subdialog, text = old_levels[i]),
                   get(glue("entry{i}")),
                   sticky = "w")
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkgrid(subButtonsFrame, sticky = "w", columnspan = 2)
        dialogSuffix(subdialog, focus = entry1, force.wait = TRUE)


    } #END: onOK

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "fct_relevel")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(getFrame(variableBox), new_name_frame,  sticky = "nsw")

    tkgrid(labelRcmdr(new_name_frame,
                      fg = getRcmdr("title.color"),
                      text = gettextRcmdr("Method")), # New factor with reordered levels:
           sticky = "nsw",
           pady = c(0, 5))

    tkgrid(which_nameFrame)
    tkgrid(new_name_field, sticky = "ws", pady = c(15, 0))

    # tkgrid(same_name_check_box,
    #        labelRcmdr(new_name_frame, text = gettextRcmdr("Same as original")),
    #        sticky = "w")
    # tkgrid(new_name_frame, sticky = "w")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "ew", columnspan = 2)
    dialogSuffix(preventGrabFocus = TRUE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
