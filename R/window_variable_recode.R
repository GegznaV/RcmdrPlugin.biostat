# TODO:
#
# 1. Add popup menu for right-click, that enables quick change of row position, deletion, etc.
#
# [+] DONE:
#
# 1. Add interactivity for double click.
#    a.  It should add all unique values to recodes window as
#        "old_val_1" = ""
#        "old_val_2" = ""
#        "old_val_3" = ""
#        ...
#
#    b. Change variable name to the name of selected variable +
#       tkfocus to variable name window.
#
# 2. Add menu for arguments .missing and .default.
#        # Add options if explicit level is needed:
#        .default = NULL
#        .missing = NULL
#
# 3. Add examples in the free area of the window.
#
# 4. Change new variable name and add _1, _2, _3, etc. if variable with that
#    name exists in active dataframe
#


# right-click context menus
right_click_menu <- function(tcl_widget) {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onCopy <- function() {
        # focused <- tkfocus()
        selection <- strsplit(tclvalue(tktag.ranges(tcl_widget, "sel")), " ")[[1]]
        if (is.na(selection[1])) return()
        text <- tclvalue(tkget(tcl_widget, selection[1], selection[2]))
        tkclipboard.clear()
        tkclipboard.append(text)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onDelete <- function(){
        # focused <- tkfocus()
        selection <- strsplit(tclvalue(tktag.ranges(tcl_widget, "sel")), " ")[[1]]
        if (is.na(selection[1])) return()
        tkdelete(tcl_widget, selection[1], selection[2])
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onCut <- function(){
        onCopy()
        onDelete()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onPaste <- function(){
        onDelete()
        # focused <- tkfocus()
        text <- tclvalue(.Tcl("selection get -selection CLIPBOARD"))
        if (length(text) == 0) return()
        tkinsert(tcl_widget, "insert", text)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onFind <- function() {
        # focused <- tkfocus()
        focused <- tcl_widget

        initializeDialog(title=gettextRcmdr("Find"))
        textFrame <- tkframe(top)
        textVar <- tclVar(getRcmdr("last.search"))
        textEntry <- ttkentry(textFrame, width="20", textvariable=textVar)
        checkBoxes(frame="optionsFrame", boxes=c("regexpr", "case"), initialValues=c("0", "1"),
                   labels=gettextRcmdr(c("Regular-expression search", "Case sensitive")))
        radioButtons(name="direction", buttons=c("foward", "backward"), labels=gettextRcmdr(c("Forward", "Backward")),
                     values=c("-forward", "-backward"), title=gettextRcmdr("Search Direction"))
        onOK <- function() {
            text <- tclvalue(textVar)
            putRcmdr("last.search", text)
            if (text == ""){
                errorCondition(recall=onFind, message=gettextRcmdr("No search text specified."))
                return()
            }
            type <- if (tclvalue(regexprVariable) == 1) "-regexp" else "-exact"
            case <- tclvalue(caseVariable) == 1
            direction <- tclvalue(directionVariable)
            stop <- if (direction == "-forward") "end" else "1.0"
            where.txt <- if (case) tksearch(focused, type, direction, "--", text, "insert", stop)
            else tksearch(focused, type, direction, "-nocase", "--", text, "insert", stop)
            where.txt <- tclvalue(where.txt)
            if (where.txt == "") {
                Message(message=gettextRcmdr("Text not found."),
                        type="note")
                if (GrabFocus()) tkgrab.release(top)
                tkdestroy(top)
                tkfocus(CommanderWindow())
                return()
            }
            if (GrabFocus()) tkgrab.release(top)
            tkfocus(focused)
            tkmark.set(focused, "insert", where.txt)
            tksee(focused, where.txt)
            tkdestroy(top)
        }
        .exit <- function(){
            text <- tclvalue(textVar)
            putRcmdr("last.search", text)
            return("")
        }
        OKCancelHelp()
        tkgrid(labelRcmdr(textFrame, text=gettextRcmdr("Search for:")), textEntry, sticky="w")
        tkgrid(textFrame, sticky="w")
        tkgrid(optionsFrame, sticky="w")
        tkgrid(directionFrame, sticky="w")
        tkgrid(buttonsFrame, sticky="w")
        dialogSuffix(focus=textEntry)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onSelectAll <- function() {
        # focused <- tkfocus()
        tktag.add(tcl_widget, "sel", "1.0", "end")
        tkfocus(tcl_widget)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onSelectRow <- function() {
        # focused <- tkfocus()
        tktag.add(tcl_widget, "sel", "current linestart", "current lineend + 1 chars")
        tkfocus(tcl_widget)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onDeleteRow <- function(){
        onSelectRow()
        onDelete()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onClear <- function(){
        onSelectAll()
        onDelete()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onUndo <- function(){
        # focused <- tkfocus()
        tcl(tcl_widget, "edit", "undo")
    }
    #
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onRedo <- function(){
        # focused <- tkfocus()
        tcl(tcl_widget, "edit", "redo")
    }
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



    # ========================================================================
    crete_context_menu <- function() {

        # if (tclvalue(tkfocus()) != tcl_widget$ID) return()

        contextMenu <- tkmenu(tkmenu(tcl_widget), tearoff = FALSE)

        # tkadd(contextMenu, "command", label = gettextRcmdr("Submit"), command = onSubmit)

        # tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label = gettextRcmdr("Cut"),        command = onCut)
        tkadd(contextMenu, "command", label = gettextRcmdr("Copy"),       command = onCopy)
        tkadd(contextMenu, "command", label = gettextRcmdr("Paste"),      command = onPaste)
        tkadd(contextMenu, "command", label = gettextRcmdr("Delete"),     command = onDelete)

        tkadd(contextMenu, "separator")
        # tkadd(contextMenu, "command", label = gettextRcmdr("Find..."),    command = onFind)
        # tkadd(contextMenu, "command", label = gettextRcmdr("Select all"), command = onSelectAll)
        tkadd(contextMenu, "command", label = gettextRcmdr("Select row"), command = onSelectRow)
        tkadd(contextMenu, "command", label = gettextRcmdr("Delete row"), command = onDeleteRow)

        # tkadd(contextMenu, "separator")
        # tkadd(contextMenu, "command", label = gettextRcmdr("Undo"),       command = onUndo)
        # tkadd(contextMenu, "command", label = gettextRcmdr("Redo"),       command = onRedo)

        # tkadd(contextMenu, "separator")
        # tkadd(contextMenu, "command", label = gettextRcmdr("Clear directives"), command = onClear)
        # tkadd(contextMenu, "command", label = gettextRcmdr("Reset directives"), command = variable_doubleclick)

        tkpopup(contextMenu, tkwinfo("pointerx", tcl_widget), tkwinfo("pointery", tcl_widget))
    }


    tkbind(tcl_widget, "<ButtonPress-3>", crete_context_menu)

}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper functions

recode_values_template <- function(x) {

    if (is.character(x)) {
        x <- forcats::as_factor(x)
    }

    unique_values <-
        if (is.factor(x)) {
            levels(x)

        } else {
            sort(unique(x))
        }

    rez <-
        glue::glue('"{unique_values}" = ""') %>%
        paste(collapse = "\n") %>%
        paste('\n',
              '\n.default = ""',
              '\n.missing = ""')
    rez
    # writeLines(rez)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_variable_recode0 <- function() {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_Bio("Recode Variable Values"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <-
        list(
            # initial_make_factor     = 1,
            initial_recode_into       = "nominal",
            initial_variables         = NULL,
            initial_name              = "recoded_variable",
            initial_recode_directives = "",
            initial_selected_variable = "{none}"
        )

    dialog_values <- getDialog("window_variable_recode0", defaults)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    selected_var_frame <- tkframe(top)
    selected_variable  <- tclVar(dialog_values$initial_selected_variable)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    variable_doubleclick <- function() {
        active_variable  <- getSelection(variablesBox)
        var_val          <- eval_glue("{activeDataSet()}${active_variable}")

        tclvalue(selected_variable) <- active_variable

        # Change contents of "recodes" box
        tkdelete(recodes, "1.0", "end")
        tkinsert(recodes, "1.0", recode_values_template(var_val))

        # Change new variable name
        tclvalue(newVariableName) <-
            unique_colnames(active_variable, suffix = "_recoded")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)

    variablesBox <-
        variableListBox2(
            listHeight = 7,
            upper_frame,
            Variables(),
            onDoubleClick_fun = variable_doubleclick,
            # selectmode = "multiple",
            title = gettext_Bio("Variable to recode \n(double-click to pick one)"),
            initialSelection = varPosn(dialog_values$initial_variables, "all")
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    recodesFrame <- tkframe(upper_frame)
    recodes <-
        tktext(
            recodesFrame,
            bg     = "white",
            font   = getRcmdr("logFont"),
            height = "7",
            width  = "40",
            wrap   = "none",
            undo   = TRUE)

    right_click_menu(recodes)

    recodesXscroll <-
        ttkscrollbar(
            recodesFrame,
            orient  = "horizontal",
            command = function(...) tkxview(recodes, ...)
        )
    recodesYscroll <-
        ttkscrollbar(
            recodesFrame,
            command = function(...) tkyview(recodes, ...)
        )
    tkconfigure(
        recodes,
        xscrollcommand = function(...) tkset(recodesXscroll, ...)
    )
    tkconfigure(
        recodes,
        yscrollcommand = function(...) tkset(recodesYscroll, ...)
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkinsert(recodes, "1.0", dialog_values$initial_recode_directives)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    lower_frame <- tkframe(top)
    lower_options_frame <- tkframe(lower_frame)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    variablesFrame  <- tkframe(lower_options_frame)
    newVariableName <- tclVar(dialog_values$initial_name)
    newVariable <-
        ttkentry(variablesFrame,
                 width = "23",
                 textvariable = newVariableName)
    # ------------------------------------------------------------------------
    variable_type_frame <- tkframe(lower_options_frame)

    # factor_checkbox_frame <- tkframe(variable_type_frame)

    # make_factorVariable   <- tclVar(dialog_values$initial_make_factor)
    # make_factorCheckBox   <- ttkcheckbutton(factor_checkbox_frame,
    #                                         variable = make_factorVariable)

    Rcmdr::radioButtons(variable_type_frame,
                            name          = "recode_into",
                            # title       = gettext_Bio("Use functions: "),
                            # title.color = getRcmdr("title.color"),
                            buttons       = c("nominal", "ordinal", "other"),
                            values        = c("nominal", "ordinal", "other"),
                            initialValue  = dialog_values$initial_recode_into,
                            labels        = gettext_Bio(c("Nominal factor",
                                                          "Ordinal factor",
                                                          "Do not convert")))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        # make_factor <- tclvalue(make_factorVariable) == "1"
        # make_factor <- tclvalue_lgl(make_factorVariable)
        recode_into       <- tclvalue(recode_intoVariable)
        variables         <- getSelection(variablesBox)
        selected_variable <- tclvalue(selected_variable)
        name              <- trim.blanks(tclvalue(newVariableName))

        # Read recode directives
        save_recodes <- trim.blanks(tclvalue(tkget(recodes, "1.0", "end")))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Format code into one row
        recode_directives <-
            stringr::str_replace_all(save_recodes,
                                     # Remove intact rows
                                     c('".*" = ""' = "",
                                       '\\.default = (NULL|"")' = "",
                                       '\\.missing = (NULL|"")' = "",
                                       # Remove leading commas and spaces
                                       "[, ]+(\n)"    = "\\1",
                                       # Remove emplty lines
                                       "(\n){2,}"  = "\n",
                                       # Remove tailing new line
                                       "\n$"       = "",
                                       # Remove leading spaces in each non-first row
                                       "\n( )*"    = ", ",
                                       # Remove leading spaces and commas in
                                       # the first row
                                       "^[, ]*"    = "",
                                       # Remove tailing commas and spaces
                                       "[, ]*$"    = "")
                   )
        # str_detect(recode_directives,
        #            "[\\p{Alphabetic}\\p{Mark}\\p{Decimal_Number}]")

        # Is empty? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (recode_directives == "") {
            errorCondition(
                recall  = window_variable_recode0,
                message = gettext_Bio("No recode directives specified.")
            )
            return()
        }


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # variables
        # If no variable is selected
        if (length(variables) == 0 || selected_variable == "{none}") {
            errorCondition(
                recall  = window_variable_recode0,
                message = gettext_Bio("You must select a variable.")
            )
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # name

        # check variable name for validity
        if (!is.valid.name(name)) {
            errorCondition(
                recall  = window_variable_recode0,
                message = glue::glue('"{name}"',
                                     gettext_Bio("is not a valid name."))
            )
            return()
        }

        if (name %in% Variables()) {
            if ("no" == tclvalue(checkReplace(name))) {
                window_variable_recode0()
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_variable_recode0",
                  list(
                      # initial_make_factor       = make_factor,
                      initial_variables         = variables,
                      initial_name              = name,
                      initial_recode_directives = save_recodes,
                      initial_recode_into       = recode_into,
                      initial_selected_variable = selected_variable
                  )
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        switch(recode_into,
               "nominal" = {
                   recode_fun     <- "dplyr::recode_factor"
                   ordered_factor <- ""
               },
               "ordinal" = {
                   recode_fun     <- "dplyr::recode_factor"
                   ordered_factor <- ", .ordered = TRUE" # .ordered = FALSE ???
               },
               "other" = {
                   recode_fun     <- "dplyr::recode"
                   ordered_factor <- ""
               })

        dataSet <- activeDataSet()

        command <- glue::glue(
            "## ", gettext_Bio("Recode variable values"), "\n\n",
            "{dataSet} <- {dataSet} %>% \n",
            "   dplyr::mutate({name} = {recode_fun}({selected_variable}, ",
            " {recode_directives}{ordered_factor}))"
        ) %>%
            style_cmd()

        result <- doItAndPrint(command)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error")
            activeDataSet(dataSet,
                          flushModel = FALSE,
                          flushDialogMemory = FALSE)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")
    tkgrid(label_rcmdr(
        top,
        text = gettextRcmdr("Recode variable values"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(selected_var_frame) # selected_variable
    tkgrid(
        label_rcmdr(selected_var_frame, text = "Selected variable: "),
        label_rcmdr(selected_var_frame,
                    textvariable = selected_variable,
                    font = tkfont.create(weight = "bold", size = 8),
                    fg = "darkred"),
        pady = c(0, 9)
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     OKCancelHelp(helpSubject = "recode_factor", helpPackage = "dplyr",
                 reset = "window_variable_recode0",
                 apply = "window_variable_recode0")

    tkgrid(upper_frame, sticky = "nw")
    tkgrid(getFrame(variablesBox),
           label_rcmdr(upper_frame, text = "  "),
           recodesFrame, sticky = "nw")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # tkgrid()

    tkgrid(label_rcmdr(
        variablesFrame,
        fg = getRcmdr("title.color"),
        text = gettext_Bio("Name for recoded variable: ")),
        sticky = "w",
        pady = c(2, 0))
    tkgrid(newVariable, sticky = "w")

    tkgrid(
        label_rcmdr(variable_type_frame,
                   text = gettext_Bio("Convert variable into: "),
                   fg = getRcmdr("title.color")),
        sticky = "w",
        pady = c(10, 0)
    )

    tkgrid(recode_intoFrame, sticky = "w")

    tkgrid(
        label_rcmdr(
            recodesFrame,
            text = gettext_Bio(
                "Enter recode directives\n(one directive per row; change order of rows, if needed)"),
                # "Enter recode directives\n(one directive per row or comma separated)"),
            fg = getRcmdr("title.color"),
            font = "RcmdrTitleFont"
        ),
        sticky = "w"
    )

    tkgrid(recodes, recodesYscroll, sticky = "nw")
    tkgrid(recodesXscroll)

    tkgrid(lower_frame,         sticky = "w")
    tkgrid(variablesFrame,      sticky = "w")
    tkgrid(variable_type_frame, sticky = "w")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    examples_frame <- tkframe(lower_frame)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid_text <- function(text = "", frame = examples_frame, fg = "black",
                            sticky = "w", padx = 10, pady = 0, ...) {
        tkgrid(label_rcmdr(frame, text = gettext_Bio(text), fg = fg),
               sticky = sticky, padx = padx, pady = pady, ...)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid_text("Examples of recode directives:", fg = fg_col, pady = c(2, 0))
    tkgrid_text(paste(
        '      Example 1:       "old value 1" = "new value 1"\n',
                           '\t\t"old value 3" = "new value 3"\n',
                           '\t\t"old value 2" = "new value 2"\n'))

    # tkgrid_text('      Example 2:       "old 1" = "new 1", "old 2" = "new 2"\n')
    tkgrid_text('      .default - a default value for all non-specified values.')
    tkgrid_text('      .missing - a new explicit value for missing (NA) values.')
    # tkgrid_text('      Unmodified recode directives will be removed automatically.')

    # if (!is.null(incorrect_expr_msg)) {
    #     tkgrid_text(incorrect_expr_msg, fg = "darkred", sticky = "e",
    #                 pady = c(5, 0))
    # }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkgrid(lower_options_frame, examples_frame,
           sticky = "nw",
           columnspan = 2)
    # ==========================================================================


    tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
    tkgrid.configure(recodesXscroll, sticky = "ew")
    tkgrid.configure(recodesYscroll, sticky = "ns")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dialogSuffix(bindReturn = FALSE)
}
