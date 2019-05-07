# TODO:
#
# 1. When push "Apply" button and an error occurs, two windows open.
#    "Apply" is now disabled and this should be fixed.
#
# 2. In variable box text "[factor]" should be differentiated
#    to "[character]", "[logical]", "[factor]"
#
# 3. Add buttons "+", "-", "*", etc. in style as used in "fit linear model" window


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Correctly initializes window `window_variable_mutate()`
window_variable_mutate0  <- function(variables) {
    window_variable_mutate()
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# var_name (character) - variable name
# expr (character) - expression to be computed
# incorrect_expr_msg (character) - Message for incorrect expression.
window_variable_mutate <- function(var_name = NULL,
                                   init_express = NULL,
                                   incorrect_expr_msg = NULL) {

    # Functions --------------------------------------------------------------
    onDoubleClick_variable <- function() {
        var <- trim.blanks(getSelection(variablesBox))

        word <- str_glue('\\[{gettext_bs("factor")}\\]')

        if (length(grep(word, var)) == 1)
            var <- trim.blanks(sub(word, "", var))
        tkfocus(compute)
        expr <- trim.blanks(tclvalue(computeVar))
        tclvalue(computeVar) <-
            if (expr == "") {
                var
            } else {
                last_chr <- stringr::str_sub(expr, -1)
                expr_sep <- if (last_chr %in% c("(", "[")) "" else " "
                paste(expr, var, sep = expr_sep)
            }
        tkicursor(compute, "end")
        tkxview.moveto(compute, "1")
    }

    # Dialog -----------------------------------------------------------------
    .ds <- active_dataset()

    initializeDialog(
        title = gettext_bs("Mutate: create new or replace existing variable"))

    .variables <- Variables()
    variables <-
        paste(.variables,
              ifelse(.variables %in% Factors(),
                     yes = gettext_bs("[factor]"),
                     no  = ""
              ))

    var_box_Frame <- tkframe(top)

    variablesBox <-
        variableListBox2(
            var_box_Frame,
            variables,
            title = gettext_bs("Current variables \n(double-click to add to expression)"),
            listHeight = 8,
            onDoubleClick_fun = onDoubleClick_variable
        )

    # tkbind(variablesBox$listbox, "<Double-ButtonPress-1>", onDoubleClick_variable)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    computeFrame <- tkframe(top)

    if (is.null(var_name))
        var_name <- unique_colnames_2(gettext_bs("new_variable"))

    newVariableName <- tclVar(var_name)

    newVariable <-
        ttkentry(computeFrame,
                 width = "20",
                 textvariable = newVariableName)

    expression_Frame <- tkframe(computeFrame)

    if (is.null(init_express)) init_express <- ""

    computeVar <- tclVar(init_express)
    compute <-
        ttkentry(
            expression_Frame,
            font = getRcmdr("logFont"),
            width = "50",
            textvariable = computeVar
        )
    computeXscroll <- ttkscrollbar(
        expression_Frame,
        orient = "horizontal",
        command = function(...)
            tkxview(compute, ...)
    )
    tkconfigure(
        compute,
        xscrollcommand = function(...)
            tkset(computeXscroll, ...)
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        new_name  <- trim.blanks(tclvalue(newVariableName))
        express <- tclvalue(computeVar)



        # Check if expression is not empty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        check_empty <- gsub(";", "", gsub(" ", "", express))

        if (check_empty == "") {
            show_error_messages(
                gettext_bs("No expression was specified!"),
                # "No ???  was selected.\nPlease select a ???.",
                title = "Expression Is Missing",
                parent = top)
            return()
        }

        # Add linting for the expression [???]
        # Message box should contain
        if (is_try_error(try_command(check_empty))) {
            show_error_messages(
                str_c(
                    "The expression is incomplete or contains error(s)!\n",
                    "Please, correct the expression."),
                title = "Invalid Expression",
                parent = top)
            return()
        }

        # Check validity of var name ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (is_empty_name(new_name, parent = top)) {
            return()
        }

        if (is_not_valid_name(new_name, parent = top)) {
            return()
        }

        if (forbid_to_replace_variables(new_name, parent = top)) {
            return()
        }

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        command <- str_glue(
            "## Compute a variable: {new_name}\n",
            "{.ds} <- \n ",
            "   {.ds} %>% \n",
            "   mutate({new_name} = {express})")

        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        Library("tidyverse")

        result <- justDoIt(command)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            logger(style_cmd(command))
            # doItAndPrint(style_cmd(command))

            active_dataset(.ds, flushModel = FALSE, flushDialogMemory = FALSE)

            # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # closeDialog()


        } else {
            logger_error(command, error_msg = result)
            show_code_evaluation_error_message(parent = top)
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command_dataset_refresh()
        tkfocus(CommanderWindow())
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(
        close_on_ok = TRUE,
        helpSubject = "mutate", helpPackage = "dplyr",
        reset = "window_variable_mutate",
        apply = "window_variable_mutate"
    )

    tkgrid(var_box_Frame, sticky = "nw")

    examples_Frame <- tkframe(var_box_Frame)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid_text <- function(text = "", frame = examples_Frame, fg = "black",
                            sticky = "w", padx = 20, pady = 0, ...) {
        tkgrid(labelRcmdr(frame, text = gettext_bs(text), fg = fg),
               sticky = sticky, padx = padx, pady = pady, ...)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid_text("\nExamples of expressions", fg = getRcmdr("title.color"))
    tkgrid_text('Polular operations:   +   -   *   /   ^   sqrt()    log()   rank()', fg = "darkgreen")

    tkgrid_text("Example 1: log(age)")
    tkgrid_text("Example 2: a + b")
    tkgrid_text("Example 3: as.factor(color)")
    tkgrid_text("Example 4: weight / (height^2)")
    tkgrid_text('Example 5: ifelse(age < 50, "young", "old")')

    if (!is.null(incorrect_expr_msg)) {
        tkgrid_text(incorrect_expr_msg, fg = "darkred", sticky = "e",
                    pady = c(5, 0))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkgrid(getFrame(variablesBox), examples_Frame,
           sticky = "nw",
           columnspan = 2)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(
        labelRcmdr(computeFrame,
                   fg = getRcmdr("title.color"),
                   text = gettext_bs("New variable name")),
        labelRcmdr(computeFrame, text = "   "),
        labelRcmdr(computeFrame,
                   fg = getRcmdr("title.color"),
                   text = gettext_bs("Expression to compute")),
        pady = c(15, 0),
        sticky = "nw")

    tkgrid(newVariable,
           labelRcmdr(computeFrame, text = " = "),
           expression_Frame,
           sticky = "nw")

    tkgrid(compute, sticky = "ew")
    tkgrid(computeXscroll, sticky = "ew")

    # tkgrid(variablesFrame, sticky = "nw")
    tkgrid(computeFrame, sticky = "nw")


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "ew", columnspan = 2)
    dialogSuffix(rows = 3,
                 columns = 2,
                 focus = compute)
}
