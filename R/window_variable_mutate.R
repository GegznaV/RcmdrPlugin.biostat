# TODO:
#
# [+] 1. When push "Apply" button and an error occurs, two windows open.
#        "Apply" is now disabled and this should be fixed.
#
# 2. In variable box text "[factor]" should be differentiated
#    to "[character]", "[logical]", "[factor]"
#
# 3. Add buttons "+", "-", "*", etc. in style as used in "fit linear model" window
#
# 4. Add examples. May be button for pop-up window with examples
#
# 5. Enable computations by group
#
# 6. on apply sucess:  View and select the new variable [???]



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
window_variable_mutate <- function() {

    # Functions --------------------------------------------------------------
    insert_selected_variable <- function() {
        # var <- trim.blanks(getSelection(variablesBox))
        var <- get_selection(f1_vars$y)

        word <- str_glue('\\[{gettext_bs("factor")}\\]')

        if (length(grep(word, var)) == 1)
            var <- trim.blanks(sub(word, "", var))

        tkfocus(f2_entry_expr$obj_text)

        expr <- get_values(f2_entry_expr)

        new_expr <-
            if (expr == "") {
                var
            } else {
                last_chr <- stringr::str_sub(expr, -1)
                expr_sep <- if (last_chr %in% c("(", "[")) "" else " "
                paste(expr, var, sep = expr_sep)
            }

        set_values(f2_entry_expr, new_expr)

        tkicursor(f2_entry_expr$obj_text,      "end")
        tkxview.moveto(f2_entry_expr$obj_text, "1")
    }


    # onOK -------------------------------------------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        new_name   <- get_values(f2_name_entry)
        express    <- get_values(f2_entry_expr)
        use_groups <- get_values(f1_vars$checkbox)
        group_vars <- get_values(f1_vars$gr)

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



    # Dialog -----------------------------------------------------------------
    .ds <- active_dataset()

    initializeDialog(
        title = gettext_bs("Mutate: create new or replace existing variable"))



    get_labeled_variables <- function(variables) {

        vars <- Variables()
        paste(vars,
              ifelse(vars %in% Factors(),
                     yes = gettext_bs("[factor]"),
                     no  = ""
              ))
    }


    f1 <- tkframe(top)

    f1_vars <-
        bs_listbox_y_gr(
            parent      = f1,
            y_title     = gettext_bs("Current variables \n(double-click to add to expression)"),
            list_height = 8,
            y_vars      = get_labeled_variables(),
            y_params    = list(on_double_click = insert_selected_variable),
            ch_label    = "Compute by group"
        )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2 <- tkframe(top)

    f2_name_entry <- bs_entry(
        parent = f2,
        width = "20",
        label = "New variable name",
        label_position = "above",
        value = unique_colnames_2("new_variable")
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2_entry_expr <- bs_entry(
        f2,
        label = "Expression to compute",
        label_position = "above",
        width = 62,
        value = "",
        scroll_x = TRUE)

    # Grid -------------------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(
        close_on_ok = TRUE,
        helpSubject = "mutate", helpPackage = "dplyr",
        reset = "window_variable_mutate",
        apply = "window_variable_mutate",
        after_apply_success_fun = function() {
            set_values(f2_name_entry, unique_colnames_2("new_variable"))
            set_values(f1_vars$y, get_labeled_variables())

            # View and select the new variable [???]
        }
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # examples_Frame <- tkframe(f1)
    #
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # tkgrid_text <- function(text = "", frame = examples_Frame, fg = "black",
    #                         sticky = "w", padx = 20, pady = 0, ...) {
    #     tkgrid(labelRcmdr(frame, text = gettext_bs(text), fg = fg),
    #            sticky = sticky, padx = padx, pady = pady, ...)
    # }
    #
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # tkgrid_text("\nExamples of expressions", fg = getRcmdr("title.color"))
    # tkgrid_text('Polular operations:   +   -   *   /   ^   sqrt()    log()   rank()', fg = "darkgreen")
    #
    # tkgrid_text("Example 1: log(age)")
    # tkgrid_text("Example 2: a + b")
    # tkgrid_text("Example 3: as.factor(color)")
    # tkgrid_text("Example 4: weight / (height^2)")
    # tkgrid_text('Example 5: ifelse(age < 50, "young", "old")')

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f1_vars$frame,  # examples_Frame,
           sticky = "nw",
           columnspan = 2)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f1, sticky = "nw")
    tkgrid(f2, sticky = "nw")

    f2_eq <- bs_label(f2, text = " = ")

    tkgrid(f2_name_entry$frame, f2_eq, f2_entry_expr$frame,
           pady = c(15, 0),
           sticky = "new")

    tkgrid.configure(f2_eq, sticky = "ns")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "ew", columnspan = 2)
    dialogSuffix(rows = 3, columns = 2, focus = f2_entry_expr$obj_text)
}

