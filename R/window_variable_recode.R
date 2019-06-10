# TODO:
#
# 1. Solve problem of window duplication on error when apply button is pressed.
#
# 2. Better handling for code evaluation error, e. g. if only '.missing = "NA"'
# is enered and other values are not.
#
# [+/-] 3. Add popup menu for right-click and key stroke bindings, that enable quick
# change of row position, deletion, etc.
#
# 4. Update onOk function.
#
# .
#
# [+] DONE:
#
#  1. Add buttons on the window to insert `recode_values_template` and `
# recode_values_template_2`.
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
# 5. Add tk_see or set_yview for [->] and [->|] buttons
#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper functions
# x - a vector
recode_values_template <- function(x, template = "1") {

    if (is.character(x)) {
        x <- forcats::as_factor(x)
    }

    unique_values <-
        if (is.factor(x)) {
            levels(x)

        } else {
            sort(unique(x))
        }

    switch(as.character(template),
           "1" = {
               rez <-
                   str_glue('"{unique_values}" = ""') %>%
                   paste(collapse = "\n") %>%
                   paste('\n',
                         '\n.default = ""',
                         '\n.missing = ""')
           },
           "1a" = {
               rez <-
                   str_glue('"{unique_values}" = ""') %>%
                   paste(collapse = "\n") %>%
                   paste('\n',
                         '\n.default = ""',
                         '\n.missing = ""')
               # %>% align_at_equal()
           },
           "2" = {
               rez <-
                   str_glue('"{unique_values}" = "{unique_values}"') %>%
                   paste(collapse = "\n") %>%
                   paste('\n',
                         '\n.default = ""',
                         '\n.missing = ""')
           },
           "2a" = {
               rez <-
                   str_glue('"{unique_values}" = "{unique_values}"') %>%
                   paste(collapse = "\n") %>%
                   paste('\n',
                         '\n.default = ""',
                         '\n.missing = ""')
               # %>% align_at_equal()
           }
    )

    rez
    # writeLines(rez)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_cursor_position <- function(recodes) {
    setNames(
        as.integer(
            str_split_fixed(
                tkindex(recodes, "insert"), # gets cursor position
                "\\.", n = 2)
        ),
        c("row", "col")
    )
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_selection_ind <- function(recodes) {

    selection <- tclvalue(tktag.ranges(recodes, "sel"))

    setNames(
        as.integer(str_split_fixed(selection, "[. ]", n = 4)),
        c("start_row", "start_col", "end_row", "end_col")
    )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
select_line <- function(recodes, i) {
    tktag.delete(recodes, "sel")
    tktag.add(recodes, "sel", str_glue("{i}.0"), str_glue("{i}.end"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
move_elements <- function(x, ind, move_to) {
    #       x - vector;
    #     ind - selected rows;
    # move_to - "top", "-1", "+1", "end" - place to move rows to.

    move_to <- match.arg(move_to, choices = c("top", "-1", "+1", "end"))

    switch(
        move_to,
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        "top" = {
            x <- c(x[ind], x[-ind])
        },

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        "-1" = {
            if ((min(ind) - 1) < 1)
                return(x)

            for (i in sort(ind)) {
                x <- swap(x, i, i - 1)
            }
        },

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        "+1" = {
            n <- length(x)

            if ((max(ind) + 1) > n)
                return(x)

            for (i in sort(ind, decreasing = TRUE)) {
                x <- swap(x, i, i + 1)
            }
        },

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        "end"  = {
            x <- c(x[-ind], x[ind])
        }
    )
    x
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
move_selected_row_in_tktext <- function(recodes, move_to = "+1") {

    move_to <- match.arg(move_to, choices = c("top", "-1", "+1", "end"))

    # Get text
    recodes_text  <- str_remove(tclvalue(tkget(recodes, "1.0", "end")), "\\n$")
    original_text <- structure(str_split(recodes_text, "\n")[[1]], class = "glue")

    # Get y view
    y_view <- str_split_fixed(tkyview(recodes), " ", n = 2)[1]

    # # Get selection

    # selection <- get_selection_ind(recodes)
    # n_rows <- selection["end_row"] - selection["start_row"] + 1
    #
    # if (is.na(n_rows)) {
    #     i <- get_cursor_position(recodes)["row"]
    #     select_line(recodes, i)
    #     tksee(recodes, str_glue("{i}.0"))
    #     return()
    #
    # } else if (n_rows > 1) {
    #     i <- get_j(move_to, selection[c("start_row", "end_row")], length(original_text))
    #     select_line(recodes, i)
    #     tksee(recodes, str_glue("{i}.0"))
    #     return()
    #
    # } else {
    #     i <- selection["start_row"]
    # }

    i <- get_cursor_position(recodes)["row"]
    new_text <- move_elements(x = original_text, ind = i, move_to = move_to)

    swapped <- structure(str_c(new_text, collapse = "\n"), class = "glue")

    tkdelete(recodes, "1.0", "end")
    tkinsert(recodes, "1.0", swapped)
    tkyview.moveto(recodes, y_view) # reset y view

    # [???] adapt for multiple selected lines
    j <- get_j(move_to, i, length(original_text))
    pos_j <- str_glue("{j}.0")

    tkmark.set(recodes, "insert", pos_j)

    tclAfter(1, function() {
        tkfocus(recodes)
        select_line(recodes, j)
        tksee(recodes, pos_j)
    })
}

# ___ Main function ___ ======================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_variable_recode0 <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Recode Variable Values"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <-
        list(
            # make_factor     = 1,
            recode_into       = "nominal",
            variables         = NULL,
            name              = unique_colnames("recoded"),
            recode_directives = "",
            selected_variable = "{none}"
        )

    initial <- getDialog("window_variable_recode0", defaults)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    selected_var_frame <- tkframe(top)
    selected_variable  <- tclVar(initial$selected_variable)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    insert_template <- function(template = "1") {
        active_variable  <- get_selection(variablesBox)
        var_val          <- get_active_ds()[[active_variable]]
        # var_val        <- str_glue_eval("{active_dataset()}${active_variable}")

        get_active_ds()[[active_variable]]

        tclvalue(selected_variable) <- active_variable

        # Change contents of "recodes" box
        tkdelete(recodes, "1.0", "end")
        tkinsert(recodes, "1.0", recode_values_template(var_val, template))

        # Change new variable name
        tclvalue(newVariableName) <-
            unique_colnames(active_variable, suffix = "_recoded")
    }

    tk_see_current_variable <- function() {
        tk_see(
            variablesBox,
            which(get_selection(variablesBox) == get_values(variablesBox)))
    }

    insert_template_1  <- function() {
        insert_template("1")
        tk_see_current_variable()
        tkmark.set(recodes, "insert", "1.0")
    }

    insert_template_1a <- function() {
        insert_template("1a")
        tk_see_current_variable()
        tkmark.set(recodes, "insert", "1.0")
    }

    insert_template_2  <- function() {
        insert_template("2")
        tk_see_current_variable()
        tkmark.set(recodes, "insert", "1.0")
    }

    insert_template_2a <- function() {
        insert_template("2a")
        tk_see_current_variable()
        tkmark.set(recodes, "insert", "1.0")
    }

    clean_directives <- function(str) {
        stringr::str_replace_all(
            str,
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
              # "(\n)( )*"    = ",\\1", # TODO [???]: maybie this is a better option

              # Remove leading spaces and commas in
              # the first row
              "^[, ]*"    = "",

              # Remove tailing commas and spaces
              "[, ]*$"    = "")
        )
        # str_detect(recode_directives,
        #            "[\\p{Alphabetic}\\p{Mark}\\p{Decimal_Number}]")
    }

    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        recode_into       <- tclvalue(recode_intoVariable)
        variables         <- get_selection(variablesBox)
        selected_variable <- tclvalue(selected_variable)
        name              <- tclvalue_chr(newVariableName)

        # Read recode directives
        save_recodes <- trimws(tclvalue(tkget(recodes, "1.0", "end")))

        # Format code into one row
        recode_directives <- clean_directives(save_recodes)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog(
            "window_variable_recode0",
            list(
                # make_factor       = make_factor,
                variables         = variables,
                name              = name,
                recode_directives = save_recodes,
                recode_into       = recode_into,
                selected_variable = selected_variable
            )
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # variables
        # If no variable is selected
        if (length(variables) == 0 || selected_variable == "{none}") {
            show_error_messages(
                parent = top,
                "No variable is selected.",
                str_c("Prease, select a variable."),
                title = "No Variable Selected"
            )
            return()
        }
        # Is empty? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (recode_directives == "") {
            show_error_messages(
                parent = top,
                "No recode directives were specified: the template was not filled.",
                str_c("No recode directives were specified: ",
                      "the template was not filled.\n\n",
                      "Please create a template and fill it where necessary. ",
                      "Some (but not all) fields may be left empty.  \n\n",
                      "To create a template, either use arrow buttons that are ",
                      "between the boxes or double left/right click on a variable name."
                ),
                title = "Missing Recode Directives"
            )
            return()

        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (is_empty_name(name, parent = top))              {return()}
        if (is_not_valid_name(name, parent = top))          {return()}
        if (forbid_to_replace_variable(name, parent = top)) {return()}
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        switch(
            recode_into,
            "nominal" = {
                recode_fun     <- "dplyr::recode_factor"
                ordered_factor <- ""                  # TODO: .ordered = FALSE ???
            },
            "ordinal" = {
                recode_fun     <- "dplyr::recode_factor"
                ordered_factor <- ", .ordered = TRUE"
            },
            "other" = {
                recode_fun     <- "dplyr::recode"
                ordered_factor <- ""
            })

        .ds <- active_dataset()

        command <- str_glue(
            "## ", gettext_bs("Recode variable values"), "\n\n",
            "{.ds} <- \n",
            "   {.ds} %>% \n",
            "   dplyr::mutate(\n",
            "   {name} = {recode_fun}({selected_variable}, \n",
            "   {recode_directives}{ordered_factor}))"
        )

        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        Library("tidyverse")

        result <- justDoIt(command)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            # closeDialog()
            logger(style_cmd(command))
            active_dataset(.ds, flushModel = FALSE, flushDialogMemory = FALSE)

        } else {
            logger_error(command, error_msg = result)
            show_code_evaluation_error_message()
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }
    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")

    title_text <- gettext_bs("Recode Variable Values")
    tk_title(top, title_text)

    # Widgets ----------------------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f1 <- tkframe(top)

    variablesBox <-
        bs_listbox(
            parent = f1,
            height = 7,
            values = variables_all(),
            on_double_click = insert_template_1,
            on_triple_click = insert_template_1a,
            on_double_click_3 = insert_template_2,
            on_triple_click_3 = insert_template_2a,
            # selectmode = "multiple",
            title = gettext_bs("Variable to recode \n(double-click to pick one)"),
            value = initial$variables
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    recodesFrame <- tkframe(f1)
    recodes <-
        tktext(
            recodesFrame,
            bg     = "white",
            font   = getRcmdr("logFont"),
            height = "7",
            width  = "40",
            wrap   = "none",
            undo   = TRUE)

    right_click_menu_text(recodes, undo = TRUE)

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
    tkinsert(recodes, "1.0", initial$recode_directives)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    f1_but_set_2 <- tkframe(f1)

    f1_but_2_1 <- tk2button(
        f1_but_set_2,
        image = "::image::bs_go_next",
        command =  function() {
            if (get_selection_length(variablesBox) == 0) {
                set_selection(variablesBox, 1)
            }
            insert_template_1()
        },
        tip = "Create/Reset a template to recode \n values of selected variable."
    )

    f1_but_2_2 <- tk2button(
        f1_but_set_2,
        image = "::image::bs_go_last",
        command = function() {
            if (get_selection_length(variablesBox) == 0) {
                set_selection(variablesBox, 1)
            }
            insert_template_2()
        },
        tip = "Create/Reset a template to reorder \nlevels of selected variable."
    )

    tkgrid(f1_but_2_1)
    tkgrid(f1_but_2_2)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    f1_but_set_1 <- tkframe(f1)

    f1_but_1_1 <- tk2button(
        f1_but_set_1,
        image = "::image::bs_go_top",
        command = function() {
            move_selected_row_in_tktext(recodes, move_to = "top")
        },
        tip = "Move selected line \nto the top."
    )

    f1_but_1_2 <- tk2button(
        f1_but_set_1,
        image = "::image::bs_go_up",
        command = function() {
            move_selected_row_in_tktext(recodes, move_to = "-1")
        },
        tip = "Move selected line \nup by 1 position."
    )

    f1_but_1_3 <- tk2button(
        f1_but_set_1,
        image = "::image::bs_go_down",
        command = function() {
            move_selected_row_in_tktext(recodes, move_to = "+1")
        },
        tip = "Move selected line \ndown by 1 position."
    )

    f1_but_1_4 <- tk2button(
        f1_but_set_1,
        image = "::image::bs_go_bottom",
        command = function() {
            move_selected_row_in_tktext(recodes, move_to = "end")
        },
        tip = "Move selected line \nto the bottom."
    )

    tkgrid(f1_but_1_1)
    tkgrid(f1_but_1_2)
    tkgrid(f1_but_1_3)
    tkgrid(f1_but_1_4)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2 <- tkframe(top)
    lower_options_frame <- tkframe(f2)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    variablesFrame  <- tkframe(lower_options_frame)
    newVariableName <- tclVar(initial$name)

    newVariable <-
        ttkentry(variablesFrame,
                 width = "23",
                 textvariable = newVariableName)
    # ------------------------------------------------------------------------
    variable_type_frame <- tkframe(lower_options_frame)


    Rcmdr::radioButtons(
        variable_type_frame,
        name          = "recode_into",
        # title       = gettext_bs("Use functions: "),
        # title.color = getRcmdr("title.color"),
        buttons       = c("nominal", "ordinal", "other"),
        values        = c("nominal", "ordinal", "other"),
        initialValue  = initial$recode_into,
        labels        = gettext_bs(c("Nominal factor",
                                     "Ordinal factor",
                                     "Do not convert")))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkgrid(selected_var_frame) # selected_variable
    tkgrid(
        bs_label(selected_var_frame, text = "Selected variable: "),
        bs_label(selected_var_frame,
                 textvariable = selected_variable,
                 font = tkfont.create(weight = "bold", size = 8),
                 fg = "darkred"),
        pady = c(0, 9)
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ok_cancel_help(
        helpSubject = "recode_factor", helpPackage = "dplyr",
        close_on_ok = TRUE,
        reset = "window_variable_recode0()",
        apply = "window_variable_recode0()",
        after_apply_success_fun = function() {
            new_name <- tclvalue_chr(newVariableName)

            set_values(variablesBox, variables_all())
            tk_see(variablesBox, new_name)
            set_selection(variablesBox, new_name)

            tclvalue(selected_variable) <- new_name

            tkdelete(recodes, "1.0", "end")

            tclvalue(newVariableName) <-
                unique_colnames(new_name, all_numbered = TRUE)
        })
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkgrid(f1, sticky = "nw")

    tkgrid(getFrame(variablesBox), f1_but_set_2, recodesFrame, f1_but_set_1,
           sticky = "nw")

    tkgrid.configure(f1_but_set_1, sticky = "")
    tkgrid.configure(f1_but_set_2, sticky = "", padx = c(3, 4))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # tkgrid()

    tkgrid(bs_label(
        variablesFrame,
        fg = getRcmdr("title.color"),
        text = gettext_bs("Name for recoded variable: ")),
        sticky = "w",
        pady = c(2, 0))
    tkgrid(newVariable, sticky = "w")

    tkgrid(
        bs_label(variable_type_frame,
                 text = gettext_bs("Convert variable into: "),
                 fg = getRcmdr("title.color")),
        sticky = "w",
        pady = c(10, 0)
    )

    tkgrid(recode_intoFrame, sticky = "w")

    tkgrid(
        bs_label(
            recodesFrame,
            text = gettext_bs(str_c(
                "Enter recode directives\n",
                "(one directive per row; change order of rows, if needed)")),
            # "Enter recode directives\n(one directive per row or comma separated)"),
            fg = getRcmdr("title.color"),
            font = "RcmdrTitleFont"
        ),
        sticky = "w"
    )

    tkgrid(recodes, recodesYscroll, sticky = "nw")
    tkgrid(recodesXscroll)

    tkgrid(f2,         sticky = "w")
    tkgrid(variablesFrame,      sticky = "w")
    tkgrid(variable_type_frame, sticky = "w")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    examples_frame <- tkframe(f2)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid_text <- function(text = "", frame = examples_frame, fg = "black",
                            sticky = "w", padx = 10, pady = 0, ...) {
        tkgrid(bs_label(frame, text = gettext_bs(text), fg = fg),
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
    # ========================================================================

    tkgrid(buttonsFrame, sticky = "ew")
    # tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
    tkgrid.configure(recodesXscroll, sticky = "ew")
    tkgrid.configure(recodesYscroll, sticky = "ns")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dialogSuffix(bindReturn = FALSE)
}

# ============================================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
