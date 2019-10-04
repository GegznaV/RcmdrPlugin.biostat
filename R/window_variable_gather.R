# TODO:
# 1. Add possibility to select columns, that must not be gathered,
#    e.g. "-Species" vs. "Species".
#
# 2. Quote variable names with ticks where needed:
# (if include space or special symbol or if make.names(x) != x):
#
# 3. Add option to sort values of key column:
#        - move multiple selected rows

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_variable_gather <- function() {

    # Functions --------------------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    activate_arrow_buttons <- function(variables) {
        # Arrow buttons are enabled if anything in f1_y_var_box is selected
        n <- get_selection_length(f1_y_var_box)

        objs <- list(f1_but_1_1, f1_but_1_2, f1_but_1_3, f1_but_1_4)

        tips <- list(
            tip1 = "Move the first selected line \nto the top.  ",
            tip2 = "Move the first selected line \nup by 1 position.  ",
            tip3 = "Move the first selected line \ndown by 1 position.",
            tip4 = "Move the first selected line \nto the bottom."
        )

        tip_disabled <- "Select a variable to enable \narrow buttons."

        if (n == 0) {
            walk(objs, tk_disable)
            walk(objs, ~ (tip(.x) <- tip_disabled))

        } else {
            walk(objs, tk_normalize)
            walk2(objs, tips, ~ (tip(.x) <- .y))
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    show_selected_variables <- function() {
        n <- get_selection_length(f1_y_var_box)

        color <- "green"

        if (n == 0) {
            n <- "all"

        } else if (n == 1) {
            color <- "darkred"

        } else {
          color <- "darkgreen"
        }

        txt <- str_glue("Number of variables to gather: {n}")

        tkconfigure(f1_text, text = txt, foreground = color)
        activate_arrow_buttons()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    reset_y_var_box_selection <- function() {
        if (isTRUE(get_values(f1_opts, "gather_all"))) {
            set_selection(f1_y_var_box, 0, clear = TRUE) # Clear variable box

        } else {
            set_selection(f1_y_var_box, 1, clear = TRUE) # Clear variable box
            tk_see(f1_y_var_box, 1)
        }

        show_selected_variables()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    activate_gather_all_box <- function() {
        # On mouse relese select/deselect checkbox
        if (get_selection_length(f1_y_var_box) == 0) {
            set_values(f1_opts, gather_all = 1)

        } else {
            set_values(f1_opts, gather_all = 0)
        }

        show_selected_variables()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        .ds <- active_dataset() # active_dataset_0()

        variables     <- get_selection(f1_y_var_box)

        key_colname   <- get_values(f1_key)
        value_colname <- get_values(f1_value)
        new_dataset   <- get_values(f1_dsname)

        gather_all    <- get_values(f1_opts, "gather_all")
        factor_key    <- get_values(f1_opts, "factor_key")
        convert_key   <- get_values(f1_opts, "convert_key")
        na_rm         <- get_values(f1_opts, "na_rm")


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Check input

        if (is_empty_name(key_colname, which_name = "key column name", parent = top)) {
            return()
        }

        if (is_not_valid_name(key_colname, parent = top)) {
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if (is_empty_name(value_colname, which_name = "values column name", parent = top)) {
            return()
        }

        if (is_not_valid_name(value_colname, parent = top)) {
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if (is_empty_name(new_dataset, which_name = "output dataset's name", parent = top)) {
            return()
        }

        if (is_not_valid_name(new_dataset, parent = top)) {
            return()
        }

        if (forbid_to_replace_object(new_dataset, parent = top)) {
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # closeDialog()
        # --------------------------------------------------------------------
        putDialog("window_variable_gather", list(
            y_var          = variables,
            key_colname    = key_colname,
            value_colname  = value_colname,
            gather_all     = gather_all,
            factor_key     = factor_key,
            convert_key    = convert_key,
            na_rm          = na_rm
            # include_exclude     = ...
            # dsname       = str_glue("{active_dataset()}_long"),
        ))


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Construct code
        variables <-
            if (gather_all == TRUE) {
                ""

            } else {
                stringr::str_c(
                    ",\n", stringr::str_c(safe_names(variables), collapse = ", "))
            }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        options_new_line <-
            if (any(na_rm, convert_key, factor_key)) {
                ",\n"
            } else {
                ""
            }

        na_rm_text <-
            if (na_rm == TRUE) {
                "na.rm = TRUE"
            } else {
                NULL
            }

        convert_key_text <-
            if (convert_key == TRUE) {
                "convert = TRUE"
            } else {
                NULL
            }

        factor_key_text <-
            if (factor_key == TRUE) {
                "factor_key = TRUE"
            } else {
                NULL
            }

        opts_text <- stringr::str_c(
            options_new_line,
            stringr::str_c(na_rm_text, factor_key_text, convert_key_text,
                           sep = ", "))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        command <- str_glue(
            '## Convert to long-format data frame \n',
            '{new_dataset} <- \n',
            '{.ds} %>% \n',
            'tidyr::gather(key = "{key_colname}", value = "{value_colname}"',
            '{variables}{opts_text}',
            ')')

        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("tidyverse")
        Library("tidyr")

        result <- justDoIt(command)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            logger(style_cmd(command))

            active_dataset(new_dataset)

        } else {
            logger_error(command, error_msg = result)
            show_code_evaluation_error_message(parent = top)
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Initial values ---------------------------------------------------------

    # Initialize dialog window and title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    initializeDialog(title = gettext_bs("Gather: Convert Dataset into Long Format"))
    tk_title(top, gettext_bs("Gather / Stack Columns into Key-Value Pairs"))


    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    defaults <- list(
        y_var          = NULL,
        # dsname       = unique_df_name(suffix = "_long"),
        key_colname    = unique_colnames("key"),
        value_colname  = unique_colnames("value"),
        gather_all     = TRUE,
        na_rm          = FALSE,
        factor_key     = TRUE,
        convert_key    = FALSE
        # include_exclude = ...
    )

    initial <- getDialog("window_variable_gather", defaults)

    # Widgets ----------------------------------------------------------------

    f1 <- tkframe(top)

    f1_y_var_box <-
        bs_listbox(
            parent     = f1,
            height     = 7,
            values     = variables_all(),
            value      = initial$y_var,
            selectmode = "multiple",
            on_release = activate_gather_all_box,
            tip        = "Select variables to gather.",
            title      = gettext_bs("Variables to gather \n(pick none, one or more)"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    f1_but_set_1 <- tkframe(f1)

    f1_but_1_1 <- tk2button(
        f1_but_set_1,
        image = "::image::bs_go_top",
        command = function() {
            move_selected_row_in_listbox(f1_y_var_box, move_to = "top")
            show_selected_variables()
        }
    )

    f1_but_1_2 <- tk2button(
        f1_but_set_1,
        image = "::image::bs_go_up",
        command = function() {
            move_selected_row_in_listbox(f1_y_var_box, move_to = "-1")
            show_selected_variables()
        }
    )

    f1_but_1_3 <- tk2button(
        f1_but_set_1,
        image = "::image::bs_go_down",
        command = function() {
            move_selected_row_in_listbox(f1_y_var_box, move_to = "+1")
            show_selected_variables()
        }
    )

    f1_but_1_4 <- tk2button(
        f1_but_set_1,
        image = "::image::bs_go_bottom",
        command = function() {
            move_selected_row_in_listbox(f1_y_var_box, move_to = "end")
            show_selected_variables()
        }
    )

    tkgrid(f1_but_1_1)
    tkgrid(f1_but_1_2)
    tkgrid(f1_but_1_3)
    tkgrid(f1_but_1_4)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f1_opts_frame <- tkframe(f1)

    f1_opts <- bs_checkboxes(
        parent = f1_opts_frame,
        border = TRUE,
        title  = "Options",
        boxes  = c("gather_all", "na_rm", "factor_key", "convert_key"),
        labels = gettext_bs(
            c(  "Gather all variables",
                "Remove missing values from output",
                "Convert key column to factor",
                "Convert key column to numeric, integer, or logical ")),
        values = c(
            initial$gather_all,
            initial$na_rm,
            initial$factor_key,
            initial$convert_key
        ),
        commands = list("gather_all"  = reset_y_var_box_selection,
                        "na_rm"       = do_nothing,
                        "factor_key"  = do_nothing,
                        "convert_key" = do_nothing
        )
    )

    f1_text <- bs_label(f1_opts_frame, text = "", fg = "green")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2 <- tkframe(top)

    f1_dsname <- bs_entry(
        parent   = top,
        width    = 48,
        value    = unique_df_name(suffix = "_long"),
        label    = gettext_bs("Output dataset name: "),
        tip      = "Name for the new long-format dataset.  ",
        validate = "focus",
        validatecommand = validate_var_name_string,
        invalidcommand  = make_red_text
    )

    f1_value  <- bs_entry(
        parent   = top,
        width    = 48,
        label    = gettext_bs("Values column name:"),
        value    = initial$value_colname,
        tip      = "Name for the value column.  ",
        validate = "focus",
        validatecommand = validate_var_name_string,
        invalidcommand  = make_red_text
    )

    f1_key    <- bs_entry(
        parent   = top,
        width    = 48,
        label    = gettext_bs("Key column name:"),
        value    = initial$key_colname,
        tip      = "Name for the key column.  ",
        validate = "focus",
        validatecommand = validate_var_name_string,
        invalidcommand  = make_red_text
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkgrid(f1)

    tkgrid(f1_but_set_1, f1_y_var_box$frame, f1_opts_frame, sticky = "nw",
           columnspan = 3)

    # tkgrid(gather_options_frame, pady = c(15, 0))
    tkgrid(f1_opts$frame, pady = c(15, 0))
    tkgrid(f1_text, sticky = "ws", pady = c(2, 0))

    tkgrid.configure(f1_but_set_1, sticky = "s", pady = c(0, 6), padx = c(0, 7))
    tkgrid.configure(f1_y_var_box$frame, padx = c(0, 6))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkgrid(f2, pady = c(10, 0))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f1_dsname$frame, sticky = "e", pady = 2)
    tkgrid(f1_key$frame,    sticky = "e", pady = 2)
    tkgrid(f1_value$frame,  sticky = "e", pady = 2)

    # ========================================================================
    ok_cancel_help(
        helpSubject = "gather", helpPackage = "tidyr",
        close_on_ok = TRUE,
        apply = "window_variable_gather()",
        reset = "window_variable_gather()",
        after_apply_success_fun = function() {

            set_values(f1_y_var_box, variables_all())
            set_values(f1_dsname,    unique_df_name(suffix = "_long"))
            set_values(f1_key,       unique_colnames("key"))
            set_values(f1_value,     unique_colnames("value"))

            activate_gather_all_box()
            reset_y_var_box_selection()

            tkselection.range(f1_dsname$obj_text, "0", "end")
            tkfocus(f1_dsname$obj_text)
            tkicursor(f1_dsname$obj_text, "0")

        })

    tkgrid(buttonsFrame, sticky = "we", columnspan = 2)
    dialogSuffix(preventGrabFocus = TRUE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    activate_gather_all_box()
    reset_y_var_box_selection()
}
