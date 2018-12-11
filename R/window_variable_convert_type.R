#' Rcmdr windows for variable class conversion
#'
#' @export
#' @keywords internal
#' @family conversion

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
window_variable_convert_type <- function() {
    # Functions --------------------------------------------------------------
    change_name_suffix <- function() {

        opt_1 <- tclvalue_chr(widget_2$var_radiobuttons)
        opt_2 <- tclvalue_chr(widget_2$var_suffix)

        if (opt_1 != "overwrite" &&
            opt_2 %in% c("", "_chr", "_fct", "_ord", "_num", "_int", "_lgl")) {

            tclvalue(widget_2$var_suffix) <-
                switch(tclvalue(into_Variable),
                       "character" = "_chr",
                       "text"      = "_chr",
                       "factor"    = "_fct",
                       "nominal"   = "_fct",
                       "ordinal"   = "_ord",
                       "numeric"   = "_num",
                       "integer"   = "_int",
                       "logical"   = "_lgl",
                       "")
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    control_checkbox_activation <- function() {

        opt_1 <- tclvalue_chr(widget_2$var_radiobuttons)
        switch(opt_1,
               "overwrite" = {
                   # Clear values
                   tclvalue(widget_2$var_checkbox) <- "0"
                   tclvalue(widget_2$var_prefix)   <- ""
                   tclvalue(widget_2$var_suffix)   <- ""

                   # Disable widgets
                   tk_disable(widget_2$obj_checkbox)
                   tk_disable(widget_2$obj_prefix)
                   tk_disable(widget_2$obj_suffix)
               },

               "modify" = {
                   # Activate widgets
                   tk_activate(widget_2$obj_checkbox)
                   tk_activate(widget_2$obj_prefix)
                   tk_activate(widget_2$obj_suffix)

                   change_name_suffix()
               },

               stop("Unrecognized option")
        )
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_update_list_and_activation <- function() {

        tclvalue(into_Variable) <- "character"

        switch(
            get_selection(var_type_box),
            "All" = {
                val_list <- variables_all()

                tk_normalize(characterButton)
                tk_normalize(nominalButton)
                tk_normalize(ordinalButton)
                tk_normalize(integerButton)
                tk_normalize(numericButton)
                tk_normalize(logicalButton)
            },
            "Text (character)" = {
                val_list <- variables_chr()

                tk_disable(characterButton)
                tk_normalize(nominalButton)
                tk_normalize(ordinalButton)
                tk_normalize(integerButton)
                tk_normalize(numericButton)
                tk_normalize(logicalButton)

                tclvalue(into_Variable) <- "nominal"
            },
            "Numeric" = {
                val_list <- variables_num()

                tk_normalize(characterButton)
                tk_normalize(nominalButton)
                tk_normalize(ordinalButton)
                tk_normalize(integerButton)
                tk_normalize(numericButton)
                tk_normalize(logicalButton)
            },
            "Integer (whole numbers)" = {
                val_list <- variables_int()

                tk_normalize(characterButton)
                tk_normalize(nominalButton)
                tk_normalize(ordinalButton)
                tk_disable(integerButton)
                tk_normalize(numericButton)
                tk_normalize(logicalButton)
            },
            "Real numbers" = {
                val_list <- variables_dbl()

                tk_normalize(characterButton)
                tk_normalize(nominalButton)
                tk_normalize(ordinalButton)
                tk_normalize(integerButton)
                tk_disable(numericButton)
                tk_normalize(logicalButton)
            },
            "Factor" = {
                val_list <- variables_fct()

                tk_normalize(characterButton)
                tk_normalize(nominalButton)
                tk_normalize(ordinalButton)
                tk_disable(integerButton)
                tk_disable(numericButton)
                tk_disable(logicalButton)
            },
            "Logical" = {
                val_list <- variables_lgl()

                tk_normalize(characterButton)
                tk_normalize(nominalButton)
                tk_normalize(ordinalButton)
                tk_normalize(integerButton)
                tk_disable(numericButton)
                tk_disable(logicalButton)
            },
            "Other" = {
                val_list <- variables_oth()

                tk_normalize(characterButton)
                tk_disable(nominalButton)
                tk_disable(ordinalButton)
                tk_disable(integerButton)
                tk_disable(numericButton)
                tk_disable(logicalButton)
            }
        )

        # Values can be set only in non-disabled modes
        tk_normalize(var_y_box)
        set_values(var_y_box, val_list)

        # Enable/Diable
        if (length(val_list) > 0) {
            tk_normalize(var_y_box)
        } else {
            tk_disable(var_y_box)
        }

        change_name_suffix()
    }

    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        variables   <- get_selection(var_y_box)
        into        <- tclvalue_chr(into_Variable)

        names_action <- tclvalue_chr(widget_2$var_radiobuttons)
        make_unique  <- tclvalue_lgl(widget_2$var_checkbox)
        prefix       <- tclvalue_chr(widget_2$var_prefix)
        suffix       <- tclvalue_chr(widget_2$var_suffix)

        selected_type <- get_selection(var_type_box)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (variable_is_not_selected(variables, "variable")) {
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Process new names
        switch(
            names_action,

            # Overwrite names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            "overwrite" = {
                new_names <- variables
            },

            # Use new names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            "modify" = {
                new_names <- str_c(prefix, variables, suffix)
            }
        )

        if (make_unique) {
            new_names <- unique_colnames(new_names)
        }

        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (are_not_valid_names(new_names)) {
            return()
        }

        if (forbid_to_replace_variables(new_names)) {
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_variable_convert_type",
                  list(
                      variables     = variables    ,
                      into          = into         ,
                      names_action  = names_action ,
                      make_unique   = make_unique  ,
                      prefix        = prefix       ,
                      suffix        = suffix       ,
                      selected_type = selected_type
                  )
        )

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        into_fun <- switch(
            into,
            "character"  = "as.character" ,
            "text"       = "as.character" ,
            "factor"     = "as.factor" ,
            "nominal"    = "factor" ,
            "ordinal"    = "factor" ,
            "numeric"    = "as.numeric"  ,
            "integer"    = "as.integer" ,
            "logical"    = "as.logical" ,
            stop("Unexpected choice"))

        fct_type <- switch(
            into,
            "ordinal"    = ", ordered = TRUE" ,
            "nominal"    = ", ordered = FALSE" ,
            "")

        tans_txt <- str_glue("{new_names} = {into_fun}({variables}{fct_type})")

        command <-
            if (length(tans_txt) == 1) {
                str_glue("{ds} <- {ds} %>%\n",
                         "dplyr::mutate({tans_txt})\n")

            } else {
                str_glue("{ds} <- {ds} %>%\n",
                         'dplyr::mutate(\n{paste0(tans_txt, collapse = ",\n")}\n',
                         ')\n')
            }

        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("tidyverse")

        # doItAndPrint(command)
        result <- justDoIt(command)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            msg <- str_glue(
                "## Convert variables into {into} variables \n\n",
                "# New variable(s): \n",
                paste("#   ", new_names, collapse = "\n"), "\n\n\n")

            logger(paste0(msg, style_cmd(command), collapse = "\n"))
            activeDataSet(ds, flushDialogMemory = FALSE)

            # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            closeDialog()


        } else {
            logger_error(command, error_msg = result)
            show_code_evaluation_error_message()
            return()
        }

        # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command_dataset_refresh()
        tkfocus(CommanderWindow())
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }


    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ds     <- activeDataSet()
    fg_col <- Rcmdr::getRcmdr("title.color")

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Convert Variable Types"))

    main_frame <- tk2frame(top)
    tkgrid(main_frame, columnspan = 2)
    tk_title(main_frame, "Convert variable types")

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        # variables     = NULL,
        into          = "character",
        names_action  = "modify",
        make_unique   = FALSE,
        prefix        = "",
        suffix        = "",
        selected_type = "All"
    )

    initial <- getDialog("window_variable_convert_type", defaults)

    # Widgets ----------------------------------------------------------------

    upper_frame     <- tk2frame(main_frame)
    variables_frame <- tk2frame(upper_frame)

    var_y_box <- bs_listbox(
        parent = variables_frame,
        values      = NULL, # <-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        height      = 7,
        width       = 35,
        selectmode  = "multiple",
        title       = gettext_bs("Variables (pick one or more)"),
        on_keyboard = "scroll",
        tip         = tip_multiple_ctrl_letters
    )


    type_frame <- tk2frame(variables_frame)

    var_types <- c("All", "Text (character)", "Factor",
                   "Numeric", "Integer (whole numbers)", "Real numbers",
                   "Logical", "Other")

    var_type_box <- bs_combobox(
        type_frame,
        values = var_types,
        value  = initial$selected_type,
        width  = 21,
        tip    = "Filter variables by variable type",
        on_select = cmd_update_list_and_activation
    )

    tkgrid(var_y_box$frame, sticky = "w")
    tkgrid(type_frame, sticky = "w", pady = c(5, 0))
    tkgrid(bs_label_b(type_frame, text = "Type filter: "), var_type_box$frame,
           sticky = "w")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    into_outter_Frame <- tkframe(upper_frame)
    Rcmdr::radioButtons(
        into_outter_Frame,
        name    = "into_",
        title   = gettext_bs("Convert into"),
        buttons = c("character", "nominal", "ordinal", "integer", "numeric", "logical"),
        values  = c("character", "nominal", "ordinal", "integer", "numeric", "logical"),
        initialValue = initial$into,
        labels  = gettext_bs(
            c("Text (character)",
              "Nominal factors",
              "Ordinal factors"
              , "Integers", "Real numbers", "Logical"
            )),
        command = change_name_suffix
    )

    # Layout
    tkgrid(upper_frame, sticky = "w")
    tkgrid(variables_frame, into_outter_Frame, sticky = "nw")
    tkgrid(into_Frame, padx = c(15, 5))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    widget_2 <- tk_widget_modify_names(
        main_frame,
        width = 65,
        init_val_radiobuttons = initial$names_action,
        init_val_checkbox     = initial$make_unique,
        init_val_prefix       = initial$prefix,
        init_val_suffix       = initial$suffix,
        cmd_radiobuttons      = control_checkbox_activation
    )

    # Finalize ---------------------------------------------------------------
    ok_cancel_help(helpSubject = "as.character",
                   apply = "window_variable_convert_type()",
                   reset = "window_variable_convert_type()")
    tkgrid(buttonsFrame, sticky = "ew", columnspan = 2)
    dialogSuffix(rows = 4, columns = 2, preventGrabFocus = TRUE)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_update_list_and_activation()
    control_checkbox_activation()
    change_name_suffix()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
