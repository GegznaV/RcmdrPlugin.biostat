
# TODO:
#
# Change interface for name input:
#        + add separate window for name input
#        + add boxes for prefix/suffix inputs.

#' Rcmdr windows for variable class conversion
#'
#' @export
#' @keywords internal
#' @family conversion

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
window_convert_any <- function() {
    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ds     <- activeDataSet()
    fg_col <- Rcmdr::getRcmdr("title.color")

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(
        title = gettext_bs("Convert Variables into Other Classes"))

    tk_title(top, "Convert variables")

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        variables    = NULL,
        into         = "character",
        names_action = "overwrite",
        make_unique  = FALSE,
        prefix       = "",
        suffix       = ""
    )

    dialog_values <- getDialog("window_convert_any", defaults)

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

    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        variables   <- getSelection(var_y_box)
        into        <- tclvalue_chr(into_Variable)

        names_action <- tclvalue_chr(widget_2$var_radiobuttons)
        make_unique  <- tclvalue_lgl(widget_2$var_checkbox)
        prefix       <- tclvalue_chr(widget_2$var_prefix)
        suffix       <- tclvalue_chr(widget_2$var_suffix)
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
        putDialog("window_convert_any",
                  list(
                      variables    = variables    ,
                      into         = into         ,
                      names_action = names_action ,
                      make_unique  = make_unique  ,
                      prefix       = prefix       ,
                      suffix       = suffix
                  )
        )

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        into_fun <- switch(into,
                           "character"  = "as.character" ,
                           "text"       = "as.character" ,
                           "factor"     = "as.factor" ,
                           "nominal"    = "factor" ,
                           "ordinal"    = "factor" ,
                           "numeric"    = "as.numeric"  ,
                           "integer"    = "as.integer" ,
                           "logical"    = "as.logical" ,
                           stop("Unexpected choice"))

        fct_type <- switch(into,
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
            activeDataSet(ds, flushModel = FALSE, flushDialogMemory = FALSE)

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

    # Widgets ----------------------------------------------------------------

    upper_frame <- tkframe(top)

    var_y_box <- variableListBox2(
        upper_frame,
        Variables(),    # <-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        listHeight = 7,
        selectmode = "multiple",
        title      = gettext_bs("Variables (pick one or more)"),
        initialSelection = var_pos_n(dialog_values$variables, "all")
    )

    into_outter_Frame <- tkframe(upper_frame)
    Rcmdr::radioButtons(
        into_outter_Frame,
        name    = "into_",
        title   = gettext_bs("Convert into"),
        buttons = c("character", "nominal", "ordinal"
                    # , "integer", "numeric", "logical"
                    ),
        values  = c("character", "nominal", "ordinal"
                    # , "integer", "numeric", "logical"
                    ),
        initialValue = dialog_values$into,
        labels  = gettext_bs(
            c("Text (character)",
              "Nominal factors",
              "Ordinal factors"
              # , "Integers", "Real numbers", "Logical"
            )),
        command = change_name_suffix
    )

    # Layout
    tkgrid(upper_frame)
    tkgrid(getFrame(var_y_box), into_outter_Frame, sticky = "nw")
    tkgrid(into_Frame, padx = c(15, 5))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    widget_2 <- tk_widget_modify_names(
        top,
        init_val_radiobuttons = dialog_values$names_action,
        init_val_checkbox     = dialog_values$make_unique,
        init_val_prefix       = dialog_values$prefix,
        init_val_suffix       = dialog_values$suffix,
        cmd_radiobuttons      = control_checkbox_activation
    )

    # Finalize ---------------------------------------------------------------
    ok_cancel_help(helpSubject = "as.character")
    tkgrid(buttonsFrame, sticky = "ew", columnspan = 2)
    dialogSuffix(rows = 4, columns = 2, preventGrabFocus = TRUE)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    change_name_suffix()
    control_checkbox_activation()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
