
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
window_num_convert <- function() {
    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ds     <- activeDataSet()
    fg_col <- Rcmdr::getRcmdr("title.color")

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(
        title = gettext_bs("Convert Numeric Variables into Other Classes"))

    tk_title(top, "Convert numeric variables")

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        prefix       = "",
        suffix       = "",
        into         = "numeric",
        names_action = "modify",
        make_unique  = FALSE,
        variables    = NULL
    )

    dialog_values <- getDialog("window_num_convert", defaults)

    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    change_name_suffix <- function() {

        opt_1 <- tclvalue_chr(names_action_Variable)
        opt_2 <- tclvalue_chr(suffix_var)

        if (opt_1 != "overwrite" &
            opt_2 %in% c("", "_chr", "_fct", "_ord", "_num", "_int", "_lgl")) {

                tclvalue(suffix_var) <-
                    switch(tclvalue(intoVariable),
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

        opt_1 <- tclvalue_chr(names_action_Variable)
        switch(opt_1,
               "overwrite" = {
                   # Clear values
                   tclvalue(make_uniqueVariable) <- "0"
                   tclvalue(prefix_var) <- ""
                   tclvalue(suffix_var) <- ""

                   # Disable widgets
                   tk_disable(make_uniqueCheckBox)
                   tk_disable(prefix_field)
                   tk_disable(suffix_field)
               },
               "modify" = {
                   # Activate widgets
                   tk_activate(make_uniqueCheckBox)
                   tk_activate(prefix_field)
                   tk_activate(suffix_field)

                   change_name_suffix()
               },
               stop("Unrecognized option")

        )
    }

    # Function onOK ----------------------------------------------------------
    onOK <- function() {

        show_error_messages("Function is not implemented yet",
                            title = "Not implemented")

        return()
        prefix      <- tclvalue_chr(prefix_var)
        suffix      <- tclvalue_chr(suffix_var)
        into        <- tclvalue(intoVariable)
        names_action <- tclvalue(names_action_Variable)
        variables   <- getSelection(var_y_box)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_num_convert",
                  list(suffix = {if (nchar(suffix) == 0) gettext_bs("<automatic suffix>") else suffix},
                       into = into,
                       names_action = names_action,
                       variables = variables
                  )
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(variables) == 0) {
            errorCondition(recall = window_num_convert,
                           message = gettext_bs("You must select a variable."))
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        switch(
            names_action,

            # Overwrite names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            "overwrite" = {
                new_names <- variables
            },

            # Use new names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            "new_names" = {
                new_names <-
                    if (suffix == gettext_bs("<automatic suffix>")) {
                        suffix <- switch(into,
                                         "character" = "chr",
                                         "factor"    = "fct",
                                         "nominal"   = "fct",
                                         "ordinal"   = "ord",
                                         "numeric"   = "num",
                                         "integer"   = "int",
                                         "logical"   = "lgl",
                                         into)
                        paste0(variables, "_", suffix)

                    } else if (length(variables) == 1) {
                        suffix

                    } else {
                        paste0(variables, suffix)
                    }

                # Check if new variable names are not duplicated ~~~~~~~~~~~~~~~~~~~~~~
                for (i in seq_along(variables)) {

                    if (!is.valid.name(new_names[i])) {
                        errorCondition(
                            recall = window_num_convert,
                            message = paste(new_names[i], gettext_bs("is not a valid name."))
                        )
                        return()
                    }
                    if (is.element(new_names[i], Variables())) {
                        if ("no" == tclvalue(checkReplace(new_names[i]))) {
                            window_num_convert()
                            return()
                        }
                    }
                }
            }
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        Library("tidyverse")

        into_fun <- switch(into,
                           "character"  = "as.character" ,
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

        tans_txt <- glue("{new_names} = {into_fun}({variables}{fct_type})")

        command <-
            if (length(tans_txt) == 1) {
                glue("{ds} <- {ds} %>%\n",
                     "dplyr::mutate({tans_txt})\n")

            } else {
                glue("{ds} <- {ds} %>%\n",
                     'dplyr::mutate(\n{paste0(tans_txt, collapse = ",\n")}\n',
                     ')\n')
            }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command <- style_cmd(command)

        result <- justDoIt(command)

        if (class(result)[1] != "try-error")
            activeDataSet(ds, flushModel = FALSE)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        msg <- glue(
            "#---  ", gettext_bs("Convert numeric variables into"),
            " {into} variables ---#\n\n",
            "# ", gettext_bs("New variable(s):"), " \n",
            paste("#   ", new_names, collapse = "\n"), "\n\n\n")

        logger(paste0(msg, command, collapse = "\n"))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
    }
    # [end: onOK] ------------------------------------------------------------

    # Frames and widgets -----------------------------------------------------
    # Initialize ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # initializeDialog(title = gettext_bs("xxx_title"))

    # Widgets ----------------------------------------------------------------

    upper_frame <- tkframe(top)

    var_y_box <- variableListBox2(
        upper_frame,
        Numeric(),
        listHeight = 7,
        selectmode = "multiple",
        title      = gettext_bs("Variables (pick one or more)"),
        initialSelection = var_pos_n(dialog_values$variables, "numeric")
    )

    into_outter_Frame <- tkframe(upper_frame)
    Rcmdr::radioButtons(
        into_outter_Frame,
        name    = "into_",
        title   = gettext_bs("Convert into"),
        buttons = c("numeric", "integer", "character", "nominal", "ordinal"),
        values  = c("numeric", "integer", "character", "nominal", "ordinal"),
        initialValue = dialog_values$into,
        labels =  gettext_bs(
            c("Real numbers",
              "Integers",
              "Text",
              "Nominal factors",
              "Ordinal factors")),
        command = change_name_suffix
    )

    # Layout
    tkgrid(upper_frame)
    tkgrid(getFrame(var_y_box), into_outter_Frame, sticky = "nw")
    tkgrid(into_Frame, padx = c(15, 5))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    middle_frame <- tkframe(top)

    var_name_opts_frame <- tkframe(middle_frame)
    radioButtons_horizontal(
        window       = var_name_opts_frame,
        name         = "names_action_",
        title        = gettext_bs("Variable names: "),
        title.color  = fg_col,
        buttons      = c("overwrite", "modify"),
        values       = c("overwrite", "modify"),
        initialValue = dialog_values$names_action,
        labels       = gettext_bs(c("Overwrite", "Copy & modify")),
        command      = control_checkbox_activation
    )

    make_unique_outer_frame <- tkframe(var_name_opts_frame)
    bs_check_boxes(make_unique_outer_frame,
                   frame = "make_unique_frame",
                   boxes = c("make_unique"),
                   # commands = list("make_unique" = control_checkbox_activation),
                   initialValues = c(dialog_values$make_unique),
                   labels = gettext_bs(c("Make unique"))
    )

    lower_frame <- tkframe(middle_frame)

    prefix_var   <- tclVar(dialog_values$prefix)
    prefix_field <- ttkentry(lower_frame, width = "37", textvariable = prefix_var)

    suffix_var   <- tclVar(dialog_values$suffix)
    suffix_field <- ttkentry(lower_frame, width = "37", textvariable = suffix_var)

    # Layout
    tkgrid(middle_frame, sticky = "ew")

    tkgrid(var_name_opts_frame, sticky = "ws")
    tkgrid(names_action_Frame, make_unique_outer_frame, sticky = "ws", pady = c(10, 0))
    tkgrid(make_unique_frame,   sticky = "ws")

    tkgrid(middle_frame, sticky = "ew")

    tkgrid(lower_frame, sticky = "ew")
    tkgrid(
        labelRcmdr(lower_frame, text = gettext_bs("Prefix:"), fg = fg_col),
        labelRcmdr(lower_frame, text = gettext_bs("     ")),
        prefix_field,
        pady = c(10, 2)
    )
    tkgrid(
        labelRcmdr(lower_frame, text = gettext_bs("Suffix:"), fg = fg_col),
        labelRcmdr(lower_frame, text = gettext_bs("     ")),
        suffix_field
    )

    # Finalize ---------------------------------------------------------------
    ok_cancel_help(helpSubject = "as.character")
    tkgrid(buttonsFrame, sticky = "ew", columnspan = 2)
    dialogSuffix(rows = 4, columns = 2, preventGrabFocus = TRUE)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    change_name_suffix()
    control_checkbox_activation()

    # Interactive bindings ---------------------------------------------------

    # Add interactivity for `fname_frame` and `fname_label`
    # tkbind(file_label,     "<ButtonPress-1>", on_click)
    # tkbind(fname_frame,    "<ButtonPress-1>", on_click)
    # tkbind(fname_label,    "<ButtonPress-1>", on_click)
    #
    # tkbind(fname_frame, "<Enter>",
    #        function() tkconfigure(fname_label, foreground = "blue"))
    # tkbind(fname_frame, "<Leave>",
    #        function() tkconfigure(fname_label, foreground = "black"))
    # # tkconfigure(file_label,     cursor = "hand2")
    # tkconfigure(fname_frame,    cursor = "hand2")
    # tkconfigure(button_ch_file, cursor = "hand2")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
