# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_rm_duplicated <- function() {
    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    cmd_activation <- function() {
        val <- tclvalue_chr(scope_Variable)
        switch(val,
               "search_all" = {
                   tk_disable(var_y_box)
                   tk_disable(keep_all_Button)
                   tk_disable(keep_selected_Button)
               },
               {
                   tk_normalize(var_y_box)
                   tk_normalize(keep_all_Button)
                   tk_normalize(keep_selected_Button)
               }
        )
    }
    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        scope          <- tclvalue_chr(scope_Variable)
        new_name       <- get_values(name_box)
        vars_y         <- get_selection(var_y_box)
        keep_variables <- tclvalue_chr(keep_Variable)

        # Reset widget properties before checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # tkconfigure(name_entry, foreground = "black")

        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (scope == "search_selected") {
            if (variable_is_not_selected(vars_y, "variable")) {return()}
        }

        if (is_empty_name(new_name))            {return()}
        if (is_not_valid_name(new_name))        {return()}
        if (forbid_to_replace_object(new_name)) {return()}
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_rows_rm_duplicated", list(
            scope          = scope,
            var_y          = vars_y,
            keep_variables = keep_variables
        ))


        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        vars_y_txt <- str_c(safe_names(vars_y), collapse = ", ")
        keep_lgl   <- keep_variables == "keep_all_"

        if (scope == "search_selected") {
            command_1 <- str_glue(
                "{new_name} <- {.ds} %>% \n ",
                "dplyr::distinct({vars_y_txt}, .keep_all = {keep_lgl})")

        } else {
            command_1 <- str_glue(
                "{new_name} <- {.ds} %>% dplyr::distinct()")
        }

        command <- str_glue(
            '## Select unique rows \n',
            "{command_1}")



        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("tidyverse")

        result <- justDoIt(command)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            logger(style_cmd(command))
            activeDataSet(new_name, flushDialogMemory = TRUE)

            # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            closeDialog()


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

    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    .ds     <- activeDataSet()
    fg_col <- Rcmdr::getRcmdr("title.color")

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Remove Duplicated Rows"))

    tk_title(top, "Remove rows with duplicated values") # Title ~~~~~~~~~~~~~~

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        position       = "first",
        scope          = "search_all",
        var_y          = NULL,
        keep_variables = "keep_selected_"

    )
    initial <- getDialog("window_rows_rm_duplicated", defaults)

    # Widgets ----------------------------------------------------------------
    main_frame <- tkframe(top)

    left_frame  <- tkframe(main_frame)
    right_frame <- tkframe(main_frame)

    tkgrid(main_frame, sticky = "nsw")
    tkgrid(left_frame, right_frame, sticky = "w")

    # Listbox
    var_y_box <- bs_listbox(
        right_frame,
        values      = variables_all(),
        value       = initial$var_y,
        title       = title_var_n,
        selectmode  = "extended",
        on_keyboard = "scroll",
        height      = 7,
        width       = c(26, Inf),
        tip         = tip_multiple_ctrl_letters
    )

    tkgrid(var_y_box$frame, padx = c(5, 0))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    upper_frame <- tkframe(left_frame)

    radioButtons_horizontal(
        upper_frame,
        title           = "Variables to take into account:",
        title.color     = fg_col,

        # right.buttons = FALSE,
        name            = "scope_",
        sticky_buttons  = "e",
        buttons         = c("search_all"    , "search_selected"),
        values          = c("search_all"    , "search_selected"),
        labels          = c("All variables" , "Selected variables"),
        initialValue    = initial$scope,
        command         = cmd_activation
    )

    tkgrid(upper_frame, sticky = "w")
    tkgrid(scope_Frame)

    # Keep variables: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    middle_frame <- tkframe(left_frame)
    radioButtons_horizontal(
        middle_frame,
        title = "Keep variables: ",
        title.color = fg_col,

        # right.buttons = FALSE,
        name = "keep_",
        sticky_buttons = "w",
        buttons = c("keep_all_",  "keep_selected_"),
        values =  c("keep_all_",  "keep_selected_"),
        labels =  c("All  ","Selected only  "),
        initialValue = initial$keep_variables
    )

    tkgrid(middle_frame, sticky = "w", pady = c(5, 2))
    tkgrid(keep_Frame)

    # Name
    init_name <-
        str_c(.ds, "_unique_rows") %>%
        str_trunc(50, ellipsis = "") %>%
        unique_obj_names()

    name_box <- bs_tk_textbox(
        left_frame,
        width = 32,
        value = init_name,
        label = "New dataset's name:",
        label_position = "above"
    )

    tkgrid(name_box$frame, sticky = "ws", pady = c(15, 0))


    # Finalize ---------------------------------------------------------------
    ok_cancel_help(helpSubject = "distinct", helpPackage = "dplyr",
                   reset = "window_rows_rm_duplicated()")

    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_activation()

    # Interactive bindings ---------------------------------------------------

}
