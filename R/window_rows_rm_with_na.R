# TODO: add option to remove rows with all NA in selected variables
#
# library(tidyverse)
# df = tibble::tribble(
#   ~x, ~y, ~z, ~q,
#   1,   1 , 1,  1,
#   2,   2,  2, NA,
#   3,   3, NA, NA,
#   4,  NA, NA, NA
# )
# df %>% filter_all(any_vars(!is.na(.)))            # remove empty rows
# df %>% filter_at(vars(q, z), any_vars(!is.na(.))) # remove rows with all NA in selected vars


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_rm_with_na <- function() {

    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    cmd_activation <- function() {
        val <- tclvalue_chr(scope_Variable)
        switch(val,
               "search_all" = {
                   tk_disable(var_y_box)
                   tkconfigure(label_bottom, text = label_bottom_text_all)
               },

               # Search in the selected variables
               {
                   tk_normalize(var_y_box)
                   tkconfigure(label_bottom, text = label_bottom_text_selected)
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
        putDialog("window_rows_rm_with_na", list(
            scope          = scope,
            var_y          = vars_y
        ))

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        vars_y_txt <- str_c(safe_names(vars_y), collapse = ", ")

        if (scope == "search_selected") {
            command_1 <- str_glue(
                "{new_name} <- {.ds} %>% \n ",
                "tidyr::drop_na({vars_y_txt})")

        } else {
            command_1 <- str_glue(
                "{new_name} <- {.ds} %>% tidyr::drop_na()")
        }

        command <- str_glue(
            '## Remove rows containing missing values \n',
            "{command_1}")



        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("tidyverse")

        result <- justDoIt(command)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            logger(style_cmd(command))
            active_dataset(new_name)

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
    .ds    <- active_dataset()
    fg_col <- Rcmdr::getRcmdr("title.color")

    label_bottom_text_all <-
        "The rows that contain at least one missing (NA) value will be removed.\n"

    label_bottom_text_selected <- str_c(
        "The rows that contain at least one missing (NA) value in the selected\n",
        "variables will be removed.")

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Remove Rows with Missing Values"))

    tk_title(top, "Drop Rows Containing Missing Values") # Title ~~~~~~~~~~~~~~

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        position       = "first",
        scope          = "search_all",
        var_y          = NULL
    )
    initial <- getDialog("window_rows_rm_with_na", defaults)

    # Widgets ----------------------------------------------------------------
    main_frame <- tkframe(top)

    left_frame  <- tkframe(main_frame)
    right_frame <- tkframe(main_frame)

    tkgrid(main_frame, sticky = "nsw")
    tkgrid(left_frame, right_frame, sticky = "nsw")

    # Listbox
    var_y_box <-
        bs_listbox(
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

    tkgrid(upper_frame, sticky = "nw")
    tkgrid(scope_Frame, sticky = "nw")

    # Name
    init_name <-
        str_c(.ds, "_rm_na_rows") %>%
        str_trunc(50, ellipsis = "") %>%
        unique_obj_names()

    name_box <- bs_entry(
        left_frame,
        width = 32,
        value = init_name,
        label = "New dataset's name:",
        label_position = "above"
    )

    tkgrid(name_box$frame, sticky = "ws", pady = c(60, 0))


    # Information

    label_bottom <- bs_label(top, text = "\n\n")
    tkgrid(label_bottom, pady = c(10, 0), sticky = "ws")

    # Finalize ---------------------------------------------------------------
    ok_cancel_help(helpSubject = "drop_na", helpPackage = "tidyr",
                   reset = "window_rows_rm_with_na()")

    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_activation()
}
