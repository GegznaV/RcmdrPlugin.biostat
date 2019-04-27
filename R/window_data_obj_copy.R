#' @rdname Menu-window-functions
#' @export
#' @keywords internal
#'
# TODO:
# 1. Add checkbox to toggle between non-hidden and all objects.
#

window_data_obj_copy <- function() {
    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        obj_names     <- get_selection(var_y_box)
        new_obj_names <- get_values(text_box_1)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (object_is_not_selected(obj_names, "object"))   {return()}

        if (is_not_valid_name(new_obj_names))              {return()}
        if (forbid_to_replace_object(new_obj_names))       {return()}
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Deselect active dataset if it should be copied
        if (isTRUE(any(active_ds %in% obj_names))) {active_dataset_0(NULL)}
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        obj_names_str <- str_c(obj_names, collapse = ", ")

        # Construct the command ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command_0 <- str_glue("{new_obj_names} <- {obj_names}\n")
        command   <- str_glue(
            "## Creato a copy of the object \n",
            "{command_0}\n"
        )

        result <- justDoIt(command)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            logger(style_cmd(command))

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
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_new_name <- function(variables) {
        tclvalue(text_box_1) <-
            get_selection(var_y_box) %>%
            unique_obj_names()
    }

    update_obj_list <- function(variables) {
        ws_objects <- objects(envir = .GlobalEnv, all.names = TRUE)
        set_values(var_y_box,  ws_objects)
        set_values(text_box_1, "")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Initialize =============================================================
    initializeDialog(title = "Create a Copy of an Object")
    tk_title(top, "Create a Copy of an Object")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ws_objects <- objects(envir = .GlobalEnv, all.names = TRUE)
    active_ds  <- active_dataset_0()

    # Widgets ================================================================
    upper_frame <- tkframe(top)
    tkgrid(upper_frame)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    var_y_box <- bs_listbox(
        parent       = upper_frame,
        title        = "Object (select one)",
        values       = ws_objects,
        value        = active_ds,
        selectmode   = "single",
        height       = 9,
        width        = 30,
        on_keyboard  = "select",
        on_select    = update_new_name,
        tip          = tip_single_ctrl_letters
    )
    if (!is.null(active_ds)) {
        tk_see(var_y_box, which(ws_objects %in% active_ds))
    }

    tkgrid(var_y_box$frame, sticky = "w", padx = c(10, 0))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text_box_1 <- bs_tk_textbox(
        parent = upper_frame,
        width  = 30,
        label  = "Name of object's copy:  ",
        label_position = "above",
        tip = "Enter a new name for the selected object.")

    tkgrid(text_box_1$frame, sticky = "w", padx = c(10, 0))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(apply = "window_data_obj_copy")
    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_new_name()
}
