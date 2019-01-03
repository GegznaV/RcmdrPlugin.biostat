#' @rdname Menu-window-functions
#' @export
#' @keywords internal
#'

window_data_obj_delete <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        obj_names <- get_selection(var_y_box)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (object_is_not_selected(obj_names))   {
            return()
        }

        obj_names_str <- str_c(obj_names, collapse = ", ")

        if (length(obj_names) == 1) {
            msg <- str_glue("Do you agree to DELETE object",
                            '\n"{obj_names_str}" permanently?')

        } else {
            msg <- str_glue(
                "Do you agree to DELETE {length(obj_names)} following ",
                "objects permanently: \n\n",
                "{obj_names_str}"
            )
        }

        option <-
            tk_messageBox(
                parent = top,
                type = "yesno",
                default = "no",
                caption = "Delete objects:",
                message = msg
            )
        if (option != "yes") {
            Message("No objects were deleted.", type = "note")
            return()

        } else {
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Deselect active dataset if it should be deleted.
            if (isTRUE(any(active_ds %in% obj_names))) {
                ActiveDataSet(NULL)
            }

            # Construct the command ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            command <-
                str_glue("## Delete objects \n",
                         "remove({obj_names_str})") %>%
                style_cmd()
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            doItAndPrint(command)

            Message(str_c("The object(s) were deleted: \n", obj_names_str),
                    type = "warn")
        }

        closeDialog()
        tkfocus(CommanderWindow())

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ws_objects <- objects(envir = .GlobalEnv)
    active_ds  <- ActiveDataSet()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = "Delete Objects")
    tk_title(top, text = "Delete objects", fg = "darkred")


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    var_y_box <- bs_listbox(
        parent       = top,
        title        = "Objects to delete\n(pick one or several)",
        values       = ws_objects,
        value        = active_ds,
        selectmode   = "multiple",
        height       = 8,
        width        = 30,
        on_keyboard  = "scroll",
        tip          = tip_multiple_ctrl_letters,
        use_filter   = TRUE

    )

    if (!is.null(active_ds)) {
        tk_see(var_y_box, which(ws_objects %in% active_ds))
    }

    tkgrid(getFrame(var_y_box), pady = c(0, 5))


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(apply = "window_data_obj_delete")
    tkgrid(buttonsFrame)
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
