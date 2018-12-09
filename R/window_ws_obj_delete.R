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
        if (object_is_not_selected(obj_names))   {return()}

        obj_names_str <- str_c(obj_names, collapse = ", ")

        if (length(obj_names) == 1) {
            msg <- str_glue(
                "Do you agree to DELETE object",
                '\n"{obj_names_str}" permanently?')

        } else {
            msg <- str_glue(
                "Do you agree to DELETE {length(obj_names)} following ",
                "objects permanently: \n\n",
                "{obj_names_str}")
        }

        option <-
            tk_messageBox(
                "yesno",
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
                str_glue(
                    "## Delete objects \n",
                    "remove({obj_names_str})"
                ) %>%
                style_cmd()
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            doItAndPrint(command)

            Message(
                str_c("The object(s) were deleted: \n", obj_names_str),
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

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    var_y_box <- bs_listbox(
        parent       = top,
        title        = "Objects to delete\n(pick one or several)",
        values       = ws_objects,
        value        = active_ds,
        selectmode   = "multiple",
        height       = 9,
        width        = 30,
        on_keyboard  = "scroll",
        tip  = str_c("Hold 'Ctrl' key and left-click mouse\n",
                     "to select several objects or deselect.\n",
                     "Use letters on keyboard for quicker \n",
                     "navigation.")

    )

    if (!is.null(active_ds)) {
        tk_see(var_y_box, which(ws_objects %in% active_ds))
    }

    # ========================================================================
    ok_cancel_help()

    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")

    tkgrid(label_rcmdr(
        top,
        text = gettext_bs("Delete objects"),
        font = tkfont.create(weight = "bold", size = 9),
        fg   = "darkred"),
        pady = c(5, 9))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(getFrame(var_y_box), sticky = "e")

    tkgrid(buttonsFrame, columnspan = 2, sticky = "ew")
    dialogSuffix()
}
