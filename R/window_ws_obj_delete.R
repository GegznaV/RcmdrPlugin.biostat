#' @rdname Menu-window-functions
#' @export
#' @keywords internal
#'

window_data_obj_delete <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        obj_names <- getSelection(dataSet1Box)

        closeDialog()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(obj_names) == 0) {
            Message("No objects were deleted.", type = "note")
            return()

        } else {
            obj_names_str <- str_c(obj_names, collapse = ", ")

            if (length(obj_names) == 1) {
                msg <- str_glue(
                    "Do you want to permanently DELETE ",
                    'the object \n"{obj_names_str}"?')

            } else {
                msg <- str_glue(
                    "Do you want to permanently DELETE ",
                    "{length(obj_names)} following objects: \n\n",
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
        }

        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ws_objects  <- objects(envir = .GlobalEnv)
    active_ds <- ActiveDataSet()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = "Delete Objects")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dataSet1Box <- bs_listbox(
        parent       = top,
        title        = "Objects to delete\n(pick one or several)",
        values       = ws_objects,
        value        = active_ds,
        selectmode   = "multiple",
        height       = 9,
        width        = 24,
        on_keyboard  = "scroll",
        tip  = str_c("Hold 'Ctrl' key and left-click mouse\n",
                     "to select several objects or deselect.\n",
                     "Use letters on keyboard for quicker \n",
                     "navigation.")

    )

    if (!is.null(active_ds)) {
        set_see(dataSet1Box, which(ws_objects %in% active_ds))
    }

    # dataSet1Box <- variableListBox2(
    #     selectmode = "multiple",
    #
    #     listHeight = 9,
    #     top,
    #     ws_objects,
    #     title = "Objects\n(pick one or several)",
    #
    #     initialSelection = NULL
    # )

    # ========================================================================
    OKCancelHelp()

    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")

    tkgrid(label_rcmdr(
        top,
        text = gettext_bs("Delete objects"),
        font = tkfont.create(weight = "bold", size = 9),
        fg   = "darkred"),
        pady = c(5, 9))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(getFrame(dataSet1Box), sticky = "e")

    tkgrid(buttonsFrame, columnspan = 2)
    dialogSuffix()
}
