#' @rdname Menu-window-functions
#' @export
#' @keywords internal
#'

window_data_obj_delete <- function() {
    dataSets  <- objects(envir = .GlobalEnv)
    active_ds <- activeDataSet()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = "Delete Objects")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dataSet1Box <- variableListBox2(
        selectmode = "multiple",

        listHeight = 9,
        top,
        dataSets,
        title = "Objects\n(pick one or several)",

        initialSelection = NULL
    )
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
                if (active_ds %in% obj_names) {
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
    # ========================================================================
    OKCancelHelp()

    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")

    tkgrid(label_rcmdr(
        top,
        text = gettextRcmdr("Delete objects"),
        font = tkfont.create(weight = "bold", size = 9),
        fg   = "darkred"),
        pady = c(5, 9))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(getFrame(dataSet1Box))

    tkgrid(buttonsFrame, columnspan = 2)
    dialogSuffix()
}
