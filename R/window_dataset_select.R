#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_select <- function() {
    dataSets <- listDataSets()
    .ds <- ActiveDataSet()
    # if ((length(dataSets) == 1) && !is.null(.ds)) {
    #     Message(
    #         message = gettext_bs("There is only one dataset in memory."),
    #         type = "warning"
    #     )
    #     tkfocus(CommanderWindow())
    #     return()
    # }


    initializeDialog(title = gettext_bs("Select Dataset"))

    tk_title(top, "Select a dataset")

    var_ds_box <-
        variableListBox2(
            parentWindow = top,
            variableList = dataSets,
            listHeight   = 7,
            listWidth    = c(25, Inf),
            title        = gettext_bs("Datasets (pick one)"),
            initialSelection = if (is.null(.ds)) NULL else which(.ds == dataSets) - 1
        )


    import_buttons_frame <- tkframe(top)

    b1 <- tk2button(import_buttons_frame,
                    text = "Create",
                    width = 8,
                    command = function() {
                        closeDialog()
                        window_dataset_new_rcmdr()
                    })

    b2 <- tk2button(import_buttons_frame,
                    text = "Paste",
                    width = 8,
                    command = function() {
                        window_import_text_delim(init_location = "clipboard")
                        closeDialog()
                    })

    b3 <- tk2button(import_buttons_frame,
                    text = "Import",
                    width = 8,
                    command = function() {
                        cat("Import\n")
                        closeDialog()
                    })


    onOK <- function() {
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))


        if (length(dataSets) == 0) {
            RcmdrTkmessageBox(
                "There are no datasets in R memory.\nPlease, create or import a dataset.",
                icon = "warning",
                title = "No Dataset Found in R",
                type = "ok")

            return()
        }


        if (get_selection_length(var_ds_box) == 0) {
            RcmdrTkmessageBox("Please, select a dataset.",
                              title = "Dataset Not Selected",
                              icon = "warning",
                              type = "ok")
            return()
        }

        selection <- getSelection(var_ds_box)
        closeDialog()

        active_dataset(selection)
        # active_datatet(selection)
        tkfocus(CommanderWindow())
    }

    ok_cancel_help()

    tkgrid(getFrame(var_ds_box))

    tkgrid(b1, b2, b3)
    tkgrid(import_buttons_frame, pady = c(10, 0))

    tkgrid(buttonsFrame, pady = c(5, 0))

    dialogSuffix()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(dataSets) == 0) {
        tk_disable(var_ds_box)
    }

}