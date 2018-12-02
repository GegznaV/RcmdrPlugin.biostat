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
            listHeight   = 8,
            listWidth    = c(26, Inf),
            title        = gettext_bs("Datasets (pick one)"),
            initialSelection = if (is.null(.ds)) NULL else which(.ds == dataSets) - 1
        )


    import_buttons_frame <- tkframe(top)

    b1 <- tk2button(import_buttons_frame,
                    text = "Create new",
                    width = 12,
                    command = function() {
                        closeDialog()
                        window_dataset_new_rcmdr()
                    })

    b2 <- tk2button(import_buttons_frame,
                    text = "From clipboard",
                    width = 14,
                    command = function() {
                        closeDialog()
                        window_import_text_delim0(init_location = "clipboard")
                    })

    b3 <- tk2button(import_buttons_frame,
                    text = "From file",
                    width = 12,
                    command = function() {
                        closeDialog()
                        cat("Import\n")
                    })

    b4 <- tk2button(import_buttons_frame,
                    text = "From package",
                    width = 14,
                    command = function() {
                        closeDialog()
                        window_import_from_pkg()
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

    tkgrid(getFrame(var_ds_box), sticky = "e")

    tkgrid(b1, b2)
    tkgrid(b3, b4)
    tkgrid(import_buttons_frame, pady = c(10, 0), sticky = "e")

    tkgrid(buttonsFrame, pady = c(0, 0))

    dialogSuffix()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(dataSets) == 0) {
        tk_disable(var_ds_box)
    }

}