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

    ds_selection_callback  <- function() {
        if (get_size(var_ds_box) == 0 || get_selection_length(var_ds_box) == 0) {
            tk_disable(bi1)
            tk_disable(bi2)
            tk_disable(bi3)
            tk_disable(bi4)

        } else {
            tk_normalize(bi1)
            tk_normalize(bi2)
            tk_normalize(bi3)
            tk_normalize(bi4)
        }

    }

    initializeDialog(title = gettext_bs("Select Dataset"))

    tk_title(top, "Select a dataset")

    var_ds_box <-
        variableListBox2(
            parentWindow     = top,
            variableList     = dataSets,
            listHeight       = 8,
            listWidth        = c(26, Inf),
            onRelease_fun    = ds_selection_callback,
            title            = gettext_bs("Datasets (pick one)"),
            initialSelection = if (is.null(.ds)) NULL else which(.ds == dataSets) - 1
        )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    info_buttons_frame <- tkframe(top)

    bi1 <- tk2button(
        info_buttons_frame,
        text = "Size",
        width = 0,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)
            doItAndPrint(str_glue(
                "## The size of dataset '{.ds_1}' (rows, columns)\n",
                "dim({.ds_1})"))
        })

    bi2 <- tk2button(
        info_buttons_frame,
        text = "Class",
        width = 0,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)

            doItAndPrint(str_glue(
                "## The class of dataset '{.ds_1}'\n",
                "class({.ds_1})"))
        })

    bi3 <- tk2button(
        info_buttons_frame,
        text = "Structure",
        width = 0,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)

            doItAndPrint(str_glue(
                "## The structure of dataset '{.ds_1}'\n",
                "glimpse({.ds_1})"))
        })

    bi4 <- tk2button(
        info_buttons_frame,
        text = "Preview",
        width = 0,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)
            doItAndPrint(str_glue(
                "## Preview dataset '{.ds_1}'\n",
                "View({.ds_1})"
            ))
        })

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    import_buttons_frame <- tkframe(top)

    b1 <- tk2button(
        import_buttons_frame,
        text = "Create new",
        width = 12,
        command = function() {
            closeDialog()
            window_dataset_new_rcmdr()
        })

    b2 <- tk2button(
        import_buttons_frame,
        text = "From clipboard",
        width = 14,
        command = function() {
            closeDialog()
            window_import_text_delim0(init_location = "clipboard")
        })

    b3 <- tk2button(
        import_buttons_frame,
        text = "From file",
        width = 12,
        command = function() {
            closeDialog()
            cat("Import\n")
        })

    b4 <- tk2button(
        import_buttons_frame,
        text = "From package",
        width = 14,
        command = function() {
            closeDialog()
            window_import_from_pkg()
        })
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

    tkgrid(bi1, bi2, bi3, bi4)
    tkgrid(info_buttons_frame, pady = c(10, 0), sticky = "e")

    tkgrid(b1, b2)
    tkgrid(b3, b4)
    tkgrid(import_buttons_frame, pady = c(5, 0), sticky = "e")

    tkgrid(buttonsFrame, pady = c(0, 0))

    dialogSuffix()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(dataSets) == 0) {
        tk_disable(var_ds_box)

    } else if (!is.null(.ds)) {
        set_new_selection(var_ds_box, .ds)
        tkyview(var_ds_box$listbox, which(get_values(var_ds_box) == .ds) - 1)
    }

    ds_selection_callback()
}
