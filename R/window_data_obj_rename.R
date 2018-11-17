#' @rdname Menu-window-functions
#' @export
#' @keywords internal
#'
# Based on RcmdrPlugin.EZR::StatMedCopyDataset

window_data_obj_rename <- function() {
    # dataSets <- listDataSets()
    dataSets  <- objects(envir = .GlobalEnv)
    active_ds <- ActiveDataSet()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = "Rename Object")

    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_new_name <- function(variables) {
        tclvalue(dsname) <-
            getSelection(var_y_box) %>%
            unique_obj_names()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dsname <- tclVar("")

    dsnameFrame <- tkframe(top)
    entryDsname <- ttkentry(dsnameFrame,
                            width = "28",
                            textvariable = dsname)

    var_y_box <-
        variableListBox2(
            listHeight = 6,
            top,
            dataSets,
            onRelease_fun = update_new_name,
            title = "Object\n(pick one)",

            initialSelection =
                if (is.null(active_ds)) {
                    NULL
                } else {
                    which(active_ds == dataSets) - 1
                }
        )

    update_new_name()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        new_obj_names <- trim.blanks(tclvalue(dsname))
        obj_names     <- getSelection(var_y_box)

        closeDialog()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (any(new_obj_names %in% obj_names)) {
            errorCondition(recall = window_data_obj_rename,
                           message = gettext(domain = "R-RcmdrPlugin.EZR",
                                             "You must enter a different object name."
            ))
            return()
        }

        if (new_obj_names == "") {
            errorCondition(
                recall = window_data_obj_rename,
                message = gettext(
                    domain = "R-RcmdrPlugin.EZR",
                    "You must enter the name of an object."
                ))
            return()
        }

        if (!is.valid.name(new_obj_names)) {
            errorCondition(recall = window_data_obj_rename,
                           message = paste(
                               "\"", new_obj_names, "\" ",
                               gettext(
                                   domain = "R-RcmdrPlugin.EZR",
                                   "is not a valid name."
                               ),
                               sep = ""
                           ))
            return()
        }
        if (is.element(new_obj_names, listDataSets())) {
            if ("no" == tclvalue(checkReplace(
                new_obj_names,
                gettext(domain = "R-RcmdrPlugin.EZR", "Object")))) {

                window_data_obj_rename()
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(obj_names) == 0) {
            errorCondition(recall = window_data_obj_rename,
                           message = gettext(domain = "R-RcmdrPlugin.EZR",
                                             "You must select an object."
            ))
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Deselect active dataset if it should be deleted.
        if (isTRUE(any(active_ds %in% obj_names))) {
            ActiveDataSet(NULL)
            ds_ind <- which(active_ds %in% obj_names)
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        obj_names_str <- str_c(obj_names, collapse = ", ")

        # Construct the command ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command_0 <- str_glue("{new_obj_names} <- {obj_names}\n")
        command   <- str_glue(
            "## Rename object \n",
            "{command_0}\n",
            "remove({obj_names_str})"
        )

        doItAndPrint(command)

        if (length(ds_ind) > 0) {
            activeDataSet(new_obj_names[ds_ind])
        }

        tkfocus(CommanderWindow())
    }
    # ========================================================================
    OKCancelHelp()

    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")

    tkgrid(label_rcmdr(
        top,
        text = gettextRcmdr("Rename object"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(getFrame(var_y_box))

    tkgrid(
        labelRcmdr(dsnameFrame,
                   fg = fg_col,
                   text = gettext(
                       domain = "R-RcmdrPlugin.EZR",
                       "New name:  "
                   )),
        pady = c(5, 0),
        sticky = "w"
    )

    tkgrid(
        entryDsname,
        pady = c(0, 5)
    )

    tkgrid(dsnameFrame, columnspan = 2)
    tkgrid(buttonsFrame, columnspan = 2)
    dialogSuffix()
}
