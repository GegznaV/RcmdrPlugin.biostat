#' @rdname Menu-window-functions
#' @export
#' @keywords internal
#'
# Based on RcmdrPlugin.EZR::StatMedCopyDataset

window_data_obj_copy <- function() {
    # dataSets <- listDataSets()
    dataSets <- objects(envir = .GlobalEnv)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    active_ds <- ActiveDataSet()
    initializeDialog(title = "Copy Object")

    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_new_name <- function(variables) {
        new_name <-
            getSelection(dataSet1Box) %>%
            str_c("_copy") %>%
            unique_obj_names(all_numbered = TRUE)

        tclvalue(dsname) <- new_name
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dsname <- tclVar("")

    dsnameFrame <- tkframe(top)
    entryDsname <- ttkentry(dsnameFrame,
                            width = "28",
                            textvariable = dsname)

    dataSet1Box <-
        variableListBox2(
            top,
            dataSets,
            listHeight = 9,
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
        dsnameValue <- trim.blanks(tclvalue(dsname))
        obj_name    <- getSelection(dataSet1Box)

        closeDialog()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (dsnameValue == obj_name) {
            errorCondition(recall = window_data_obj_copy,
                           message = gettext(
                               domain = "R-RcmdrPlugin.EZR",
                               "You must enter a different object name."
                           ))
            return()
        }
        if (dsnameValue == "") {
            errorCondition(recall = window_data_obj_copy,
                           message = gettext(
                               domain = "R-RcmdrPlugin.EZR",
                               "You must enter the name of an object."
                           ))
            return()
        }
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall = window_data_obj_copy,
                           message = paste0(
                               "\"", dsnameValue, "\" ",
                               gettext(
                                   domain = "R-RcmdrPlugin.EZR",
                                   "is not a valid name."
                               )
                           ))
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(
                dsnameValue,
                gettext(domain = "R-RcmdrPlugin.EZR", "Object")))) {
                window_data_obj_copy()
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(obj_name) == 0) {
            errorCondition(recall = window_data_obj_copy,
                           message = gettext(
                               domain = "R-RcmdrPlugin.EZR",
                               "You must select an object."
                           ))
            return()
        }

        # Construct the command ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command <- str_glue(
            "## Copy object \n",
            "{dsnameValue} <- {obj_name}"
        )

        doItAndPrint(command)

        # if (is.data.frame(get(dsnameValue, envir = .GlobalEnv))) {
        #     activeDataSet(dsnameValue)
        # }

        tkfocus(CommanderWindow())
    }
    # ========================================================================
    OKCancelHelp()

    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")

    tkgrid(label_rcmdr(
        top,
        text = gettextRcmdr("Copy object"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(getFrame(dataSet1Box))

    tkgrid(
        labelRcmdr(dsnameFrame,
                   fg = getRcmdr("title.color"),
                   text = gettext(
                       domain = "R-RcmdrPlugin.EZR",
                       "Name of object's copy:  "
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
