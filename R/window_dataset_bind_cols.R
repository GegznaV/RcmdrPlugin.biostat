#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_bind_cols <- function() {
    dataSets <- listDataSets()
    .activeDataSet <- ActiveDataSet()

    # [!!!] functions to get and put diaglog are needed.
    # [!!!] Show number of rows in each dataset beneeth the dataset selection box.

    initializeDialog(title = gettextRcmdr("Bind Columns of Datasets"))
    dsname <- tclVar("new_dataset_binded_by_cols")

    names_Frame <- tkframe(top)
    entry_dsname <- ttkentry(names_Frame, width = "30", textvariable = dsname)


    dataSet1Box <-
        variableListBox2(
            top,
            dataSets,
            listHeight = 7,
            title = gettextRcmdr("First dataset (left) \n(pick one)"),
            initialSelection = if (is.null(.activeDataSet)) {
                NULL
            } else {
                which(.activeDataSet == dataSets) - 1
            }
        )
    dataSet2Box <-
        variableListBox2(top,
                        dataSets,
                        listHeight = 7,
                        title = gettextRcmdr("Second dataset (right) \n(pick one)"))
    # commonVar <- tclVar("0")
    # commonFrame <- tkframe(top)
    # commonButton <- ttkcheckbutton(commonFrame, variable = commonVar)

    onOK <- function() {
        dsnameValue <- trim.blanks(tclvalue(dsname))
        idnameValue <- trim.blanks(tclvalue(idname))
        if (dsnameValue == "") {
            errorCondition(
                recall = window_dataset_bind_cols,
                message = gettextRcmdr("You must enter the name of the new dataset.")
            )
            return()
        }
        if (!is.valid.name(dsnameValue)) {
            errorCondition(
                recall = window_dataset_bind_cols,
                message = glue::glue('"{dsnameValue}" ', gettextRcmdr("is not a valid name."))
            )
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Dataset")))) {
                closeDialog()
                window_dataset_bind_cols()
                return()
            }
        }
        name1 <- getSelection(dataSet1Box)
        name2 <- getSelection(dataSet2Box)
        if (length(name1) == 0) {
            errorCondition(
                recall = window_dataset_bind_cols,
                message = gettextRcmdr("You must select a dataset (left).")
            )
            return()
        }
        if (length(name2) == 0) {
            errorCondition(
                recall = window_dataset_bind_cols,
                message = gettextRcmdr("You must select a dataset (right).")
            )
            return()
        }
        # if (name1 == name2) {
        #     errorCondition(
        #         recall = window_dataset_bind_cols,
        #         message = gettextRcmdr("You cannot bind a dataset with itself.")
        #     )
        #     return()
        # }

        if (nrow(eval_glue("{name1}")) != nrow(eval_glue("{name2}"))) {
            errorCondition(
                recall = window_dataset_bind_cols,
                message = gettextRcmdr("To bind by columns, number of rows in each dataset must match.")
            )
            return()
        }


        command <- style_cmd(glue::glue(
            '{dsnameValue} <- dplyr::bind_cols({name1}, {name2})'
        ))

        doItAndPrint(command)


        activeDataSet(dsnameValue)
        closeDialog()
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "bind_cols")


    tkgrid(labelRcmdr(names_Frame, text = gettextRcmdr("Name for the resulting dataset:  ")),
           entry_dsname)

    tkgrid(names_Frame, pady = c(0, 10), columnspan = 3, sticky = "sew")

    tkgrid(getFrame(dataSet1Box), getFrame(dataSet2Box), sticky = "new")

    # tkgrid(
    #     commonButton,
    #     labelRcmdr(commonFrame, text = gettextRcmdr("Merge only common\nrows or columns")),
    #     sticky = "nw"
    # )

    tkgrid(buttonsFrame, sticky = "we", columnspan = 3)
    dialogSuffix()
}