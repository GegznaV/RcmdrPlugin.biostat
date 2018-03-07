#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_bind_rows <- function() {
    dataSets <- listDataSets()
    .activeDataSet <- ActiveDataSet()

    # [!!!] functions to get and put diaglog are needed.
    initializeDialog(title = gettextRcmdr("Bind Rows of Datasets"))
    dsname <- tclVar("new_dataset")
    idname <- tclVar(".original_dataset_name")

    # [!!!] Check id name does not have duplicated names in any of datasets in : idname


    names_Frame <- tkframe(top)
    entry_dsname <- ttkentry(names_Frame, width = "25", textvariable = dsname)
    entry_idname <- ttkentry(names_Frame, width = "25", textvariable = idname)

    dataSet1Box <-
        variableListBox(
            top,
            dataSets,
            title = gettextRcmdr("First dataset (left) \n(pick one)"),
            initialSelection = if (is.null(.activeDataSet)) {
                NULL
            } else {
                which(.activeDataSet == dataSets) - 1
            }
        )
    dataSet2Box <-
        variableListBox(top,
                        dataSets,
                        title = gettextRcmdr("Second dataset (right)\n(pick one)"))
    # commonVar <- tclVar("0")
    # commonFrame <- tkframe(top)
    # commonButton <- ttkcheckbutton(commonFrame, variable = commonVar)

    onOK <- function() {
        dsnameValue <- trim.blanks(tclvalue(dsname))
        idnameValue <- trim.blanks(tclvalue(idname))
        if (dsnameValue == "") {
            errorCondition(
                recall = window_dataset_bind_r,
                message = gettextRcmdr("You must enter the name of the new dataset.")
            )
            return()
        }
        if (!is.valid.name(dsnameValue)) {
            errorCondition(
                recall = window_dataset_bind_r,
                message = glue::glue('"{dsnameValue}" ', gettextRcmdr("is not a valid name."))
            )
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Dataset")))) {
                closeDialog()
                window_dataset_bind_r()
                return()
            }
        }
        name1 <- getSelection(dataSet1Box)
        name2 <- getSelection(dataSet2Box)
        if (length(name1) == 0) {
            errorCondition(
                recall = window_dataset_bind_r,
                message = gettextRcmdr("You must select a dataset (left).")
            )
            return()
        }
        if (length(name2) == 0) {
            errorCondition(
                recall = window_dataset_bind_r,
                message = gettextRcmdr("You must select a dataset (right).")
            )
            return()
        }
        # if (name1 == name2) {
        #     errorCondition(
        #         recall = window_dataset_bind_r,
        #         message = gettextRcmdr("You cannot bind a dataset with itself.")
        #     )
        #     return()
        # }

        if (idnameValue %in% c("")) {
            # No .id variable
            command <- glue::glue(
    "{dsnameValue} <- dplyr::bind_rows({name1}, {name2})")

        } else {
            # Use .id variable
            command <- glue::glue(
    "{dsnameValue} <- dplyr::bind_rows({name1}, {name2}, .id = '{idnameValue}')")
        }

        doItAndPrint(command)

        # doItAndPrint(command)
        # command <- glue::glue("rownames({dsnameValue}) <- {dsnameValue}$Row.names")
        # doItAndPrint(command)
        # command <- glue::glue("{dsnameValue}$Row.names <- NULL")
        # doItAndPrint(command)

        activeDataSet(dsnameValue)
        closeDialog()
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "bind_rows")

    tkgrid(labelRcmdr(names_Frame,
                      text = gettextRcmdr("Name for resulting dataset:  ")),
           entry_dsname)

    tkgrid(labelRcmdr(names_Frame,
                      text = gettextRcmdr("Name for dataset .id variable:\n(leave blank to ignore)  ")),
           entry_idname)

    # tkgrid(names_Frame, sticky = "w", pady = c(15, 5))
    tkgrid(names_Frame, pady = c(0, 10), columnspan = 3, sticky = "sw")

    tkgrid(getFrame(dataSet1Box),  getFrame(dataSet2Box), sticky = "nwe")
    # tkgrid(
    #     commonButton,
    #     labelRcmdr(commonFrame, text = gettextRcmdr("Merge only common\nrows or columns")),
    #     sticky = "nw"
    # )

    tkgrid(buttonsFrame, sticky = "we", columnspan = 3)
    dialogSuffix()
}