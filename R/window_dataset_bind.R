#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_bind <- function() {
    dataSets <- listDataSets()
    .activeDataSet <- ActiveDataSet()
    initializeDialog(title = gettextRcmdr("Bind Datasets"))
    dsname <- tclVar("new_dataset")
    idname <- tclVar(".old_dataset_id")


    names_Frame <- tkframe(top)
    entry_dsname <- ttkentry(names_Frame, width = "20", textvariable = dsname)
    entry_idname <- ttkentry(names_Frame, width = "20", textvariable = idname)

    radioButtons(
        top,
        "direction",
        title = gettextRcmdr("Direction:"),
        labels = gettextRcmdr(c("Bind rows", "Bind columns")),
        buttons = c("rows", "columns")
    )


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
                        title = gettextRcmdr("Second dataset (right) \n(pick one)"))
    # commonVar <- tclVar("0")
    # commonFrame <- tkframe(top)
    # commonButton <- ttkcheckbutton(commonFrame, variable = commonVar)

    onOK <- function() {
        dsnameValue <- trim.blanks(tclvalue(dsname))
        idnameValue <- trim.blanks(tclvalue(idname))
        if (dsnameValue == "") {
            errorCondition(
                recall = window_dataset_bind,
                message = gettextRcmdr("You must enter the name of a new dataset.")
            )
            return()
        }
        if (!is.valid.name(dsnameValue)) {
            errorCondition(
                recall = window_dataset_bind,
                message = glue::glue('"{dsnameValue}" ', gettextRcmdr("is not a valid name."))
            )
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Dataset")))) {
                closeDialog()
                window_dataset_bind()
                return()
            }
        }
        name1 <- getSelection(dataSet1Box)
        name2 <- getSelection(dataSet2Box)
        if (length(name1) == 0) {
            errorCondition(
                recall = window_dataset_bind,
                message = gettextRcmdr("You must select a dataset (left).")
            )
            return()
        }
        if (length(name2) == 0) {
            errorCondition(
                recall = window_dataset_bind,
                message = gettextRcmdr("You must select a dataset (right).")
            )
            return()
        }
        # if (name1 == name2) {
        #     errorCondition(
        #         recall = window_dataset_bind,
        #         message = gettextRcmdr("You cannot bind a dataset with itself.")
        #     )
        #     return()
        # }

        # common <- if (tclvalue(commonVar) == "1") {
        #     TRUE
        # } else {
        #     FALSE
        # }
        direction <- tclvalue(directionVariable)
        switch(direction,
               "rows" = {
                   # command <- glue::glue("{dsnameValue} <- mergeRows({name1}, {name2}, common.only = {common})")
                   if (idnameValue == "NULL") {
                       # No .id variable
                       command <- glue::glue("{dsnameValue} <- dplyr::bind_rows({name1}, {name2})")
                   } else {
                       # Use .id variable
                       command <- glue::glue("{dsnameValue} <- dplyr::bind_rows({name1}, {name2}, .id = '{idnameValue}')")
                   }

               },
               "columns" =  {
                   # command <- glue::glue('{dsnameValue} <- merge({name1}, {name2}, all = {!common}, by="row.names")')},
                   command <- glue::glue('{dsnameValue} <- dplyr::bind_cols({name1}, {name2})')
                   },
               # else
               stop("Unrecognized option.")
        )

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


    tkgrid(labelRcmdr(names_Frame, text = gettextRcmdr("Name for resulting dataset:  ")),
           entry_dsname)

    tkgrid(labelRcmdr(names_Frame, text = gettextRcmdr("Name for .id variable:  ")),
           entry_idname)

    # tkgrid(directionFrame, commonFrame, pady = c(0, 10), sticky = "sw")
    # tkgrid(names_Frame, sticky = "w", pady = c(15, 5))
    tkgrid(names_Frame, pady = c(0, 10), columnspan = 3, sticky = "sw")

    tkgrid(getFrame(dataSet1Box),  getFrame(dataSet2Box), directionFrame,
           sticky = "nw")

    # tkgrid(
    #     commonButton,
    #     labelRcmdr(commonFrame, text = gettextRcmdr("Merge only common\nrows or columns")),
    #     sticky = "nw"
    # )

    tkgrid(buttonsFrame, sticky = "w", columnspan = 3)
    dialogSuffix()
}