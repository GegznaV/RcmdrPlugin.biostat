#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_join <- function() {
    dataSets <- listDataSets()
    .activeDataSet <- ActiveDataSet()
    initializeDialog(title = gettextRcmdr("Bind Datasets"))
    dsname <- tclVar("new_dataset")
    by_x_name <- tclVar("")
    by_y_name <- tclVar("")


    names_Frame <- tkframe(top)

    dsnameFrame <- tkframe(names_Frame)
    entryDsname <- ttkentry(dsnameFrame, width = "20", textvariable = dsname)
    entry_by_x_name <- ttkentry(dsnameFrame, width = "20", textvariable = by_x_name)
    entry_by_y_name <- ttkentry(dsnameFrame, width = "20", textvariable = by_y_name)


    middle_Frame <- tkframe(top)
    radioButtons(
        middle_Frame,
        "join_type",
        title = gettextRcmdr("Join type:"),
        labels = gettextRcmdr(c("Full join", "Left join", "Right join", "Inner join", "Semi join", "Anti join")),
        buttons = c("full_join", "left_join", "right_join",  "inner_join", "semi_join",  "anti_join")
    )

    dataSet1Box <-
        variableListBox(
            middle_Frame,
            dataSets,
            title = gettextRcmdr("First dataset (left, x) \n(pick one)"),
            initialSelection = if (is.null(.activeDataSet)) {
                NULL
            } else {
                which(.activeDataSet == dataSets) - 1
            }
        )
    dataSet2Box <-
        variableListBox(middle_Frame,
                        dataSets,
                        title = gettextRcmdr("First dataset (right, y) \n(pick one)"))
    # commonVar <- tclVar("0")
    # commonFrame <- tkframe(top)
    # commonButton <- ttkcheckbutton(commonFrame, variable = commonVar)

    onOK <- function() {
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(
                recall = window_dataset_join,
                message = gettextRcmdr("You must enter the name of a new dataset.")
            )
            return()
        }
        if (!is.valid.name(dsnameValue)) {
            errorCondition(
                recall = window_dataset_join,
                message = glue::glue('"{dsnameValue}" ', gettextRcmdr("is not a valid name."))
            )
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Dataset")))) {
                closeDialog()
                window_dataset_join()
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        by_x_nameValue <- trim.blanks(tclvalue(by_x_name))
        by_y_nameValue <- trim.blanks(tclvalue(by_y_name))
        if (by_x_nameValue == "" & by_y_nameValue == "" ) {
            by_ <- ""

        } else if (by_x_nameValue != "" & by_y_nameValue == "") {
            by_ <- glue::glue(', by = "{by_x_nameValue}"')

        } else if (by_x_nameValue == "" & by_y_nameValue != "") {
            by_ <- glue::glue(', by = "{by_y_nameValue}"')
        } else {
            by_ <- glue::glue(', by = c("{by_x_nameValue}" = "{by_y_nameValue}")')

        }


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        name1 <- getSelection(dataSet1Box)
        name2 <- getSelection(dataSet2Box)
        if (length(name1) == 0) {
            errorCondition(
                recall = window_dataset_join,
                message = gettextRcmdr("You must select a dataset (left).")
            )
            return()
        }
        if (length(name2) == 0) {
            errorCondition(
                recall = window_dataset_join,
                message = gettextRcmdr("You must select a dataset (right).")
            )
            return()
        }
        if (name1 == name2) {
            errorCondition(
                recall = window_dataset_join,
                message = gettextRcmdr("You cannot merge a dataset with itself.")
            )
            return()
        }
        # common <- if (tclvalue(commonVar) == "1") {
        #     TRUE
        # } else {
        #     FALSE
        # }
        join_type <- tclvalue(join_typeVariable)
        command <- glue::glue("{dsnameValue} <- dplyr::{join_type}({name1}, {name2}{by_})")
        doItAndPrint(command)

        activeDataSet(dsnameValue)
        closeDialog()
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "join")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(names_Frame, pady = c(0, 10), sticky = "sw", columnspan = 3)
    tkgrid(labelRcmdr(dsnameFrame,
                      text = gettextRcmdr("Name for resulting dataset:  ")),
           entryDsname, pady = c(0, 15))

    tkgrid(labelRcmdr(dsnameFrame,
                      text = gettextRcmdr("Variable name in x to join by:  \n(or leave blank)")),
           entry_by_x_name, sticky = "n",
           pady = c(0, 5))

    tkgrid(labelRcmdr(dsnameFrame,
                      text = gettextRcmdr("Variable name in y to join by:  \n(or leave blank)")),
           entry_by_y_name, sticky = "n")

    tkgrid(dsnameFrame, sticky = "w", pady = c(15, 5))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(middle_Frame, sticky = "sw", columnspan = 3)
    tkgrid(getFrame(dataSet1Box),  getFrame(dataSet2Box), join_typeFrame,
           sticky = "nw", padx = c(0, 10))

    # tkgrid(
    #     commonButton,
    #     labelRcmdr(commonFrame, text = gettextRcmdr("Merge only common\nrows or columns")),
    #     sticky = "nw"
    # )

    tkgrid(buttonsFrame, sticky = "w", columnspan = 3)
    dialogSuffix()
}