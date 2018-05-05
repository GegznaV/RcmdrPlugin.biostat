# TODO:
# 1. Add dadiobuttons with values: "Use numeric ID" "Use named ID"
# 2. If "Use named ID" is selected, then values of `ds_x_as_name` and `ds_y_as_name`
#    and appropriate textbox values should change if name of a dataset is
#    double-clicked (or mouse released) in appropriate variable box
#    (dataset `ds_x` and `ds_y`).
#

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_bind_rows <- function() {
    dataSets <- listDataSets()
    .activeDataSet <- ActiveDataSet()

    # [!!!] functions to get and put diaglog are needed.
    initializeDialog(title = gettextRcmdr("Bind Rows of Datasets"))
    dsname <- tclVar(unique_df_name("new_dataset_binded_by_rows"))
    idname <- tclVar(".id")

    # [!!!] Check id name does not have duplicated names in any of datasets in : idname


    names_Frame <- tkframe(top)
    entry_dsname <- ttkentry(names_Frame, width = "32", textvariable = dsname)

    var_name_Frame <- tkframe(top)
    entry_idname <- ttkentry(var_name_Frame, width = "62", textvariable = idname)

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
                        title = gettextRcmdr("Second dataset (right)\n(pick one)"))
    # commonVar <- tclVar("0")
    # commonFrame <- tkframe(top)
    # commonButton <- ttkcheckbutton(commonFrame, variable = commonVar)


    ds_x_as_name <- tclVar("")
    ds_y_as_name <- tclVar("")
    ds_x_as <- ttkentry(top, width = "20", textvariable = ds_x_as_name)
    ds_y_as <- ttkentry(top, width = "20", textvariable = ds_y_as_name)

    onOK <- function() {
        dsnameValue <- trim.blanks(tclvalue(dsname))
        idnameValue <- trim.blanks(tclvalue(idname))
        ds_x_as_name_Value <- trim.blanks(tclvalue(ds_x_as_name))
        ds_y_as_name_Value <- trim.blanks(tclvalue(ds_y_as_name))

        if (dsnameValue == "") {
            errorCondition(
                recall = window_dataset_bind_rows,
                message = gettextRcmdr("You must enter the name of the new dataset.")
            )
            return()
        }
        if (!is.valid.name(dsnameValue)) {
            errorCondition(
                recall = window_dataset_bind_rows,
                message = glue::glue('"{dsnameValue}" ', gettextRcmdr("is not a valid name."))
            )
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Dataset")))) {
                closeDialog()
                window_dataset_bind_rows()
                return()
            }
        }
        name1 <- getSelection(dataSet1Box)
        name2 <- getSelection(dataSet2Box)
        if (length(name1) == 0) {
            errorCondition(
                recall = window_dataset_bind_rows,
                message = gettextRcmdr("You must select a dataset (left).")
            )
            return()
        }
        if (length(name2) == 0) {
            errorCondition(
                recall = window_dataset_bind_rows,
                message = gettextRcmdr("You must select a dataset (right).")
            )
            return()
        }
        # if (name1 == name2) {
        #     errorCondition(
        #         recall = window_dataset_bind_rows,
        #         message = gettextRcmdr("You cannot bind a dataset with itself.")
        #     )
        #     return()
        # }


        rename1 <- if (ds_x_as_name_Value %in% c("")) {
            ""
        } else {
            glue::glue("`{ds_x_as_name_Value}` = ")
        }

        rename2 <- if (ds_y_as_name_Value %in% c("")) {
            ""
        } else {
            glue::glue("`{ds_y_as_name_Value}` = ")
        }

        # If only one is set,
        # the problem is that `dplyr` resets both names to numbers
        if (xor(rename1 != "", rename2 != "")) {
            if (rename1 == "") {
                rename1 <- glue::glue("`{name1}` = ")
            }
            if (rename2 == "") {
                rename2 <- glue::glue("`{name2}` = ")
            }

        }


        if (idnameValue %in% c("")) {
            # No .id variable
            command <- style_cmd(glue::glue(
                "{dsnameValue} <- dplyr::bind_rows(\n{rename1}{name1}, {rename2}{name2})"))

        } else {
            # Use .id variable
            command <- style_cmd(glue::glue(
                "{dsnameValue} <- dplyr::bind_rows(\n{rename1}{name1}, {rename2}{name2}, ",
                ".id = '{idnameValue}')"))
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
    OKCancelHelp(helpSubject = "bind_rows", helpPackage = "dplyr")

    tkgrid(labelRcmdr(names_Frame,
                      text = gettextRcmdr("Name for the resulting dataset:  ")),
           entry_dsname)

    # tkgrid(names_Frame, sticky = "w", pady = c(15, 5))
    tkgrid(names_Frame, pady = c(0, 10), columnspan = 3, sticky = "sw")

    tkgrid(getFrame(dataSet1Box),  getFrame(dataSet2Box), sticky = "nwe")

    tkgrid(
        labelRcmdr(top, fg = getRcmdr("title.color"),
                   text = gettextRcmdr("Name in ID column\n(optional):")),
        labelRcmdr(top, fg = getRcmdr("title.color"),
                   text = gettextRcmdr("Name in ID column\n(optional):")),
        sticky = "w")

    tkgrid(ds_x_as, ds_y_as, sticky = "w")

    tkgrid(var_name_Frame, pady = c(0, 10), columnspan = 3, sticky = "sw")

    tkgrid(labelRcmdr(var_name_Frame,
                      fg = getRcmdr("title.color"),
                      text = gettextRcmdr(
                          paste0("ID variable name\n",
                                 "(stores names of original datasets; ",
                                 "leave blank to exclude the variable)"))),
           pady = c(15, 0), sticky = "w")

    tkgrid(entry_idname, sticky = "w")

    # tkgrid(
    #     commonButton,
    #     labelRcmdr(commonFrame,
    #                text = gettextRcmdr("Merge only common\nrows or columns")),
    #     sticky = "nw"
    # )

    tkgrid(buttonsFrame, sticky = "we", columnspan = 3)
    dialogSuffix()
}