#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_join <- function() {

    dataSets <- listDataSets()
    .activeDataSet <- ActiveDataSet()

    initializeDialog(title = gettextRcmdr("Join Two Datasets"))

    ds_name <- tclVar("new_joint_dataset")
    ds_name_Frame <- tkframe(top)
    entry_ds_name <- ttkentry(ds_name_Frame, width = "37", textvariable = ds_name)

    middle_Frame <- tkframe(top)
    radioButtons(
        middle_Frame,
        "join_type",
        title = gettextRcmdr("Join type: "),
        labels = gettextRcmdr(c(
            "Full join",
            "Left join",
            "Right join",
            "Inner join",
            "Semi join",
            "Anti join"
        )),
        buttons = c("full_join",
                    "left_join",
                    "right_join",
                    "inner_join",
                    "semi_join",
                    "anti_join")
    )


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    by_x_name_1 <- tclVar("")
    by_y_name_1 <- tclVar("")
    by_x_name_2 <- tclVar("")
    by_y_name_2 <- tclVar("")
    by_x_name_3 <- tclVar("")
    by_y_name_3 <- tclVar("")

    var_names_Frame <- tkframe(top)

    # v_x1 <- inputComboBox(var_names_Frame,
    #                       variableList = c(""),
    #                       # variableList = c("", getSelection(dataSet1Box)),
    #                       initialSelection = "")
    #
    # v_y1 <- inputComboBox(var_names_Frame,
    #                       variableList = c("", getSelection(dataSet2Box)),
    #                       initialSelection = "")




    entry_by_x_name_1 <- ttkentry(var_names_Frame, width = "20", textvariable = by_x_name_1)
    entry_by_y_name_1 <- ttkentry(var_names_Frame, width = "20", textvariable = by_y_name_1)
    entry_by_x_name_2 <- ttkentry(var_names_Frame, width = "20", textvariable = by_x_name_2)
    entry_by_y_name_2 <- ttkentry(var_names_Frame, width = "20", textvariable = by_y_name_2)
    entry_by_x_name_3 <- ttkentry(var_names_Frame, width = "20", textvariable = by_x_name_3)
    entry_by_y_name_3 <- ttkentry(var_names_Frame, width = "20", textvariable = by_y_name_3)




    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Interactive functions

    cmd_onRelease_ds_1 <- function() {
        # On mouse relese select/deselect checkbox
        if (length(getSelection(gr_var_Box)) == 0) {
            tclvalue(by_groupsVariable) <- "0"
            tkconfigure(by_groupsCheckBox, state = "disabled")

        } else {
            tclvalue(by_groupsVariable) <- "1"
            tkconfigure(by_groupsCheckBox, state = "active")
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    dataSet1Box <-
        variableListBox2(
            middle_Frame,
            dataSets,
            listHeight = 7,
            title = gettextRcmdr("First dataset (left, x) \n(pick one)"),
            initialSelection = if (is.null(.activeDataSet)) {
                NULL
            } else {
                which(.activeDataSet == dataSets) - 1
            }
        )

    dataSet2Box <-
        variableListBox2(middle_Frame,
                         dataSets,
                         listHeight = 7,
                         title = gettextRcmdr("Second dataset (right, y) \n(pick one)"))

    # commonVar <- tclVar("0")
    # commonFrame <- tkframe(top)
    # commonButton <- ttkcheckbutton(commonFrame, variable = commonVar)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        ds_nameValue <- trim.blanks(tclvalue(ds_name))

        # Check if the new name is not missing
        if (ds_nameValue == "") {
            errorCondition(
                recall = window_dataset_join,
                message = gettextRcmdr("You must enter the name of the new joint dataset.")
            )
            return()
        }

        # Check if the new name is valid
        if (!is.valid.name(ds_nameValue)) {
            errorCondition(
                recall = window_dataset_join,
                message = glue::glue('"{ds_nameValue}" ',
                                     gettextRcmdr("is not a valid name for a dataset."))
            )
            return()
        }

        # Check if the new name is not duplicated
        # (if a dataset with the same name does not exist in the workspace)
        if (is.element(ds_nameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(ds_nameValue, gettextRcmdr("Dataset")))) {
                closeDialog()
                window_dataset_join()
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # [!!!] A more robust code for this section is needed.

        by_x_name_1_Value <- trim.blanks(tclvalue(by_x_name_1))
        by_y_name_1_Value <- trim.blanks(tclvalue(by_y_name_1))

        by_x_name_2_Value <- trim.blanks(tclvalue(by_x_name_2))
        by_y_name_2_Value <- trim.blanks(tclvalue(by_y_name_2))

        by_x_name_3_Value <- trim.blanks(tclvalue(by_x_name_3))
        by_y_name_3_Value <- trim.blanks(tclvalue(by_y_name_3))


        # If only one pair of names is entered
        if (by_x_name_2_Value == "" & by_y_name_2_Value == "" &
            by_x_name_3_Value == "" & by_y_name_3_Value == ""   ) {
                # If no variable names entered,
                # all matching names are used.
                if (by_x_name_1_Value == "" & by_y_name_1_Value == "") {

                    by_ <- ""

                    # If one name is entered, it is used for both datasets.
                } else if (by_x_name_1_Value != "" & by_y_name_1_Value == "") {
                    by_ <- glue::glue(', \nby = "{by_x_name_1_Value}"')

                } else if (by_x_name_1_Value == "" & by_y_name_1_Value != "") {
                    by_ <- glue::glue(', \nby = "{by_y_name_1_Value}"')

                    # If both names are entered, they are matched as a pair.
                } else {
                    by_ <- glue::glue(', \nby = c("{by_x_name_1_Value}" = "{by_y_name_1_Value}")')

                }

        } else {
            # Match each pair if not missing or write NULL
            if (by_x_name_1_Value != "" & by_y_name_1_Value != "") {
                a <- glue::glue('"{by_x_name_1_Value}" = "{by_y_name_1_Value}"')
            } else {
                a <- NULL
            }

            if (by_x_name_2_Value != "" & by_y_name_2_Value != "") {
                b <- glue::glue('"{by_x_name_2_Value}" = "{by_y_name_2_Value}"')
            } else {
                b <- NULL
            }

            if (by_x_name_3_Value != "" & by_y_name_3_Value != "") {
                c <- glue::glue('"{by_x_name_3_Value}" = "{by_y_name_3_Value}"')
            } else {
                c <- NULL
            }

            # If all pairs are missing:
            if (is.null(a) && is.null(b) && is.null(C)) {
                by_ <- ""

            # If at least one pair is not missing:
            } else {
                by_ <- paste0(", \nby = c(", stringr::str_c(a, b, c, sep = ", "), ")")
            }
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ds_name_1 <- getSelection(dataSet1Box)
        ds_name_2 <- getSelection(dataSet2Box)


        # Check if datasets are selected
        if (length(ds_name_1) == 0) {
            errorCondition(
                recall = window_dataset_join,
                message = gettextRcmdr("You must select a dataset (left).")
            )
            return()
        }
        if (length(ds_name_2) == 0) {
            errorCondition(
                recall = window_dataset_join,
                message = gettextRcmdr("You must select a dataset (right).")
            )
            return()
        }
        # Check if names does not match
        if (ds_name_1 == ds_name_2) {
            errorCondition(
                recall = window_dataset_join,
                message = gettextRcmdr("You cannot merge a dataset with itself.")
            )
            return()
        }

        # Code to join the datasets
        join_type <- tclvalue(join_typeVariable)
        command <- style_cmd(glue::glue(
            "{ds_nameValue} <- dplyr::{join_type}({ds_name_1}, {ds_name_2}{by_})"))

        doItAndPrint(command)

        activeDataSet(ds_nameValue)
        closeDialog()
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "join")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(ds_name_Frame, pady = c(0, 10), sticky = "sw", columnspan = 3)

    tkgrid(labelRcmdr(ds_name_Frame,
                      text = gettextRcmdr("Name for the joint dataset:  ")),
           entry_ds_name, pady = c(10, 15))


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(middle_Frame, sticky = "sw", columnspan = 3)

    tkgrid(getFrame(dataSet1Box),  getFrame(dataSet2Box), join_typeFrame,
           sticky = "nw", padx = c(0, 10))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(labelRcmdr(var_names_Frame,
               fg = getRcmdr("title.color"),
               text = gettextRcmdr(paste(
                   "     Variable names to join the datasets by \n",
                   "(leave blank to choose all matching names)"))),
           columnspan = 3, padx = c(0, 5)
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(
        labelRcmdr(var_names_Frame,
                   text = gettextRcmdr("Variable in x")),

        labelRcmdr(var_names_Frame,
                   text = gettextRcmdr("      ")),

        labelRcmdr(var_names_Frame,
                   text = gettextRcmdr("Variable in y"))

    )

    # labelRcmdr(var_names_Frame,
               # text = gettextRcmdr("Variable name in x to join by:  \n(or leave blank)")),
    # labelRcmdr(var_names_Frame,
    #            text = gettextRcmdr("Variable name in y to join by:  \n(or leave blank)")),


    # tkgrid(getFrame(v_x1),
    #        label_rcmdr(var_names_Frame, text = " = "),
    #        getFrame(v_y1), sticky = "n", pady = c(0, 5))

    tkgrid(entry_by_x_name_1,
           label_rcmdr(var_names_Frame, text = " = "),
           entry_by_y_name_1, sticky = "n", pady = c(0, 5))

    tkgrid(entry_by_x_name_2,
           label_rcmdr(var_names_Frame, text = " = "),
           entry_by_y_name_2, sticky = "n", pady = c(0, 5))

    tkgrid(entry_by_x_name_3,
           label_rcmdr(var_names_Frame, text = " = "),
           entry_by_y_name_3, sticky = "n", pady = c(0, 5))


    tkgrid(var_names_Frame, sticky = "w", pady = c(15, 5))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkgrid(buttonsFrame, sticky = "ew", columnspan = 3, pady = c(5, 0))
    dialogSuffix()
}