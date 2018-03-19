#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_join <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dataSets <- listDataSets()
    .activeDataSet <- ActiveDataSet()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettextRcmdr("Join Two Datasets"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Functions

    reset_combobox <- function(obj, values = "", selected = "", ...) {
        tclvalue(obj$combovar) <- selected
        tkconfigure(obj$combobox, values = values, ...)
    }

    tk_pair_control <- function(obj1, obj2,
                                state = c("readonly", "disabled", "active", "normal"),
                                reset = FALSE,
                                selected = c("", ""), ...) {
        state <- match.arg(state)
        if (reset == TRUE) {
            tclvalue(obj1$combovar) <- selected[1]
            tclvalue(obj2$combovar) <- selected[2]
        }

        tkconfigure(obj1$combobox, state = state, ...)
        tkconfigure(obj2$combobox, state = state, ...)
    }

    check_v2_v3 <- function() {
        if (getSelection(v_x1) != "" && getSelection(v_y1) != "") {
            # Enable
            tk_pair_control(v_x2, v_y2, "readonly")
            # tk_pair_control(v_x3, v_y3, "readonly")

        } else {
            # Reset and disable
            tk_pair_control(v_x2, v_y2, "disabled", reset = TRUE)
            tk_pair_control(v_x3, v_y3, "disabled", reset = TRUE)

        }
    }
    check_v3 <- function() {
        if (getSelection(v_x2) != "" && getSelection(v_y2) != "") {
            # enable
            tk_pair_control(v_x3, v_y3, "readonly")

        } else {
            # reset and disable
            tk_pair_control(v_x3, v_y3, "disabled", reset = TRUE)

        }
    }

    cmd_onRelease_ds_x <- function() {
        # On mouse relese
        ds <- getSelection(dataset_x_box)
        # Names of variables plus blank:
        vars_in_ds <- c("", eval_glue("colnames({ds})"))

        reset_combobox(v_x1, values = vars_in_ds)
        reset_combobox(v_x2, values = vars_in_ds)
        reset_combobox(v_x3, values = vars_in_ds)
    }

    cmd_onRelease_ds_y <- function() {
        # On mouse relese
        ds <- getSelection(dataset_y_box)
        # Names of variables plus blank:
        vars_in_ds <- c("", eval_glue("colnames({ds})"))

        reset_combobox(v_y1, values = vars_in_ds)
        reset_combobox(v_y2, values = vars_in_ds)
        reset_combobox(v_y3, values = vars_in_ds)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Widgets
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ds_name <- tclVar("new_joint_dataset")
    ds_name_Frame <- tkframe(top)
    entry_ds_name <- ttkentry(ds_name_Frame, width = "37", textvariable = ds_name)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    middle_Frame <- tkframe(top)
    Rcmdr::radioButtons(
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

    # by_x_name_1 <- tclVar("")
    # by_y_name_1 <- tclVar("")
    # by_x_name_2 <- tclVar("")
    # by_y_name_2 <- tclVar("")
    # by_x_name_3 <- tclVar("")
    # by_y_name_3 <- tclVar("")
    #
    # entry_by_x_name_1 <- ttkentry(var_names_Frame, width = "20", textvariable = by_x_name_1)
    # entry_by_y_name_1 <- ttkentry(var_names_Frame, width = "20", textvariable = by_y_name_1)
    # entry_by_x_name_2 <- ttkentry(var_names_Frame, width = "20", textvariable = by_x_name_2)
    # entry_by_y_name_2 <- ttkentry(var_names_Frame, width = "20", textvariable = by_y_name_2)
    # entry_by_x_name_3 <- ttkentry(var_names_Frame, width = "20", textvariable = by_x_name_3)
    # entry_by_y_name_3 <- ttkentry(var_names_Frame, width = "20", textvariable = by_y_name_3)


    # tk_messageBox("ok", "message", icon = "warning", parent = top)

    var_names_Frame <- tkframe(top)

    v_x1 <- inputComboBox(var_names_Frame, variableList = "",
                          onSelect_fun = check_v2_v3)

    v_x2 <- inputComboBox(var_names_Frame, variableList = "", state = "disabled",
                          onSelect_fun = check_v3)

    v_x3 <- inputComboBox(var_names_Frame, variableList = "", state = "disabled")


    v_y1 <- inputComboBox(var_names_Frame, variableList = "",
                          onSelect_fun = check_v2_v3)

    v_y2 <- inputComboBox(var_names_Frame, variableList = "", state = "disabled",
                          onSelect_fun = check_v3)

    v_y3 <- inputComboBox(var_names_Frame, variableList = "", state = "disabled")


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dataset_x_box <-
        variableListBox2(
            middle_Frame,
            dataSets,
            listHeight = 7,
            title = gettextRcmdr("First dataset (left, x) \n(pick one)"),
            onRelease_fun = cmd_onRelease_ds_x,
            initialSelection = if (is.null(.activeDataSet)) {
                NULL
            } else {
                which(.activeDataSet == dataSets) - 1
            }
        )

    dataset_y_box <-
        variableListBox2(middle_Frame,
                         dataSets,
                         listHeight = 7,
                         initialSelection = 0,
                         onRelease_fun = cmd_onRelease_ds_y(),
                         title = gettextRcmdr("Second dataset (right, y) \n(pick one)"))

    cmd_onRelease_ds_x()
    cmd_onRelease_ds_y()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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
        by_x_name_1_Value <- getSelection(v_x1)
        by_y_name_1_Value <- getSelection(v_y1)

        by_x_name_2_Value <- getSelection(v_x2)
        by_y_name_2_Value <- getSelection(v_y2)

        by_x_name_3_Value <- getSelection(v_x3)
        by_y_name_3_Value <- getSelection(v_y3)


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
                    by_ <- glue::glue(
                        ', \nby = c("{by_x_name_1_Value}" = "{by_y_name_1_Value}")')

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
        ds_name_x <- getSelection(dataset_x_box)
        ds_name_y <- getSelection(dataset_y_box)

        # Check if datasets are selected
        if (length(ds_name_x) == 0) {
            errorCondition(
                recall = window_dataset_join,
                message = gettextRcmdr("You must select a dataset (left).")
            )
            return()
        }
        if (length(ds_name_y) == 0) {
            errorCondition(
                recall = window_dataset_join,
                message = gettextRcmdr("You must select a dataset (right).")
            )
            return()
        }
        # Check if names does not match
        if (ds_name_x == ds_name_y) {
            errorCondition(
                recall = window_dataset_join,
                message = gettextRcmdr("You cannot merge a dataset with itself.")
            )
            return()
        }

        # Code to join the datasets
        join_type <- tclvalue(join_typeVariable)
        command <- style_cmd(glue::glue(
            "{ds_nameValue} <- dplyr::{join_type}({ds_name_x}, {ds_name_y}{by_})"))

        rez <- doItAndPrint(command)

        activeDataSet(ds_nameValue)
        closeDialog()
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "join")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(middle_Frame, sticky = "sw", columnspan = 3)

    tkgrid(getFrame(dataset_x_box),  getFrame(dataset_y_box), join_typeFrame,
           sticky = "nw", padx = c(0, 10))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(labelRcmdr(var_names_Frame,
               fg = getRcmdr("title.color"),
               text = gettextRcmdr(paste(
                   "     Variable names to join the datasets by \n",
                   "(leave blank to choose all matching names)"))),
           columnspan = 3, padx = c(5, 5)
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

    tkgrid(getFrame(v_x1),
           label_rcmdr(var_names_Frame, text = " = "),
           getFrame(v_y1), sticky = "n", pady = c(0, 5))

    tkgrid(getFrame(v_x2),
           label_rcmdr(var_names_Frame, text = " = "),
           getFrame(v_y2), sticky = "n", pady = c(0, 5))

    tkgrid(getFrame(v_x3),
           label_rcmdr(var_names_Frame, text = " = "),
           getFrame(v_y3), sticky = "n", pady = c(0, 5))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(labelRcmdr(ds_name_Frame,
                      fg = getRcmdr("title.color"),
                      text = gettextRcmdr("Name for the joint dataset:  ")),
           entry_ds_name, pady = c(10, 0))

    tkgrid(ds_name_Frame, pady = c(0, 10), sticky = "sw", columnspan = 3)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(var_names_Frame, sticky = "w", pady = c(0, 5),  columnspan = 3)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkgrid(buttonsFrame, sticky = "ew", columnspan = 3, pady = c(5, 0))
    dialogSuffix()
}