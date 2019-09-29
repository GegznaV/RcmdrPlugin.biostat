# FIXME: Prevent from merging if no column is selected and no common columns are
#        detected, e.g.:
# gss_cat_women_full_join_1 <- dplyr::full_join(forcats::gss_cat, women)
#
# [7] ERROR: `by` required, because the data sources have no common variables
# Call `rlang


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_join <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dataSets <- listDataSets()
    .ds <- active_dataset_0()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    win_title <- gettext_bs("Join Two Datasets")
    initializeDialog(title = win_title)
    tk_title(top, text = win_title, columnspan = 3)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Functions --------------------------------------------------------------

    reset_combobox <- function(obj, values = "", selected = "", ...) {
        tclvalue(obj$combovar) <- selected
        tkconfigure(obj$combobox, values = values, ...)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    check_v2_v3 <- function() {
        if (get_selection(v_x1) != "" && get_selection(v_y1) != "") {
            # Enable
            tk_pair_control(v_x2, v_y2, "readonly")
            # tk_pair_control(v_x3, v_y3, "readonly")

        } else {
            # Reset and disable
            tk_pair_control(v_x2, v_y2, "disabled", reset = TRUE)
            tk_pair_control(v_x3, v_y3, "disabled", reset = TRUE)

        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    check_v3 <- function() {
        if (get_selection(v_x2) != "" && get_selection(v_y2) != "") {
            # enable
            tk_pair_control(v_x3, v_y3, "readonly")

        } else {
            # reset and disable
            tk_pair_control(v_x3, v_y3, "disabled", reset = TRUE)

        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_on_release_ds_x <- function() {
        # On mouse relese
        ds <- get_selection(ds_1_box)
        # Names of variables plus blank:
        vars_in_ds <- c("", str_glue_eval("colnames({ds})", envir_eval = .GlobalEnv))

        reset_combobox(v_x1, values = vars_in_ds)
        reset_combobox(v_x2, values = vars_in_ds)
        reset_combobox(v_x3, values = vars_in_ds)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_on_release_ds_y <- function() {
        # On mouse relese
        ds <- get_selection(ds_2_box)
        # Names of variables plus blank:
        vars_in_ds <- c("", str_glue_eval("colnames({ds})", envir_eval = .GlobalEnv))

        reset_combobox(v_y1, values = vars_in_ds)
        reset_combobox(v_y2, values = vars_in_ds)
        reset_combobox(v_y3, values = vars_in_ds)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    set_ds_name <- function() {
        join_type                  <- tclvalue(join_typeVariable)
        ds_1                       <- get_selection(ds_1_box)
        ds_2                       <- get_selection(ds_2_box)
        base_name                  <- paste(ds_1, ds_2, join_type, sep = "_")
        unique_base_name           <- unique_df_name(base_name, all_numbered = TRUE)
        tclvalue(ds_name_variable) <- unique_base_name
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Widgets ----------------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ds_name_variable <- tclVar("joint_dataset")
    # ds_name_variable <- tclVar(unique_df_name("joint_dataset", all_numbered = TRUE))
    ds_name_Frame <- tkframe(top)
    entry_ds_name <- ttkentry(ds_name_Frame, width = "41",
                              textvariable = ds_name_variable)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    middle_Frame <- tkframe(top)

    Rcmdr::radioButtons(
        middle_Frame,
        "join_type",
        title = gettext_bs("Join type: "),
        labels = gettext_bs(c(
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
                    "anti_join"),
        command = set_ds_name
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # tk_messageBox("ok", "message", icon = "warning", parent = top)

    var_names_Frame <- tkframe(top)

    v_x1 <- bs_combobox(var_names_Frame, values = "", on_select = check_v2_v3)

    v_x2 <- bs_combobox(var_names_Frame, values = "", state = "disabled",
                        on_select = check_v3)

    v_x3 <- bs_combobox(var_names_Frame, values = "", state = "disabled")


    v_y1 <- bs_combobox(var_names_Frame, values = "", on_select = check_v2_v3)

    v_y2 <- bs_combobox(var_names_Frame, values = "", state = "disabled",
                        on_select = check_v3)

    v_y3 <- bs_combobox(var_names_Frame, values = "", state = "disabled")


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ds_1_box <-
        bs_listbox(
            parent = middle_Frame,
            values = dataSets,
            value  = .ds,
            height = 7,
            title = gettext_bs("First dataset (left, x) \n(pick one)"),
            on_select = function() {
                set_ds_name()
                cmd_on_release_ds_x()
            })

    ds_2_box <-
        bs_listbox(
            parent = middle_Frame,
            values = dataSets,
            height = 7,
            selection = 1,
            on_select = function() {
                set_ds_name()
                cmd_on_release_ds_y()
            },
            title = gettext_bs("Second dataset (right, y) \n(pick one)"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_on_release_ds_x()
    cmd_on_release_ds_y()
    set_ds_name()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # commonVar <- tclVar("0")
    # commonFrame <- tkframe(top)
    # commonButton <- ttkcheckbutton(commonFrame, variable = commonVar)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        ds_name_x <- get_selection(ds_1_box)
        ds_name_y <- get_selection(ds_2_box)

        ds_name <- tclvalue_chr(ds_name_variable)

        by_x_name_1_Value <- get_selection(v_x1)
        by_y_name_1_Value <- get_selection(v_y1)

        by_x_name_2_Value <- get_selection(v_x2)
        by_y_name_2_Value <- get_selection(v_y2)

        by_x_name_3_Value <- get_selection(v_x3)
        by_y_name_3_Value <- get_selection(v_y3)

        join_type <- tclvalue(join_typeVariable)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Check if the new name is not missing
        if (ds_name == "") {
            errorCondition(
                recall = window_dataset_join,
                message = gettext_bs("You must enter a name of the new joint dataset.")
            )
            return()
        }

        # Check if the new name is valid
        if (!is.valid.name(ds_name)) {
            errorCondition(
                recall = window_dataset_join,
                message = str_glue('"{ds_name}" ',
                                   gettext_bs("is not a valid name for a dataset."))
            )
            return()
        }

        # Check if the new name is not duplicated
        # (if a dataset with the same name does not exist in the workspace)
        if (is.element(ds_name, listDataSets())) {
            if ("no" == tclvalue(checkReplace(ds_name, gettext_bs("Dataset")))) {
                closeDialog()
                window_dataset_join()
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # If only one pair of names is entered
        if (by_x_name_2_Value == "" & by_y_name_2_Value == "" &
            by_x_name_3_Value == "" & by_y_name_3_Value == ""   ) {
            # If no variable names entered,
            # all matching names are used.
            if (by_x_name_1_Value == "" & by_y_name_1_Value == "") {

                by_ <- ""

                # If one name is entered, it is used for both datasets.
            } else if (by_x_name_1_Value != "" & by_y_name_1_Value == "") {
                by_ <- str_glue(', \nby = "{by_x_name_1_Value}"')

            } else if (by_x_name_1_Value == "" & by_y_name_1_Value != "") {
                by_ <- str_glue(', \nby = "{by_y_name_1_Value}"')

                # If both names are entered, they are matched as a pair.
            } else {
                by_ <- str_glue(
                    ', \nby = c("{by_x_name_1_Value}" = "{by_y_name_1_Value}")')

            }

        } else {
            # Match each pair if not missing or write NULL
            if (by_x_name_1_Value != "" & by_y_name_1_Value != "") {
                a <- str_glue('"{by_x_name_1_Value}" = "{by_y_name_1_Value}"')
            } else {
                a <- NULL
            }

            if (by_x_name_2_Value != "" & by_y_name_2_Value != "") {
                b <- str_glue('"{by_x_name_2_Value}" = "{by_y_name_2_Value}"')
            } else {
                b <- NULL
            }

            if (by_x_name_3_Value != "" & by_y_name_3_Value != "") {
                c <- str_glue('"{by_x_name_3_Value}" = "{by_y_name_3_Value}"')
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
        # Check if datasets are selected
        if (length(ds_name_x) == 0) {
            errorCondition(
                recall = window_dataset_join,
                message = gettext_bs("You must select the first dataset (left).")
            )
            return()
        }
        if (length(ds_name_y) == 0) {
            errorCondition(
                recall = window_dataset_join,
                message = gettext_bs("You must select the second dataset (right).")
            )
            return()
        }
        # # Check if names does not match
        # if (ds_name_x == ds_name_y) {
        #     errorCondition(
        #         recall = window_dataset_join,
        #         message = gettext_bs("You cannot merge a dataset with itself.")
        #     )
        #     return()
        # }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Code to join the datasets ------------------------------------------
        command <- str_glue(
            "## Join two datasets\n",
            "{ds_name} <- dplyr::{join_type}({ds_name_x}, {ds_name_y}{by_})") %>%
            style_cmd()

        rez <- doItAndPrint(command)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        active_dataset(ds_name)
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Layout -----------------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(middle_Frame, sticky = "sw", columnspan = 3)

    tkgrid(getFrame(ds_1_box),  getFrame(ds_2_box), join_typeFrame,
           sticky = "nw", padx = c(0, 10))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(labelRcmdr(var_names_Frame,
                      fg = getRcmdr("title.color"),
                      text = gettext_bs(paste(
                          "     Variable names to join the datasets by \n",
                          "(leave blank to choose all matching names)"))),
           columnspan = 3, padx = c(5, 5), pady = c(5, 5)
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(
        labelRcmdr(var_names_Frame,
                   text = gettext_bs("Variable in x")),

        labelRcmdr(var_names_Frame,
                   text = gettext_bs("      ")),

        labelRcmdr(var_names_Frame,
                   text = gettext_bs("Variable in y"))

    )

    tkgrid(getFrame(v_x1),
           bs_label(var_names_Frame, text = " = "),
           getFrame(v_y1), sticky = "n", pady = c(0, 5))

    tkgrid(getFrame(v_x2),
           bs_label(var_names_Frame, text = " = "),
           getFrame(v_y2), sticky = "n", pady = c(0, 5))

    tkgrid(getFrame(v_x3),
           bs_label(var_names_Frame, text = " = "),
           getFrame(v_y3), sticky = "n", pady = c(0, 5))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(var_names_Frame, sticky = "w", pady = c(0, 5),  columnspan = 3)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(labelRcmdr(ds_name_Frame,
                      fg = getRcmdr("title.color"),
                      text = gettext_bs("Name for joint dataset:  ")),
           entry_ds_name, pady = c(5, 0))

    tkgrid(ds_name_Frame, pady = c(0, 5), sticky = "sw", columnspan = 3)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ok_cancel_help(helpSubject = "join", helpPackage = "dplyr")

    tkgrid(buttonsFrame, sticky = "ew", columnspan = 3, pady = c(5, 0))
    dialogSuffix()
}
