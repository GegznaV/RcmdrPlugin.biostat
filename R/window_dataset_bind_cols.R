# TODO:
# 1. Add suggestion in the window:
#   'If appropriate, consider using "join_*" functions instead of "bind_cols".'
# 2. In dataframe selection boxes 2 and 3 show only those dataframes, that
#    are compatible with data frame in box 1 (have the same number of rows)
# 3. Functions to get and put dialog are needed.
# 4. Show number of rows in each dataset beneeth the dataset selection box.
#
# [!!!] functions to get and put dialog are needed.
# [!!!] Show number of rows in each dataset beneeth the dataset selection box.
#
#
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_bind_cols <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    win_title <- gettext_bs("Bind Columns of Datasets")
    initializeDialog(title = win_title)
    tk_title(top, win_title,  pady = c(5, 15), columnspan = 3)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Functions --------------------------------------------------------------
    set_ds_name <- function() {
        ds_1             <- getSelection(ds_1_box)
        base_name        <- paste(ds_1, "with_cols_added", sep = "_")
        unique_base_name <- unique_df_name(base_name, all_numbered = TRUE)
        tclvalue(new_ds_name_variable) <- unique_base_name
    }

    # Widgets ----------------------------------------------------------------
    new_ds_name_variable <- tclVar(
        unique_df_name("bound_by_cols", all_numbered = TRUE))

    names_Frame  <- tkframe(top)
    entry_new_ds_name <- ttkentry(names_Frame,
                                  width = "68",
                                  textvariable = new_ds_name_variable)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dataSets <- listDataSets()
    .ds      <- active_dataset_0()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)

    ds_1_box <-
        bs_listbox(
            parent    = upper_frame,
            values    = dataSets,
            value     = .ds,
            height    = 7,
            on_select = set_ds_name,
            title = gettext_bs("First dataset (left) \n(pick one)"))

    ds_2_box <-
        bs_listbox(
            parent = upper_frame,
            values = dataSets,
            height = 7,
            title  = gettext_bs("Second dataset \n(pick one)"))

    ds_3_box <-
        bs_listbox(
            parent = upper_frame,
            values = dataSets,
            height =  7,
            title  = gettext_bs("Third dataset \n(pick none or one)"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {

        new_ds_name <- tclvalue_chr(new_ds_name_variable)
        # idnameValue <- trim.blanks(tclvalue(idname))

        name_ds_1 <- get_selection(ds_1_box)
        name_ds_2 <- get_selection(ds_2_box)
        name_ds_3 <- get_selection(ds_3_box)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (new_ds_name == "") {
            errorCondition(
                recall = window_dataset_bind_cols,
                message = gettext_bs("You must enter the name of the new dataset.")
            )
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (!is.valid.name(new_ds_name)) {
            errorCondition(
                recall = window_dataset_bind_cols,
                message = str_glue('"{new_ds_name}" ', gettext_bs("is not a valid name."))
            )
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (is.element(new_ds_name, listDataSets())) {
            if ("no" == tclvalue(checkReplace(new_ds_name, gettext_bs("Dataset")))) {
                closeDialog()
                window_dataset_bind_cols()
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(name_ds_1) == 0) {
            errorCondition(
                recall = window_dataset_bind_cols,
                message = gettext_bs("You must select the first dataset.")
            )
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(c(name_ds_1, name_ds_2, name_ds_3)) < 2) {
            errorCondition(
                recall = window_dataset_bind_cols,
                message = gettext_bs("You must select at least two datasets.")
            )
            return()
        }
        # if (name_ds_1 == name_ds_2) {
        #     errorCondition(
        #         recall = window_dataset_bind_cols,
        #         message = gettext_bs("You cannot bind a dataset with itself.")
        #     )
        #     return()
        # }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(name_ds_2) == 1) {
            nrow_1 <- str_glue_eval("nrow({name_ds_1})", envir = .GlobalEnv)
            nrow_2 <- str_glue_eval("nrow({name_ds_2})", envir = .GlobalEnv)

            if (nrow_1 != nrow_2) {
              errorCondition(
                recall = window_dataset_bind_cols,
                message = gettext_bs(str_c(
                  "To bind columns, number of rows in each dataset must match.\n",
                  "The first and the second datasets differ in number of rows."))
              )
              return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(name_ds_3) == 1) {
            nrow_1 <- str_glue_eval("nrow({name_ds_1})", envir = .GlobalEnv)
            nrow_3 <- str_glue_eval("nrow({name_ds_3})", envir = .GlobalEnv)

            if (nrow_1 != nrow_3) {
                errorCondition(
                    recall = window_dataset_bind_cols,
                    message = gettext_bs(str_c(
                      "To bind columns, number of rows in each dataset must match.\n",
                      "The first and the third datasets differ in number of rows."
                    ))
                )
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        all_new_ds_names <-
            stringr::str_c(name_ds_1, name_ds_2, name_ds_3, sep = ", ")

        command <- style_cmd(str_glue(
            '## Bind columns of datasets\n',
            '{new_ds_name} <- \n',
            'dplyr::bind_cols({all_new_ds_names})'
        ))

        doItAndPrint(command)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        active_dataset(new_ds_name)
        tkfocus(CommanderWindow())
    }
    # Layout -----------------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(helpSubject = "bind_cols", helpPackage = "dplyr")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame)
    tkgrid(ds_1_box$frame, ds_2_box$frame, ds_3_box$frame, sticky = "new")

    tkgrid(
      tk_label_blue(names_Frame, text = gettext_bs("Name for resulting dataset:  ")),
      sticky = "w",
      columnspan = 3
    )
    tkgrid(entry_new_ds_name, sticky = "ew")

    tkgrid(names_Frame, pady = c(10, 5), columnspan = 3, sticky = "sew")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "we", columnspan = 3)
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    set_ds_name()

}
