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
    initializeDialog(title = gettextRcmdr("Bind columns of datasets"))
    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")
    tkgrid(label_rcmdr(
        top,
        text = gettextRcmdr("Bind columns of datasets"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 15), columnspan = 3)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    new_ds_name_variable <- tclVar(
        unique_df_name("new_dataset_binded_by_cols", all_numbered = TRUE))

    names_Frame  <- tkframe(top)
    entry_new_ds_name <- ttkentry(names_Frame,
                             width = "68",
                             textvariable = new_ds_name_variable)

    dataSets       <- listDataSets()
    .activeDataSet <- ActiveDataSet()

    upper_frame <- tkframe(top)
    ds_1 <-
        variableListBox2(
            upper_frame,
            dataSets,
            listHeight = 7,
            title = gettextRcmdr("First dataset (left) \n(pick one)"),
            initialSelection = if (is.null(.activeDataSet)) {
                NULL
            } else {
                which(.activeDataSet == dataSets) - 1
            }
        )

    ds_2 <-
        variableListBox2(upper_frame,
                         dataSets,
                         listHeight = 7,
                         title = gettextRcmdr("Second dataset \n(pick one)"))

    ds_3 <-
        variableListBox2(upper_frame,
                         dataSets,
                         listHeight = 7,
                         title = gettextRcmdr("Third dataset \n(pick none or one)"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        new_ds_name <- trim.blanks(tclvalue(new_ds_name_variable))
        # idnameValue <- trim.blanks(tclvalue(idname))

        name_ds_1 <- getSelection(ds_1)
        name_ds_2 <- getSelection(ds_2)
        name_ds_3 <- getSelection(ds_3)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (new_ds_name == "") {
            errorCondition(
                recall = window_dataset_bind_cols,
                message = gettextRcmdr("You must enter the name of the new dataset.")
            )
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (!is.valid.name(new_ds_name)) {
            errorCondition(
                recall = window_dataset_bind_cols,
                message = glue::glue('"{new_ds_name}" ', gettextRcmdr("is not a valid name."))
            )
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (is.element(new_ds_name, listDataSets())) {
            if ("no" == tclvalue(checkReplace(new_ds_name, gettextRcmdr("Dataset")))) {
                closeDialog()
                window_dataset_bind_cols()
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(name_ds_1) == 0) {
            errorCondition(
                recall = window_dataset_bind_cols,
                message = gettextRcmdr("You must select the first dataset.")
            )
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(c(name_ds_1, name_ds_2, name_ds_3)) < 2) {
            errorCondition(
                recall = window_dataset_bind_cols,
                message = gettextRcmdr("You must select at least two datasets.")
            )
            return()
        }
        # if (name_ds_1 == name_ds_2) {
        #     errorCondition(
        #         recall = window_dataset_bind_cols,
        #         message = gettextRcmdr("You cannot bind a dataset with itself.")
        #     )
        #     return()
        # }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(name_ds_2) == 1) {
            nrow_1 <- eval_glue("nrow({name_ds_1})", envir = .GlobalEnv)
            nrow_2 <- eval_glue("nrow({name_ds_2})", envir = .GlobalEnv)

            if (nrow_1 != nrow_2) {
                errorCondition(
                    recall = window_dataset_bind_cols,
                    message = gettextRcmdr("To bind columns, number of rows in each dataset must match.\nThe first and the second datasets differ in number of rows.")
                )
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(name_ds_3) == 1) {
            nrow_1 <- eval_glue("nrow({name_ds_1})", envir = .GlobalEnv)
            nrow_3 <- eval_glue("nrow({name_ds_3})", envir = .GlobalEnv)

            if (nrow_1 != nrow_3) {
                errorCondition(
                    recall = window_dataset_bind_cols,
                    message = gettextRcmdr("To bind columns, number of rows in each dataset must match.\nThe first and the third datasets differ in number of rows.")
                )
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        all_new_ds_names <- stringr::str_c(name_ds_1, name_ds_2, name_ds_3,
                                          sep = ", ")

        command <- style_cmd(glue::glue(
            '## Bind columns of datasets\n',
            '{new_ds_name} <- \n',
            'dplyr::bind_cols({all_new_ds_names})'
        ))

        doItAndPrint(command)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        activeDataSet(new_ds_name)
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "bind_cols", helpPackage = "dplyr")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame)
    tkgrid(getFrame(ds_1), getFrame(ds_2), getFrame(ds_3),
           sticky = "new")

    tkgrid(labelRcmdr(names_Frame,
                      text = gettextRcmdr("Name for the resulting dataset:  "),
                      fg = fg_col),
           sticky = "w",
           columnspan = 3)
    tkgrid(entry_new_ds_name, sticky = "ew")

    tkgrid(names_Frame, pady = c(10, 5), columnspan = 3, sticky = "sew")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "we", columnspan = 3)
    dialogSuffix()
}