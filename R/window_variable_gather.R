# TODO:
# 1. Add possibility to select columns, that must not be gathered,
#    e.g. "-Species" vs. "Species".
#
#

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_variable_gather <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettextRcmdr("Gather / Stack variables"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(initial_y_var         = NULL,
                     initial_dsname        = unique_df_name(suffix = "_long"),
                     initial_key_colname   = "key",
                     initial_value_colname = "values",
                     initial_gather_all    = TRUE,
                     initial_na_rm         = FALSE,
                     initial_factor_key    = TRUE,
                     initial_convert_key   = FALSE
                     # include_exclude     = ...
    )

    dialog_values <- getDialog("window_variable_gather", defaults)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Functions --------------------------------------------------------------

    cmd_onClick_gather_all <- function() {
        if (tclvalue_lgl(gather_allVariable) == TRUE) {
            # Clear factor variable box
            for (sel in seq_along(y_var_box$varlist) - 1)
                tkselection.clear(y_var_box$listbox, sel)
        }
    }

    cmd_onRelease_y_var_box <- function() {
        # On mouse relese select/deselect checkbox
        if (length(getSelection(y_var_box)) == 0) {
            tclvalue(gather_allVariable) <- "1"
        } else {
            tclvalue(gather_allVariable) <- "0"
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)

    y_var_box <-
        variableListBox2(
            upper_frame,
            initialSelection = dialog_values$initial_y_var,
            listHeight = 6,
            # Numeric(),
            selectmode = "multiple",
            onRelease_fun = cmd_onRelease_y_var_box,
            title = gettextRcmdr("Variables to gather \n(pick none, one or more)"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    options_right_frame <- tkframe(upper_frame)

    bs_check_boxes(options_right_frame,
                   ttk = TRUE,
                   frame = "gather_options_frame",
                   title = "Options",
                   boxes = c("gather_all","na_rm", "factor_key", "convert_key"),
                   initialValues = c(
                       dialog_values$initial_gather_all,
                       dialog_values$initial_na_rm,
                       dialog_values$initial_factor_key,
                       dialog_values$initial_convert_key
                       ),
                   labels = gettextRcmdr(
                       c(  "Gather all variables",
                           "Remove missing values from output",
                           "Convert key column to factor",
                           "Convert key column to numeric, integer, or logical"
                       )
                   ),
                   commands = list("gather_all"  = cmd_onClick_gather_all,
                                   "na_rm"       = function(){},
                                   "factor_key"  = function(){},
                                   "convert_key" = function(){}
                                   )
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    lower_frame <- tkframe(top)

    key_colname_variable    <- tclVar(dialog_values$initial_key_colname)
    key_colnameField        <- ttkentry(lower_frame,
                                        width        = "48",
                                        textvariable = key_colname_variable)

    values_colname_variable <- tclVar(dialog_values$initial_value_colname)
    values_colnameField     <- ttkentry(lower_frame,
                                        width        = "48",
                                        textvariable = values_colname_variable)

    dataset_name_variable   <- tclVar(dialog_values$initial_dsname)
    dataset_nameField       <- ttkentry(lower_frame,
                                        width        = "48",
                                        textvariable = dataset_name_variable)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # =========================================================================
    onOK <- function() {
        variables     <- getSelection(y_var_box)
        key_colname   <- tclvalue(key_colname_variable)
        value_colname <- tclvalue(values_colname_variable)
        dsname        <- tclvalue(dataset_name_variable)
        gather_all    <- tclvalue_lgl(gather_allVariable)
        factor_key    <- tclvalue_lgl(factor_keyVariable)
        convert_key   <- tclvalue_lgl(convert_keyVariable)
        na_rm         <- tclvalue_lgl(na_rmVariable)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # --------------------------------------------------------------------
        putDialog("window_variable_gather",
                  list(# initial_y_var  = y_var,
                      # initial_dsname      = glue("{activeDataSet()}_long"),
                      initial_key_colname   = key_colname,
                      initial_value_colname = value_colname,
                      initial_gather_all    = gather_all,
                      initial_factor_key    = factor_key,
                      initial_convert_key   = convert_key,
                      initial_na_rm         = na_rm
                      # include_exclude     = ...
                  )
        )

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Check input

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # if (length(variables) == 1) {
        #     errorCondition(recall = window_variable_gather,
        #                    message = gettextRcmdr("You must select either none or at least two variables."))
        #     return()
        # }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (!is.valid.name(key_colname)) {
            errorCondition(recall = window_variable_gather,
                           message = glue('"{key_colname}" ',
                                          gettextRcmdr("is not a valid name.")))
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (!is.valid.name(value_colname)) {
            errorCondition(recall = window_variable_gather,
                           message = glue('"{value_colname}" ',
                                          gettextRcmdr("is not a valid name.")))

            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (!is.valid.name(dsname)) {
            errorCondition(recall = window_variable_gather,
                           message = glue('"{dsname}" ',
                                          gettextRcmdr("is not a valid name.")))
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (is.element(dsname, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsname, gettextRcmdr("Data set")))){
                window_variable_gather()
                return()
            }
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Construct code
        variables <-
            if (gather_all == TRUE) {
                ""
            } else {
                stringr::str_c(",\n", stringr::str_c(variables, collapse = ", "))
            }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        options_new_line <-
            if (any(na_rm, convert_key, factor_key)) {
                ",\n"
            } else {
                NULL
            }

        na_rm_text <-
            if (na_rm == TRUE) {
                "na.rm = TRUE"
            } else {
                NULL
            }

        convert_key_text <-
            if (convert_key == TRUE) {
                "convert = TRUE"
            } else {
                NULL
            }

        factor_key_text <-
            if (factor_key == TRUE) {
                "factor_key = TRUE"
            } else {
                NULL
            }

        opts_text <- stringr::str_c(options_new_line,
                           stringr::str_c(na_rm_text, factor_key_text, convert_key_text,
                                 sep = ", "))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("tidyr")

        command <- glue(
            "## Convert to long-format data frame \n",
            '{dsname} <- {activeDataSet()} %>% \n',
            'tidyr::gather(key = "{key_colname}", value = "{value_colname}"',
            '{variables}{opts_text}',
            ')') %>%
            style_cmd()

        logger(command)

        result <- justDoIt(command)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] !=  "try-error") activeDataSet(dsname)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
    }
    # ========================================================================
    OKCancelHelp(helpSubject = "gather")

    fg_col <- Rcmdr::getRcmdr("title.color")

    tkgrid(label_rcmdr(
        top,
        text = gettextRcmdr("Gather columns into key-value pairs: convert dataset into long format"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame)

    tkgrid(getFrame(y_var_box), options_right_frame,
           sticky = "nw",
           columnspan = 2)

    tkgrid(gather_options_frame, pady = c(15, 0))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # tkgrid(labelRcmdr(top, text = ""))

    tkgrid(lower_frame, pady = c(20, 0))


    tkgrid(label_rcmdr(lower_frame, text = gettextRcmdr("Output dataset name:        "),
                       fg = fg_col),
           dataset_nameField,
           sticky = "w",
           pady = 2)

    tkgrid(label_rcmdr(lower_frame, text = gettextRcmdr("Key column name:  "),
                       fg = fg_col),
           key_colnameField,
           sticky = "w",
           pady = 2)

    tkgrid(label_rcmdr(lower_frame, text = gettextRcmdr("Value column name:"),
                       fg = fg_col),
           values_colnameField,
           sticky = "w",
           pady = 2)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "we", columnspan = 2)
    dialogSuffix(preventGrabFocus = TRUE)
}
