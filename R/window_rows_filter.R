# TODO:
#
# 1. Check code for possible inconsisnencies and bugs.
# 2. When push "Apply" and error occurs, two windows open. Apply is now disabled.
#    It should be fixed.
# 3. Add buttons "==", "!=", "<" etc. in style as used in "fit linear model" window

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Correctly initializes window `window_rows_filter()`
window_rows_filter0  <- function(variables) {
    window_rows_filter()
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# new_dsname (character) - data frame name
# init_conditions (character) - conditions to be evaluated to select rows
# incorrect_cond_msg (character) - Message for incorrect expression.
window_rows_filter <- function(new_dsname = NULL,
                               init_conditions = NULL,
                               incorrect_cond_msg = NULL) {

    # Functions --------------------------------------------------------------
    onDoubleClick_variable <- function() {
        var <- trim.blanks(get_selection(y_var_box))

        word <- str_glue('\\[{gettext_bs("factor")}\\]')

        if (length(grep(word, var)) == 1)
            var <- trim.blanks(sub(word, "", var))
        tkfocus(conditions_field)

        conds <- tclvalue_chr(conditions_variable)
        tclvalue(conditions_variable) <-
            if (conds == "") {
                var
            } else {
                last_chr <- stringr::str_sub(conds, -1)
                expr_sep <- if (last_chr %in% c("(", "[")) "" else " "
                paste(conds, var, sep = expr_sep)
            }
        tkicursor(conditions_field, "end")
        tkxview.moveto(conditions_field, "1")
    }

    # Dialog -----------------------------------------------------------------
    win_title <- gettext_bs("Filter: Create a Subset of Rows That Match Conditions")
    initializeDialog(title = win_title)
    tk_title(top, win_title)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)

    variable_labels <-
        paste(Variables(), ifelse(Variables() %in% Factors(),
                                  yes = gettext_bs("[factor]"),
                                  no  = ""
        ))

    y_var_box <-
        bs_listbox(
            parent = upper_frame,
            title = gettext_bs("Current variables \n(double-click to add to conditions)"),
            values = variable_labels,
            height = 8,
            on_double_click = onDoubleClick_variable
        )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    lower_frame <- tkframe(top)

    if (is.null(new_dsname))  new_dsname <- unique_df_name(suffix = "_subset")
    new_dsname_variable <- tclVar(new_dsname)
    new_dsname_field    <- ttkentry(lower_frame,
                                    width = "30",
                                    textvariable = new_dsname_variable)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    conditions_frame <- tkframe(lower_frame)

    if (is.null(init_conditions)) init_conditions <- ""

    conditions_variable <- tclVar(init_conditions)
    conditions_field    <- ttkentry(conditions_frame,
                                    font = getRcmdr("logFont"),
                                    width = "50",
                                    textvariable = conditions_variable
    )
    conditionsXscroll <- ttkscrollbar(conditions_frame,
                                      orient = "horizontal",
                                      command = function(...)
                                          tkxview(conditions_field, ...)
    )
    tkconfigure(conditions_field,
                xscrollcommand = function(...)
                    tkset(conditionsXscroll, ...)
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        new_dsname  <- trim.blanks(tclvalue(new_dsname_variable))
        conditions  <- tclvalue(conditions_variable)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()

        # Check validity of var name ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (!is.valid.name(new_dsname)) {
            msg <- str_glue(gettext_bs("Dataset name"), ' "{new_dsname}" ',
                            gettext_bs("is not valid!"))

            Message(message = msg, type = "error")
            window_rows_filter(new_dsname = make.names(new_dsname),
                               init_conditions = conditions,
                               incorrect_cond_msg = msg)
            return()
        }

        # Check if expression is not empty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        check.empty <- gsub(";", "", gsub(" ", "", conditions))

        if ("" == check.empty) {
            Message(message = gettext_bs("No conditions were specified!"),
                    type = "error")
            window_rows_filter(new_dsname = new_dsname,
                               init_conditions = conditions,
                               incorrect_cond_msg = "No conditions were specified!")
            return()
        }

        # Check if dataset name already exists ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (is.element(new_dsname, listDataSets())) {
            if ("no" == tclvalue(checkReplace(new_dsname,
                                              gettext_bs("Data set")))) {
                window_rows_filter(new_dsname         = new_dsname,
                                   init_conditions    = conditions,
                                   incorrect_cond_msg =
                                       str_glue('Chose other name than "{new_dsname}".'))
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("dplyr")
        command <- str_glue(
            "## Select rows that match conditions \n",
            "{new_dsname} <- {active_dataset()} %>% \n",
            "dplyr::filter({conditions})") %>%
            style_cmd()

        result <- justDoIt(command)

        if (class(result)[1] !=  "try-error") {
            # Change active dataset
            active_dataset(new_dsname, flushModel = FALSE)

        } else {
            # If evaluation of conditions results in error
            Message(message = gettext_bs("Evaluation of conditions resulted in error!"),
                    type = "error")
            window_rows_filter(new_dsname = new_dsname,
                               init_conditions = conditions,
                               incorrect_cond_msg = "The definition of conditions contains error(s) or is invalid!")
            return()
        }

        logger(command)
        tkfocus(CommanderWindow())
    }

    # ========================================================================
    ok_cancel_help(helpSubject = "filter", helpPackage = "dplyr",
                   reset = "window_rows_filter"
                   # , apply = "window_rows_filter"
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame, sticky = "nw")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    examples_frame <- tkframe(upper_frame)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid_text <- function(text = "", frame = examples_frame, fg = "black",
                            sticky = "w", padx = 20, pady = 0, ...) {
        tkgrid(labelRcmdr(frame, text = gettext_bs(text), fg = fg),
               sticky = sticky, padx = padx, pady = pady, ...)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid_text("\nExamples of logical conditions", fg = getRcmdr("title.color"))
    tkgrid_text('Polular operations:   ==   !=   <   <=   >   >=    %in%   |   &   !   between()', fg = "darkgreen")
    tkgrid_text("Example 1: age < 10")
    tkgrid_text('Example 2: color %in% c("red", "yellow")')
    tkgrid_text('Example 3: !is.na(color)')
    tkgrid_text('Example 4: sex == "male", age >= 20, between(weight, 50, 80)')
    tkgrid_text('Example 5: age_group != "young" | income > 500')
    if (!is.null(incorrect_cond_msg)) {
        tkgrid_text(incorrect_cond_msg, fg = "darkred", sticky = "e",
                    pady = c(5, 0))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(getFrame(y_var_box), examples_frame,
           sticky = "nw",
           columnspan = 2)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(lower_frame)

    tkgrid(
        labelRcmdr(lower_frame,
                   fg = getRcmdr("title.color"),
                   text = gettext_bs("Name for filtered dataset")),
        labelRcmdr(lower_frame, text = "   "),
        labelRcmdr(lower_frame,
                   fg = getRcmdr("title.color"),
                   text = gettext_bs("Conditions for rows to include")),
        pady = c(15, 0),
        sticky = "nw")

    tkgrid(new_dsname_field,
           labelRcmdr(lower_frame, text = "    "),
           conditions_frame,
           sticky = "nw")

    tkgrid(conditions_field,  sticky = "ew")
    tkgrid(conditionsXscroll, sticky = "ew")

    # tkgrid(variablesFrame, sticky = "nw")
    tkgrid(conditions_frame,  sticky = "nw")


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "ew", columnspan = 2)
    dialogSuffix(rows = 3,
                 columns = 2,
                 focus = conditions_field)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
