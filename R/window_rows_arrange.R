# TODO:
#
# 1. Check code for bugs.
# 2. Add ability to choose to either update dataset or to create new.
# 3. Add ability to use several variables for sorting.
#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_arrange <- function() {
    initializeDialog(title = gettext_bs("Arrange Rows"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    var_y_box <-
        variableListBox2(
            upper_frame,
            Variables(),
            selectmode = "single",
            initialSelection = NULL,
            title            = gettext_bs("Variable for sorting"),
            listHeight       = 8
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    optionsFrame <- tkframe(upper_frame)

    Rcmdr::radioButtons(
        optionsFrame,
        name    = "decreasing",
        buttons = gettext_bs(c("Ascending", "Descending")),
        values  = c("FALSE", "TRUE"),
        labels  = gettext_bs(c("Ascending", "Descending")),
        title   = gettext_bs("Sorting order")
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        y          <- get_selection(var_y_box)
        decreasing <- as.logical(tclvalue(decreasingVariable))


        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (variable_is_not_selected(y, "variable", parent = top)) {
            return()
        }

        if (length(y) == 0) {
            errorCondition(
                recall = window_rows_arrange,
                message = gettext_bs("You must select a variable.")
            )
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("dplyr")

        # y <- stringr::str_c("`", y, "`")
        y <- safe_names(y)

        variables <- if (decreasing) {
            str_glue("desc({y})")

        } else {
            y
        }

        .ds        <- active_dataset()  # [???]
        new_dsname <- active_dataset()

        command <- str_glue(
            "## Sort rows \n",
            "{new_dsname} <- {.ds} %>% \n",
            "dplyr::arrange({variables})") %>%
            style_cmd()

        result <- justDoIt(command)

        if (class(result)[1] !=  "try-error") {
            # Change active dataset
            active_dataset(new_dsname, flushModel = FALSE)

        } else {
            # If evaluation of conditions results in error
            Message(message = gettext_bs("Evaluation of conditions resulted in error!"),
                    type = "error")

            window_rows_filter(
                new_dsname = new_dsname,
                init_conditions = conditions,
                incorrect_cond_msg = "The definition of conditions contains error(s) or is invalid!")
            return()
        }

        logger(command)
        tkfocus(CommanderWindow())
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(helpSubject = "arrange", helpPackage = "dplyr")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame, sticky = "new")
    tkgrid(getFrame(var_y_box), optionsFrame, sticky = "new", columnspan = 2)
    tkgrid(decreasingFrame, sticky = "nw")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
    dialogSuffix(rows = 6, columns = 1)
}
