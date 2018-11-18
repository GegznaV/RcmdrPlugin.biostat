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
    initializeDialog(title = gettext_Bio("Arrange rows"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    var_y_box <-
        variableListBox2(
            upper_frame,
            Variables(),
            selectmode = "single",
            initialSelection = NULL,
            title            = gettext_Bio("Variable for sorting"),
            listHeight       = 8
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    optionsFrame <- tkframe(upper_frame)
    Rcmdr::radioButtons(
        optionsFrame,
        name    = "decreasing",
        buttons = gettext_Bio(c("Ascending", "Descending")),
        values  = c("FALSE", "TRUE"),
        labels  = gettext_Bio(c("Ascending", "Descending")),
        title   = gettext_Bio("Sorting order")
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        y          <- getSelection(var_y_box)
        decreasing <- as.logical(tclvalue(decreasingVariable))
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(y) == 0) {
            errorCondition(
                recall = window_rows_arrange,
                message = gettext_Bio("You must select a variable.")
            )
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("dplyr")

        y <- stringr::str_c("`", y, "`")

        variables <- if (decreasing) {
            glue("desc({y})")

        } else {
            y
        }

        dataSet    <- activeDataSet()
        new_dsname <- activeDataSet()

        command <- glue(
            "## Sort rows \n",
            "{new_dsname} <- {dataSet} %>% \n",
            "dplyr::arrange({variables})") %>%
            style_cmd()

        result <- justDoIt(command)

        if (class(result)[1] !=  "try-error") {
            # Change active dataset
            activeDataSet(new_dsname, flushModel = FALSE)

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


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "arrange", helpPackage = "dplyr")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame, sticky = "new")
    tkgrid(getFrame(var_y_box), optionsFrame, sticky = "new", columnspan = 2)
    tkgrid(decreasingFrame, sticky = "nw")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
    dialogSuffix(rows = 6, columns = 1)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_arrange_tmp <- function() {
    Library("tidyverse")

    doItAndPrint(
        str_glue(
            '## Examples of code \n\n',

            '# Sort rows ascending: \n',
            '# new_df <- dplyr::arrange({ActiveDataSet()}, {listVariables()[1]}) \n\n',

            '# Sort rows descending:: \n',
            '# new_df <- dplyr::arrange({ActiveDataSet()}, desc({listVariables()[1]})) \n'
        )
    )

    # command_dataset_refresh()
}
