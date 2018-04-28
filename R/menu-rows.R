# ============================================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_rownames <- function() {
    Library("tidyverse")

    doItAndPrint(glue::glue("rownames({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_rowid_to_col <- function() {
    Library("tidyverse")

    # This row deletes row names:
    # '# new_df <- tibble::rowid_to_column({ActiveDataSet()}, var = "rows_id")'


    # cabbages %>%
    #     mutate(row_number = 1:n()) %>%
    #     select(row_number, everything())

    new_var <- "row_number"

    which_position <- "first"

    # Do commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_position <-
        switch(which_position,
            "first" = glue("%>% \n",
                           "dplyr::select({new_var}, everything())"),
            "last" = "")

    cmd_ungroup <- if (is_grouped_df(ActiveDataSet())) "ungroup() %>% \n" else ""

    command <- style_cmd(glue::glue(
        '## An example of code: \n\n',

        '## Add row numbers \n',
        "{ActiveDataSet()} <- {ActiveDataSet()} %>% \n",
        "{cmd_ungroup}",
        "dplyr::mutate({new_var} = 1:n())",
        "{cmd_position}"))

    doItAndPrint(command)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' #' @rdname Menu-window-functions
#' #' @export
#' #' @keywords internal
#' command_rownames_to_col <- function() {
#'     Library("tidyverse")
#'
#'     doItAndPrint(glue::glue(
#'         '## An example of code: \n\n',
#'         '# new_df <- tibble::rownames_to_column({ActiveDataSet()}, var = "row_names")'))
#' }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_col_to_rownames <- function() {
    Library("tidyverse")

    doItAndPrint(glue::glue(
        '## An example of code: \n\n',
        '# new_df <- tibble::column_to_rownames({ActiveDataSet()}, var = "row_names")'))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_arrange <- function() {
    Library("tidyverse")

    doItAndPrint(glue::glue(
        '## Examples of code \n\n',

        '# Sort rows ascending: \n',
        '# new_df <- dplyr::arrange({ActiveDataSet()}, {listVariables()[1]}) \n\n',

        '# Sort rows descending:: \n',
        '# new_df <- dplyr::arrange({ActiveDataSet()}, desc({listVariables()[1]})) \n'))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rownames_to_col <- function(){
    dataSet <- activeDataSet()

    initializeDialog(title = gettextRcmdr("Move row names to column"))

    name_variable <- tclVar(unique_colname("row_name"))
    name_frame <- tkframe(top)
    name_entry <- ttkentry(name_frame, width = "47", textvariable = name_variable)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        new_name <- trim.blanks(tclvalue(name_variable))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (!is.valid.name(new_name)) {
            errorCondition(
                recall = command_rownames_to_col,
                message = paste0( '"',new_name,'" ', gettextRcmdr("is not a valid name."))
            )
            return()
        }

        if (is.element(new_name, listDataSets())) {
            if ("no" == tclvalue(checkReplace(new_name,
                                              type = gettextRcmdr("Variable")))) {
                closeDialog()
                command_rownames_to_col()
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        Library("tibble")

        command <- glue(
            "## ", gettext_Bio("Move row names to column"), "\n\n",
            "{activeDataSet()} <- {activeDataSet()} %>% \n",
            'tibble::rownames_to_column("{new_name}")') %>%
            style_cmd()

        logger(command)
        result <- justDoIt(command)

        if (class(result)[1] !=  "try-error")
            activeDataSet(activeDataSet())

        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "rownames_to_column")

    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")
    tkgrid(label_rcmdr(
        top,
        text = gettextRcmdr("Move row names to column"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(name_frame, sticky = "w")

    tkgrid(
        label_rcmdr(
            name_frame,
            text = gettextRcmdr("Column name for row names:"),
            foreground = getRcmdr("title.color")),
        sticky = "w"
    )

    tkgrid(name_entry, sticky = "w")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
}