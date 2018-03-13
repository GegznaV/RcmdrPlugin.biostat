# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# parse_logical(x, na = c("", "NA"), locale = default_locale(),
#               trim_ws = TRUE)
#
# parse_integer(x, na = c("", "NA"), locale = default_locale(),
#               trim_ws = TRUE)
#
# parse_double(x, na = c("", "NA"), locale = default_locale(),
#              trim_ws = TRUE)
#
# parse_factor(x, levels, ordered = FALSE, na = c("", "NA"),
#              locale = default_locale(), include_na = TRUE, trim_ws = TRUE)

#' Rcmdr windows for variable class conversion
#'
#' @export
#' @keywords internal
#' @family conversion
#'
window_lgl_convert <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_Bio("Convert logical variables into other classes"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(suffix = gettext_Bio("<automatic suffix>"),
                     into = "integer",
                     which_names = "add_suffix",
                     variables = NULL)

    dialog_values <- getDialog("window_lgl_convert", defaults)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_Frame <- tkframe(top)

    variableBox <-
        variableListBox2(upper_Frame,
                         variables_lgl(),                                                     # [!!!] "lgl" is neaded
                         selectmode = "multiple",
                         title = gettext_Bio("Logical variables \n(pick one or more)"),
                         initialSelection = var_pos_n(dialog_values$variables, "logical"), # [!!!] "lgl" is neaded
                         listHeight = 7
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    into_outter_Frame <- tkframe(upper_Frame)
    radioButtons(into_outter_Frame,
                 name = "into",
                 title = gettext_Bio("\nConvert into:"),
                 buttons = c("integer", "factor",  "text"),
                 values  = c("integer", "factor",  "text"),
                 initialValue = dialog_values$into,
                 labels =  gettext_Bio(
                     c("Integers",
                       "Factors",
                       "Text variables"))
    )
    # as_factor
    # parse_factor
    # parse_double
    # parse_integer
    # parse_logical

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    suffix      <- tclVar(dialog_values$suffix)
    suffixField <- ttkentry(top, width = "20", textvariable = suffix)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    radioButtons_horizontal(name = "which_names",
                            title = gettext_Bio("Names for converted variable: "),
                            title.color = getRcmdr("title.color"),
                            initialValue = dialog_values$which_names,
                            buttons = c("overwrite", "add_suffix", "new_names"),
                            values  = c("overwrite", "add_suffix", "new_names"),
                            labels =  gettext_Bio(c("Overwrite", "Add suffixes", "Create new names")))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        suffix      <- trim.blanks(tclvalue(suffix))
        into        <- tclvalue_chr(intoVariable)
        which_names <- tclvalue_chr(which_namesVariable)
        variables   <- getSelection(variableBox)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_lgl_convert",
                  list(suffix = {if (nchar(suffix) == 0) gettext_Bio("<automatic suffix>") else suffix},
                       into = into,
                       which_names = which_names,
                       variables = variables
                  )
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(variables) == 0) {
            errorCondition(recall = window_lgl_convert,
                           message = gettext_Bio("You must select a variable."))
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        .activeDataSet <- ActiveDataSet()

        switch(
            which_names,

            # Overwrite names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            "overwrite" = {
                new_names <- variables
            },

            # Use new names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            "new_names" = {
                stop("[!!!] New name window is NOT implemented yet!")   # [!!!] -------------
                new_names <- new_name_window(old_names = variables,
                                             new_names = NULL,
                                             avoid_names = Variables())
            },
            "add_suffix" = {
                new_names <-
                    if (suffix == gettext_Bio("<automatic suffix>")) {
                        suffix <- switch(into,
                                         "factor"  = "fct",
                                         "number"  = "num",
                                         "integer" = "int",
                                         "logical" = "lgl",
                                         "text"    = "chr",
                                         into)
                        paste0(variables, "_", suffix)

                    } else {
                        paste0(variables, suffix)
                    }
            })

        # Check if new variable names are not duplicated ~~~~~~~~~~~~~~~~~~~~~~
        switch(
            which_names,
            "new_names" = ,
            "add_suffix" = {

                for (i in seq_along(variables)) {

                    if (!is.valid.name(new_names[i])) {
                        errorCondition(
                            recall = window_lgl_convert,
                            message = paste(new_names[i], gettext_Bio("is not a valid name."))
                        )
                        return()
                    }
                    if (is.element(new_names[i], Variables())) {
                        if ("no" == tclvalue(checkReplace(new_names[i]))) {
                            window_lgl_convert()
                            return()
                        }
                    }
                }
            })
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        Library(c("tidyverse", "rlang"))


        into_fun <- switch(into,
                           "integer" = "rlang::as_integer" ,
                           "factor"  = "forcats::as_factor"   ,
                           "text"    = "rlang::as_character"   ,
                           stop("Unexpected choice"))

        tans_txt <- glue("{new_names} = {into_fun}({variables})")

        command <-
            if (length(tans_txt) == 1) {
                glue("{.activeDataSet} <- {.activeDataSet} %>%\n",
                     "dplyr::mutate({tans_txt})\n")

            } else {
                glue("{.activeDataSet} <- {.activeDataSet} %>%\n",
                     'dplyr::mutate(\n{paste0(tans_txt, collapse = ",\n")}\n',
                     ')\n')
            }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command <- style_cmd(command)

        result <- justDoIt(command)

        if (class(result)[1] != "try-error")
            activeDataSet(.activeDataSet, flushModel = FALSE)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        msg <- glue("#---  ", gettext_Bio("Convert logical variables into"), " {into} variables ---#\n\n",
                    "# ", gettext_Bio("New variable(s):"), " \n",
                    paste("#   ", new_names, collapse = "\n"), "\n\n\n")

        logger(paste0(msg, command, collapse = "\n"))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
    } # [end: onOK] ----------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "vector-coercion")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(intoFrame, padx = c(15, 5))
    tkgrid(getFrame(variableBox), into_outter_Frame, sticky = "nw")

    tkgrid(upper_Frame)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(which_namesFrame,
           sticky = "w",
           pady = c(10, 0))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(labelRcmdr(top,
                      text = gettext_Bio("Suffix for variable names:"),
                      fg = getRcmdr("title.color")),
           sticky = "w",
           pady = c(10, 0))

    tkgrid(suffixField, sticky = "ew", columnspan = 2)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "ew", columnspan = 2)

    dialogSuffix(rows = 4,
                 columns = 2,
                 preventGrabFocus = TRUE)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
