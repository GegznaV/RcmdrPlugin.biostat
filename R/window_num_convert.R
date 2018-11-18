
# TODO:
#
# Change interface for name input:
#        + add separate window for name input
#        + add boxes for prefix/suffix inputs.

#' Rcmdr windows for variable class conversion
#'
#' @export
#' @keywords internal
#' @family conversion

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
window_num_convert <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Convert numeric variables into other classes"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(suffix      = gettext_bs("<automatic suffix>"),
                     into        = "character",
                     which_names = "new_names",
                     variables   = NULL)

    dialog_values <- getDialog("window_num_convert", defaults)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_Frame <- tkframe(top)

    variableBox <-
        variableListBox2(upper_Frame,
                         Numeric(),
                         selectmode       = "multiple",
                         title            = gettext_bs("Variables (pick one or more)"),
                         initialSelection = var_pos_n(dialog_values$variables, "numeric"),
                         listHeight       = 7
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    into_outter_Frame <- tkframe(upper_Frame)
    Rcmdr::radioButtons(into_outter_Frame,
                        name = "into",
                        title = gettext_bs("Convert into"),
                        buttons = c("nominal", "ordinal", "character", "numeric", "integer"),
                        values  = c("nominal", "ordinal", "character", "numeric", "integer"),
                        initialValue = dialog_values$into,
                        labels =  gettext_bs(
                            c("Nominal factors",
                              "Ordinal factors",
                              "Text",
                              "Real numbers",
                              "Integers"))
    )
    # as_factor
    # parse_factor
    # parse_double
    # parse_integer
    # parse_logical

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    suffix_var  <- tclVar(dialog_values$suffix)
    suffixField <- ttkentry(top, width = "20", textvariable = suffix_var)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    radioButtons_horizontal(name = "which_names",
                            title = gettext_bs("Variable names: "),
                            title.color = getRcmdr("title.color"),
                            buttons = c("overwrite", "new_names"),
                            values  = c("overwrite", "new_names"),
                            initialValue = dialog_values$which_names,
                            labels =  gettext_bs(c("Overwrite", "Create new")))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        suffix      <- trim.blanks(tclvalue(suffix_var))
        into        <- tclvalue(intoVariable)
        which_names <- tclvalue(which_namesVariable)
        variables   <- getSelection(variableBox)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_num_convert",
                  list(suffix = {if (nchar(suffix) == 0) gettext_bs("<automatic suffix>") else suffix},
                       into = into,
                       which_names = which_names,
                       variables = variables
                  )
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(variables) == 0) {
            errorCondition(recall = window_num_convert,
                           message = gettext_bs("You must select a variable."))
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
                new_names <-
                    if (suffix == gettext_bs("<automatic suffix>")) {
                        suffix <- switch(into,
                                         "character" = "chr",
                                         "factor"    = "fct",
                                         "nominal"   = "fct",
                                         "ordinal"   = "ord",
                                         "number"    = "num",
                                         "integer"   = "int",
                                         "logical"   = "lgl",
                                         into)
                        paste0(variables, "_", suffix)

                    } else if (length(variables) == 1) {
                        suffix

                    } else {
                        paste0(variables, suffix)
                    }

                # Check if new variable names are not duplicated ~~~~~~~~~~~~~~~~~~~~~~
                for (i in seq_along(variables)) {

                    if (!is.valid.name(new_names[i])) {
                        errorCondition(
                            recall = window_num_convert,
                            message = paste(new_names[i], gettext_bs("is not a valid name."))
                        )
                        return()
                    }
                    if (is.element(new_names[i], Variables())) {
                        if ("no" == tclvalue(checkReplace(new_names[i]))) {
                            window_num_convert()
                            return()
                        }
                    }
                }
            }
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        Library("tidyverse")

        into_fun <- switch(into,
                           "character"  = "as.character" ,
                           "factor"     = "as.factor" ,
                           "nominal"    = "factor" ,
                           "ordinal"    = "factor" ,
                           "number"     = "as.numeric"  ,
                           "integer"    = "as.integer" ,
                           "logical"    = "as.logical" ,
                           stop("Unexpected choice"))

        fct_type <- switch(into,
                           "ordinal"    = ", ordered = TRUE" ,
                           "nominal"    = ", ordered = FALSE" ,
                           "")

        tans_txt <- glue("{new_names} = {into_fun}({variables}{fct_type})")

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
        msg <- glue(
            "#---  ", gettext_bs("Convert numeric variables into"),
            " {into} variables ---#\n\n",
            "# ", gettext_bs("New variable(s):"), " \n",
            paste("#   ", new_names, collapse = "\n"), "\n\n\n")

        logger(paste0(msg, command, collapse = "\n"))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
    } # [end: onOK] ----------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "as.character"
                 # , helpPackage = "readr"
    )
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
                      text = gettext_bs("New variable name or suffix for multiple variables:"),
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
