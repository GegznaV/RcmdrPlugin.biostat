# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TODO:
# [v] 1. Change interface for name input and prefix/suffix input:
#        make separate options either to write names or to add preffix and suffix.


#' Rcmdr window for log transformation
#'
#' @export
#' @keywords internal
#' @family transformations
#'
window_transform_log <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_Bio("Logarithmic transformation"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")
    tkgrid(label_rcmdr(
        top,
        text = gettext_bs("Logarithmic transformation"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9), columnspan = 2)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    change_prefix <- function() {
        tclvalue(prefix_variable) <- glue("{tclvalue(log_txtVariable)}_")
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(prefix    = "log_",
                     suffix    = "",
                     log_txt   = "log10",
                     fun_type  = "Tidyverse",
                     variables = NULL)

    dialog_values <- getDialog("window_transform_log", defaults)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)

    variableBox <-
        variableListBox2(upper_frame,
                         Numeric(),
                         selectmode = "multiple",
                         title = gettext_Bio("Variables (pick one or more)"),
                         initialSelection = varPosn(dialog_values$variables, "numeric"),
                         listHeight = 7
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    middle_frame <- tkframe(top)

    prefix_variable <- tclVar(dialog_values$prefix)
    prefix_field    <- ttkentry(middle_frame,
                                width = "29",
                                textvariable = prefix_variable)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    suffix_variable <- tclVar(dialog_values$suffix)
    suffix_field    <- ttkentry(middle_frame,
                                width = "29",
                                textvariable = suffix_variable)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    log_txt_outter_Frame <- tkframe(upper_frame)
    Rcmdr::radioButtons(log_txt_outter_Frame,
                        name         = "log_txt",
                        title        = gettext_Bio("Choose of logarithmic transformation"),
                        buttons      = c("common", "binary", "natural", "natural_1p"),
                        values       = c("log10", "log2", "log", "log1p"),
                        initialValue = dialog_values$log_txt,
                        labels       =  gettext_Bio(
                            c("Common,  log(x, base = 10)",
                              "Binary,  log(x, base = 2)",
                              "Natural, log(x, base = e)",
                              "Natural, log(x + 1, base = e)")),
                        command      = change_prefix
    )
    change_prefix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    radioButtons_horizontal(name         = "fun_type",
                            title        = gettext_Bio("Use functions: "),
                            title.color  = getRcmdr("title.color"),
                            buttons      = c("tidyverse", "base"),
                            values       = c("Tidyverse", "Base_R"),
                            initialValue = dialog_values$fun_type,
                            labels       = gettext_Bio(c("Tidyverse", "Base R")))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        prefix    <- trim.blanks(tclvalue(prefix_variable))
        suffix    <- trim.blanks(tclvalue(suffix_variable))
        log_txt   <- tclvalue(log_txtVariable)
        fun_type  <- tclvalue(fun_typeVariable)
        variables <- getSelection(variableBox)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_transform_log",
                  list(prefix    = prefix,
                       suffix    = suffix,
                       log_txt   = log_txt,
                       fun_type  = fun_type,
                       variables = variables
                  )
        )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(variables) == 0) {
            errorCondition(recall  = window_transform_log,
                           message = gettext_Bio("You must select a variable."))
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        .activeDataSet <- ActiveDataSet()

        new_names <- paste0(prefix, variables, suffix) %>% make.names()

        # Check if new variable names are not duplicated ~~~~~~~~~~~~~~~~~~~~~~
        for (i in seq_along(variables)) {

            if (!is.valid.name(new_names[i])) {
                errorCondition(
                    recall  = window_transform_log,
                    message = paste(new_names[i], gettext_Bio("is not a valid name."))
                )
                return()
            }
            if (is.element(new_names[i], Variables())) {
                if ("no" == tclvalue(checkReplace(new_names[i]))) {
                    window_transform_log()
                    return()
                }
            }
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command <- switch(fun_type,
               # Use base R functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               "Base_R" = {
                   tans_txt <- glue("{new_names} <- {log_txt}({variables})")

                   glue("{.activeDataSet} <- within({.activeDataSet}, {{\n",
                        "{paste(tans_txt, collapse ='\n')} \n",
                        "}})\n")
               },

               # Use Tidyverse functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               "Tidyverse" = {
                   Library("tidyverse")

                   tans_txt <- glue("{new_names} = {log_txt}({variables})")

                   if (length(tans_txt) == 1) {
                       glue("{.activeDataSet} <- {.activeDataSet} %>%\n",
                            "dplyr::mutate({tans_txt})\n")

                   } else {
                       glue("{.activeDataSet} <- {.activeDataSet} %>%\n",
                            'dplyr::mutate(\n{paste0(tans_txt, collapse = ",\n")}\n',
                            ')\n')
                   }

               },
               # default
               stop("Unrecognized option:", fun_type)
               )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command <- style_cmd(command)

        result <- justDoIt(command)

        if (class(result)[1] != "try-error")
            activeDataSet(.activeDataSet, flushModel = FALSE)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        msg <- glue("#---  ", gettext_Bio("Logarithmic transformation"), "  ---#\n\n",
                    "# ", gettext_Bio("New variable(s):"), " \n",
                    paste("#   ", new_names, collapse = "\n"), "\n\n\n")

        logger(paste0(msg, command, collapse = "\n"))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
    } # [end: onOK] ----------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "log")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame)
    tkgrid(log_txtFrame, padx = c(15, 5))
    tkgrid(getFrame(variableBox), log_txt_outter_Frame, sticky = "nw")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(middle_frame, sticky = "ew")
    tkgrid(labelRcmdr(middle_frame,
                      text = gettext_Bio("Prefix for variable names (optional):"),
                      fg = fg_col),
           labelRcmdr(middle_frame, text = gettext_Bio("     ")),
           labelRcmdr(middle_frame,
                      text = gettext_Bio("Suffix for variable names (optional):"),
                      fg = fg_col),
           sticky = "ew",
           pady = c(10, 0))
    tkgrid(prefix_field,
           labelRcmdr(middle_frame, text = gettext_Bio("     ")),
           suffix_field, sticky = "ew")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(fun_typeFrame,
           sticky = "w",
           pady = c(10, 0))
    #
    tkgrid(buttonsFrame, sticky = "ew", columnspan = 2)

    dialogSuffix(rows = 4,
                 columns = 2,
                 preventGrabFocus = TRUE)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
