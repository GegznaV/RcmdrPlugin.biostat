# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TODO:
# [v] 1. Change interface for name input and prefix/suffix input:
#        make separate options either to write names or to add prefix
#        and suffix.


#' Rcmdr window for log transformation
#'
#' @export
#' @keywords internal
#' @family transformations
#'
window_num_transform_log <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Logarithmic transformation"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")
    tkgrid(bs_label(
        top,
        text = gettext_bs("Logarithmic transformation"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9), columnspan = 2)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    change_prefix <- function() {
        tclvalue(prefix_variable) <- str_glue("{tclvalue(log_txtVariable)}_")
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(prefix    = "log_",
                     suffix    = "",
                     log_txt   = "log10",
                     fun_type  = "Tidyverse",
                     variables = NULL)

    initial <- getDialog("window_num_transform_log", defaults)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)

    variableBox <-
        bs_listbox(
            parent     = upper_frame,
            values     = variables_num(),
            selectmode = "multiple",
            title      = gettext_bs("Variables (pick one or more)"),
            value      = initial$variables,
            height     = 7
        )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    middle_frame <- tkframe(top)

    prefix_variable <- tclVar(initial$prefix)
    prefix_field    <- ttkentry(middle_frame,
                                width = "29",
                                textvariable = prefix_variable)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    suffix_variable <- tclVar(initial$suffix)
    suffix_field    <- ttkentry(middle_frame,
                                width = "29",
                                textvariable = suffix_variable)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    log_txt_outter_Frame <- tkframe(upper_frame)
    Rcmdr::radioButtons(
        log_txt_outter_Frame,
        name         = "log_txt",
        title        = gettext_bs("Choose of logarithmic transformation"),
        buttons      = c("common", "binary", "natural", "natural_1p"),
        values       = c("log10", "log2", "log", "log1p"),
        initialValue = initial$log_txt,
        labels       =  gettext_bs(
            c("Common,  log(x, base = 10)",
              "Binary,  log(x, base = 2)",
              "Natural, log(x, base = e)",
              "Natural, log(x + 1, base = e)")),
        command      = change_prefix
    )
    change_prefix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    radioButtons_horizontal(
        name         = "fun_type",
        title        = gettext_bs("Use functions: "),
        title.color  = getRcmdr("title.color"),
        buttons      = c("tidyverse", "base"),
        values       = c("Tidyverse", "Base_R"),
        initialValue = initial$fun_type,
        labels       = gettext_bs(c("Tidyverse", "Base R")))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        prefix    <- tclvalue_chr(prefix_variable)
        suffix    <- tclvalue_chr(suffix_variable)
        log_txt   <- tclvalue(log_txtVariable)
        fun_type  <- tclvalue(fun_typeVariable)
        variables <- get_selection(variableBox)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_num_transform_log",
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
            errorCondition(recall  = window_num_transform_log,
                           message = gettext_bs("You must select a variable."))
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        .ds <- active_dataset_0()

        new_names <- paste0(prefix, variables, suffix) %>% make.names()

        # Check if new variable names are not duplicated ~~~~~~~~~~~~~~~~~~~~~~
        for (i in seq_along(variables)) {

            if (!is.valid.name(new_names[i])) {
                errorCondition(
                    recall  = window_num_transform_log,
                    message = paste(new_names[i], gettext_bs("is not a valid name."))
                )
                return()
            }
            if (is.element(new_names[i], Variables())) {
                if ("no" == tclvalue(checkReplace(new_names[i]))) {
                    window_num_transform_log()
                    return()
                }
            }
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command <- switch(
            fun_type,
            # Use base R functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            "Base_R" = {
                tans_txt <- str_glue("{new_names} <- {log_txt}({variables})")

                str_glue("{.ds} <- within({.ds}, {{\n",
                         "{paste(tans_txt, collapse ='\n')} \n",
                         "}})\n")
            },

            # Use Tidyverse functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            "Tidyverse" = {
                Library("tidyverse")

                tans_txt <- str_glue("{new_names} = {log_txt}({variables})")

                if (length(tans_txt) == 1) {
                    str_glue("{.ds} <- {.ds} %>%\n",
                             "dplyr::mutate({tans_txt})\n")

                } else {
                    str_glue("{.ds} <- {.ds} %>%\n",
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
            active_dataset(.ds, flushModel = FALSE)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        msg <- str_glue(
            "#---  ", gettext_bs("Logarithmic transformation"), "  ---#\n\n",
            "# ", gettext_bs("New variable(s):"), " \n",
            paste("#   ", new_names, collapse = "\n"), "\n\n\n")

        logger(paste0(msg, command, collapse = "\n"))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
    } # [end: onOK] ----------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(helpSubject = "log")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame)
    tkgrid(log_txtFrame, padx = c(15, 5))
    tkgrid(getFrame(variableBox), log_txt_outter_Frame, sticky = "nw")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(middle_frame, sticky = "ew")
    tkgrid(labelRcmdr(middle_frame,
                      text = gettext_bs("Prefix for variable names (optional):"),
                      fg = fg_col),
           labelRcmdr(middle_frame, text = gettext_bs("     ")),
           labelRcmdr(middle_frame,
                      text = gettext_bs("Suffix for variable names (optional):"),
                      fg = fg_col),
           sticky = "ew",
           pady = c(10, 0))
    tkgrid(prefix_field,
           labelRcmdr(middle_frame, text = gettext_bs("     ")),
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
