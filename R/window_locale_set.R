
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_locale_set <- function() {

    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fg_col <- Rcmdr::getRcmdr("title.color")

    locales <-
        if (.Platform$OS.type == "windows") {
            windows_languages
        } else {
            system("locale -a", intern = TRUE)
        }

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Change Locale"))

    tk_title(top, "Change locale") # Title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    cmd_control_activation <- function() {

        switch(tclvalue_chr(options_Variable),
               "default" = {
                   set_values(var_y_box, "")
                   tk_disable(var_y_box$listbox, background = "grey95")
                   tk_disable(locale_entry)
                   tclvalue(locale_variable) <- "System's default"
               },
               "other" = {
                   tk_normalize(var_y_box$listbox, background = "white")
                   set_values(var_y_box, vals = locales)
                   tk_activate(locale_entry)
                   tclvalue(locale_variable) <- ""
               }
        )
    }

    cmd_update_textentry <- function() {
        tclvalue(locale_variable) <- get_selection(var_y_box)
    }

    cmd_update_selection <- function() {

        txt <- tclvalue_chr(locale_variable)

        # \Q ...\E  escape regular expression
        pattern <- stringr::regex(str_c("^\\Q", txt, "\\E"), ignore_case = TRUE)
        str     <- tolower(get_values(var_y_box))
        sel_ind <- stringr::str_which(str, pattern)[1]

        # message(str_c(sel_ind, txt, pattern, sep = "\t"))

        if (!is.na(sel_ind)) {
            set_new_selection(var_y_box, sel_ind)
            set_yview(var_y_box, sel_ind)
        }
    }

    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        locale_value <- tclvalue_chr(locale_variable)
        opt          <- tclvalue_chr(options_Variable)
        check_locale <- tclvalue_lgl(check_locale_Variable)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (variable_is_not_selected(locale_value, "locale")) {
            return()
        }

        if (check_locale && (opt != "default")) {

            suppressWarnings({
                initial_locale <- Sys.getlocale()

                # On error the string for new locale is empty (on Windows)
                desired_locale  <- Sys.setlocale(locale = locale_value)
                real_locale     <- Sys.getlocale()

                mismatch <- !identical(desired_locale, real_locale)

                Sys.setlocale(locale = initial_locale)

                if (mismatch) {
                    show_error_messages(
                        title = "Locale Not Changed",
                        message = str_glue(
                            "It seems that locale '{locale_value}' cannot be \n",
                            "used on your computer. Try another locale."))
                    return()
                }
            })
        }

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        switch(
            opt,
            "default" = {
                cmd_locale_set <- "Sys.setlocale()"
                txt_locale_set <- "system's default"
            },
            "other"   = {
                cmd_locale_set <- str_glue('Sys.setlocale(locale = "{locale_value}")')
                txt_locale_set <- locale_value
            }
        )

        command <- str_glue(
            '## Set locale to {txt_locale_set} \n',
            "{cmd_locale_set}",
            )

        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # doItAndPrint(command)
        result <- try_command(command)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            doItAndPrint(style_cmd(command))

            # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            closeDialog()


        } else {
            logger_error(command, error_msg = result)
            show_code_evaluation_error_message()
            return()
        }

        # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

    # Widgets ----------------------------------------------------------------

    upper_frame <- tkframe(top)

    # Radiobuttons
    options_outer_frame <- tkframe(upper_frame)

    radioButtons_horizontal(
        options_outer_frame,
        title = "Change locale into: ",
        title.color = fg_col,

        # right.buttons = FALSE,
        name = "options_",
        sticky_buttons = "w",
        buttons = c("default"          , "other"),
        values =  c("default"          , "other"),
        labels =  c("System's default", "Other"),
        initialValue = "other",
        command = cmd_control_activation
    )

    # List of locales
    var_y_box <- variableListBox2(
        upper_frame,
        title        = gettext_bs("List of Locales"),
        variableList = locales,
        listHeight = 7,
        selectmode = "single",
        initialSelection = NULL,
        onRelease_fun     = cmd_update_textentry,
        onDoubleClick_fun = cmd_update_textentry
    )

    # Text entry box
    # locale_variable <- tclVar(Sys.getlocale())
    locale_variable <- tclVar("")
    locale_frame    <- tkframe(upper_frame)
    locale_entry    <- ttkentry(locale_frame, width = "38",
                                textvariable = locale_variable)


    # Check box
    bs_check_boxes(upper_frame,
                   frame         = "check_locale_frame",
                   boxes         = c("check_locale_"),
                   # commands      = list("check_locale_" = cmd_checkbox),
                   initialValues = "1",
                   labels        = gettext_bs(c("Check if locale can be used on this computer."))
    )


    # Layout

    tkgrid(upper_frame, sticky = "w", padx = c(15, 0))

    tkgrid(options_outer_frame, sticky = "nw")
    tkgrid(options_Frame, sticky = "nw")

    tkgrid(getFrame(var_y_box),  pady = c(5, 0))

    tkgrid(locale_frame, sticky = "w", pady = c(5, 0))
    tkgrid(
        label_rcmdr(
            locale_frame,
            text = gettext_bs("Locale:"),
            foreground = getRcmdr("title.color")),
        sticky = "w"
    )
    tkgrid(locale_entry, pady = c(5, 5), sticky = "w")

    tkgrid(check_locale_frame, sticky = "sw")


    # Finalize ---------------------------------------------------------------
    ok_cancel_help(helpSubject = "locales")
    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Interactive bindings ---------------------------------------------------
    tkbind(locale_entry, "<KeyRelease>", cmd_update_selection)

    # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_control_activation()
}
