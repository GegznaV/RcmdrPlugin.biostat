# Locale ---------------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_get_locale <- function() {
    paste0(
        '## Current locale  \n',
        'Sys.getlocale()') %>%
        Rcmdr::doItAndPrint()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_locale_set <- function() {
    window_locale_set_0()
}


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_locale_set_0 <- function(parent = CommanderWindow()) {

    # Functions --------------------------------------------------------------
    cmd_control_activation <- function() {

        switch(tclvalue_chr(options_Variable),
               "default" = {
                   set_values(var_y_box, "")
                   tk_disable(var_y_box)
                   tk_disable(locale_entry)
                   tclvalue(locale_variable) <- "System's default"

                   tk_disable(check_locale_CheckBox)
                   tclvalue(check_locale_Variable) <- "0"

               },
               "other" = {
                   tk_normalize(var_y_box)
                   set_values(var_y_box, values = locales)
                   tk_activate(locale_entry)
                   tclvalue(locale_variable) <- ""
                   tk_activate(check_locale_CheckBox)
                   tclvalue(check_locale_Variable) <- "1"
               }
        )
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_update_textentry <- function() {
        tclvalue(locale_variable) <- get_selection(var_y_box)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_clear_textentry <- function() {
        tclvalue(locale_variable) <- ""
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_update_selection_0 <- function(txt = NULL) {
        if (is.null(txt)) {
            txt <- tclvalue_chr(locale_variable)
        }
        # \Q ...\E  escape regular expression
        pattern <- stringr::regex(str_c("^\\Q", txt, "\\E"), ignore_case = TRUE)
        str     <- tolower(get_values(var_y_box))
        sel_ind <- stringr::str_which(str, pattern)[1]

        # message(str_c(sel_ind, txt, pattern, sep = "\t"))
        if (!is.na(sel_ind)) {
            set_selection(var_y_box, sel_ind)
            tk_see(       var_y_box, sel_ind)
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_update_selection <- function() {
        cmd_update_selection_0(NULL)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_get_locale <- function() {

       cur_loc <- Sys.getlocale()

       tclvalue(locale_variable) <- cur_loc

       if (.Platform$OS.type == "windows") {
           txt <- str_extract(cur_loc, "(?<=\\=).*?(?=(\\.|;|_))")
       } else {
           txt <- str_sub(cur_loc, 1, 5)
       }

       cmd_update_selection_0(txt = txt)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_print_current_locale <- function() {

        if (isTRUE(tclvalue_lgl(print_locale))) {
            paste0(
                '## Current locale  \n',
                'Sys.getlocale()') %>%
                Rcmdr::doItAndPrint()

            tclvalue(print_locale) <- FALSE
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
        hide_output  <- tclvalue_lgl(hide_output_Variable)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (variable_is_not_selected(locale_value, "locale")) {
            return()
        }

        if (check_locale && (opt != "default")) {
           # Default locale does not pass this function (on Windows)

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

        if (hide_output) {
            cmd_locale_set <- str_glue("invisible({cmd_locale_set})")
        }

        command <- str_glue(
            '## Change locale to {txt_locale_set} \n',
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
        tkfocus(parent)
        tkraise(parent)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fg_col <- Rcmdr::getRcmdr("title.color")
    print_locale <- tclVar(TRUE)


    if (.Platform$OS.type == "windows") {
        locales <- windows_languages
    } else {
        locales <- system("locale -a", intern = TRUE)
    }

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Change Locale"))

    tk_title(top, "Change locale") # Title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Widgets ----------------------------------------------------------------

    upper_frame <- tkframe(top)

    # Radiobuttons
    options_outer_frame <- tkframe(upper_frame)

    radioButtons_horizontal(
        options_outer_frame,
        title           = "Change locale into: ",
        title.color     = fg_col,

        # right.buttons = FALSE,
        name            = "options_",
        sticky_buttons  = "e",
        buttons         = c("default"          , "other"),
        values          = c("default"          , "other"),
        labels          = c("System's default" , "Other"),
        initialValue    = "other",
        command         = cmd_control_activation
    )

    # Show locale button
    show_locale_frame <- tk2frame(options_outer_frame)
    b1 <- tk2button(
        show_locale_frame,
        text = "Show current locale",
        tip  = str_c(
            "Click to select current locale. \n",
            "Double-click to print locale in console."
            ),
        command = cmd_get_locale)


    # List of locales
    var_y_box <- bs_listbox(
        parent          = upper_frame,
        title           = gettext_bs("List of locales"),
        values          = locales,
        value           = NULL,
        height          = 7,
        width           = c(43, Inf),
        selectmode      = "single",
        on_release      = cmd_update_textentry,
        on_double_click = onOK,
        # on_triple_click = onOK,
        tip = str_c(
            "Short names of locales.\n",
            "NOTE: some of the listed locales may \n",
            "not be available on your computer. \n",
            'Click and release to update "Locale" box. \n',
            'Double-click to set the selected locale.'
        )
    )

    # Text entry box
    # locale_variable <- tclVar(Sys.getlocale())
    locale_variable <- tclVar("")
    locale_frame    <- tkframe(upper_frame)
    locale_entry    <- tk2entry(
        locale_frame,
        tip = str_c(
            "Enter either short or full name of locale.\n",
            "Double-right-click to clear entry."
        ),
        width = "46",
        textvariable = locale_variable)


    # Check box
    bs_check_boxes(upper_frame,
                   frame         = "check_locale_frame",
                   boxes         = c("check_locale_", "hide_output_"),
                   # commands      = list("check_locale_" = cmd_checkbox),
                   initialValues = c("1", "0"),
                   labels        = gettext_bs(c(
                       "Check if the locale can be used on this computer",
                       "Hide output"))
    )


    # Layout

    tkgrid(upper_frame, sticky = "s", padx = c(10, 10))

    tkgrid(options_outer_frame, sticky = "s")
    tkgrid(options_Frame, show_locale_frame, sticky = "s")
    tkgrid(b1, padx = c(18, 0), sticky = "se")

    tkgrid(getFrame(var_y_box),  pady = c(5, 0))

    tkgrid(locale_frame, sticky = "w", pady = c(5, 0))
    tkgrid(
        bs_label(
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
    tkbind(locale_entry, "<KeyRelease>",      cmd_update_selection)
    tkbind(locale_entry, "<Double-Button-3>", cmd_clear_textentry)

    tkbind(b1,           "<Double-Button-1>", cmd_print_current_locale)


    # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_control_activation()
}
