# TODO:
# 1. [ ] Show system locale for Mac and Linux
# 2. [ ] for (local) button: add context menu either to show or to print locale
# 3. [ ] Replace functions:
#       - radioButtons_horizontal() -> bs_radiobuttons()
#       - bs_check_boxes()          -> bs_checkboxes()
# 4. [ ] Select icon of a flag that depends on current R locale:
#       - for lt, en, ru, de (an first)
#       - all other languages (the next step)
#       - Use the default flag, if appropriate specific flag is not found

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

# str_split(cur_loc_all, pattern = ";")[[1]]
window_locale_set_0 <- function(parent = CommanderWindow()) {

    # Functions --------------------------------------------------------------
    cmd_control_activation <- function() {

        switch(tclvalue_chr(options_Variable),
               "default" = {
                   set_values(var_y_box, "")
                   tk_disable(var_y_box)
                   tk_disable(locale_entry)
                   set_values(locale_entry, "System's default")

                   tk_disable(check_locale_CheckBox)
                   tclvalue(check_locale_Variable) <- "0"

               },
               "other" = {
                   tk_normalize(var_y_box)
                   set_values(var_y_box, values = locales)
                   tk_activate(locale_entry)
                   set_values(locale_entry, "")
                   tk_normalize(check_locale_CheckBox)
                   tclvalue(check_locale_Variable) <- "1"
               }
        )
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_update_locale_entry <- function() {
        set_values(locale_entry, get_selection(var_y_box))
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_clear_locale_entry <- function() {
        set_values(locale_entry, "")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_update_yview_to <- function(txt = NULL) {
        if (is.null(txt)) {
            txt <- get_values(locale_entry)
        }
        # \Q ...\E  escape regular expression
        pattern <- stringr::regex(str_c("^\\Q", txt, "\\E"), ignore_case = TRUE)
        str     <- tolower(get_values(var_y_box))
        sel_ind <- stringr::str_which(str, pattern)[1]

        if (!is.na(sel_ind)) {
            # set_selection(var_y_box, sel_ind)
            # tk_see(var_y_box, sel_ind)

            set_selection(var_y_box, "", clear = TRUE)
            set_yview(var_y_box, sel_ind)
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_update_yview <- function() {
        cmd_update_yview_to(NULL)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_get_locale <- function() {

        cur_loc_all <- Sys.getlocale()

        if (.Platform$OS.type == "windows") {
            pattern <-
                "^LC_COLLATE=(.*?);LC_CTYPE=(.*?);LC_MONETARY=(.*?);LC_NUMERIC=(.*?);LC_TIME=(.*?)$"
            # "^LC_COLLATE=(.*?);LC_CTYPE=(.*?);LC_MONETARY=(.*?);LC_NUMERIC=C;LC_TIME=(.*?)$"

            # 'cur_loc' must be informative enough
            # cur_loc <-
            #     cur_loc_all %>%
            #     str_match(pattern) %>%
            #     as.vector() %>%
            #     .[-1] %>%
            #     unique() %>%
            #     str_c(collapse = "; ")

            cur_loc <-
                cur_loc_all %>%
                str_match(pattern) %>%
                as.vector() %>%
                .[-1] %>%
                forcats::fct_infreq() %>%
                levels() %>%
                .[1]

            # 'loc_to_view' must be short enough
            loc_to_view <- str_extract(cur_loc_all, "(?<=\\=).*?(?=(\\.|;|_))")

        } else {
            cur_loc <- cur_loc_all
            loc_to_view <- str_sub(cur_loc_all, 1, 5)
        }

        if (tclvalue(options_Variable) == "default") {
            tclvalue(options_Variable) <- "other"
            cmd_control_activation()
        }

        set_values(locale_entry, cur_loc)

        cmd_update_yview_to(loc_to_view)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_print_current_locale <- function() {

        if (isTRUE(tclvalue_lgl(print_r_locale))) {
            paste0(
                '## Current R locale \n',
                'Sys.getlocale()') %>%
                Rcmdr::doItAndPrint()

            tclvalue(print_r_locale) <- FALSE
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @rdname Menu-window-functions
    #' @export
    #' @keywords internal
    cmd_get_locale_of_os <- function() {

        # Cursor
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get information
        if (isTRUE(tclvalue_lgl(print_os_locale))) {

            sys_info <- get_system_info()

            if (is.null(sys_info)) {
                msg <-
                    str_c("## No information about operating system (OS)\n",
                          "## locale is available for Mac and Linux.")

            } else {

                # .Platform$OS.type == "windows"
                os_locale <-
                    sys_info %>%
                    str_subset(regex("(locale|OS Name)", ignore_case = TRUE)) %>%
                    str_replace("           ", " ") %>%
                    str_c("# - ", .)

                msg <-
                    c('## Current locale of operating system (OS):\n',
                      os_locale,
                      "\n# NOTE: OS locale cannot be changed from R.") %>%
                    str_c(collapse = "\n")


            }
        }

        # Print information
        Rcmdr::logger(msg)

        tclvalue(print_os_locale) <- FALSE
        tk_disable(b2)
        tip(b2) <- str_c("This information is already printed.\n\n", msg)
    }

    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        locale_value <- get_values(locale_entry)
        opt          <- tclvalue_chr(options_Variable)
        check_locale <- tclvalue_lgl(check_locale_Variable)
        show_output  <- tclvalue_lgl(show_output_Variable)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (variable_is_not_selected(locale_value, "locale")) {
            return()
        }

        if (check_locale && (opt != "default")) {
            # Default locale does not pass this function (on Windows)

            suppressWarnings({
                initial_locale <- Sys.getlocale()

                # On error, the string for new locale is empty (on Windows)
                desired_locale  <- Sys.setlocale(locale = locale_value)
                real_locale     <- Sys.getlocale()

                mismatch <- !identical(desired_locale, real_locale)

                Sys.setlocale(locale = initial_locale)

                if (mismatch) {
                    show_error_messages(
                        title = "Locale Not Changed",
                        message = str_glue(
                            "It seems that locale '{locale_value}' cannot be \n",
                            "used on your computer. Please, try another loacle."))
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

        if (show_output == FALSE) {
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


            this_parent <- tcl_get_parent(top)

            # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            closeDialog()

            tkfocus(parent)
            tkraise(parent)

        } else {
            logger_error(command, error_msg = result)
            show_code_evaluation_error_message()
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fg_col <- Rcmdr::getRcmdr("title.color")

    print_r_locale  <- tclVar(TRUE)
    print_os_locale <- tclVar(TRUE)


    if (.Platform$OS.type == "windows") {
        locales <- windows_languages
    } else {
        locales <- system("locale -a", intern = TRUE)
    }

    locales <- sort(locales)

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize_dialog(title = gettext_bs("Change R Locale"), parent = parent)

    tk_title(top, "Change R Locale") # Title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
    show_locale_frame <- tk2frame(top)

    b1 <- tk2button(
        show_locale_frame,
        width = 25,
        text = "Show current R locale (local)",
        tip  = str_c(
            "Show current locale used in R. \n",
            "Double-click to print this information in console."
        ),
        command = cmd_get_locale)

    b2 <- tk2button(
        width = 22,
        show_locale_frame,
        text = "Show OS locale (global)",
        tip  = str_c(
            "Global locale of operating sytem (OS). \n",
            "Administrator password may be required."
        ),
        command = cmd_get_locale_of_os)


    # List of locales
    var_y_box <-
        bs_listbox(
            parent          = upper_frame,
            title           = gettext_bs("List of locales"),
            values          = locales,
            value           = NULL,
            height          = 7,
            width           = c(44, Inf),
            selectmode      = "single",
            on_keyboard_fun = cmd_update_locale_entry,
            on_release      = cmd_update_locale_entry,
            on_double_click = onOK,
            # on_triple_click = onOK,
            tip = str_c(
                "Short names of locales.\n",
                "NOTE: some of the listed locales may \n",
                "not be available on your computer. \n",
                'Select one to update "Locale" box. \n',
                'Double-click to set the selected locale.'
            )
        )

    # Text entry box
    # locale_variable <- tclVar(Sys.getlocale())

    locale_entry <- bs_entry(
        parent = upper_frame,
        label_position = "above",
        label = "Locale:",
        tip = str_c(
            "Enter either a name (short or full) of locale."
        ),
        width = 46,
        on_key_release    = cmd_update_yview,
        on_double_click_3 = cmd_clear_locale_entry
    )

    # Check box
    bs_check_boxes(
        upper_frame,
        frame         = "check_locale_frame",
        boxes         = c("check_locale_", "show_output_"),
        # commands      = list("check_locale_" = cmd_checkbox),
        initialValues = c("1", "1"),
        labels        = gettext_bs(c(
            "Check if locale can be used on this computer",
            "Print information about locale after it is set"))
    )

    # Layout

    tkgrid(upper_frame, sticky = "sew", padx = c(10, 10))

    tkgrid(options_outer_frame, sticky = "sw")
    # tkgrid(options_Frame, show_locale_frame, sticky = "s")
    tkgrid(options_Frame, sticky = "s")


    tkgrid(getFrame(var_y_box),  pady = c(5, 0), sticky = "w")

    tkgrid(locale_entry$frame, sticky = "w", pady = c(5, 0))

    tkgrid(check_locale_frame, sticky = "sw")


    tkgrid(show_locale_frame, sticky = "")
    tkgrid(b1, b2, sticky = "sw", pady = c(5, 0))

    # Help menus -------------------------------------------------------------
    help_menu <- function() {

      menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

      tkadd(menu_main, "command",
        label    = "About setting locales in R",
        command  = open_help("locales", package = "base"))

      tkadd(menu_main, "command",
        label    = "Localization information",
        command  = open_help("l10n_info", package = "base"))

      tkpopup(menu_main,
        tkwinfo("pointerx", top),
        tkwinfo("pointery", top))
    }

    # Finalize ---------------------------------------------------------------
    ok_cancel_help(on_help = help_menu)
    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Interactive bindings ---------------------------------------------------

    tkbind(b1, "<Double-Button-1>", cmd_print_current_locale)

    # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_control_activation()
}
