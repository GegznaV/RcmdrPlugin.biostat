# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_summary_missings <- function() {
    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    activate_new_plots_window <- function() {
        if (get_values(f1_options, "use_plot") == TRUE) {
            tk_normalize(f1_options, "new_plots_window")

        } else {
            tk_disable(f1_options, "new_plots_window")
        }
    }

    activate_all <- function() {
        activate_new_plots_window()
    }

    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        use_plot         <- get_values(f1_options, "use_plot")
        new_plots_window <- get_values(f1_options, "new_plots_window")
        use_numeric      <- get_values(f1_options, "use_numeric")

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if ((!use_plot) && (!use_numeric)) {
            show_error_messages(
                str_c(
                    "You should either draw a plot \n",
                    "or print a numeric summary,   \n",
                    "or do both options."),
                title = "Select What to Do",
                parent = top)

            return()
        }

        # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_summary_missings", list(
            use_plot = use_plot,
            new_plots_window = new_plots_window,
            use_numeric = use_numeric
        ))

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (use_plot) {

            Library("DescTools")
            Library("tidyverse")

            command_plot <- str_glue(
                .trim = FALSE,
                "## Plot missing values\n",
                'DescTools::PlotMiss({.ds}) \n',
                'title(main = "Missing values in dataset \'{.ds}\'",\n xlab = "Row number")\n')

            if (new_plots_window) {
                open_new_plots_window()
            }

            result <- justDoIt(command_plot)
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if (class(result)[1] != "try-error") {
                logger(style_cmd(command_plot))

            } else {
                logger_error(command_plot, error_msg = result)
                show_code_evaluation_error_message(parent = top, add_msg = result)
                return()
            }

            remove(result)

        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (use_numeric) {

            Library("DescTools")
            command_count <- str_glue(
                "## Count missing values\n",
                'DescTools::CountCompCases({.ds}) %>% \n print(digits = 1)')

            result <- justDoIt(command_count)
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if (class(result)[1] != "try-error") {
                logger(style_cmd(command_count))

            } else {
                logger_error(command_count, error_msg = result)
                show_code_evaluation_error_message(parent = top, add_msg = result)
                return()
            }

            remove(result)
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    .ds <- active_dataset() # active_dataset_0()

    # Initialize dialog window and title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    dialogue_title <- gettext_bs("Screen Missing Data")
    initializeDialog(title = dialogue_title)
    tk_title(top, dialogue_title)

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        use_plot = TRUE,
        new_plots_window = is_plot_in_separate_window(),
        use_numeric = TRUE
    )
    initial <- getDialog("window_summary_missings", defaults)


    # Widgets ----------------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f1 <- tkframe(top)

    f1_options <- bs_checkboxes(
        parent   =  f1,
        title    = "Options:",
        border   = TRUE,
        boxes    = c("use_plot", "new_plots_window", "use_numeric"),
        labels   = gettext_bs(
            "Plot missing values",
            "Create new window for plots",
            "Numeric summary"),
        values   = list(
            use_plot         = initial$use_plot,
            new_plots_window = initial$new_plots_window,
            use_numeric      = initial$use_numeric),
        commands = list("use_plot" = activate_new_plots_window)
    )

    # Layout
    tkgrid(f1, sticky = "new")
    tkgrid(f1_options$frame, sticky = "news")

    # Help menus -------------------------------------------------------------
    help_menu <- function() {

        menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkadd(menu_main, "command",
              label    = "Count missing and complete cases",
              command  = open_help("CountCompCases", package = "DescTools"))

        tkadd(menu_main, "command",
              label    = "Plot missing data",
              command  = open_help("PlotMiss", package = "DescTools"))

        tkadd(menu_main, "separator")

        tkadd(menu_main, "command",
              label    = "Missing values (NA - Not Available)",
              command  = open_help("NA", package = "base"))


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkpopup(menu_main,
                tkwinfo("pointerx", top),
                tkwinfo("pointery", top))
    }
    # Finalize ---------------------------------------------------------------

    # Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(
        close_on_ok = TRUE,
        on_help = help_menu,
        apply = "command_summary_missings()",
        reset_location = TRUE)

    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    activate_all()
}
