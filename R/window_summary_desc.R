# TODO:
#
# 1) implement function `activate_results_name`
# 2) DescTools options may be incorrect at start up, if only some options are reset.

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_summary_desc <- function() {
    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_default <- function(val, default) {

        if (is.null(val) || is.na(val)) {
            default
        } else {
            val
        }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    activate_num <- function() {

        if (get_values(f2_num_enable)) {
            tkgrid(f2_num_sub)

        } else {
            tkgrid.remove(f2_num_sub)
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    activate_plots <- function() {

        if (get_values(f2_plot_enable)) {
            tkgrid(f2_plot_sub)

        } else {
            tkgrid.remove(f2_plot_sub)
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    activate_results_name <- function() {

        # [???]

        # if (get_values(f1_keep_results)) {
        #     tkgrid(...)
        #
        # } else {
        #     tkgrid.remove(...)
        # }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    activate_all <- function() {
        activate_num()
        activate_plots()
        activate_results_name()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_big_mark_list <- function() {
        tibble::tribble(
            ~name,                       ~fun,
            gettext_bs("None")     ,       "",
            gettext_bs("Space [ ]"),       " ",
            gettext_bs("Apostrophe [']"),  "'",
            gettext_bs("Comma [,]"),       ",",
            gettext_bs("Backtick [`]"),    "`")
    }

    get_big_mark_symbol <- function() {
        name  <- get_selection(f2_big_mark)
        bands <- get_big_mark_list()
        bands[bands$name == name, ]$fun
    }

    get_big_mark_label <- function(symbol = get_big_mark_symbol()) {
        bands <- get_big_mark_list()
        bands[bands$fun == symbol, ]$name
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_lab_direction_list <- function() {
        tibble::tribble(
            ~name,                                  ~las,
            gettext_bs("Parallel to the axis"),      0,
            gettext_bs("Always horizontal"),         1,
            gettext_bs("Perpendicular to the axis"), 2,
            gettext_bs("Always vertical"),           3
        )
    }

    get_lab_direction_las <- function() {
        name  <- get_selection(f2_plot_las)
        bands <- get_lab_direction_list()
        bands[bands$name == name, ]$las
    }

    get_lab_direction_label <- function(las = get_lab_direction_las()) {
        bands <- get_lab_direction_list()
        bands[bands$las == las, ]$name
    }

    # ...

    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        by_group  <- get_values(f1_widget_y_gr$checkbox)
        y_var     <- get_selection(f1_widget_y_gr$y)
        gr_var    <- get_selection(f1_widget_y_gr$gr)

        # Numeric output
        print_num  <- get_values(f2_num_enable)
        digits_per <- get_values(f2_digits_per)
        digits_num <- get_values(f2_digits_num)
        scipen     <- get_values(f2_scipen)
        big_mark   <- get_big_mark_symbol()

        # Plots
        use_plot             <- get_values(f2_plot_enable)
        new_plots_window     <- get_values(f2_plot_opts, "new_plots_window")
        las                  <- get_lab_direction_las()

        # General
        keep_results         <- get_values(f1_keep_results,  "keep_results")
        force_options        <- get_values(f2_force_options, "force_options")

        # # Not implemented yet
        # conf_level           <- get_values(f2_conf)
        # results_name         <- get_values(f2_results_name)


        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (variable_is_not_selected(y_var, "response variable", parent = top)) {
            return()
        }

        if (isTRUE(by_group)) {
            if (variable_is_not_selected(gr_var, "explanatory variable",
                                         parent = top, article = "an")) {
                return()
            }
        }

        # if (is_empty_name(new_name, parent = top))              {return()}
        # if (is_not_valid_name(new_name, parent = top))          {return()}
        # if (forbid_to_replace_variable(new_name, parent = top)) {return()}


        all_selected <- c(y_var, gr_var)
        non_standard <- all_selected != make.names(all_selected)

        if (by_group && any(non_standard)) {
            # RcmdrTkmessageBox(popup_msg, icon = "error", title = title, type = "ok")
            tk_messageBox(
                # parent = top,
                message = str_c(collapse = "\n",
                    "The calculations may fail due to non-standard variable names: \n",
                    all_selected[non_standard]
                ),
                caption = "Non-standard Variable Names",
                type = "ok",
                icon = "warning")
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_summary_desc", list(
            # Variables
            y_var     = y_var,
            gr_var    = gr_var,
            by_group  = by_group,

            keep_results  = keep_results,
            force_options = force_options,
            # conf_level   = 0.95,    # Not implemented

            # Numeric output
            print_num  = print_num,
            digits_per = digits_per,
            digits_num = digits_num,
            scipen     = scipen,
            big_mark   = get_big_mark_label(big_mark),

            # Plots
            use_plot         = use_plot,
            new_plots_window = new_plots_window,
            labels_direction = get_lab_direction_label(las)
        ))

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        y_var  <- safe_names(y_var)
        gr_var <- safe_names(gr_var)


        rez <- unique_obj_names("desc_summary", all_numbered = TRUE)

        if (isTRUE(print_num)) {
            print_code <- str_glue("print({rez}, plotit = FALSE) \n", .trim = FALSE)

            # Ensure required DescTools options are set properly
            if (!isTRUE(biostat_env$desctools_opts_are_set)) {
                force_options <- TRUE
                biostat_env$desctools_opts_are_set <- TRUE
            }

            opts_code <-
                get_desctools_opts_str(
                    big_mark = big_mark,
                    num_digits = digits_num,
                    per_digits = digits_per,
                    scipen = scipen,
                    .force = force_options)

        } else {
            print_code <- ""
            opts_code  <- ""
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (isTRUE(use_plot)) {

            if (new_plots_window == TRUE) {
                open_new_plots_window()
            }

            plot_code <-
                if (las == par("las")) {
                    str_glue("plot({rez}) \n", .trim = FALSE)

                } else {
                    str_glue(
                        .trim = FALSE,
                        "withr::with_par(list(las = {las}), plot({rez})) \n")
                }


        } else {
            plot_code <- ""
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        opts <- ""
        if (isTRUE(by_group)) {
            variables <- str_glue("{y_var} ~ {gr_var}")

        } else {
            variables <- str_glue("{y_var}")
            if (any(variables %in% variables_fct() &
                    !variables %in% variables_fct_like_2_lvls())) {
                opts <- ', ord = "level"'
                # TODO: add possibility to use other options of "ord":
                # "level" by factor levels
                # "name"  alphabetical order
                # "asc"   by frequencies ascending
                # "desc"  by frequencies descending
            }
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (isTRUE(keep_results)) {
            rm_code <- ""

        } else {
            rm_code <- str_glue("remove({rez}) \n")

        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command <- str_glue(
            .trim = FALSE,
            "{opts_code}",
            "## Summary of variables\n",
            "{rez} <- \n   with({.ds}, DescTools::Desc({variables}{opts})) \n",
            "{print_code}",
            "{plot_code}",
            "{rm_code}")

        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("DescTools")

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Checks for syntax errors
        result <- try_command(command)

        if (class(result)[1] == "try-error") {
            logger_error(command, error_msg = as.character(result))
            show_code_evaluation_error_message(parent = top, result$message)
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Checks for code evaluation errors

        result <- doItAndPrint(style_cmd(command))

        if (class(result)[1] == "try-error") {

            logger_error(command, error_msg = result)
            show_code_evaluation_error_message(parent = top, result)
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    .ds    <- active_dataset() # active_dataset_0()

    # Initialize dialog window and title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    dialogue_title <- gettext_bs("Summarize Single Variable or Pair of Variables")
    initializeDialog(title = dialogue_title)
    tk_title(top, dialogue_title)

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        # Variables
        y_var     = NULL,
        gr_var    = NULL,
        by_group  = FALSE,

        keep_results  = FALSE,
        force_options = FALSE,
        conf_level    = 0.95,   # Not implemented

        # Numeric output
        print_num  = TRUE,
        digits_per = get_default(DescTools::Fmt()$per$digits,       1),
        digits_num = get_default(DescTools::Fmt()$num$digits,       3),
        scipen     = get_default(dplyr::na_if(options()$scipen, 0), 9),
        big_mark   = gettext_bs("None"),

        # Plots
        use_plot         = TRUE,
        new_plots_window = is_plot_in_separate_window(),
        labels_direction = gettext_bs("Parallel to the axis")
    )

    initial <- getDialog("window_summary_desc", defaults)


    # Widgets ----------------------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f0 <- tkframe(top)

    # F1 ---------------------------------------------------------------------
    f1 <- tkframe(f0)

    f1_widget_y_gr <- bs_listbox_y_gr(
        parent         = f1,
        y_title        = gettext_bs("Response variable (Y)\n(pick one)"),
        y_var_type     = "all",
        y_initial      = initial$y_var,
        y_select_mode  = "single",

        gr_title       = gettext_bs("Explanatory/Groups variable (X)\n(pick one or none)"),
        gr_var_type    = "all",
        gr_initial     = initial$gr_var,
        gr_select_mode = "single",

        ch_initial     = initial$by_group,
        ch_label = "Two-variable summary",
        ch_tip = str_c(
            "If selected - two-variable summary; \n",
            "If unselected - single-variable summary.")
    )

    f1_keep_results <- bs_checkboxes(
        parent   = f1,
        boxes    = "keep_results",
        labels   = gettext_bs("Keep results in R memory"),
        values   = initial$keep_results,
        commands = list("keep_results"  = activate_results_name)
    )

    # F2 ---------------------------------------------------------------------
    f2 <- tkframe(f0)

    # F2 numeric output ------------------------------------------------------
    f2_num <- tk2labelframe(f2, text = "Numeric output options")

    f2_num_enable <- bs_checkboxes(
        parent   = f2_num,
        boxes    = "print_num",
        labels   = gettext_bs("Print numeric output"),
        values   = initial$print_num,
        commands = list("print_num"  = activate_num)
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2_num_sub <- tk2frame(f2_num)

    f2_digits_per <- bs_entry(
        parent = f2_num_sub,
        width  = 3,
        label  = "Decimal digits in percentages:",
        label_color = "black",
        value = initial$digits_per,
        tip = str_c(
            "The number of decimal  \n",
            "digits in percentages. \n",
            "1   10.1%   \n",
            "2   10.12%   \n",
            "3   10.123% "),
        justify  = "right",
        validate = "focus",
        validatecommand = validate_int_0_inf,
        invalidcommand  = make_red_text_reset_val("1")
    )

    f2_digits_num <- bs_entry(
        parent = f2_num_sub,
        width  = 3,
        label  = "Decimal digits in numbers:",
        label_color = "black",
        value = initial$digits_num,
        tip = str_c(
            "The number of decimal digits \n",
            "in real numbers.         \n",
            "1   10.1   \n",
            "2   10.12  \n",
            "3   10.123"),
        justify  = "right",
        validate = "focus",
        validatecommand = validate_int_0_inf,
        invalidcommand  = make_red_text_reset_val("3")
    )

    f2_scipen <- bs_entry(
        parent = f2_num_sub,
        width  = 3,
        label  = "Width of numbers (scipen): ",
        label_color = "black",
        value = initial$scipen,
        tip = str_c("If the width of a number exceeds \n",
                    "defined width (in digits), it is \n",
                    "converted into a scientific notation.\n",
                    "E.g., 1000 is printed as 1e+03."),
        justify  = "right",
        validate = "focus",
        validatecommand = validate_int_0_inf,
        invalidcommand  = make_red_text_reset_val("9")
    )

    f2_big_mark <- bs_combobox(
        parent = f2_num_sub,
        width  = 16,
        label  = "Big mark: ",
        label_color = "black",
        values = gettext_bs(c(
            "None", "Space [ ]", "Comma [,]", "Apostrophe [']", "Backtick [`]")),
        tip = str_c("Big mark, e.g.: \n10000 \n10 000 \n10'000 \n10`000 \n10,000"),
        value = initial$big_mark
    )

    f2_force_options <- bs_checkboxes(
        parent   = f2_num_sub,
        boxes    = "force_options",
        labels   = gettext_bs("Force to print rounding options"),
        default_tip = str_c(
            "Force to print the code of rounding options\n",
            "even if the options are not changed."),
        values   = initial$force_options
    )


    # F2 plot ----------------------------------------------------------------

    f2_plot <- tk2labelframe(f2, text = "Plot options")

    f2_plot_enable <- bs_checkboxes(
        parent   = f2_plot,
        boxes    = "use_plot",
        labels   = gettext_bs("Draw summary plot"),
        values   = initial$use_plot,
        commands = list("use_plot" = activate_plots)
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2_plot_sub <- tk2frame(f2_plot)

    f2_plot_opts <- bs_checkboxes(
        parent = f2_plot_sub,
        border = FALSE,
        boxes  = c("new_plots_window"),
        values = c(initial$new_plots_window),
        labels = gettext_bs(c(
            "Create new window for plots"
        ))
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f2_plot_las <- bs_combobox(
        parent = f2_plot_sub,
        width  = 24,
        label  = "Direction of plot axis labels:",
        label_position = "above",
        values = gettext_bs(c(
            "Parallel to the axis",
            "Perpendicular to the axis",
            "Always horizontal",
            "Always vertical")),
        value = initial$labels_direction,
        tip = "Graphical parameter 'las' in function 'par()'.")

    # Layout -----------------------------------------------------------------
    tkgrid(f0)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f1,                    sticky = "nwe", padx = c(0,  4))
    tkgrid(f1_widget_y_gr$frame,  sticky = "nwe", padx = c(10, 0))
    tkgrid(f1_keep_results$frame, sticky = "nwe", padx = c(10, 0))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(f2, pady = c(5, 0),  sticky = "")
    tkgrid(f2_num, f2_plot,     sticky = "ns", padx = c(0, 5))

    tkgrid(f2_num_enable$frame, sticky = "nwe", padx = c(5, 65))
    tkgrid(f2_num_sub,          sticky = "nwe")
    tkgrid(f2_digits_per$frame, sticky = "e",   padx = 5, pady = 1)
    tkgrid(f2_digits_num$frame, sticky = "e",   padx = 5, pady = 1)
    tkgrid(f2_scipen$frame,     sticky = "e",   padx = 5, pady = 1)
    tkgrid(f2_big_mark$frame,   sticky = "e",   padx = 5, pady = c(1, 5))
    tkgrid(f2_force_options$frame,sticky = "w", padx = 5, pady = c(1, 5))

    tkgrid(f2_plot_enable$frame, sticky = "nwe", padx = c(5, 50))
    tkgrid(f2_plot_sub,          sticky = "nwe")
    tkgrid(f2_plot_opts$frame,   sticky = "nwe", padx = c(5, 0))
    tkgrid(f2_plot_las$frame,    sticky = "w",   padx = 5, pady = 5)

    # Help menus -------------------------------------------------------------
    help_menu <- function() {

        menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

        tkadd(menu_main, "command",
              label    = "Function 'Desc' (package 'DescTools')",
              command  = open_help("Desc", package = "DescTools"))

        tkadd(menu_main, "separator")

        tkadd(menu_main, "command",
              label    = "How to interpret the results (sections 2-10 in PDF)",
              command  = open_help(vignette = "DescToolsCompanion", package = "DescTools"))

        tkadd(menu_main, "separator")

        tkadd(menu_main, "command",
              label    = "Options for package 'DescTools' (see 'Fmt()')",
              command  = open_help("DescToolsOptions", package = "DescTools"))

        tkadd(menu_main, "command",
              label    = "R options (see section 'scipen')",
              command  = open_help("options", package = "base"))

        tkadd(menu_main, "command",
              label    = "Direction of axis labels (see section 'las')",
              command  = open_help("par", package = "graphics"))

        tkadd(menu_main, "command",
              label    = "Function 'with'",
              command  = open_help("with", package = "base"))

        tkadd(menu_main, "command",
              label    = "Function 'with_par'",
              command  = open_help("with_par", package = "withr"))

        # tkadd(menu_qq, "command",
        #       label    = "Package 'xxx'",
        #       command  = open_online_fun("https://   xxx"))

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
        reset_location = TRUE,
        reset = "window_summary_desc()",
        apply = "window_summary_desc()")

    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    activate_all()

}

# Helper functions ===========================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Returns only those options that have to be changed.
# .force = TRUE  -- print options string even if options are not changed
get_desctools_opts_str <- function(num_digits = 3, per_digits = 1, scipen = 9,
    big_mark = "", abs_big_mark = big_mark, num_big_mark = big_mark,
    .force = FALSE)
{

    x <- DescTools::Fmt()
    str <- ""

    if (options("scipen")$scipen != scipen || .force) {
        str <- str_c(str, str_glue(
            .trim = FALSE,
            "options(scipen = {scipen}) \n"
        ))
    }

    if (isTRUE(x$abs$big.mark != abs_big_mark) || .force) {
        str <- str_c(str, str_glue(
            .trim = FALSE,
            'Fmt(abs = Fmt("abs", big.mark = "{abs_big_mark}")) # Whole numbers \n'
        ))
    }

    if (isTRUE(x$num$big.mark != num_big_mark) || .force) {
        str <- str_c(str, str_glue(
            .trim = FALSE,
            'Fmt(num = Fmt("num", big.mark = "{num_big_mark}")) # Real numbers\n'
        ))
    }

    if (isTRUE(x$num$digits != num_digits) || .force) {
        str <- str_c(str, str_glue(
            .trim = FALSE,
            'Fmt(num = Fmt("num", digits = {num_digits})) # Real numbers \n'
        ))
    }

    if (isTRUE(x$per$digits != per_digits) || .force) {
        str <- str_c(str, str_glue(
            .trim = FALSE,
            'Fmt(per = Fmt("per", digits = {per_digits})) # Percentages \n'
        ))
    }

    # Suppress output of `Fmt()`
    if (str_detect(str, "Fmt")) {
        str <- str_glue(.trim = FALSE, "invisible({{\n{str}}})")
    }

    if (str_detect(str, "Fmt|options")) {
        str <- str %>% style_cmd() %>% str_c("\n")
    }

    structure(str, class = c("glue", "string"))
}

