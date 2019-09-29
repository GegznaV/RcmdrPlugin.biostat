#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_select <- function() {

    dataSets <- listDataSets()
    .ds      <- active_dataset_0()

    # Functions --------------------------------------------------------------
    cmd_refresh_listbox  <- function(variables) {

        set_values(var_ds_box, listDataSets())

        if (get_size(var_ds_box) == 0) {
            tk_disable(var_ds_box)

        } else if (!is.null(.ds)) {
            set_selection(var_ds_box, .ds)
            tkyview(var_ds_box$listbox, which(get_values(var_ds_box) == .ds) - 1)
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_ds_selection_callback  <- function() {

        envir = parent.frame()
        button_obj <- c("i1", "i3", "i5", "i6")

        if (get_size(var_ds_box) == 0 || get_selection_length(var_ds_box) == 0) {
            # Disable buttons
            str_glue_eval("tk_disable({button_obj})",    eval_envir = envir)

        } else {
            # Normalize buttons
            str_glue_eval("tk_normalize({button_obj})", eval_envir = envir)
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_ds_class_print <- function() {
        .ds_1 <- get_selection(var_ds_box) %>% safe_names()
        doItAndPrint(str_glue(
            "## The class of dataset '{.ds_1}'\n",
            "class({.ds_1})"))
    }

    cmd_ds_dims <- function() {
        .ds_1 <- get_selection(var_ds_box) %>% safe_names()
        command_dataset_dim_0(.ds_1)
    }

    cmd_ds_size <- function() {
        .ds_1 <- get_selection(var_ds_box) %>% safe_names()
        Library("tidyverse")
        doItAndPrint(str_glue(
            "## The size of dataset '{.ds_1}'\n",
            'object.size({.ds_1}) %>% print(unit = "auto")'
          ))
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_var_type_summary <- function() {
        .ds_1 <- get_selection(var_ds_box) %>% safe_names()
        summary_var_types_0(.ds_1)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_var_type_summary_plot <- function() {
        .ds_1 <- get_selection(var_ds_box) %>% safe_names()
        summary_var_types_plot_0(.ds_1)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_var_names_print <- function() {
        .ds_1 <- get_selection(var_ds_box) %>% safe_names()
        doItAndPrint(str_glue(
            "## Variable names in dataset '{.ds_1}'\n",
            "names({.ds_1})"
        ))
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_ds_glimpse <- function() {
        .ds_1 <- get_selection(var_ds_box) %>% safe_names()
        doItAndPrint(str_glue(
            "## The structure of dataset '{.ds_1}'\n",
            "dplyr::glimpse({.ds_1})"
          ))
    }
    cmd_ds_glimpse_legend <- function() {
        doItAndPrint(str_c(
          "# Variable type abbreviations: \n",
          "#     <fct> nominal (factor, categorical)\n",
          "#     <ord> ordinal \n",
          "#     <int> numeric (integers) \n",
          "#     <dbl> numeric (double, real numbers) \n",
          "#     <lgl> logical \n",
          "#     <chr> text (character, strings) \n",
          "#     <date> dates\n",
          "#     <dttm> dates and times \n"
        ))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_var_summary_dfSummary <- function() {
        .ds_1 <- get_selection(var_ds_box) %>% safe_names()
        Library("summarytools")

        doItAndPrint(str_glue(
            "## The summary of dataset '{.ds_1}'\n",
            "dfSummary({.ds_1})" # FIXME: , round.digits = 2

        ))
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_var_summary_desc <- function() {
      # DescTools
        .ds_1     <- get_selection(var_ds_box) %>% safe_names()

        opts_code <- get_desctools_opts_str()
        Library("DescTools")
        command <- str_glue(
            .trim = FALSE,
            "## The summary ofall variables\n",
            "{opts_code}",
            'DescTools::Desc({.ds_1}, plotit = FALSE, ord = "level")'
          )
        doItAndPrint(command)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_var_summary_descr <- function() {
      # SummaryTools
        .ds_1     <- get_selection(var_ds_box) %>% safe_names()

        # If any numeric variables exist
        ds_numeric <-
            purrr::map_lgl(str_glue_eval("{.ds_1}", envir_eval = .GlobalEnv),
                           ~is.numeric(.))
        if (any(ds_numeric)) {
          Library("tidyverse")
          Library("summarytools")
          command <- str_glue(
            .trim = FALSE,
            "## The summary of numeric variables\n",
            "{.ds_1} %>% ",
            # "  group_by() %>%",
            "  select_if(is.numeric) %>%",
            "  descr(round.digits = 2)"
          )
          doItAndPrint(style_cmd(command))

        } else {
            doItAndPrint("## No numeric variables found")
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_var_summary_skim <- function() {
        .ds_1 <- get_selection(var_ds_box) %>% safe_names()
        summary_skim(.ds_1)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_var_summary_fct <- function() {
        .ds_1 <- get_selection(var_ds_box) %>% safe_names()

        # If any factors exist
        ds_factors <-
            purrr::map_lgl(str_glue_eval("{.ds_1}", envir_eval = .GlobalEnv),
                           ~inherits(., "factor"))

        if (any(ds_factors)) {
            Library("tidyverse")
            doItAndPrint(style_cmd(str_glue(
                "## The summary ofcategorical variables\n",
                "{.ds_1} %>% \n ",
                "dplyr::select_if(is.factor) %>% \n",
                "purrr::map(~data.frame(n = summary(.)))"
            )))

        } else {
            doItAndPrint("## No categorical variables (factors) found")
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_ds_view <- function() {
        .ds_1 <- get_selection(var_ds_box) %>% safe_names()
        doItAndPrint(str_glue(
            "## Preview dataset '{.ds_1}'\n",
            "View({.ds_1})"
        ))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_open_ds_manager <- function() {
          closeDialog()
          window_data_obj_manage()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        if (dataset_not_persent()) {
            return()
        }

        if (get_selection_length(var_ds_box) == 0) {
            tk_messageBox(
                # parent = top,
                "Please, select a dataset.",
                title = "Dataset Not Selected",
                icon = "warning",
                type = "ok")
            return()
        }

        selection <- getSelection(var_ds_box) %>% safe_names()
        closeDialog()

        active_dataset(selection)
        # active_datatet(selection)
        tkfocus(CommanderWindow())
    }


    # Menus ------------------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_dataset <- function() {

        if (get_selection_length(var_ds_box) == 0) {
           return()
        }

        menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)
        # i1 <- tk2button(
        #     info_buttons_frame,
        #     text = "Class",
        #     tip  = "Print selected dataset's class",
        #     width = 5,
        #     command = cmd_ds_class_print)
        #
        # i2 <- tk2button(
        #     info_buttons_frame,
        #     text = "Size",
        #     tip  = "Print selected dataset's size",
        #     width = 4,
        #     command = cmd_ds_dims)

        tkadd(menu_p, "command",
              label    = "Class",
              command  = cmd_ds_class_print)

        tkadd(menu_p, "command",
              label    = "Size in memory",
              command  = cmd_ds_size)

        tkadd(menu_p, "command",
              label    = "Number of rows and columns",
              command  = cmd_ds_dims)

        tkadd(menu_p, "command",
              label    = "Structure",
              command  = cmd_ds_glimpse)

        tkadd(menu_p, "command",
              label    = "Legend for structure",
              command  = cmd_ds_glimpse_legend)
        # tip  = str_c("Print selected dataset's structure:",
        #              "variable names, types and several ",
        #              "first values",
        #              sep = "\n")

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkpopup(menu_p, tkwinfo("pointerx", top), tkwinfo("pointery", top))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_variables <- function() {

        if (get_selection_length(var_ds_box) == 0) {
           return()
        }

        menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)

        tkadd(menu_p, "command",
              label    = "Variable names",
              # compound = "left",
              # image    = "::image::bs_locale",
              command  = cmd_var_names_print)

        tkadd(menu_p, "command",
              label    = "Variable type summary and size", #  & dataset size
              # compound = "left",
              # image    = "::image::bs_locale",
              command  = cmd_var_type_summary)

        tkadd(menu_p, "command",
              label    = "Variable type summary plots", #  & dataset size
              # compound = "left",
              # image    = "::image::bs_locale",
              command  = cmd_var_type_summary_plot)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkpopup(menu_p, tkwinfo("pointerx", top), tkwinfo("pointery", top))
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_summary <- function() {

        if (get_selection_length(var_ds_box) == 0) {
            return()
        }

        menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)

        tkadd(menu_p, "command",
              label    = "of all variables (dfSummary)",
              # compound = "left",
              # image    = "::image::bs_locale",
              command  = cmd_var_summary_dfSummary)

        tkadd(menu_p, "command",
              label    = "of all variables (Desc)",
              # compound = "left",
              # image    = "::image::bs_locale",
              command  = cmd_var_summary_desc)

        tkadd(menu_p, "command",
              label    = "of all variables (skim)",
              # compound = "left",
              # image    = "::image::bs_locale",
              command  = cmd_var_summary_skim)

        tkadd(menu_p, "command",
              label    = "of numeric variables",
              # compound = "left",
              # image    = "::image::bs_locale",
              command  = cmd_var_summary_descr)
        #
        # tkadd(menu_p, "command",
        #       label    = "The summary offactor variables",
        #       # compound = "left",
        #       # image    = "::image::bs_locale",
        #       command  = cmd_var_summary_fct)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkpopup(menu_p, tkwinfo("pointerx", top), tkwinfo("pointery", top))
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_settings <- function() {

        menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)

        # tkadd(menu_p, "command",
        #       label    = "Settings",
        #       compound = "left",
        #       image    = "::image::bs_settings",
        #       command  = do_nothing # FIXME: should open a menu for rounding settings
        #   )

        tkadd(menu_p, "command",
              label    = "Open Dataset Manager",
              compound = "left",
              image    = "::image::bs_objects",
              command  = cmd_open_ds_manager)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkpopup(menu_p, tkwinfo("pointerx", top), tkwinfo("pointery", top))
    }


    # Initialize -------------- ----------------------------------------------
    initializeDialog(title = gettext_bs("Select & Explore Dataset"))
    tk_title(top, "Select & Explore Dataset")

    # Widgets ----------------------------------------------------------------
    var_ds_box <-
        bs_listbox(
            parent          = top,
            title           = gettext_bs("Datasets in R memory (select one)"),
            title_sticky    = "",
            values          = dataSets,
            value           = .ds,
            # selection       = if (is.null(.ds)) NULL else which(.ds == dataSets),
            height          = 10,
            width           = c(47, Inf),
            on_release      = cmd_ds_selection_callback,
            on_double_click = onOK
        )

    tkgrid(getFrame(var_ds_box), sticky = "e",  pady = c(10, 0))


    # Dataset info buttons ---------------------------------------------------
    info_buttons_frame <- tkframe(top)

    i1 <- tk2button(
        info_buttons_frame,
        text = "Dataset",
        tip  = "Information about \nselected dataset",
        width = 0,
        command = menu_dataset)


    i3 <- tk2button(
        info_buttons_frame,
        text = "Variables",
        tip  = "Information about selected \ndataset's columns",
        width = 0,
        command = menu_variables)

    # i4 <- tk2button(
    #     info_buttons_frame,
    #     text = "Structure",
    #     tip  = str_c("Print selected dataset's structure:",
    #                  "variable names, types and several ",
    #                  "first values",
    #                  sep = "\n"),
    #     width = 0,
    #     command = cmd_ds_glimpse)

    i5 <- tk2button(
      info_buttons_frame,
      text = "Summary",
      tip  = str_c(
        "Summary of selected ",
        "dataset's variable values" ,
        sep = "\n"),
      width = 0,
      command = menu_summary)

    tip_i6 <-
        if (is_rstudio()) {
            "View selected dataset \n(in RStudio)"
        } else {
            "View selected dataset \n((in a separate window))"
        }

    i6 <- tk2button(
        info_buttons_frame,
        text = "Preview",
        tip  = tip_i6,
        width = 0,
        command = cmd_ds_view)

    i7 <- tk2button(
        info_buttons_frame,
        tip     = "Settings ",
        image   = "::image::bs_settings",
        command = menu_settings)

    # tkgrid(bs_label_b(top, text = "Information about selected dataset"),
    # pady = c(5, 0))
    tkgrid(i1, i3, i5, i6, i7, sticky = "we")
    tkgrid(info_buttons_frame, sticky = "we")

    tkgrid.columnconfigure(info_buttons_frame, 0:6, weight = 1)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Finalize ---------------------------------------------------------------
    ok_cancel_help()
    tkgrid(buttonsFrame, pady = c(10, 0))
    dialogSuffix()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Apply functions --------------------------------------------------------

    cmd_ds_selection_callback()
    cmd_refresh_listbox()

    if (!isTRUE(active_dataset_0() %in% ls(all.names = TRUE, envir = .GlobalEnv))) {
        active_dataset_0(NULL)
    }
}
