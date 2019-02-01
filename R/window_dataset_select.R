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
        button_obj <- c("i1", "i2", "i3", "i4", "i5", "i6")

        if (get_size(var_ds_box) == 0 || get_selection_length(var_ds_box) == 0) {
            # Disable buttons
            eval_glue("tk_disable({button_obj})",    eval_envir = envir)

        } else {
            # Normalize buttons
            eval_glue("tk_normalize({button_obj})", eval_envir = envir)
        }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_ds_class_print <- function() {
        .ds_1 <- get_selection(var_ds_box)
        doItAndPrint(str_glue(
            "## The class of dataset '{.ds_1}'\n",
            "class({.ds_1})"))
    }

    cmd_ds_size <- function() {
        .ds_1 <- get_selection(var_ds_box)
        command_dataset_dim(.ds_1)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_var_type_summary <- function() {
        .ds_1 <- get_selection(var_ds_box)
        summary_var_types(.ds_1)
    }

    cmd_var_names_print <- function() {
        .ds_1 <- get_selection(var_ds_box)
        doItAndPrint(str_glue(
            "## Variable names in dataset '{.ds_1}'\n",
            "names({.ds_1})"
        ))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_ds_glimpse <- function() {
        .ds_1 <- get_selection(var_ds_box)
        doItAndPrint(str_glue(
            "## The structure of dataset '{.ds_1}'\n",
            "dplyr::glimpse({.ds_1})"))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_var_summary_skim <- function() {
        .ds_1 <- get_selection(var_ds_box)
        summary_skim(.ds_1)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_var_summary_fct <- function() {
        .ds_1 <- get_selection(var_ds_box)

        # If any factors exist
        ds_factors <-
            purrr::map_lgl(eval_glue("{.ds_1}", envir_eval = .GlobalEnv),
                           ~inherits(., "factor"))

        if (any(ds_factors)) {
            doItAndPrint(style_cmd(str_glue(
                "## Summary of categorical variables\n",
                "{.ds_1} %>% \n ",
                "dplyr::select_if(is.factor) %>% \n",
                "purrr::map(~data.frame(n = summary(.)))"
            )))

        } else {
            doItAndPrint(style_cmd(str_glue(
                "## No categorical variables (factors) found"
            )))
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_ds_view <- function() {
        .ds_1 <- get_selection(var_ds_box)
        doItAndPrint(str_glue(
            "## Preview dataset '{.ds_1}'\n",
            "View({.ds_1})"
        ))
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

        selection <- getSelection(var_ds_box)
        closeDialog()

        active_dataset(selection)
        # active_datatet(selection)
        tkfocus(CommanderWindow())
    }


    # Menus ------------------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_variables <- function() {

        if (get_selection_length(var_ds_box) == 0) {
           return()
        }

        menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)

        tkadd(menu_p, "command",
              label    = "Size and variable type summary",
              # compound = "left",
              # image    = "::image::bs_locale",
              command  = cmd_var_type_summary)

        tkadd(menu_p, "command",
              label    = "Variable names",
              # compound = "left",
              # image    = "::image::bs_locale",
              command  = cmd_var_names_print)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkpopup(menu_p,
                tkwinfo("pointerx", top),
                tkwinfo("pointery", top))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_summary <- function() {

        if (get_selection_length(var_ds_box) == 0) {
            return()
        }

        menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)

        tkadd(menu_p, "command",
              label    = "Summary of all variables",
              # compound = "left",
              # image    = "::image::bs_locale",
              command  = cmd_var_summary_skim)

        tkadd(menu_p, "command",
              label    = "Summary of factor variables",
              # compound = "left",
              # image    = "::image::bs_locale",
              command  = cmd_var_summary_fct)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkpopup(menu_p,
                tkwinfo("pointerx", top),
                tkwinfo("pointery", top))
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
        text = "Class",
        tip  = "Print selected dataset's class",
        width = 5,
        command = cmd_ds_class_print)

    i2 <- tk2button(
        info_buttons_frame,
        text = "Size",
        tip  = "Print selected dataset's size",
        width = 4,
        command = cmd_ds_size)

    i3 <- tk2button(
        info_buttons_frame,
        text = "Variables",
        tip  = "Print information about selected dataset's columns",
        width = 0,
        command = menu_variables)

    i4 <- tk2button(
        info_buttons_frame,
        text = "Structure",
        tip  = str_c("Print selected dataset's structure:",
                     "variable names, types and several ",
                     "first values",
                     sep = "\n"),
        width = 0,
        command = cmd_ds_glimpse)

    i5 <- tk2button(
        info_buttons_frame,
        text = "Summary",
        tip  = str_c("Print summary of the variables",
                     "in the selected dataset",
                     sep = "\n"),
        width = 0,
        command = menu_summary)

    i6 <- tk2button(
        info_buttons_frame,
        text = "Preview",
        tip  = str_c("View selected dataset",
                     "in a separate window.",
                     sep = "\n"),
        width = 0,
        command = cmd_ds_view)

    # tkgrid(bs_label_b(top, text = "Information about selected dataset"),
    # pady = c(5, 0))
    tkgrid(i1, i2, i3, i4, i5, i6)
    tkgrid(info_buttons_frame, sticky = "e")


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
