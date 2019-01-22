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
        tip  = "Print class of the selected dataset.",
        width = 5,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)

            doItAndPrint(str_glue(
                "## The class of dataset '{.ds_1}'\n",
                "class({.ds_1})"))
        })

    i2 <- tk2button(
        info_buttons_frame,
        text = "Size",
        tip  = str_c("Print selected dataset's size and",
                     "column type frequencies.",
                     sep = "\n"),

        width = 4,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)
            # doItAndPrint(str_glue(
            #     "## The size of dataset '{.ds_1}' (rows, columns)\n",
            #     "dim({.ds_1})"))

            summary_var_types(.ds)

        })


    i3 <- tk2button(
        info_buttons_frame,
        text = "Variables",
        tip  = str_c("Print variable names in",
                     "the selected dataset.",
                     sep = "\n"),
        width = 0,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)
            doItAndPrint(str_glue(
                "## Variable names in dataset '{.ds_1}'\n",
                "names({.ds_1})"
            ))
        })

    i4 <- tk2button(
        info_buttons_frame,
        text = "Structure",
        tip  = str_c("Print selected dataset's structure:",
                     "variable names, types and several ",
                     "first values.",
                     sep = "\n"),
        width = 0,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)

            doItAndPrint(str_glue(
                "## The structure of dataset '{.ds_1}'\n",
                "dplyr::glimpse({.ds_1})"))
        })

    i5 <- tk2button(
        info_buttons_frame,
        text = "Summary",
        tip  = str_c("Print summary of the variables",
                     "in the selected dataset.",
                     sep = "\n"),
        width = 0,
        command = function() {

            .ds_1 <- get_selection(var_ds_box)

            summary_skim(.ds_1)


            # If any factors exist
            ds_factors <-
                purrr::map_lgl(eval_glue("{.ds_1}", envir_eval = .GlobalEnv),
                               ~inherits(., "factor"))

            if (any(ds_factors)) {
                doItAndPrint(style_cmd(str_glue(
                    "## More details on categorical variables\n",
                    "{.ds_1} %>% \n ",
                    "dplyr::select_if(is.factor) %>% \n",
                    "purrr::map(~data.frame(n = summary(.)))"
                )))
            }

        })

    i6 <- tk2button(
        info_buttons_frame,
        text = "Preview",
        tip  = str_c("Preview the  selected dataset",
                     "in a separate window.",
                     sep = "\n"),
        width = 0,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)
            doItAndPrint(str_glue(
                "## Preview dataset '{.ds_1}'\n",
                "View({.ds_1})"
            ))
        })

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
