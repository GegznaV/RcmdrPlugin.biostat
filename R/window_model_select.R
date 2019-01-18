#' @rdname Menu-window-functions
#' @export
#' @keywords internal


window_model_select <- function() {
    models       <- listAllModels()
    .ds          <- active_dataset_0()
    .activeModel <- ActiveModel()

    # if ((length(models) == 1) && !is.null(.activeModel)) {
    #     Message(message = gettext_bs("There is only one model in memory."),
    #             type = "warning")
    #     tkfocus(CommanderWindow())
    #     return()
    # }

    if (length(models) == 0) {
        Message(
            message = gettext_bs("There are no models from which to choose."),
            type = "error"
        )
        tkfocus(CommanderWindow())
        return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Functions --------------------------------------------------------------
    cmd_model_selection_callback  <- function() {

        envir = parent.frame()
        button_obj <- c(
            "i1", "i2", "i3", "i4", "i5", "i6", "i7", "i8", "i9", "i10"
        )

        if (get_size(var_model_box) == 0 || get_selection_length(var_model_box) == 0) {
            # Disable buttons
            eval_glue("tk_disable({button_obj})",   eval_envir = envir)

        } else {
            # Normalize buttons
            eval_glue("tk_normalize({button_obj})", eval_envir = envir)
        }

        # Button "i6"
        model <- ActiveModel()
        if (!is.null(model) && class((get(model, envir = .GlobalEnv)))[1] == "lm") {
            tk_normalize(i6)
        } else {
            tk_disable(i6)
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    select_model  <- function() {
        new_model <- get_selection(var_model_box)
        cur_model <- ActiveModel()
        if (is.null(cur_model) || cur_model != new_model) {

            if (length(new_model) == 0) {
                # tkfocus(CommanderWindow())
                return()
            }

            dataSet <- as.character(get(new_model)$call$data)
            if (length(dataSet) == 0) {
                errorCondition(message = gettext_bs(
                    "There is no dataset associated with this model."))
                return()
            }

            dataSets <- listDataSets()
            if (!is.element(dataSet, dataSets)) {
                errorCondition(
                    message = sprintf(
                        gettext_bs(
                            "The dataset associated with this model, %s, is not in memory."
                        ),
                        dataSet
                    ))
                return()
            }

            if (is.null(.ds) || (dataSet != .ds))
                active_dataset(dataSet)

            putRcmdr("modelWithSubset", "subset" %in% names(get(new_model)$call))
            activeModel(new_model)
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        select_model()
        closeDialog()
        tkfocus(CommanderWindow())
    }

    # Initialize -------------------------------------------------------------
    initializeDialog(title = gettext_bs("Select & Explore Model"))
    tk_title(top, "Select and explore a model")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    var_model_box <- bs_listbox(
        parent       = top,
        values       = models,
        value        = .activeModel,
        height       = 10,
        width        = c(47, Inf),
        title        = gettext_bs("Models (pick one)"),
        title_sticky = "",
        on_release       = function() {
            select_model()
            cmd_model_selection_callback()
        },
        on_double_click  = onOK
    )

    tkgrid(getFrame(var_model_box), sticky = "e",  pady = c(10, 0))

    # Dataset info buttons ---------------------------------------------------
    info_buttons_frame <- tkframe(top)
    row_1 <- tkframe(info_buttons_frame)
    row_2 <- tkframe(info_buttons_frame)

    i1 <- tk2button(
        row_1,
        text = "Class",
        tip  = "Print model's class.",
        width = 5,
        command = function() {
            command_model_get_class()
        })

    i2 <- tk2button(
        row_1,
        text = "Print",
        tip  = str_c("Print basic results.",
                     sep = "\n"),

        width = 4,
        command = function() {
            command_model_print()
        })


    i3 <- tk2button(
        row_1,
        text = "Summary",
        tip  = str_c("Base R style summary of the model.",
                     sep = "\n"),
        width = 0,
        command = function() {
            command_model_summary()
        })

    i4 <- tk2button(
        row_1,
        text = "Glance",
        tip  = str_c("Print one-row summary of the model.",
                     sep = "\n"),
        width = 0,
        command = function() {
            command_model_glance()
        })

    i5 <- tk2button(
        row_1,
        text = "Tidy",
        tip  = str_c("Print statistical findings of the model.",
                     sep = "\n"),
        width = 0,
        command = function() {
            command_model_tidy()
        })

    i6 <- tk2button(
        row_1,
        text = "Std.coeff",
        tip  = str_c("Print standardized coefficients",
                     "of multiple linear regression.",
                     sep = "\n"),
        width = 0,
        command = function() {
            command_model_std_lm_coeffs()
        })

    i7 <- tk2button(
        row_2,
        text = "Augment",
        tip  = str_c("Add data from model to original data frame.",
                     sep = "\n"),
        width = 9,
        command = function() {
            command_model_augment()
        })

    i8 <- tk2button(
        row_2,
        text = "Basic diagnostic plots",
        tip  = str_c("Draw basic diagnostic plots for model.",
                     sep = "\n"),
        width = 0,
        command = function() {
            .mod <- activeModel()

            open_new_plots_window()
            doItAndPrint(str_glue(
                "## Basic diagnostic plots for the model \n",
                "old_par <- par(oma = c(0, 0, 3, 0), mfrow = c(2, 2)) \n",
                "plot({.mod}) \n",
                "par(old_par)"))
        })

    i9 <- tk2button(
        row_2,
        text = "Cook's distance plot",
        tip  = str_c("Plot Cook's distances",
                     "(for outlier detection).",
                     sep = "\n"),
        width = 0,
        command = function() {
            .mod <- activeModel()
            Library("tidyverse")
            Library("ggfortify")
            open_new_plots_window()
            doItAndPrint(str_glue(
                '## Cooks distance, d (outlier if d > 1) \n',
                'autoplot({.mod}, which = 4) + \n',
                'geom_hline(yintercept = 1, color = "red", lty = 2) + \n',
                'theme_bw()'))
        })

    i10 <- tk2button(
        row_1,
        text = "View",
        tip  = str_c("Explore object in a separate window.",
                     "(RStudio only)",
                     sep = "\n"),
        width = 0,
        command = function() {
            .mod <- activeModel()
            Library("tidyverse")
            Library("ggfortify")
            doItAndPrint(str_glue(
                "## Explore model's object \n",
                'View({.mod})'))
        })


    # tkgrid(bs_label_b(top, text = "Information about selected dataset"),
    # pady = c(5, 0))

    tkgrid(row_1)
    tkgrid(row_2)
    tkgrid(i1, i2, i3, i4, i5, i6, i10)
    tkgrid(i7, i8, i9)
    tkgrid(info_buttons_frame, sticky = "e")

    # ========================================================================
    ok_cancel_help()
    tkgrid(buttonsFrame)
    dialogSuffix()


    cmd_model_selection_callback()
}
