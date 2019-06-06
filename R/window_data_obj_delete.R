#' @rdname Menu-window-functions
#' @export
#' @keywords internal

window_data_obj_delete <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_list_of_objs <- function() {
        hidden    <- get_values(include_hidden_box, "hidden")
        obj_class <- get_selection(class_filter_box)

        switch(
            obj_class,
            "All"                    = get_obj_names(all.names = hidden),
            "Data frame"             = get_obj_names(all.names = hidden, "data.frame"),
            "Matrix"                 = get_obj_names(all.names = hidden, "matrix"),
            "Table"                  = get_obj_names(all.names = hidden, "table"),
            "List"                   = get_obj_names(all.names = hidden, "list"),

            "Model (lm, glm, htest)" = get_obj_names(all.names = hidden, c("lm", "glm", "htest")),
            "lm, glm, htest"         = get_obj_names(all.names = hidden, c("lm", "glm", "htest")),

            "Plot (ggplot, gg)"      = get_obj_names(all.names = hidden, c("ggplot", "gg")),
            "ggplot"                 = get_obj_names(all.names = hidden, "ggplot"),
            "ggplot, gg"             = get_obj_names(all.names = hidden, c("ggplot", "gg")),
            "gg (except ggplot)"     = get_obj_names(all.names = hidden, "gg", exclude_class = "ggplot"),

            "Function"               = get_obj_names(all.names = hidden, "function"),
            "Other"                  = get_obj_names(
                all.names = hidden,
                exclude_class = c("data.frame", "ggplot", "gg", "function",
                                  "lm", "glm", "htest",
                                  "matrix", "list", "table"))
        )
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_list_of_objects <- function() {
        new_vals <- get_list_of_objs()
        set_values(var_y_box, new_vals)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        obj_names <- get_selection(var_y_box)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (object_is_not_selected(obj_names))   {
            return()
        }

        obj_names_str <- str_c(obj_names, collapse = ", ")

        if (length(obj_names) == 1) {
            msg <- str_glue("Do you agree to DELETE object",
                            '\n"{obj_names_str}" permanently?')

        } else {
            msg <- str_glue(
                "Do you agree to DELETE {length(obj_names)} following ",
                "objects permanently: \n\n",
                "{obj_names_str}"
            )
        }

        option <-
            tk_messageBox(
                parent = top,
                caption = "Delete Objects",
                type    = "yesno",
                default = "no",
                icon    = "warning",
                message = msg
            )
        if (option != "yes") {
            Message("No objects were deleted.", type = "note")
            return()

        } else {
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Deselect active dataset if it should be deleted.
            if (isTRUE(any(active_ds %in% obj_names))) {active_dataset_0(NULL)}

            # Construct the command ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            command <-
                str_glue("## Delete objects \n",
                         "remove({obj_names_str})") %>%
                style_cmd()
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            doItAndPrint(command)

            Message(str_c("The object(s) were deleted: \n", obj_names_str),
                    type = "warn")
        }

        closeDialog()
        tkfocus(CommanderWindow())

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Widgets ----------------------------------------------------------------
    ws_objects <- objects(envir = .GlobalEnv, all.names = TRUE)
    active_ds  <- active_dataset_0()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = "Delete Objects")
    tk_title(top, text = "Delete Objects", fg = "darkred")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    var_y_box <- bs_listbox(
        parent       = top,
        title        = "Objects to delete\n(pick one or several)",
        values       = ws_objects,
        # value        = active_ds,
        selectmode   = "multiple",
        height       = 8,
        width        = c(30, Inf),
        on_keyboard  = "scroll",
        tip          = "Hold 'Ctrl' key to  select several \nor deselect objects.",
        use_filter   = TRUE,
        filter_label = "Object name filter"
    )

    class_filter_box <- bs_combobox(
        parent = var_y_box$frame,
        label  = "Object class filter",
        label_position = "above",
        width  = 30 - 2, # Get width var_y_box
        value  = "Data frame",
        values = c("Data frame", "List", "Matrix", "Table", "Plot (ggplot, gg)",
                   "Model (lm, glm, htest)", "Function", "Other", "All"),
        tip = "",
        on_select = update_list_of_objects
    )

    include_hidden_box <- bs_checkboxes(
        parent = var_y_box$frame,
        boxes  = c(hidden = "Show hidden objects"),
        values = 0,
        commands = c(hidden = update_list_of_objects)
    )

    tkgrid(getFrame(var_y_box), pady = c(0, 5))
    tkgrid(class_filter_box$frame, sticky = "w")
    tkgrid(include_hidden_box$frame, sticky = "w")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(apply = "window_data_obj_delete", ok_label = "Delete")
    tkgrid(buttonsFrame)
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_list_of_objects()

    if (!is.null(active_ds)) {
        set_selection(var_y_box, active_ds)
        tk_see(var_y_box, which(get_list_of_objs() %in% active_ds))
    }
}
