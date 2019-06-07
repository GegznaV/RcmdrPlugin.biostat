# TODO:
# 1) Add `put_dialogue` to store options.
# 2) Add activation/disabling for the buttons based on selection (none, one or several)
# 3) Add more buttons to manage dataset:
#       - chnge class
#       - export
#       - export selected to RData file
#  4) Add a pop-up window for the new name instead of current window.
#  5) Add context menu for copy, rename, delete, etc. functions.
#

#' @rdname Menu-window-functions
#' @export
#' @keywords internal

window_data_obj_manage <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_list_of_objs <- function() {
        hidden    <- get_values(include_hidden_box, "hidden")
        obj_class <- get_selection(class_filter_box)

        get_obj_names2 <- purrr::partial(get_obj_names, all.names = hidden)

        switch(
            obj_class,
            "All"                    = get_obj_names2(),
            "List"                   = get_obj_names2("list"),
            "Data frame"             = get_obj_names2("data.frame"),
            "Matrix"                 = get_obj_names2("matrix"),
            "Table"                  = get_obj_names2("table"),
            "Model (lm, glm, htest)" = get_obj_names2(c("lm", "glm", "htest")),
            "Plot (ggplot, gg)"      = get_obj_names2(c("ggplot", "gg")),
            "Function"               = get_obj_names2("function"),
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
        buttons_activation()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_new_name <- function() {
        # [???]
        if (get_selection_length(var_y_box) == 1) {
            tclvalue(text_box_1) <-
                get_selection(var_y_box) %>%
                unique_obj_names()
        }
    }


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    buttons_activation <- function() {

        len <- get_selection_length(var_y_box)

        if (len == 0) {
            # None is selected
            tk_disable(f1_b1)
            # tk_disable(f1_b2)
            tk_disable(f1_b3)
            tk_disable(f1_b4)
            tk_disable(f1_b5)
            tk_disable(f1_b6)

        } else if (len == 1) {
            # One is selected
            tk_normalize(f1_b1)
            # tk_normalize(f1_b2)
            tk_normalize(f1_b3)
            tk_normalize(f1_b4)
            tk_normalize(f1_b5)
            tk_normalize(f1_b6)

        } else {
            # Several are selected
            tk_disable(f1_b1)
            # tk_disable(f1_b2)
            tk_disable(f1_b3)
            tk_normalize(f1_b4)
            tk_disable(f1_b5)
            tk_normalize(f1_b6)
        }

    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    on_close <- function() {
        if (GrabFocus()) tkgrab.release(top)
        tkdestroy(top)
        putRcmdr("rgl.command", FALSE)
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    on_view <- function() {
        buttons_activation()

        obj_names <- get_selection(var_y_box) %>% safe_names()
        if (length(obj_names) < 1) {
            return
        }

        str_glue("View({obj_names})") %>%
            str_c(collapse = "\n") %>%
            doItAndPrint()

    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    on_show_details <- function() { buttons_activation() }
    on_change_class <- function() { buttons_activation() }
    on_copy_name    <- function() { buttons_activation() }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    on_rename_obj   <- function() {
        buttons_activation()

        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        obj_names     <- get_selection(var_y_box)
        new_obj_names <- get_values(text_box_1)
        .ds <- active_dataset_0()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (object_is_not_selected(obj_names, parent = top))       {return()}

        if (is_not_valid_name(new_obj_names, parent = top))        {return()}
        if (forbid_to_replace_object(new_obj_names, parent = top)) {return()}
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Deselect active dataset if it should be renamed
        active_ds_is_renamed <- isTRUE(any(.ds %in% obj_names))
        if (active_ds_is_renamed) {
            which_is_active_ds <- which(.ds == obj_names)
            active_dataset_0(NULL)
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        obj_names_str <- str_c(obj_names, collapse = ", ")

        # Construct the command ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command_0 <- str_glue("{new_obj_names} <- {obj_names}\n")
        command   <- str_glue(
            "## Rename the object \n",
            "{command_0}\n",
            "remove({obj_names_str})"
        )

        result <- justDoIt(command)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            logger(style_cmd(command))

            # Re-choose active dataset
            if (active_ds_is_renamed) {
                active_dataset(new_obj_names[which_is_active_ds]) # [???]
            }

        } else {
            logger_error(command, error_msg = result)
            show_code_evaluation_error_message()
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        update_list_of_objects()
        set_selection(var_y_box, new_obj_names)
        tk_see(var_y_box, new_obj_names)
        update_new_name()
        buttons_activation()

        # tkfocus(CommanderWindow())
        tkfocus(top)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    on_copy_obj <- function() {
        buttons_activation()
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        obj_names     <- get_selection(var_y_box)
        new_obj_names <- get_values(text_box_1)
        .ds <- active_dataset_0()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (object_is_not_selected(obj_names, "object", parent = top))   {return()}

        if (is_not_valid_name(new_obj_names, parent = top))              {return()}
        if (forbid_to_replace_object(new_obj_names, parent = top))       {return()}
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Deselect active dataset if it should be copied
        # if (isTRUE(any(.ds %in% obj_names))) {active_dataset_0(NULL)}
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        obj_names_str <- str_c(obj_names, collapse = ", ")

        # Construct the command ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command_0 <- str_glue("{new_obj_names} <- {obj_names}\n")
        command   <- str_glue(
            "## Create a copy of the object/dataset \n",
            "{command_0}\n"
        )

        result <- justDoIt(command)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            logger(style_cmd(command))

        } else {
            logger_error(command, error_msg = result)
            show_code_evaluation_error_message()
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        update_list_of_objects()
        set_selection(var_y_box, new_obj_names)
        tk_see(var_y_box, new_obj_names)
        update_new_name()
        buttons_activation()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # tkfocus(CommanderWindow())
        tkfocus(top)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE

    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    on_delete_obj <- function() {
        buttons_activation()
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        obj_names <- get_selection(var_y_box)
        .ds <- active_dataset_0()
        ind <- max(1, which(get_list_of_objs() %in% obj_names)[1] - 1)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (object_is_not_selected(obj_names, parent = top))   {
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
            if (isTRUE(any(.ds %in% obj_names))) {active_dataset_0(NULL)}

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

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        update_list_of_objects()
        set_selection(var_y_box, ind)
        tk_see(var_y_box, ind)
        update_new_name()
        buttons_activation()

        tkfocus(top)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Widgets ----------------------------------------------------------------
    # ws_objects <- objects(envir = .GlobalEnv, all.names = TRUE)
    .ds <- active_dataset_0()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    win_title <- gettext_bs("Manage Objects And Datasets")
    initializeDialog(title = win_title)
    tk_title(top, text = win_title, columnspan = 2)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    f1 <- tk2frame(top)

    f1a <- tk2frame(top)

    class_filter_box <- bs_combobox(
        parent = f1a, # var_y_box$frame,
        label  = "Type of objects to list",
        label_position = "above",
        width  = 30 - 2, # Get width var_y_box
        value  = "Data frame",
        values = c( "All", "Data frame", "List", "Matrix", "Table", "Plot (ggplot, gg)",
                   "Model (lm, glm, htest)", "Function", "Other"),
        tip = "",
        on_select = update_list_of_objects
    )

    include_hidden_box <- bs_checkboxes(
        parent = f1a,
        boxes  = c(hidden = "Show hidden objects"),
        values = 0,
        commands = c(hidden = update_list_of_objects)
    )

    var_y_box <- bs_listbox(
        parent       = f1a,
        title        = "Objects",
        values       = "",
        # value        = .ds,
        selectmode   = "multiple", # "single",
        height       = 8,
        width        = c(30, Inf),
        on_keyboard  = "scroll",
        tip          = tip_multiple_ctrl_letters,
        use_filter   = TRUE,
        on_select    = function() {
            update_new_name()
            buttons_activation()
        },
        filter_label = "Object name filter"
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f1b <- tk2frame(top)

    f1_b1 <- tk2button(
        f1b,
        tip      = "Rename object",
        text     = "Rename",
        image    = "::image::bs_rename",
        compound = "left",
        command  = on_rename_obj)

    # f1_b2 <- tk2button(
    #     f1b,
    #     tip      = "Copy object's name",
    #     text     = "Copy name",
    #     image    = "::image::bs_copy",
    #     compound = "left",
    #     command  = do_nothing)

    f1_b3 <- tk2button(
        f1b,
        tip      = "Create a copy on an object",
        text     = "Duplicate",
        image    = "::image::bs_copy",
        compound = "left",
        command  = on_copy_obj)

    f1_b4 <- tk2button(
        f1b,
        tip      = "Delete object",
        text     = "Delete",
        image    = "::image::bs_delete",
        compound = "left",
        command  = on_delete_obj)

    f1_b5 <- tk2button(
        f1b,
        text     = "Convert",
        tip      = "Change class",
        image    = "::image::bs_refresh",
        compound = "left",
        command  = on_change_class)

    f1_b6 <- tk2button(
        f1b,
        text     = "View",
        tip      = "View object (if possible)",
        image    = "::image::viewIcon",
        compound = "left",
        command  = on_view)

    tkgrid(f1, sticky = "w")
    tkgrid(f1a, f1b, sticky = "w")

    tkgrid(class_filter_box$frame,   sticky = "w")
    tkgrid(include_hidden_box$frame, sticky = "w")
    tkgrid(var_y_box$frame, pady = c(5, 5))

    tkgrid.configure(f1b, sticky = "w")

    tkgrid(f1_b1, sticky = "w")
    # tkgrid(f1_b2, sticky = "w")
    tkgrid(f1_b3, sticky = "w")
    tkgrid(f1_b4, sticky = "w")
    # tkgrid(f1_b5, sticky = "w")
    tkgrid(f1_b6, sticky = "w")



    tkconfigure(f1_b1, width = 10)
    # tkconfigure(f1_b2, width = 15)
    tkconfigure(f1_b3, width = 10)
    tkconfigure(f1_b4, width = 10)
    # tkconfigure(f1_b5, width = 10)
    tkconfigure(f1_b6, width = 10)

    # tkgrid(f1_close, sticky = "e")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    f1_close <- tk2button(
        top,
        tip      = "Close window",
        text     = "Close",
        image    = "::image::bs_delete",
        compound = "left",
        command  = on_close)

    text_box_1 <- bs_entry(
        parent = top,
        width  = 30,
        label  = "New name: ",
        label_position = "above",
        tip = "Enter a new name or a name for a copy of selected object.")

    tkgrid(text_box_1$frame, f1_close, sticky = "wes", padx = c(0, 0))


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ok_cancel_help(apply = "window_data_obj_manage", ok_label = "Delete")
    # tkgrid(buttonsFrame)
    dialogSuffix(onOK = do_nothing, onCancel = on_close, bindReturn = FALSE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_list_of_objects()

    if (!is.null(.ds)) {
        set_selection(var_y_box, .ds)
        tk_see(var_y_box, .ds)
    }
    update_new_name()
    buttons_activation()
}
