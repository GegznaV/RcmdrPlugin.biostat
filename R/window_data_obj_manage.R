# TODO:
# 1) Add `put_dialogue` to store options.
# 2) Add more buttons to manage dataset:
#       - chnge class
#       - export
#       - export selected to RData file
#  3) Add context menu for copy, rename, delete, etc. functions for f1_listbox_y
#

#' @rdname Menu-window-functions
#' @export
#' @keywords internal

window_data_obj_manage <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Functions --------------------------------------------------------------
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
        set_values(f1_listbox_y, new_vals)
        buttons_activation()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    buttons_activation <- function() {

        len <- get_selection_length(f1_listbox_y)

        if (len == 0) {
            # None is selected
            tk_disable(f1_view)
            tk_disable(f1_rename)
            tk_disable(f1_copy)
            tk_disable(f1_delete)

        } else if (len == 1) {
            # One is selected
            tk_normalize(f1_view)
            tk_normalize(f1_rename)
            tk_normalize(f1_copy)
            tk_normalize(f1_delete)

        } else {
            # Several are selected
            tk_disable(f1_view)
            tk_disable(f1_rename)
            tk_disable(f1_copy)
            tk_normalize(f1_delete)
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

        obj_names <- get_selection(f1_listbox_y) %>% safe_names()
        if (length(obj_names) < 1) {
            return
        }

        str_glue("View({obj_names})") %>%
            str_c(collapse = "\n") %>%
            doItAndPrint()

    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    on_select_all    <- function() {
        set_selection(f1_listbox_y, sel = 1:get_size(f1_listbox_y))
        buttons_activation()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    on_rename_obj   <- function() {
        buttons_activation()

        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        rename_object <- function() {

            # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            obj_names     <- get_selection(f1_listbox_y)
            new_obj_names <- get_values(text_box_1)
            .ds <- active_dataset_0()

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if (object_is_not_selected(obj_names, parent = pop_up_window)) {return()}

            if (is_not_valid_name(new_obj_names, parent = pop_up_window)) {return()}

            if (obj_names == new_obj_names) {
                show_error_messages(str_c(
                    "The old and the new names are the same. ",
                    "To rename the object, you must choose a different name."),
                    title = "Choose Anoter Name",
                    parent = pop_up_window)
                return()
            }

            if (forbid_to_replace_object(new_obj_names, parent = pop_up_window)) {
                return()
            }


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
            set_selection(f1_listbox_y, new_obj_names)
            tk_see(f1_listbox_y, new_obj_names)
            buttons_activation()

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            close_pop_up_window()
            tkfocus(top)

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Announce about the success to run the function `onOk()`
            TRUE
        }

        # The next code could be a function
        parent   = top
        title    = "Rename Object"
        ok_label = "Rename"
        on_ok    = rename_object

        # Functions
        close_pop_up_window <- function() {
            tkgrab.release(pop_up_window)
            tkdestroy(pop_up_window)
        }

        # Widget
        offset = 10
        pop_up_window <- tktoplevel(parent, borderwidth = 10)
        tkwm.title(pop_up_window, title)

        position <- str_c(
            "+", tclvalue_int(tkwinfo("pointerx", parent))
            - floor(tclvalue_int(tkwinfo("reqwidth",  pop_up_window)) / 2)
            ,
            "+", tclvalue_int(tkwinfo("pointery", parent))
            - floor(tclvalue_int(tkwinfo("reqheight", pop_up_window)) / 2) + 25
        )

        tkwm.geometry(pop_up_window, position) # Move to cursor's position
        tkwm.transient(pop_up_window, parent)
        tkgrab(pop_up_window) # Make dialogue modal (wait for user response)

        f_but <- tk2frame(pop_up_window)

        b1_close <- tk2button(
            f_but,
            text     = "Cancel",
            image    = "::image::cancelIcon",
            width    = 8,
            compound = "left",
            command  = close_pop_up_window)

        b1_ok <- tk2button(
            f_but,
            text     = ok_label,
            image    = "::image::okIcon",
            compound = "left",
            width    = 8,
            default  = "active",
            command  = on_ok)

        lab_1_1 <- bs_label_b(pop_up_window, text = "Name: ")
        lab_1_2 <- bs_label(  pop_up_window, text = get_selection(f1_listbox_y))
        lab_2_1 <- bs_label_b(pop_up_window, text = "New name: ")

        initlal_name <-
            get_selection(f1_listbox_y) %>%
            unique_obj_names() %>%
            .[1]

        text_box_1 <-
            bs_entry(
                parent = pop_up_window,
                width  = 30,
                value  = initlal_name,
                tip = "Enter a new name for\nthe selected object.")

        tkselection.range(text_box_1$obj_text, 0, "end")
        tkfocus(text_box_1$obj_text)

        tkgrid(lab_1_1, lab_1_2, sticky = "ws")
        tkgrid(lab_2_1, text_box_1$frame, sticky = "ws", pady = c(2, 0))

        tkgrid(f_but, columnspan = 2, sticky = "e",  pady = c(10, 0))
        tkgrid(b1_ok, b1_close)

        tkgrid.configure(lab_1_1, lab_2_1, sticky = "e")

    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    on_copy_obj <- function() {
        buttons_activation()
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        duplicate_object <- function() {
            # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            obj_names     <- get_selection(f1_listbox_y)
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
            set_selection(f1_listbox_y, new_obj_names)
            tk_see(f1_listbox_y, new_obj_names)
            buttons_activation()

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            close_pop_up_window()
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            tkfocus(top)

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Announce about the success to run the function `onOk()`
            TRUE
        }

        # The next code could be a function
        parent   = top
        title    = "Duplicate Object"
        ok_label = "OK"
        on_ok    = duplicate_object

        # Functions
        close_pop_up_window <- function() {
            tkgrab.release(pop_up_window)
            tkdestroy(pop_up_window)
        }

        # Widget
        offset = 10
        pop_up_window <- tktoplevel(parent, borderwidth = 10)
        tkwm.title(pop_up_window, title)

        position <- str_c(
            "+", tclvalue_int(tkwinfo("pointerx", parent))
            - floor(tclvalue_int(tkwinfo("reqwidth",  pop_up_window)) / 2)
            ,
            "+", tclvalue_int(tkwinfo("pointery", parent))
            - floor(tclvalue_int(tkwinfo("reqheight", pop_up_window)) / 2) + 25
        )

        tkwm.geometry(pop_up_window, position) # Move to cursor's position
        tkwm.transient(pop_up_window, parent)
        tkgrab(pop_up_window) # Make dialogue modal (wait for user response)

        f_but <- tk2frame(pop_up_window)

        b1_close <- tk2button(
            f_but,
            text     = "Cancel",
            image    = "::image::cancelIcon",
            width    = 8,
            compound = "left",
            command  = close_pop_up_window)

        b1_ok <- tk2button(
            f_but,
            text     = ok_label,
            image    = "::image::okIcon",
            compound = "left",
            width    = 8,
            default  = "active",
            command  = on_ok)

        lab_1_1 <- bs_label_b(pop_up_window, text = "Original name: ")
        lab_1_2 <- bs_label(  pop_up_window, text = get_selection(f1_listbox_y))
        lab_2_1 <- bs_label_b(pop_up_window, text = "Name of a copy: ")

        initlal_name <-
            get_selection(f1_listbox_y) %>%
            unique_obj_names() %>%
            .[1]

        text_box_1 <-
            bs_entry(
                parent = pop_up_window,
                width  = 30,
                value  = initlal_name,
                tip = "Enter a name for the copy \nof the selected object.")

        tkselection.range(text_box_1$obj_text, 0, "end")
        tkfocus(text_box_1$obj_text)

        tkgrid(lab_1_1, lab_1_2, sticky = "ws")
        tkgrid(lab_2_1, text_box_1$frame, sticky = "ws", pady = c(2, 0))

        tkgrid(f_but, columnspan = 2, sticky = "e",  pady = c(10, 0))
        tkgrid(b1_ok, b1_close)

        tkgrid.configure(lab_1_1, lab_2_1, sticky = "e")

    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    on_delete_obj <- function() {
        buttons_activation()
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        obj_names <- get_selection(f1_listbox_y)
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
                "Do you agree to permanently DELETE {length(obj_names)} ",
                "the following objects: \n\n",
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
        set_selection(f1_listbox_y, ind)
        tk_see(f1_listbox_y, ind)
        buttons_activation()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        close_pop_up_window()
        tkfocus(top)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Widgets ----------------------------------------------------------------
    .ds <- active_dataset_0()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    win_title <- gettext_bs("Manage Objects And Datasets")
    initializeDialog(title = win_title)
    tk_title(top, text = win_title, columnspan = 2)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f1  <- tk2frame(top)
    f1a <- tk2frame(top)

    class_filter_box <-
        bs_combobox(
            parent = f1a, # f1_listbox_y$frame,
            label  = "Type of objects to list",
            label_position = "above",
            width  = 30 - 2, # Get width f1_listbox_y
            height = 10,
            value  = "Data frame",
            values = c( "All", "Data frame", "List", "Matrix", "Table",
                        "Plot (ggplot, gg)",
                        "Model (lm, glm, htest)", "Function", "Other"),
            tip = "",
            on_select = update_list_of_objects)

    include_hidden_box <-
        bs_checkboxes(
            parent = f1a,
            boxes  = c(hidden = "Show hidden objects"),
            values = 0,
            commands = c(hidden = update_list_of_objects))

    f1_listbox_y <-
        bs_listbox(
            parent       = f1a,
            title        = "Objects / Datasets",
            values       = "",
            selectmode   = "multiple", # "single",
            height       = 8,
            width        = c(30, Inf),
            on_keyboard  = "scroll",
            tip          = tip_multiple_ctrl_letters,
            use_filter   = TRUE,
            on_select    = buttons_activation,
            filter_label = "Object name filter")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f1b <- tk2frame(top)

    f1_rename <- tk2button(
        f1b,
        tip      = "Rename an object.",
        text     = "Rename",
        image    = "::image::bs_rename",
        compound = "left",
        command  = on_rename_obj)

    f1_copy <- tk2button(
        f1b,
        tip      = "Create a copy on an object.",
        text     = "Duplicate",
        image    = "::image::bs_copy",
        compound = "left",
        command  = on_copy_obj)

    f1_delete <- tk2button(
        f1b,
        tip      = "Delete one or several objects.",
        text     = "Delete",
        image    = "::image::bs_delete",
        compound = "left",
        command  = on_delete_obj)

    f1_select_all <- tk2button(
        f1b,
        text     = "Select all",
        tip      = "Select all listed items.",
        image    = "::image::bs_select_all",
        compound = "left",
        command  = on_select_all)

    f1_view <- tk2button(
        f1b,
        text     = "View",
        tip      = "View object (if possible)",
        image    = "::image::viewIcon",
        compound = "left",
        command  = on_view)

    tkgrid(f1, sticky = "w")
    tkgrid(f1a, f1b, sticky = "w")
    tkgrid.configure(f1b, sticky = "s")

    tkgrid(class_filter_box$frame,   sticky = "w")
    tkgrid(include_hidden_box$frame, sticky = "w")
    tkgrid(f1_listbox_y$frame, pady = c(5, 5))

    tkgrid(f1_view, sticky = "w")
    tkgrid(f1_rename, sticky = "w")
    tkgrid(f1_copy, sticky = "w")
    tkgrid(f1_delete, sticky = "w")
    tkgrid(f1_select_all, sticky = "w")

    tkconfigure(f1_view, width = 10)
    tkconfigure(f1_rename, width = 10)
    tkconfigure(f1_copy, width = 10)
    tkconfigure(f1_delete, width = 10)
    tkconfigure(f1_select_all, width = 10)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f1_close <- tk2button(
        f1b,
        tip      = "Close window",
        text     = "Close",
        image    = "::image::cancelIcon",
        compound = "left",
        width    = 9,
        command  = on_close)

    tkgrid(f1_close, sticky = "es", pady = c(34 - 9, 9))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dialogSuffix(onOK = do_nothing, onCancel = on_close, bindReturn = FALSE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_list_of_objects()

    if (!is.null(.ds)) {
        set_selection(f1_listbox_y, .ds)
        tk_see(f1_listbox_y, .ds)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    buttons_activation()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkbind(f1_listbox_y$listbox, "<Control-a>", on_select_all)
    tkbind(f1_listbox_y$listbox, "<Control-A>", on_select_all)
}
