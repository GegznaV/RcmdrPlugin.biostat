#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_widget_modify_names <- function(parent = top,
                                   cmd_radiobuttons = function(){},
                                   ) {
    main_frame <- tkframe(parent)

    var_name_opts_frame <- tkframe(main_frame)

    radioButtons_horizontal(
        window       = var_name_opts_frame,
        name         = "names_action_",
        title        = gettext_bs("Variable names: "),
        title.color  = fg_col,
        buttons      = c("overwrite", "modify"),
        values       = c("overwrite", "modify"),
        initialValue = dialog_values$names_action,
        labels       = gettext_bs(c("Overwrite", "Copy & modify")),
        command      = cmd_radiobuttons # control_checkbox_activation
    )

    make_unique_outer_frame <- tkframe(var_name_opts_frame)

    bs_check_boxes(make_unique_outer_frame,
                   frame = "make_unique_frame",
                   boxes = c("make_unique"),
                   # commands = list("make_unique" = control_checkbox_activation),
                   initialValues = c(dialog_values$make_unique),
                   labels = gettext_bs(c("Make unique"))
    )

    lower_frame <- tkframe(main_frame)

    prefix_var   <- tclVar(dialog_values$prefix)
    prefix_field <- ttkentry(lower_frame, width = "37", textvariable = prefix_var)

    suffix_var   <- tclVar(dialog_values$suffix)
    suffix_field <- ttkentry(lower_frame, width = "37", textvariable = suffix_var)

    # Layout
    tkgrid(main_frame, sticky = "ew")

    tkgrid(var_name_opts_frame, sticky = "ws")
    tkgrid(names_action_Frame,  make_unique_outer_frame, sticky = "ws", pady = c(10, 0))
    tkgrid(make_unique_frame,   sticky = "ws")


    tkgrid(lower_frame, sticky = "ew")
    tkgrid(
        labelRcmdr(lower_frame, text = gettext_bs("Prefix:"), fg = fg_col),
        labelRcmdr(lower_frame, text = gettext_bs("     ")),
        prefix_field,
        pady = c(10, 2)
    )
    tkgrid(
        labelRcmdr(lower_frame, text = gettext_bs("Suffix:"), fg = fg_col),
        labelRcmdr(lower_frame, text = gettext_bs("     ")),
        suffix_field
    )

    # Return

    structure(
        list(frame = main_frame,

             var_radiobuttons = names_action_Variable,
             var_checkbox     = make_uniqueVariable,
             var_prefix       = prefix_var,
             var_suffix       = suffix_var,

             obj_radiobuttons = NULL,   # Not implemented yet
             obj_checkbox     = make_uniqueCheckBox,
             obj_prefix       = prefix_field,
             obj_suffix       = suffix_field
        ),
    class = c("tk_widget_modify_names", "list"))

}

print.tk_widget_modify_names <- function(x, ...) {
    summary(x, ...)
}