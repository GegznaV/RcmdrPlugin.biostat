# TODO:
# Replace functions:
#   - radioButtons_horizontal() -> bs_radiobuttons()
#   - bs_check_boxes()          -> bs_checkboxes()


#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_widget_modify_names <- function(
    parent = top,
    init_val_radiobuttons = c("overwrite", "modify"),
    init_val_checkbox = "0",
    init_val_prefix   = "",
    init_val_suffix   = "",
    width             = 37,
    cmd_radiobuttons  = function(){},
    cmd_checkbox      = function(){}
) {
    fg_col <- fg_col <- Rcmdr::getRcmdr("title.color")
    init_val_radiobuttons <- match.arg(init_val_radiobuttons)

    main_frame <- tkframe(parent)

    var_name_opts_frame <- tkframe(main_frame)

    radioButtons_horizontal(
        window       = var_name_opts_frame,
        name         = "names_action_",
        title        = gettext_bs("Variable names: "),
        title.color  = fg_col,
        buttons      = c("overwrite", "modify"),
        values       = c("overwrite", "modify"),
        initialValue = init_val_radiobuttons,
        labels       = gettext_bs(c("Overwrite", "Copy & modify")),
        command      = cmd_radiobuttons
    )

    make_unique_outer_frame <- tkframe(var_name_opts_frame)

    bs_check_boxes(make_unique_outer_frame,
                   frame         = "make_unique_frame",
                   boxes         = c("make_unique"),
                   commands      = list("make_unique" = cmd_checkbox),
                   initialValues = init_val_checkbox,
                   labels        = gettext_bs(c("Make unique"))
    )

    lower_frame <- tkframe(main_frame)

    prefix_var   <- tclVar(init_val_prefix)
    prefix_field <- ttkentry(lower_frame, width = width, textvariable = prefix_var)

    suffix_var   <- tclVar(init_val_suffix)
    suffix_field <- ttkentry(lower_frame, width = width, textvariable = suffix_var)


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

             # obj_radiobuttons = NULL,
             obj_button_overwrite = overwriteButton,
             obj_button_modify    = modifyButton,
             obj_checkbox     = make_uniqueCheckBox,
             obj_prefix       = prefix_field,
             obj_suffix       = suffix_field
        ),
        class = c("tk_widget_modify_names", "bs_tk_widget", "list")
    )
}
