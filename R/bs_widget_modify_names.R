# TODO:
# Replace functions:
#   - radioButtons_horizontal() -> bs_radiobuttons()
#   - bs_check_boxes()          -> bs_checkboxes()
#   - add entry field validation functions

#' TCL/TK helper widget for multiple variable names
#'
#' @export
#' @keywords internal
#' @examples
#' \dontrun{\donttest{
#' paret_frame <- tktoplevel()
#' widget <- tk_widget_modify_names(paret_frame, layout = "vvhv")
#' tkgrid(widget$frame)
#' }}

tk_widget_modify_names <- function(
    parent = top,
    init_val_radiobuttons = c("modify", "overwrite"),
    init_val_checkbox = "1",
    init_val_prefix   = "",
    init_val_suffix   = "",
    layout            = "vvhv", # f1:radiobuttons, f1:checkboxes, f1 vs f2, f2:entries,
    width             = 12, # for "vvvv" 10, for "hhvh" 12
    cmd_radiobuttons  = do_nothing,
    cmd_checkbox      = do_nothing,
    cmd_change_prefix = do_nothing,
    cmd_change_suffix = do_nothing
) {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    control_checkbox_activation <- function() {

        opt_1 <- tclvalue_chr(wid$radiobutton$var)

        switch(
            opt_1,
            "overwrite" = {
                # Clear values
                # set_values(wid$checkbox, make_unique = 0)
                set_values(wid$prefix,   "")
                set_values(wid$suffix,   "")

                # Disable widgets
                tk_disable(wid$checkbox)
                tk_disable(wid$prefix)
                tk_disable(wid$suffix)
            },
            "modify" = {
                # Activate widgets
                tk_activate(wid$checkbox)
                tk_activate(wid$prefix)
                tk_activate(wid$suffix)

                cmd_change_prefix()
                cmd_change_suffix()
            },
            stop("Unrecognized option")
        )
    }
    # ========================================================================

    init_val_radiobuttons <- match.arg(init_val_radiobuttons)

    vals <- c("v", "h")
    layout_combinations <-
        tidyr::crossing(vals, vals, vals, vals) %>%
        dplyr::transmute(v = str_c(vals, vals1, vals2, vals3)) %>%
        dplyr::pull(v)

    layout <- match.arg(layout, choices = layout_combinations)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    main_frame <- tkframe(parent)

    f1 <- tkframe(main_frame)

    f1_ratio <-
        bs_radiobuttons(
            parent  = f1,
            layout  = str_sub(layout, 1, 1),
            title   = gettext_bs("Variable names: "),
            buttons = c("overwrite", "modify"),
            value   = init_val_radiobuttons,
            labels  = gettext_bs(c("Overwrite", "Create new ")),
            tips    = list(
                overwrite = gettext_bs(
                    str_c("Overwrite values of existing variables\n",
                          "Do not change names. \n",
                          "No warning will be issued."
                    )),
                modify = gettext_bs(
                    str_c("Create new variables by adding \n",
                          "prefix/suffix to existing names \n",
                          "A warning will be issued, if \n",
                          "duplicate names are detected."))),

            default_command = function() {
                control_checkbox_activation()
                cmd_radiobuttons()
            }
        )

    f1_check_frame <- tkframe(f1)

    f1_check <-
        bs_checkboxes(
            parent   = f1_check_frame,
            boxes    = c("make_unique"),
            commands = list(make_unique = cmd_checkbox),
            values   = init_val_checkbox,
            labels   = gettext_bs(c("Make unique"))
        )

    f2 <- tkframe(main_frame)

    f2_pre <-
        bs_entry(
            parent = f2,
            width  = width,
            value  = init_val_prefix,
            label  = gettext_bs("Prefix:"))

    f2_suf <-
        bs_entry(
            parent = f2,
            width  = width,
            value  = init_val_prefix,
            label  = gettext_bs("Suffix:"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Layout -----------------------------------------------------------------

    layout_checkbox <- str_sub(layout, 2, 2)
    layout_f1_f2    <- str_sub(layout, 3, 3)
    layout_entries  <- str_sub(layout, 4, 4)

    if (layout_checkbox == "h") {
        tkgrid(f1_ratio$frame, f1_check_frame)

    } else {
        tkgrid(f1_ratio$frame)
        tkgrid(f1_check_frame)
    }

    tkgrid.configure(f1_check_frame, sticky = "s", padx = c(5, 0))

    tkgrid(f1_check$frame)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (layout_f1_f2 == "h") {
        tkgrid(f1, f2, sticky = "ws", padx = c(0, 5), pady = c(0, 5))

    } else {
        tkgrid(f1, sticky = "w")
        tkgrid(f2, padx = c(0, 0), pady = c(10, 5), sticky = "ew")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (layout_entries == "h") {
        tkgrid(f2_pre$frame, f2_suf$frame, pady = c(0 , 2))
        tkgrid.configure(f2_suf$frame,     padx = c(15, 0))

    } else {
        tkgrid(f2_pre$frame, pady = c(0, 2))
        tkgrid(f2_suf$frame, pady = c(0, 2))
        tkgrid(f2, padx = c(0, 0))
    }

    # Return -----------------------------------------------------------------
    wid <- structure(
        list(frame = main_frame,

             f1          = f1,
             radiobutton = f1_ratio,
             checkbox    = f1_check,
             f2          = f2,
             prefix      = f2_pre,
             suffix      = f2_suf

        ),
        class = c("tk_widget_modify_names", "bs_tk_widget", "list")
    )
    wid
}

# parent <- tktoplevel()

# tkconfigure(f2_suf$obj_text, width = 12)
# tkconfigure(f2_pre$obj_text, width = 12)
