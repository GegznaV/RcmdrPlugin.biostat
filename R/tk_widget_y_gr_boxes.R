# * Y and Groups box =====================================================

#' @rdname Helper-functions
#' @keywords internal
#' @export
tk_widget_boxes_y_gr <- function(
    parent_frame   = top,

    y_title        = title_var_1,
    y_var_type     = NA_character_,  # use either y_var_type or y_vars, y_initial
    y_vars         = NULL,
    y_initial      = NULL,
    y_select_mode  = "single",
    y_params       = list(), # Not implemented yet

    gr_title       = title_gr_0_n,
    gr_var_type    = NA_character_, # use either gr_var_type or gr_vars, gr_initial
    gr_vars        = NULL,
    gr_initial     = NULL,
    gr_select_mode = "multiple",
    gr_params      = list(), # Not implemented yet

    ch_label       = "Use groups",
    ch_initial     = "0",
    ch_commands    = function() {},
    ch_params      = list(), # Not implemented yet

    listHeight     = 7,
    listWidth      = c(25, Inf), # min max width
    add_to_grid    = TRUE
) {

    checkmate::assert_choice(y_var_type,     c(bs_var_types, NA))
    checkmate::assert_choice(gr_var_type,    c(bs_var_types, NA))
    checkmate::assert_choice(y_select_mode,  c("single", "multiple"))
    checkmate::assert_choice(gr_select_mode, c("single", "multiple"))

    # Initial values -----------------------------------------------------
    if (!is.na(y_var_type)) {
        if (!is.null(y_vars)) {
            warning("`y_vars` is ignored as `y_var_type` is not NA")
        }

        y_vars    <- eval_glue("variables_{y_var_type}()")
        y_initial <- var_pos_n(y_initial, y_var_type)
    }

    if (!is.na(gr_var_type)) {
        if (!is.null(gr_vars)) {
            warning("`gr_vars` is ignored as `gr_var_type` is not NA")
        }

        gr_vars    <- eval_glue("variables_{gr_var_type}()")
        gr_initial <- var_pos_n(gr_initial, gr_var_type)
    }

    # Functions ----------------------------------------------------------
    cmd_ch_box <- function() {

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # If var_gr_box contents change
        if (get_size(var_gr_box) == 0) {
            tk_normalize(var_gr_box) # Enables changing the contents
            set_values(var_gr_box, " (No variables) ")
            tk_disable(var_gr_box,
                       font = tkfont.create(slant = "italic", size = 9))

            tclvalue(use_groups_Variable) <- "0"
            tk_disable(use_groups_CheckBox)

        } else {
            tk_activate(use_groups_CheckBox)
            # tkconfigure(var_gr_box$listbox,
            #             font = tkfont.create(slant = "roman", size = 9))
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # If state of checkbox changes
        if (tclvalue_lgl(use_groups_Variable) == TRUE) {
            tk_normalize(var_gr_box)

            if (get_selection_length(var_gr_box) == 0) {
                sel_ind <- 1
                set_selection(var_gr_box, sel_ind)
                set_yview(var_gr_box,     sel_ind)
            }

        } else {
            # Clear group variable box selection
            # set_selection(var_gr_box, sel = 0)
            tk_disable(var_gr_box)
        }
    }

    cmd_gr_box <- function() {

        if (tk_get_state(var_gr_box) != "disabled") {
            if (get_selection_length(var_gr_box) == 0) {
                tclvalue(use_groups_Variable) <- "0"

            } else {
                tclvalue(use_groups_Variable) <- "1"
            }
        }
    }


    if (length(listWidth) == 1) {
        listWidth <- c(listWidth, listWidth)
    }


    #+ Widgets ============================================================
    variables_frame <- tkframe(parent_frame)


    # List box Y ----------------------------------------------------------
    var_y_box <- variableListBox2(
        parentWindow      = variables_frame,
        title             = gettext_bs(y_title),
        variableList      = y_vars,
        initialSelection  = y_initial,
        selectmode        = y_select_mode,
        listHeight        = listHeight,
        listWidth         = listWidth,
        onRelease_fun     = function() {},
        onDoubleClick_fun = function() {}
    )

    # List box gr -------------------------------------------------------------
    var_gr_frame <- tkframe(variables_frame)

    var_gr_box <- variableListBox2(
        parentWindow     = var_gr_frame,
        title            = gettext_bs(gr_title),
        variableList     = gr_vars,
        initialSelection = gr_initial,
        selectmode       = gr_select_mode,
        listHeight       = listHeight - 1,
        listWidth        = listWidth,
        onRelease_fun    = cmd_gr_box
    )

    bs_check_boxes(
        window          = var_gr_frame,
        frame           = "use_groups_frame",
        boxes           = "use_groups_",
        initialValues   = ch_initial,
        labels          = gettext_bs(ch_label),
        commands        = list("use_groups_" = function() {
            cmd_ch_box()
            ch_commands()
        })
    )

    # Layout -------------------------------------------------------------
    if (add_to_grid) {
        tkgrid(variables_frame, sticky = "nwe", padx = c(0, 4))
    }

    tkgrid(getFrame(var_y_box), var_gr_frame, sticky = "nwe", padx = c(10, 0))

    tkgrid(var_gr_frame)
    tkgrid(getFrame(var_gr_box), sticky = "nsw", padx = c(20, 0))
    tkgrid(use_groups_frame,     sticky = "sw",  padx = c(20, 0),
           pady = c(0,  5))

    # Apply functions ----------------------------------------------------
    cmd_ch_box()

    # Output -------------------------------------------------------------
    structure(
        list(
            frame = variables_frame,

            var_checkbox = use_groups_Variable,
            obj_checkbox = use_groups_CheckBox,
            listbox_y    = var_y_box,
            listbox_gr   = var_gr_box

            # obj_y_listbox    = var_y_box$listbox,
            # obj_y_scrollbar  = var_y_box$scrollbar,

            # obj_gr_scrollbar = var_gr_box$scrollbar,
            # obj_gr_listbox   = var_gr_box$listbox

        ),
        class = c("tk_widget_y_gr_boxes", "bs_tk_widget", "list")
    )

}