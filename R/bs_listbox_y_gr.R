# * Y and Groups box =====================================================

#' Widget: two listboxes with checkbox
#'
#' @keywords internal
#' @export
#' @examples
#' \dontrun{\donttest{
#' # Active dataset must be selected
#' top <- tktoplevel()
#' lb <- bs_listbox_y_gr(top)
#' tkgrid(lb$frame)
#' }}
bs_listbox_y_gr <- function(
    parent   = top,

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
    gr_on_select   = do_nothing,
    gr_params      = list(), # Not implemented yet

    ch_label       = "Use groups",
    ch_initial     = "0",
    ch_command     = do_nothing,
    ch_params      = list(), # Not implemented yet

    list_height     = 7,
    list_width      = c(25, Inf), # min max width
    add_to_grid     = TRUE
) {

    checkmate::assert_choice(y_var_type,     c(bs_var_types, NA))
    checkmate::assert_choice(gr_var_type,    c(bs_var_types, NA))
    checkmate::assert_choice(y_select_mode,  c("single", "multiple"))
    checkmate::assert_choice(gr_select_mode, c("single", "multiple"))

    # Functions ----------------------------------------------------------
    cmd_ch_box <- function() {

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # If f2_box_gr contents change
        if (get_size(f2_box_gr) == 0) {
            tk_normalize(f2_box_gr) # Enables changing the contents
            set_values(f2_box_gr, " (No variables) ")
            tk_disable(f2_box_gr,
                       font = tkfont.create(slant = "italic", size = 9))

            set_values(f2_box_ch, use_groups = 0)
            tk_disable(f2_box_ch)

        } else {
            tk_normalize(f2_box_ch)
            # tkconfigure(f2_box_gr$listbox,
            #             font = tkfont.create(slant = "roman", size = 9))
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # If state of checkbox changes
        if (get_values(f2_box_ch, "use_groups") == TRUE) {
            tk_normalize(f2_box_gr)

            if (get_selection_length(f2_box_gr) == 0) {
                sel_ind <- 1
                set_selection(f2_box_gr, sel_ind)
                set_yview(f2_box_gr,     sel_ind)
            }

        } else {
            # Clear group variable box selection
            # set_selection(f2_box_gr, sel = 0)
            tk_disable(f2_box_gr)
        }

        ch_command()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd_gr_box <- function() {

        if (tk_get_state(f2_box_gr) != "disabled") {
            if (get_selection_length(f2_box_gr) == 0) {
                set_values(f2_box_ch, use_groups = 0)

            } else {
                set_values(f2_box_ch, use_groups = 1)
            }
        }

        gr_on_select()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Initial values ---------------------------------------------------------
    if (!is.na(y_var_type)) {
        if (!is.null(y_vars)) {
            warning("`y_vars` is ignored as `y_var_type` is not NA")
        }

        y_vars    <- str_glue_eval("variables_{y_var_type}()")
        # y_initial <- var_pos_n(y_initial, y_var_type)
        y_initial <- y_initial
    }

    if (!is.na(gr_var_type)) {
        if (!is.null(gr_vars)) {
            warning("`gr_vars` is ignored as `gr_var_type` is not NA")
        }

        gr_vars    <- str_glue_eval("variables_{gr_var_type}()")
        # gr_initial <- var_pos_n(gr_initial, gr_var_type)
        gr_initial <- gr_initial
    }

    if (length(list_width) == 1) {
        list_width <- c(list_width, list_width)
    }


    #+ Widgets ============================================================
    f0 <- tkframe(parent)

    # List box Y ----------------------------------------------------------
    y_params <- modifyList(
        y_params,
        list(
            parent     = f0,
            title      = y_title,
            values     = y_vars,
            value      = y_initial,
            selectmode = y_select_mode,
            height     = list_height,
            width      = list_width)
    )

    f1_box_y <- do.call(bs_listbox, y_params)

    # List box gr -------------------------------------------------------------
    f2 <- tkframe(f0)

    gr_params <- modifyList(
        gr_params,
        list(
            parent      = f2,
            title       = gr_title,
            values      = gr_vars,
            value       = gr_initial,
            selectmode  = gr_select_mode,
            height      = list_height - 1,
            width       = list_width,
            on_release  = cmd_gr_box
        )
    )

    f2_box_gr <- do.call(bs_listbox, gr_params)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ch_params <- modifyList(
        ch_params,
        list(
            parent   = f2,
            boxes    = c("use_groups" = ch_label),
            values   = ch_initial,
            commands = list("use_groups" = cmd_ch_box)
        ))

    f2_box_ch <- do.call(bs_checkboxes, ch_params)

    # Layout -------------------------------------------------------------
    if (add_to_grid) {
        tkgrid(f0, sticky = "nwe", padx = c(0, 4))
    }

    tkgrid(f1_box_y$frame, f2, sticky = "nwe", padx = c(10, 0))

    tkgrid(f2)
    tkgrid(getFrame(f2_box_gr), sticky = "nsw", padx = c(20, 0))
    tkgrid(f2_box_ch$frame,     sticky = "sw",  padx = c(20, 0),
           pady = c(0,  5))

    # Apply functions ----------------------------------------------------
    cmd_ch_box()

    # Output -------------------------------------------------------------
    structure(
        list(
            frame   = f0,
            frame_2 = f2,

            y        = f1_box_y,
            gr       = f2_box_gr,
            checkbox = f2_box_ch,

            ch_fun = cmd_ch_box,
            gr_fun = cmd_gr_box

        ),
        class = c("tk_widget_y_gr_boxes", "bs_tk_widget", "list")
    )

}
