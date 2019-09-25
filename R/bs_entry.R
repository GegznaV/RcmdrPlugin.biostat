
#' Tcl/Tk text entry widget.
#'
#' @param parent Parent Tcl/Tk window.
#' @param width (numeric) Width of the widget.
#' @param value (character) Initial value.
#' @param label (character) Label of the widget.
#' @param label_position (character) One of "left", "above", "right", "none"
#' @param label_color (character)
#' @param padx (numeric)
#' @param pady (numeric)
#' @param sticky  (character)
#' @param sticky_label (character)
#' @param sticky_text (character)
#' @param main_frame Tcl/Tk frame for all parts of the widget.
#' @param text_frame Tcl/Tk frame for the entry widget.
#' @param label_frame  Tcl/Tk frame for the labels.
#' @param tip (character) Text visible on hover.
#' @param label_tip  (character) Text visible on hover over the label.
#' @param scroll_x (logical) Should scrollbar for x direction be added.
#' @param on_click  (function)
#' @param on_double_click  (function)
#' @param on_triple_click  (function)
#' @param on_release  (function)
#' @param on_click_3  (function)
#' @param on_double_click_3  (function)
#' @param on_triple_click_3  (function)
#' @param on_release_3  (function)
#' @param on_key_release (function)
#' @param use_context_menu (logical) Flag if the default context menu should be added
#' @param bind_clear (logical)  Bind "clear" on double right-click
#' @param variable Tcl/Tk variable.
#' @param ... Other parameters sent to `tk2entry`
#'
#' @md
#'
#' @export
#' @keywords internal
bs_entry <- function(

    parent = top,
    width = "28",
    value = "",
    label = "",
    label_position = c("left", "above", "right", "none"),
    label_color = getRcmdr("title.color"),
    padx = 0,
    pady = 0,     # pady = 5,
    sticky = "w",
    sticky_label = sticky,
    sticky_text  = sticky,
    main_frame  = tk2frame(parent),
    text_frame  = tk2frame(main_frame),
    label_frame = tk2frame(main_frame),
    tip       = "",
    label_tip = "",
    scroll_x  = FALSE,
    on_click           = do_nothing,
    on_double_click    = do_nothing,
    on_triple_click    = do_nothing,
    on_release         = do_nothing,
    on_click_3         = do_nothing,
    on_double_click_3  = do_nothing,
    on_triple_click_3  = do_nothing,
    on_release_3       = do_nothing,
    on_key_release     = do_nothing,

    use_context_menu = TRUE, # on single right-click
    bind_clear = TRUE,       # on double right-click
    variable = tclVar(value),
    ...

) {
    label_position <- match.arg(label_position)

    # var_text <- tclVar(value)
    var_text <- variable

    obj_text <- tk2entry(
        parent       = text_frame,
        tip          = tip,
        width        = width,
        textvariable = var_text,
        ...
    )

    obj_label <- tk2label(
        parent     = label_frame,
        text       = gettext_bs(label),
        foreground = label_color,
        tip        = label_tip
    )

    if (nchar(label) > 0) {

        switch(label_position,
               "above" = {
                   if (length(pady) == 1) {
                       pady <- c(pady, pady)
                   }
                   tkgrid(label_frame, sticky = sticky, padx = padx, pady = c(pady[1], 0))
                   tkgrid(text_frame,  sticky = sticky, padx = padx, pady = c(0, pady[2]))
                   tkgrid(obj_text,    sticky = sticky_text)
               },

               "left" = {
                   tkgrid(label_frame, text_frame, sticky = sticky,
                          padx = padx, pady = pady)
                   tkgrid(obj_text, sticky = sticky_text, padx = c(5, 0))
               },

               "right" = {
                   tkgrid(text_frame, label_frame, sticky = sticky,
                          padx = padx, pady = pady)
                   tkgrid(obj_text, sticky = sticky_text, padx = c(0, 5))
               }
        )
        tkgrid(obj_label, sticky = sticky_label)

    } else {
        tkgrid(text_frame, sticky = sticky, padx = padx, pady = pady)
        tkgrid(obj_text,  sticky = sticky_text)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bind_mouse_keys(obj_text)
    tkbind(obj_text, "<KeyRelease>", on_key_release)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    on_copy <- function() {
        anything_is_selected <- tclvalue_lgl(tkselection.present(obj_text))
        if (anything_is_selected) {
            # Continue, if anyting is selected.
            sel_from <- tclvalue_int(tkindex(obj_text, "sel.first")) + 1
            sel_to   <- tclvalue_int(tkindex(obj_text, "sel.last"))  + 1
            all_text <- tclvalue_chr(var_text, trim = FALSE)
            sel_text <- str_sub(all_text, sel_from, sel_to)

            clipr::write_clip(sel_text)
            # tkclipboard.clear()
            # tkclipboard.append(sel_text)
        }
    }

    on_delete <- function() {
        anything_is_selected <- tclvalue_lgl(tkselection.present(obj_text))
        if (anything_is_selected) {
            # Continue, if anyting is selected.
            del_from <- tkindex(obj_text, "sel.first")
            del_to   <- tkindex(obj_text, "sel.last")
            tkdelete(obj_text, del_from, del_to)
        }
    }

    on_cut <- function() {
        on_copy()
        on_delete()
    }

    on_paste <- function() {
        on_delete()
        tkinsert(obj_text, "insert", read_clipboard())
        # "insert" is cursor's position
    }

    on_clear_all <- function() {
        tclvalue(var_text) <- ""
    }

    on_select_all <- function() {
        tkselection.range(obj_text, 0, "end")
        tkfocus(obj_text)
    }

    clear_and_paste <- function() {
        tclvalue(var_text) <- read_clipboard()
        tkicursor(obj_text, "end")
        tkxview.moveto(obj_text, "1")
    }

    if (isTRUE(bind_clear)) {
        tkbind(obj_text, "<Double-Button-3>", on_clear_all)
    }

    if (isTRUE(use_context_menu)) {


        context_menu <- function() {

            top <- CommanderWindow()

            menu_i <- tk2menu(tk2menu(top), tearoff = FALSE)

            # tkadd(menu_i, "command",
            #       label    = "Copy",
            #       compound = "left",
            #       image    = "::image::bs_delete",
            #       command  = do_nothing)

            # tkadd(menu_i, "command",
            #       label    = "Clear and paste",
            #       compound = "left",
            #       image    = "::image::bs_paste",
            #       command  = clear_and_paste)

            tkadd(
                menu_i,
                "command",
                image = "::image::bs_cut",
                compound = "left",
                label = gettext_bs("Cut"),
                command = on_cut
            )

            tkadd(
                menu_i,
                "command",
                image = "::image::bs_copy",
                compound = "left",
                label = gettext_bs("Copy"),
                command = on_copy
            )

            tkadd(
                menu_i,
                "command",
                image = "::image::bs_paste",
                compound = "left",
                label = gettext_bs("Paste"),
                command = on_paste
            )

            tkadd(
                menu_i,
                "command",
                image = "::image::bs_delete",
                compound = "left",
                label = gettext_bs("Delete"),
                command = on_delete
            )
#
#             tkadd(menu_i, "command",
#                   label    = "Delete all",
#                   compound = "left",
#                   image    = "::image::bs_delete",
#                   command  = on_clear_all)

            tkadd(
                menu_i,
                "command",
                image = "::image::bs_select_all",
                compound = "left",
                label = gettext_bs("Select all"),
                command = on_select_all
            )

            tkpopup(menu_i,
                    tkwinfo("pointerx", top),
                    tkwinfo("pointery", top))
        }

        tkbind(obj_text, "<ButtonPress-3>",          context_menu)
        tkbind(obj_text, "<Control-ButtonPress-1>",  context_menu)
        if (MacOSXP()) {
            tkbind(obj_text, "<Meta-ButtonPress-1>", context_menu)
        }

    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid.columnconfigure(main_frame,  0, weight = 1)
    tkgrid.columnconfigure(text_frame,  0, weight = 1)
    tkgrid.columnconfigure(label_frame, 0, weight = 0)
    tkgrid.columnconfigure(obj_text,    0, weight = 1)
    tkgrid.columnconfigure(obj_label,   0, weight = 0)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (isTRUE(scroll_x)) {
        computeXscroll <- ttkscrollbar(
            text_frame,
            orient = "horizontal",
            command = function(...)
                tkxview(f2_entry_expr$obj_text, ...)
        )
        tkconfigure(
            obj_text,
            xscrollcommand = function(...)
                tkset(computeXscroll, ...)
        )
        tkgrid(computeXscroll, sticky = "ew")
        tkgrid.columnconfigure(computeXscroll, 0, weight = 1)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    structure(list(
        frame       = main_frame,
        frame_text  = text_frame,
        frame_label = label_frame,

        var_text  = var_text,
        obj_text  = obj_text,
        obj_label = obj_label
    ),
    class = c("bs_entry", "bs_tk_widget", "list"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get values =================================================================
#' @rdname bs_entry
#'
#' @param trim (logical) Should leading and tailing whitespace be trimmed in the result?
#' @export
get_values.bs_entry <- function(obj, ..., trim = TRUE) {
    tclvalue_chr(obj$var_text, ..., trim = trim)
}


# Set values =================================================================
#' @rdname bs_entry
#' @param add (logical) Should values be added to currently existing ones?
#' @export
set_values.bs_entry <- function(obj, values, ..., add = FALSE) {
    if (missing(values) || is.null(values)) {
        values <- ""
    }

    if (isTRUE(add)) {
        values <- str_c(get_values(obj), values)
    }

    tclvalue(obj$var_text) <- values
}

#' @rdname Helper-functions
#' @export
`values<-.bs_entry` <- function(x, value) {
    set_values(x, value)
}

#' @rdname Helper-functions
#' @export
`tclvalue<-.bs_entry` <- function(x, value) {
    set_values(x, value)
}


# State ======================================================================
#' @rdname bs_entry
#' @export
tk_get_state.bs_entry <- function(obj, ...) {
    tk_get_state(obj$obj_text, ...)
}

#' @rdname bs_entry
#'
#' @param foreground (string) Label text color.
#' @export
tk_disable.bs_entry <- function(obj, ..., foreground = "grey") {
    tk_disable(obj$obj_text, ...)
    tk_disable(obj$obj_label, foreground = foreground)
}

#' @rdname bs_entry
#' @export
tk_normalize.bs_entry <- function(obj, ..., foreground = getRcmdr("title.color")) {
    tk_normalize(obj$obj_text, ...)
    tk_normalize(obj$obj_label, foreground = foreground)
}

#' @rdname bs_entry
#' @export
tk_activate.bs_entry <- function(obj, ..., foreground = getRcmdr("title.color")) {
    tk_activate(obj$obj_text)
    tk_activate(obj$obj_label, foreground = foreground)
}

