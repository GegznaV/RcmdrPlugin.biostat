#' @rdname Helper-functions
#' @export
#' @keywords internal
bs_text <- function(parent, ..., label = "", undo = TRUE, context_menu = FALSE) {

    frame <- tk2frame(parent)

    obj_label <- tk_label_blue(frame, text = label)

    obj_txt <- tk2text(frame, undo = undo, ...)

    obj_xsc <- tk2scrollbar(
        frame,
        orientation = "horizontal",
        command = function(...) tkxview(obj_txt, ...)
    )

    obj_ysc <- tk2scrollbar(
        frame,
        orientation = "vertical",
        command = function(...) tkyview(obj_txt, ...)
    )

    tkconfigure(obj_txt,
                xscrollcommand = function(...) tkset(obj_xsc, ...),
                yscrollcommand = function(...) tkset(obj_ysc, ...))

    tkgrid(obj_label)
    tkgrid(obj_txt, obj_ysc)
    tkgrid(obj_xsc, "x")

    tkgrid.configure(obj_txt, sticky = "news", padx = c(10,  0))
    tkgrid.configure(obj_xsc, sticky = "we",   padx = c(10,  0))
    tkgrid.configure(obj_ysc, sticky = "ns",   padx = c(0,  10))

    tkgrid.columnconfigure(frame, 0, weight = 1)
    tkgrid.columnconfigure(frame, 1, weight = 0)

    tkgrid.rowconfigure(frame, 0, weight = 1,  minsize = 1)
    tkgrid.rowconfigure(frame, 1, weight = 10, minsize = 3)
    tkgrid.rowconfigure(frame, 2, weight = 0)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (context_menu == TRUE) {
        right_click_menu_text(obj_txt, undo = undo)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    structure(list(
        frame = frame,
        label    = obj_label,
        text     = obj_txt,
        x_scroll = obj_xsc,
        y_scroll = obj_ysc,
        context_menu_fun =
            purrr::partial(right_click_menu_text, tcl_widget = obj_txt, undo = undo)
    ),
    class = c("bs_text", "bs_tk_widget", "list"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Replace contents of text box widget
set_values.tk2text <- function(obj, values, ..., add = FALSE) {
    init_state <- tk_get_state(obj)

    if (init_state == "disabled") {
        tk_normalize(obj)
    }

    if (isTRUE(add)) {
        tkinsert(obj, "end", values)

    } else {
        tkdelete(obj, "1.0", "end")
        tkinsert(obj, "1.0", values)
    }

    if (init_state == "disabled") {
        tk_disable(obj)
    }
}

get_values.tk2text <- function(obj, ..., trim = FALSE) {
    tclvalue_chr(tkget(obj, "1.0", "end"), trim = trim)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set_values.bs_text <- function(obj, values, ..., add = FALSE) {
    set_values(obj$text, values, ..., add = add)
}

get_values.bs_text <- function(obj, ...) {
    get_values(obj$text, ...)
}

tk_normalize.bs_text <- function(obj, ...) {
    tk_normalize(obj$text, ...)
}

tk_disable.bs_text <- function(obj, ...) {
    tk_disable(obj$text, ...)
}
