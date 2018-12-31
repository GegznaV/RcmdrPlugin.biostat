#' @rdname Helper-functions
#' @export
#' @keywords internal
bs_text <- function(parent, ..., undo = TRUE) {
    obj_txt <- tk2text(
        parent,
        ...,
        undo = undo,
        xscrollcommand = function(...) tkset(obj_xsc, ...),
        yscrollcommand = function(...) tkset(obj_ysc, ...))

    obj_xsc <- tk2scrollbar(
        parent,
        orient = "horizontal",
        command = function(...) tkxview(obj_txt, ...)
    )

    obj_ysc <- tk2scrollbar(
        parent,
        orient = "vertical",
        command = function(...) tkyview(obj_txt, ...)
    )

    tkgrid(obj_txt, obj_ysc)
    tkgrid(obj_xsc, "x")

    tkgrid.configure(obj_txt, sticky = "news", padx = c(10,  0))
    tkgrid.configure(obj_xsc, sticky = "we",   padx = c(10,  0))
    tkgrid.configure(obj_ysc, sticky = "ns",   padx = c(0,  10))

    obj_txt
}


# Replace contents of text box widget
set_values.tk2text <- function(obj, values, ...) {
    init_state = tk_get_state(obj)

    if (init_state == "disabled") {
        tk_normalize(obj)
    }

    tkdelete(obj, "1.0", "end")
    tkinsert(obj, "1.0", values)

    if (init_state == "disabled") {
        tk_disable(obj)
    }
}

get_values.tk2text <- function(obj, ...) {
    tclvalue_chr(tkget(obj, "1.0", "end"))
}