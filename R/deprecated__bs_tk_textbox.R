# ============================================================================
#' @rdname Helper-functions
#' @export
#' @keywords internal
bs_tk_textbox <- function(

    parent = top,
    width = "28",
    value = "",
    label = "",
    label_position = c("left", "above", "right", "none"),
    label_color = getRcmdr("title.color"),
    padx = 0,
    pady = 0,     # pady = 5,
    sticky = "w",
    main_frame  = tkframe(parent),
    text_frame  = tkframe(main_frame),
    label_frame = tkframe(main_frame),
    tip = "",
    on_click           = function() {},
    on_double_click    = function() {},
    on_triple_click    = function() {},
    on_release         = function() {},
    on_click_3         = function() {},
    on_double_click_3  = function() {},
    on_triple_click_3  = function() {},
    on_release_3       = function() {},
    ...

) {
    label_position <- match.arg(label_position)

    var_text <- tclVar(value)

    obj_text <- tk2entry(
        parent       = text_frame,
        tip          = tip,
        width        = width,
        textvariable = var_text,
        ...
    )

    obj_label <- bs_label(
        parent     = label_frame,
        text       = gettext_bs(label),
        foreground = label_color
    )

    if (nchar(label) > 0) {

        switch(label_position,
               "above" = {
                   if (length(pady) == 1) {
                       pady <- c(pady, pady)
                   }
                   tkgrid(label_frame, sticky = sticky, padx = padx, pady = c(pady[1], 0))
                   tkgrid(text_frame,  sticky = sticky, padx = padx, pady = c(0, pady[2]))
                   tkgrid(obj_text,    sticky = sticky)
               } ,
               "left" = {
                   tkgrid(label_frame, text_frame, sticky = sticky,
                          padx = padx, pady = pady)
                   tkgrid(obj_text,   sticky = sticky, padx = c(5, 0))
               } ,
               "right" = {
                   tkgrid(text_frame, label_frame, sticky = sticky,
                          padx = padx, pady = pady)
                   tkgrid(obj_text,   sticky = sticky, padx = c(0, 5))
               }
        )
        tkgrid(obj_label, sticky = sticky)

    } else {
        tkgrid(text_frame, sticky = sticky, padx = padx, pady = pady)
        tkgrid(obj_text,  sticky = sticky)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bind_mouse_keys(obj_text)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    structure(list(
        frame       = main_frame,
        frame_text  = text_frame,
        frame_label = label_frame,

        var_text  = var_text,
        obj_text  = obj_text,
        obj_label = obj_label
    ),
    class = c("bs_tk_textbox", "bs_tk_widget", "list"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#' @rdname Helper-functions
#' @export
#' @keywords internal
set_values.bs_tk_textbox <- function(obj, values, ...) {
    if (is.null(values)) {
        values <- ""
    }
    tclvalue(obj$var_text) <- values
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
`values<-.bs_tk_textbox` <- function(x, value) {
    set_values(x, value)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
`tclvalue<-.bs_tk_textbox` <- function(x, value) {
    set_values(x, value)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_values.bs_tk_textbox <- function(obj, ...) {
    tclvalue_chr(obj$var_text)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_disable.bs_tk_textbox <- function(obj, ..., foreground = "grey") {
    tk_disable(obj$obj_text, ...)
    tk_disable(obj$obj_label, foreground = foreground)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_normalize.bs_tk_textbox <- function(obj, ..., foreground = getRcmdr("title.color")) {
    tk_normalize(obj$obj_text, ...)
    tk_normalize(obj$obj_label, foreground = foreground)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_activate.bs_tk_textbox <- function(obj, ..., foreground = getRcmdr("title.color")) {
    tk_activate(obj$obj_text)
    tk_activate(obj$obj_label, foreground = foreground)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_get_state.bs_tk_textbox <- function(obj, ...) {
    tk_get_state(obj$obj_text, ...)
}
