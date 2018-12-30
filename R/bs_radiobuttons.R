#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Radiobuttons widget.
#'
#' Radiobuttons widget with command functions and tips when mouse is over the box.
#'
#' @param parent Parent frame.
#' @param buttons (vector of strings) Value names for each radiobutton.
#' @param labels (vector of strings) Labels for each radiobutton.
#' @param title Title for the set of radiobuttons.
#' @param value  (string) Initial value. One of `buttons` values.
#' @param variable Tcl/Tk variable. Is `NULL`, a new variable will be created.
#' @param commands  A named list of commands (functions) for radiobutton. The names must match the values of "buttons".
#' @param default_command (function) A default command.
#' @param tips   A named list of strings to be used as tips for radiobutton.
#' The names must match the values of "buttons".
#' @param default_tip (string) a default tip.
#' @param border (logical) Flag if the frame should have a border.
#' @param layout (string) One of "vertical" (default) and "horizontal".
#' @param sticky_buttons (string) `sticky` option for buttons.
#' @param sticky_title (string) `sticky` option for title (if no border is used).
#'
#' @return A named list with fields `frame` (frame with the radiobuttons),
#'  `var` (tcl/tk variables for each box),
#'  and `obj` (tcl/tk objects for each box).
#'
#' @export
#'
#' @examples
#'
#' top <- tcltk::tktoplevel()
#'
#' buttons_1 <- bs_radiobuttons(top, c("A", "B", "C"))
#' tcltk::tkgrid(buttons_1$frame)
#'
#'
#' top <- tcltk::tktoplevel()
#' buttons_2 <- bs_radiobuttons(top, buttons = c("A", "B", "C"),
#'                              border = TRUE, title = "Buttons")
#' tcltk::tkgrid(buttons_2$frame)
#'
#'
#' top <- tcltk::tktoplevel()
#' buttons_3 <- bs_radiobuttons(top, c("A", "B", "C"), layout = "h",
#'                          title = "Buttons", sticky_buttons = "")
#' tcltk::tkgrid(buttons_3$frame)
#'

bs_radiobuttons <- function(
    parent          = top,
    buttons,
    labels          = buttons,
    value           = buttons[1],
    variable        = NULL,
    title           = NULL,
    commands        = list(),       # named list of functions
    default_command = function() {},
    tips            = list(),       # named list of strings
    default_tip     = "",
    border          = FALSE,
    layout          = c("vertical", "horizontal"),
    sticky_buttons  = "w",
    sticky_title    = "w"
)
{
    checkmate::assert_string(title, null.ok = TRUE)
    checkmate::assert_list(commands)
    checkmate::assert_function(default_command)
    checkmate::assert_list(tips)
    checkmate::assert_string(default_tip)
    checkmate::assert_flag(border)

    layout <- match.arg(layout)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    buttons_list <- structure(buttons, names = buttons)

    # Manage commands` ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(commands) > 0 && !all(names(commands) %in% buttons)) {
        stop(
            "Argument `commands` must be a named list of functions. ",
            "The element names must be a subset of: ",
            paste(buttons, collapse = ", "), ". Unrecognized names: ",
            paste(setdiff(names(commands), buttons), collapse = ", "), "."
        )
    }

    commands <- modifyList(
        map(buttons_list, function(x) default_command),
        commands)

    # Manage `tips` ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.null(names(tips)) && (length(tips) == length(buttons))) {
        tips <- as.list(tips)

    } else if (length(tips) > 0 && !all(names(tips) %in% buttons)) {
        stop(
            "Argument `tips` must be a named list of strings.  ",
            "The element names must be a subset of: ",
            paste(buttons, collapse = ", "), ". Unrecognized names: ",
            paste(setdiff(names(tips), buttons), collapse = ", "), "."
        )

    } else {
        tips <- modifyList(map(buttons_list, function(x) default_tip), tips)
    }


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # values <-
    #     if (is.null(values)) {
    #         rep(default_value, length(buttons))
    #
    #     } else if (length(values) == length(buttons)) {
    #         values
    #
    #     } else {
    #         stop("The length of `values` must be ", length(buttons), ", not ",
    #              length(values), ".")
    #     }

    if (is.null(variable)) {
        variable <- tclVar(value)
    } else {
        tclvalue(variable) <- value
    }


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    frame <-
        if (border) {
            if (is.null(title)) {
                tk2labelframe(parent)

            } else {
                tk2labelframe(
                    parent,
                    labelwidget = bs_label_b(parent, text = title))
            }

        } else {
            tk2frame(parent)
        }

    if (!is.null(title) && !border) {
        tkgrid(bs_label_b(frame, text = title), sticky = sticky_title)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    objs <- pmap(
        list(labels, buttons, commands, tips),
        ~ tk2radiobutton(frame,
                         variable = variable,
                         text     = ..1,
                         value    = ..2,
                         command  = ..3,
                         tip      = ..4))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    switch(layout,
           vertical = {
               walk(objs, tkgrid, sticky = sticky_buttons)
           },

           horizontal = {
               buttons_str <- paste0("objs[[", seq_along(objs), "]]",
                                     collapse = ", ")
               eval_glue('tkgrid({buttons_str}, sticky = sticky_buttons)')
           },

           stop("Unrecognized layout: ", layout)
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    structure(list(
        frame = frame,
        var   = variable,
        obj   = structure(objs, names = buttons)
    ),
    class = c("bs_radiobuttons", "bs_tk_buttonset", "bs_tk_widget", "list"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
get_values.bs_radiobuttons <- function(obj, ...) {
    tclvalue_chr(obj$var)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
set_values.bs_radiobuttons <- function(obj, values, ...) {
    tclvalue(obj$var) <- values
}

