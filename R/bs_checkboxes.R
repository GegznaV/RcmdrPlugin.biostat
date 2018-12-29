#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Checkboxes widget.
#'
#' Checkboxes widget with command functions and tips when mouse is over the box.
#'
#' @param parent Parent frame.
#' @param boxes (vector of strings) Variable names for each checkbox.
#' @param labels (vector of strings) Labels for each checkbox.
#' @param title Title for the set of checkboxes.
#' @param values  A vector of values ("0" or "1"). Overrides the `default_value`.
#' @param default_value (string) Default value ("0" or "1").
#' @param commands  A named list of commands (functions) for checkbox. The names must match the values of "boxes".
#' @param default_command (function) A default command.
#' @param tips   A named list of strings to be used as tips for checkbox.
#' The names must match the values of "boxes".
#' @param default_tip (string) a default tip.
#' @param border (logical) Flag if the frame should have a border.
#' @param layout (string) One of "vertical" (default) and "horizontal".
#' @param buttons_sticky (string) `sticky` option for buttons.
#' @param title_sticky (string) `sticky` option for title (if no border is used).
#'
#' @return A named list with fields `frame` (frame with the checkboxes),
#'  `var` (tcl/tk variables for each box),
#'  and `obj` (tcl/tk objects for each box).
#'
#' @export
#'
#' @examples
#'
#' top <- tcltk::tktoplevel()
#'
#' boxes_1 <- bs_checkboxes(top, c("A", "B", "C"))
#' tcltk::tkgrid(boxes_1$frame)
#'
#'
#' top <- tcltk::tktoplevel()
#' boxes_2 <- bs_checkboxes(top, boxes = c("A", "B", "C"), border = TRUE)
#' tcltk::tkgrid(boxes_2$frame)
#'
#'
#' top <- tcltk::tktoplevel()
#' boxes_3 <- bs_checkboxes(top, c("A", "B", "C"), layout = "h",
#'                          title = "Buttons", buttons_sticky = "")
#' tcltk::tkgrid(boxes_3$frame)
#'
#'
bs_checkboxes <- function(
    parent          = top,
    boxes,
    labels          = boxes,
    title           = NULL,
    values          = NULL,
    default_value   = "0",
    commands        = list(),          # named list of functions
    default_command = function() {},
    tips            = list(), # named list of strings
    default_tip     = "",
    border          = FALSE,
    layout          = c("vertical", "horizontal"),
    buttons_sticky  = "w",
    title_sticky    = "w"
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
    boxes_list <- structure(boxes, names = boxes)

    # Manage commands` ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(commands) > 0 && !all(names(commands) %in% boxes)) {
        stop(
            "Argument `commands` must be a named list of functions. ",
            "The element names must be a subset of: ",
            paste(boxes, collapse = ", "), ". Unrecognized names: ",
            paste(setdiff(names(commands), boxes), collapse = ", "), "."
        )
    }

    commands <- modifyList(
        map(boxes_list, function(x) default_command),
        commands)

    # Manage `tips` ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(tips) > 0 && !all(names(tips) %in% boxes)) {
        stop(
            "Argument `tips` must be a named list of strings.  ",
            "The element names must be a subset of: ",
            paste(boxes, collapse = ", "), ". Unrecognized names: ",
            paste(setdiff(names(tips), boxes), collapse = ", "), "."
        )
    }

    tips <- modifyList(map(boxes_list, function(x) default_tip), tips)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    values <-
        if (is.null(values)) {
            rep(default_value, length(boxes))

        } else if (length(values) == length(boxes)) {
            values

        } else {
            stop("The length of `values` must be ", length(boxes), ", not ",
                 length(values), ".")
        }

    vars <- map(values, tclVar)
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
        tkgrid(bs_label_b(frame, text = title), sticky = title_sticky)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    objs <- pmap(
        list(vars, labels, commands, tips),
        ~ tk2checkbutton(frame,
                         variable = ..1,
                         text     = ..2,
                         command  = ..3,
                         tip      = ..4))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    switch(layout,
        vertical = {
            walk(objs, tkgrid, sticky = buttons_sticky)
        },

        horizontal = {
            buttons_str <- paste0("objs[[", seq_along(objs), "]]",
                                  collapse = ", ")
            eval_glue('tkgrid({buttons_str}, sticky = buttons_sticky)')
        },

        stop("Unrecognized layout: ", layout)
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    structure(list(
        frame = frame,
        var  = structure(vars, names = boxes),
        obj  = structure(vars, names = boxes)
    ),
    class = c("bs_check_boxes", "bs_tk_widget", "list"))
}
