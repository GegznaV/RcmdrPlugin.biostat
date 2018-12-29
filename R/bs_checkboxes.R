#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
# Checkboxes with command functions
# commands - a named list of commands (functions) for checkbox.
#            The names are the same as in "boxes"
bs_checkboxes <- function(
    parent = top,
    boxes,
    initialValues = NULL,
    labels = boxes,
    title  = NULL,
    commands = list(),
    border = FALSE,
    tip = ""
)
{
    checkmate::assert_flag(border)

    # Manage `commands` ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(commands) > 0) {
        if (!all(names(commands) %in% boxes)) {
            stop("`commands` must be a named list with field names: \n",
                 paste(boxes, collapse = ", "),
                 ".\nCurrent names: \n",
                 paste(names(commands), collapse = ", "))
        }
    }

    boxes_list <- structure(boxes, names = boxes)

    new_cmd_list <- map(boxes_list, function(x) function() {})
    ..commands <- modifyList(new_cmd_list, commands)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    initial_values <-
        if (is.null(initialValues)) {
            rep("1", length(boxes))

        } else {
            initialValues
        }

    vars <- map(initial_values, tclVar)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    frame <-
        if (border) {
            tk2labelframe(
                parent,
                labelwidget = bs_label_b(parent, text = title))

        } else {
            tk2frame(parent)
        }

    if (!is.null(title) && !border) {
        tkgrid(bs_label_b(frame, text = title), sticky = "w")
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    objs <- pmap(
        list(vars, labels, ..commands),
        ~ tk2checkbutton(frame,
                         variable = ..1,
                         text     = ..2,
                         command  = ..3,
                         tip      = tip))

    walk(objs, tkgrid, sticky = "w")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    structure(list(
        frame = frame,

        var  = structure(vars, names = boxes),
        obj  = structure(vars, names = boxes)
    ),
    class = c("bs_check_boxes", "bs_tk_widget", "list"))
}
