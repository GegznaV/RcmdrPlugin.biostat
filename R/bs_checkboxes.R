#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
# Checkboxes with command functions
# commands - a named list of commands (functions) for checkbox.
#            The names are the same as in "boxes"
bs_checkboxes <- defmacro(
    window = top,
    frame,
    boxes,
    initialValues = NULL,
    labels,
    title = NULL,
    ttk = FALSE,
    commands = list(),
    expr = {

        # Manage `commands` ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (length(commands) > 0) {
            if (!all(names(commands) %in% boxes)) {
                stop("`commands` must be a named list with field names: \n",
                     paste(boxes, collapse = ", "),
                     ".\nCurrent names: \n",
                     paste(names(commands), collapse = ", "))
            }
        }

        new_cmd_list <- sapply(force(boxes), function(x) function() {})
        ..commands <- modifyList(new_cmd_list, commands)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ..initialValues <- if (is.null(initialValues)) rep("1", length(boxes)) else initialValues
        assign(frame,
               if (ttk) {
                   tk2labelframe(
                       window,
                       labelwidget = tk2label(
                           window,
                           text = title,
                           font = "RcmdrTitleFont",
                           foreground = getRcmdr("title.color")))
               } else {
                   tkframe(window)
               }
        )
        if (!is.null(title) && !ttk) {
            tkgrid(bs_label_b(eval_text(frame),text = title), sticky = "w")
        }

        ..variables <- paste(boxes, "Variable", sep = "")

        for (i in 1:length(boxes)) {
            assign(..variables[i], tclVar(..initialValues[i]))
            ..checkBox <- paste0(boxes[i], "CheckBox")

            assign(..checkBox,
                   ttkcheckbutton(
                       eval(parse(text = frame)),
                       variable = eval(parse(text = ..variables[i])),
                       text = labels[i],
                       command = ..commands[[i]]))

            tkgrid(eval_(..checkBox), sticky = "w")
        }
    }
)