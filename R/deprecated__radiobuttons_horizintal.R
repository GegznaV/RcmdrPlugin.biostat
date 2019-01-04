# Radio buttons ==============================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal

# This function modified based on code by Liviu Andronic (13 Dec 09) and on code by Milan Bouchet-Valat (29 Jun 12):
radioButtons_horizontal <- defmacro(
    window = top,
    name,
    buttons,
    values = NULL,
    initialValue = ..values[1],
    labels,
    title = NULL,
    title.color = NULL,
    right.buttons = FALSE,
    command = function() {},
    sticky_title = "w",
    sticky_buttons = "e",
    expr =
        {
            ..values <- if (is.null(values)) buttons else values

            ..frame <- paste0(name, "Frame")
            assign(..frame, tkframe(window))

            ..variable <- paste0(name, "Variable")
            assign(..variable, tclVar(initialValue))


            # if (title != "") {
            #     tkgrid(labelRcmdr(eval_(..frame),
            #                       text = title,
            #                       foreground = title.color,
            #                       font = "RcmdrTitleFont"),
            #            columnspan = 2,
            #            sticky = "w")
            # }

            if (!is.null(title)) {
                title_label <- bs_label(eval_(..frame), text = title, fg = title.color)
                tkgrid(title_label, sticky = sticky_title)
            }

            buttons_pan_Frame <- tkframe(eval_(..frame))

            ..current_buttons <- paste0(buttons, "Button")
            for (i in 1:length(buttons)) {
                # ..button <- paste0(buttons[i], "Button")
                ..button <- ..current_buttons[i]
                assign(..button,
                       ttkradiobutton(
                           buttons_pan_Frame,
                           # eval_(..frame),
                           variable = eval_(..variable),
                           value = ..values[i],
                           text = labels[i],
                           command = command
                       ))
            }
            ..buttons_str <- paste0(..current_buttons, collapse = ", ")

            eval_glue('tkgrid({..buttons_str})')
            tkgrid(buttons_pan_Frame, sticky = sticky_buttons)


            # tkgrid(eval_(..button), sticky = "w")
            # logger(paste(names(as.list(environment())), collapse = ", "))
            #
            # for (i in 1:length(buttons)) {
            #     ..button <- paste0(buttons[i], "Button")
            #
            #     if (right.buttons) {
            #         assign(..button,
            #                ttkradiobutton(eval_(..frame),
            #                               variable = eval_(..variable),
            #                               value = ..values[i],
            #                               command = command))
            #
            #         tkgrid(labelRcmdr(eval_(..frame),
            #                           text = labels[i],
            #                           justify = "left"),
            #                eval_(..button), sticky = "w")
            #
            #     } else {
            #         assign(..button,
            #                ttkradiobutton(eval_(..frame),
            #                               variable = eval_(..variable),
            #                               value = ..values[i],
            #                               text = labels[i],
            #                               command = command))
            #
            #         tkgrid(eval_(..button), sticky = "w")
            #     }
            # }
        }
)
