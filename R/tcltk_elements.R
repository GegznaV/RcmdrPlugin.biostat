# #' @name Helper-functions
# #' @title Helper functions for RcmdrPlugin.biostat.
# #' @description Helper functions for package \pkg{RcmdrPlugin.biostat}.
# #' @keywords internal
# NULL

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
# Label for R Commander
# see also: labelRcmdr
label_rcmdr <- function(..., fg = NULL) {
  if (is.null(fg)) ttklabel(...) else ttklabel(..., foreground = fg)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
labeled_frame <- function(parent, label = NULL, ...) {
  ttklabelframe(parent = parent,
                labelwidget = tklabel(
                  parent,
                  text = label,
                  font = "RcmdrTitleFont",
                  foreground = Rcmdr::getRcmdr("title.color"),
                  ...)
  )
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_title <- function(parent = top, text = "xxx_title", pady = c(5, 9),
                     font = tkfont.create(weight = "bold", size = 9), ...) {
  tkgrid(
    label_rcmdr(
      parent,
      text = gettext_bs(text),
      font = font,
      fg = Rcmdr::getRcmdr("title.color")),
    pady = pady, ...)
}

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
        title_label <- label_rcmdr(eval_(..frame), text = title, fg = title.color)
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal

radiobuttons_env <- function(
  window        = top,
  name          = stop("name not supplied"),
  buttons       = stop("buttons not supplied"),
  values        = NULL,
  initialValue  = ..values[1],
  labels        = stop("labels not supplied"),
  title         = "",
  title.color   = getRcmdr("title.color"),
  right.buttons = FALSE,
  command       = function() {},
  env           = parent.frame())
{

  tmp <- substitute({
    on.exit(remove(list = objects(pattern = "^\\.\\.", all.names = TRUE)))
    ..values   <- if (is.null(values)) buttons else values
    ..frame    <- paste(name, "Frame", sep = "")
    assign(..frame, tkframe(window))
    ..variable <- paste(name, "Variable", sep = "")
    assign(..variable, tclVar(initialValue))
    if (title != "") {
      tkgrid(
        labelRcmdr(
          eval(parse(text = ..frame)),
          text = title,
          foreground = title.color,
          font = "RcmdrTitleFont"
        ),
        columnspan = 2,
        sticky = "w"
      )
    }
    for (i in 1:length(buttons)) {
      ..button <- paste(buttons[i], "Button", sep = "")
      if (right.buttons) {
        assign(
          ..button,
          ttkradiobutton(
            eval(parse(text = ..frame)),
            variable = eval(parse(text = ..variable)),
            value = ..values[i],
            command = command
          )
        )
        tkgrid(labelRcmdr(
          eval(parse(text = ..frame)),
          text = labels[i],
          justify = "left"
        ),
        eval(parse(text = ..button)),
        sticky = "w")
      } else {
        assign(
          ..button,
          ttkradiobutton(
            eval(parse(text = ..frame)),
            variable = eval(parse(text = ..variable)),
            value = ..values[i],
            text = labels[i],
            command = command
          )
        )
        tkgrid(eval(parse(text = ..button)), sticky = "w")
      }
    }
  })
  eval(tmp, env)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
