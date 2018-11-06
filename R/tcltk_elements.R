# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Label for R Commander
# see also: labelRcmdr
label_rcmdr <- function(..., fg = NULL) {
    if (is.null(fg)) ttklabel(...) else ttklabel(..., foreground = fg)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Variable list box with constant length (numer of rows)
# onClick_fun - function on mouse click
# onRelease_fun - function on mouse release
variableListBox2 <-
    function(parentWindow,
             variableList = Variables(),
             bg = "white",
             selectmode = "single",
             export = "FALSE",
             initialSelection = NULL,
             listHeight = getRcmdr("variable.list.height"),
             onClick_fun = function(){},
             onDoubleClick_fun = function(){},
             onRelease_fun = function(){},
             title)
    {

    if (selectmode == "multiple")
        selectmode <- getRcmdr("multiple.select.mode")
    if (length(variableList) == 1 && is.null(initialSelection))
        initialSelection <- 0
    frame <- tkframe(parentWindow)
    minmax <- getRcmdr("variable.list.width")
    listbox <- tklistbox(frame,
                         height = listHeight,
                         selectmode = selectmode,
                         background = bg,
                         exportselection = export,
                         width = min(max(minmax[1], 2 + nchar(variableList)), minmax[2]))

    scrollbar <- ttkscrollbar(frame,
                              command = function(...) tkyview(listbox, ...))
    tkconfigure(listbox,
                yscrollcommand = function(...)
                    tkset(scrollbar,  ...)
    )

    for (var in variableList)
        tkinsert(listbox, "end", var)

    if (is.numeric(initialSelection))
        for (sel in initialSelection)
            tkselection.set(listbox, sel)

    firstChar <- tolower(substr(variableList, 1, 1))
    len <- length(variableList)
    onLetter <- function(letter) {
        letter <- tolower(letter)
        current <- 1 +
            round(as.numeric(unlist(strsplit(tclvalue(tkyview(listbox)), " "))[1]) * len)
        mat <- match(letter, firstChar[-(1:current)])
        if (is.na(mat))
            return()
        tkyview.scroll(listbox, mat, "units")
    }
    onA <- function() onLetter("a")
    onB <- function() onLetter("b")
    onC <- function() onLetter("c")
    onD <- function() onLetter("d")
    onE <- function() onLetter("e")
    onF <- function() onLetter("f")
    onG <- function() onLetter("g")
    onH <- function() onLetter("h")
    onI <- function() onLetter("i")
    onJ <- function() onLetter("j")
    onK <- function() onLetter("k")
    onL <- function() onLetter("l")
    onM <- function() onLetter("m")
    onN <- function() onLetter("n")
    onO <- function() onLetter("o")
    onP <- function() onLetter("p")
    onQ <- function() onLetter("q")
    onR <- function() onLetter("r")
    onS <- function() onLetter("s")
    onT <- function() onLetter("t")
    onU <- function() onLetter("u")
    onV <- function() onLetter("v")
    onW <- function() onLetter("w")
    onX <- function() onLetter("x")
    onY <- function() onLetter("y")
    onZ <- function() onLetter("z")
    for (letter in c(letters, LETTERS)) {
        tkbind(listbox, paste("<", letter, ">", sep = ""),
               get(paste("on", toupper(letter), sep = "")))
    }
    onClick <- function() {
        tkfocus(listbox)
        onClick_fun()
    }
    tkbind(listbox, "<ButtonPress-1>",   onClick)

    onDoubleClick <- function() {
        onDoubleClick_fun()
    }
    tkbind(listbox, "<Double-Button-1>", onDoubleClick)

    onRelease <- function() {
        onRelease_fun()
    }
    tkbind(listbox, "<ButtonRelease-1>", onRelease)


    toggleSelection <- function() {
        active <- tclvalue(tkindex(listbox, "active"))
        selected <- tclvalue(tkcurselection(listbox))
        if (selected == active)
            tkselection.clear(listbox, "active")
        else tkselection.set(listbox, "active")
    }

    if (selectmode == "single")
        tkbind(listbox, "<Control-ButtonPress-1>", toggleSelection)
    tkgrid(labelRcmdr(frame, text = title, fg = getRcmdr("title.color"),
                      font = "RcmdrTitleFont"), columnspan = 2, sticky = "w")
    tkgrid(listbox, scrollbar, sticky = "nw")
    tkgrid.configure(scrollbar, sticky = "wns")
    tkgrid.configure(listbox, sticky = "ewns")
    result <- list(frame = frame, listbox = listbox, scrollbar = scrollbar,
                   selectmode = selectmode, varlist = variableList)
    class(result) <- "listbox"
    result
    }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Checkboxes with command functions
# commands - a named list of commands (functions) for checkbox.
#            The names are the same as in "boxes"
bs_check_boxes <- defmacro(
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
                   ttklabelframe(window,
                                 labelwidget = tklabel(window,
                                                       text = title,
                                                       font = "RcmdrTitleFont",
                                                       foreground = getRcmdr("title.color")))
                   } else {
                       tkframe(window)
                   }
        )
        if (!is.null(title) && !ttk) {
            tkgrid(labelRcmdr(eval(parse(text = frame)),
                              text = title,
                              fg = getRcmdr("title.color"),
                              font = "RcmdrTitleFont"),
                   sticky = "w")
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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


