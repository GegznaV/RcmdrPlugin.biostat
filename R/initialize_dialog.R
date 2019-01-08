initialize_dialog <- defmacro(
    window   = top,
    title    = "",
    offset   = 10,
    preventCrisp,
    use.tabs = FALSE,
    notebook = notebook,
    tabs     = c("dataTab", "optionsTab"),
    suppress.window.resize.buttons = TRUE,
    parent   = CommanderWindow(),

    expr = {
        if (getRcmdr("crisp.dialogs")) tclServiceMode(on = FALSE)
        window <- tktoplevel(parent = parent, borderwidth = 10)

        if (use.tabs) {
            notebook <- ttknotebook(window)
            for (tab in tabs) assign(tab, tkframe(window))
        }

        tkwm.title(window, title)

        location <- getRcmdr("open.dialog.here")

        position <- if (!is.null(location)) {
            location
        } else {
            pos <- offset + commanderPosition()
            if (any(pos < 0)) {"-50+50"} else {paste0("+", paste(pos, collapse = "+"))
            }
        }
        tkwm.geometry(window, position)
        if (suppress.window.resize.buttons) tkwm.transient(window, parent)
    }
)
