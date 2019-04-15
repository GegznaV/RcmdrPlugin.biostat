# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Based on funcrion from Rcmdr v 2.5-1
commander_position <- function() {
    ID <- CommanderWindow()$ID
    as.numeric(c(
        tclvalue(.Tcl(paste("winfo rootx", ID))),
        tclvalue(.Tcl(paste("winfo rooty", ID)))
    ))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Based on funcrion from Rcmdr v 2.5-1

initialize_dialog <- defmacro(
  window = top,
  title = "",
  offset = 10,
  preventCrisp,
  use.tabs = FALSE,
  notebook = notebook,
  tabs = c("dataTab", "optionsTab"),
  suppress.window.resize.buttons = TRUE,

  expr = {
    if (getRcmdr("crisp.dialogs")) tclServiceMode(on = FALSE)

    window <- tktoplevel(borderwidth = 10)

    if (use.tabs) {
      notebook <- ttknotebook(window)
      for (tab in tabs) assign(tab, tkframe(window))
    }

    tkwm.title(window, title)

    location <- getRcmdr("open.dialog.here")

    position <- if (!is.null(location)) {
      location

    } else {
      pos <- offset + commander_position()
      if (any(pos < 0)) {
        "-50+50"

      } else {
        paste0("+", paste(pos, collapse = "+"))
      }
    }

    tkwm.geometry(window, position)

    if (suppress.window.resize.buttons) tkwm.transient(window, CommanderWindow())
  }
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Based on funcrion from Rcmdr v 2.5-1
close_dialog <- defmacro(
  window = top,
  release = TRUE,
  expr = {
    if (release && GrabFocus()) tkgrab.release(window)
    tkdestroy(window)
  }
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Based on funcrion from Rcmdr v 2.5-1
dialog_suffix <- defmacro(
  window             = top,
  onOK               = onOK,
  onCancel           = onCancel,
  rows,
  columns,
  focus              = top,
  bindReturn         = TRUE,
  preventGrabFocus   = FALSE,
  preventDoubleClick = FALSE,
  preventCrisp,
  use.tabs           = FALSE,
  notebook           = notebook,
  tabs               = c("dataTab", "optionsTab"),
  tab.names          = c("Data", "Options"),
  grid.buttons       = FALSE,
  resizable          = FALSE,
  force.wait         = FALSE,

  expr = {

    if (use.tabs) {
      for (i in 1:length(tabs)) {
        tkadd(notebook, get(tabs[i]),
              text = gettextRcmdr(tab.names[i]),
              padding = 6,
              sticky = "nsew")
      }

      tkgrid(notebook, sticky = "nsew")
    }

    if (grid.buttons) tkgrid(buttonsFrame, sticky = "ew")

    if (use.tabs &&
        exists("dialog.values") &&
        !is.null(dialog.values$initial.tab) &&
        getRcmdr("restoreTab")) {

      tkselect(notebook, dialog.values$initial.tab)
    }

    .Tcl("update idletasks")

    tkwm.resizable(window, as.numeric(resizable), as.numeric(resizable))

    if (bindReturn)
        tkbind(window, "<Return>", onOK)

    tkbind(window, "<Escape>", onCancel)

    if (getRcmdr("double.click") && (!preventDoubleClick))
        tkbind(window, "<Double-ButtonPress-1>", onOK)

    tkwm.deiconify(window)

    # focus grabs appear to cause problems for some dialogs
    if (GrabFocus() && (!preventGrabFocus)) tkgrab.set(window)

    tkfocus(focus)

    if (getRcmdr("tkwait.dialog") || force.wait) tkwait.window(window)
    if (getRcmdr("crisp.dialogs")) tclServiceMode(on = TRUE)
  }
)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~