# right-click context menus
right_click_menu_text <- function(tcl_widget, undo = TRUE, menu_rm = FALSE) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(menu_rm)) {
    disabled_menu <- function() {
      context_menu <- tk2menu(tk2menu(tcl_widget), tearoff = FALSE)
      tkadd(context_menu, "command", label = gettext_bs("Menu is disabled"))
      tkpopup(context_menu,
        tkwinfo("pointerx", tcl_widget),
        tkwinfo("pointery", tcl_widget))
    }
    tkbind(tcl_widget, "<ButtonPress-3>", disabled_menu)
    return()
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onCopy <- function() {
    # focused <- tkfocus()
    selection <- strsplit(tclvalue(tktag.ranges(tcl_widget, "sel")), " ")[[1]]
    if (is.na(selection[1])) return()
    text <- tcltk::tclvalue(tkget(tcl_widget, selection[1], selection[2]))
    tkclipboard.clear()
    tkclipboard.append(text)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onDelete <- function() {
    # focused <- tkfocus()
    selection <- strsplit(tclvalue(tktag.ranges(tcl_widget, "sel")), " ")[[1]]
    if (is.na(selection[1])) return()
    tkdelete(tcl_widget, selection[1], selection[2])
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onCut <- function() {
    onCopy()
    onDelete()
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onPaste <- function(){
    onDelete()
    # focused <- tkfocus()

    # text <- tcltk::tclvalue(.Tcl("selection get -selection CLIPBOARD"))
    # if (length(text) == 0) return()

    text <- read_clipboard()
    if (nchar(text) == 0) return()

    tkinsert(tcl_widget, "insert", text)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onFind <- function() {
    # focused <- tkfocus()
    focused <- tcl_widget

    initializeDialog(title = gettext_bs("Find"))
    textFrame <- tkframe(top)
    textVar <- tclVar(getRcmdr("last.search"))
    textEntry <- ttkentry(textFrame, width = "20", textvariable = textVar)
    checkBoxes(frame = "optionsFrame",
      boxes = c("regexpr", "case"),
      initialValues = c("0", "1"),
      labels = gettext_bs(c("Regular-expression search", "Case sensitive"))
    )
    radioButtons(name    = "direction",
      buttons = c("foward", "backward"),
      labels  = gettext_bs(c("Forward", "Backward")),
      values  = c("-forward", "-backward"),
      title   = gettext_bs("Search Direction"))

    onOK <- function() {
      text <- tclvalue(textVar)
      putRcmdr("last.search", text)
      if (text == "") {
        errorCondition(
          recall = onFind,
          message = gettext_bs("No search text specified."))
        return()
      }
      type <- if (tclvalue(regexprVariable) == 1) "-regexp" else "-exact"
      case <- tclvalue(caseVariable) == 1
      direction <- tclvalue(directionVariable)
      stop <- if (direction == "-forward") "end" else "1.0"
      where.txt <-
        if (case) tksearch(focused, type, direction, "--", text, "insert", stop)
      else tksearch(focused, type, direction, "-nocase", "--", text, "insert", stop)
      where.txt <- tclvalue(where.txt)
      if (where.txt == "") {
        Message(message = gettext_bs("Text not found."),
          type = "note")
        if (GrabFocus()) tkgrab.release(top)
        tkdestroy(top)
        tkfocus(CommanderWindow())
        return()
      }
      if (GrabFocus()) tkgrab.release(top)
      tkfocus(focused)
      tkmark.set(focused, "insert", where.txt)
      tksee(focused, where.txt)
      tkdestroy(top)
    }
    .exit <- function(){
      text <- tclvalue(textVar)
      putRcmdr("last.search", text)
      return("")
    }
    OKCancelHelp()
    tkgrid(labelRcmdr(textFrame, text = gettext_bs("Search for:")),
      textEntry, sticky = "w")
    tkgrid(textFrame, sticky = "w")
    tkgrid(optionsFrame, sticky = "w")
    tkgrid(directionFrame, sticky = "w")
    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix(focus = textEntry)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onSelectAll <- function() {
    # focused <- tkfocus()
    tktag.add(tcl_widget, "sel", "1.0", "end")
    tkfocus(tcl_widget)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onSelectRow <- function() {
    # focused <- tkfocus()
    tktag.add(tcl_widget, "sel", "current linestart", "current lineend + 1 chars")
    tkfocus(tcl_widget)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onDeleteRow <- function() {
    onSelectRow()
    onDelete()
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onClear <- function() {
    onSelectAll()
    onDelete()
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onUndo <- function(){
    # focused <- tkfocus()
    tcl(tcl_widget, "edit", "undo")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onRedo <- function(){
    # focused <- tkfocus()
    tcl(tcl_widget, "edit", "redo")
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ========================================================================
  crete_context_menu <- function() {

    # if (tclvalue(tkfocus()) != tcl_widget$ID) return()

    contextMenu <- tk2menu(tk2menu(tcl_widget), tearoff = FALSE)

    # tkadd(contextMenu, "command", label = gettext_bs("Submit"), command = onSubmit)

    # tkadd(contextMenu, "separator")

    # tkadd(contextMenu, "command", label = gettext_bs("Move row up"),     command = onUp)
    # tkadd(contextMenu, "command", label = gettext_bs("Move row down"),   command = onDown)
    # tkadd(contextMenu, "command", label = gettext_bs("Move row to top"), command = onTop)
    #
    # tkadd(contextMenu, "separator")

    tkadd(contextMenu, "command",
      label = gettext_bs("Cut"),
      image = "::image::bs_cut",
      compound = "left",
      command = onCut)

    tkadd(contextMenu, "command",
      label = gettext_bs("Copy"),
      image = "::image::bs_copy",
      compound = "left",
      command = onCopy)

    tkadd(contextMenu, "command",
      label = gettext_bs("Paste"),
      image = "::image::bs_paste",
      compound = "left",
      command = onPaste)

    tkadd(contextMenu, "command",
      label = gettext_bs("Delete"),
      image = "::image::bs_delete",
      compound = "left",
      command = onDelete)

    tkadd(contextMenu, "command",
      label = gettext_bs("Clear all"),
      image = "::image::bs_delete",
      compound = "left",
      command = onClear)

    tkadd(contextMenu, "command", label = gettext_bs("Select all"), command = onSelectAll)

    # tkadd(contextMenu, "separator")
    # tkadd(contextMenu, "command", label = gettext_bs("Find..."),    command = onFind)
    # tkadd(contextMenu, "command", label = gettext_bs("Select all"), command = onSelectAll)
    # tkadd(contextMenu, "command", label = gettext_bs("Select row"), command = onSelectRow)
    # tkadd(contextMenu, "command", label = gettext_bs("Delete row"), command = onDeleteRow)

    if (undo == TRUE) {
      tkadd(contextMenu, "separator")

      tkadd(contextMenu, "command",
        label = gettext_bs("Undo"),
        image = "::image::bs_undo",
        compound = "left",
        command = onUndo)

      tkadd(contextMenu, "command",
        label = gettext_bs("Redo"),
        image = "::image::bs_redo",
        compound = "left",
        command = onRedo)
    }

    # tkadd(contextMenu, "separator")
    # tkadd(contextMenu, "command", label = gettext_bs("Reset directives"), command = variable_doubleclick)

    tkpopup(contextMenu,
      tkwinfo("pointerx", tcl_widget),
      tkwinfo("pointery", tcl_widget))
  }

  tkbind(tcl_widget, "<ButtonPress-3>", crete_context_menu)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# context_menu_for_code <- function() {
#
#     top <- CommanderWindow()
#
#     menu_i <- tk2menu(tk2menu(top), tearoff = FALSE)
#
#     # tkadd(menu_i, "command",
#     #       label    = "Copy",
#     #       compound = "left",
#     #       image    = "::image::bs_delete",
#     #       command  = do_nothing)
#
#     tkadd(menu_i, "command",
#           label    = "Clear",
#           compound = "left",
#           image    = "::image::bs_delete",
#           command  = function() {
#               set_values(f4_code_input, "")
#           })
#
#     tkadd(menu_i, "command",
#           label    = "Clear and paste",
#           compound = "left",
#           image    = "::image::bs_paste",
#           command  = function() {
#               set_values(f4_code_input, read_clipboard())
#           })
#
#     tkpopup(menu_i,
#             tkwinfo("pointerx", top),
#             tkwinfo("pointery", top))
# }
#
# tkbind(f4_code_input$text, "<ButtonPress-3>", context_menu_for_code)
