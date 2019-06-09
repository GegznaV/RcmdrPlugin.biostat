ttkentry <- function(parent, ...) {

    widget <- tcltk::ttkentry(parent, ...)
    wid <- widget$ID

    onCopy <- function() {
        if ("0" == tclvalue(.Tcl(paste(wid, "selection present"))))
            return()
        sel.1 <- tclvalue(.Tcl(paste(wid, "index sel.first")))
        sel.2 <- tclvalue(.Tcl(paste(wid, "index sel.last")))
        text <- tclvalue(tkget(widget))
        text <- substr(text, as.numeric(sel.1) + 1, as.numeric(sel.2) + 1)
        tkclipboard.clear()
        tkclipboard.append(text)
    }

    onDelete <- function() {
        if ("0" == tclvalue(.Tcl(paste(wid, "selection present"))))
            return()
        sel.1 <- tclvalue(.Tcl(paste(wid, "index sel.first")))
        sel.2 <- tclvalue(.Tcl(paste(wid, "index sel.last")))
        .Tcl(paste(wid, "delete", sel.1, sel.2))
    }

    onCut <- function() {
        onCopy()
        onDelete()
    }

    onPaste <- function() {
        onDelete()

        text <- tclvalue(.Tcl("selection get -selection CLIPBOARD"))
        if (length(text) == 0) return()
        .Tcl(paste(wid, "insert", "insert", text))
    }

    onSelectAll <- function() {
        .Tcl(paste(wid, "selection range 0 end"))
        tkfocus(widget)
    }

    right_click_menu <- function() {
        menu_p <- tkmenu(tkmenu(widget), tearoff = FALSE)
        tkadd(
            menu_p,
            "command",
            image = "::image::bs_cut",
            compound = "left",
            label = gettext_bs("Cut"),
            command = onCut
        )
        tkadd(
            menu_p,
            "command",
            image = "::image::bs_copy",
            compound = "left",
            label = gettext_bs("Copy"),
            command = onCopy
        )
        tkadd(
            menu_p,
            "command",
            image = "::image::bs_paste",
            compound = "left",
            label = gettext_bs("Paste"),
            command = onPaste
        )
        tkadd(
            menu_p,
            "command",
            image = "::image::bs_delete",
            compound = "left",
            label = gettext_bs("Delete"),
            command = onDelete
        )
        tkadd(
            menu_p,
            "command",
            image = "::image::bs_select_all",
            compound = "left",
            label = gettext_bs("Select all"),
            command = onSelectAll
        )

        tkpopup(
            menu_p,
            tkwinfo("pointerx", widget),
            tkwinfo("pointery", widget))
    }

    tkbind(widget, "<ButtonPress-3>", right_click_menu)

    tkbind(widget, "<Control-ButtonPress-1>", right_click_menu)

    if (MacOSXP()) {
        tkbind(widget, "<Meta-ButtonPress-1>", right_click_menu)
    }
    widget
}