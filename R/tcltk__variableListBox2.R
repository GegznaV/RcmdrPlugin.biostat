#' @rdname Helper-functions
#' @export
#' @keywords internal
# Variable list box with constant length (numer of rows)
# onClick_fun - function on mouse click
# onRelease_fun - function on mouse release
variableListBox2 <- function(parentWindow,
                             variableList = Variables(),
                             bg = "white",
                             selectmode = "single",
                             export = "FALSE",
                             initialSelection = NULL,
                             listHeight = getRcmdr("variable.list.height"),
                             listWidth  = getRcmdr("variable.list.width"),
                             onClick_fun       = function(){},
                             onDoubleClick_fun = function(){},
                             onTripleClick_fun = function(){},
                             onRelease_fun     = function(){},
                             onClick3_fun       = function(){},
                             onDoubleClick3_fun = function(){},
                             onTripleClick3_fun = function(){},
                             onRelease3_fun     = function(){},
                             title) {

    if (selectmode == "multiple")
        selectmode <- getRcmdr("multiple.select.mode")

    if (length(variableList) == 1 && is.null(initialSelection))
        initialSelection <- 0

    frame   <- tkframe(parentWindow)
    # minmax  <- getRcmdr("variable.list.width")
    minmax  <- listWidth
    listbox <- tklistbox(
        frame,
        height     = listHeight,
        selectmode = selectmode,
        background = bg,
        exportselection = export,
        width = min(max(minmax[1], 2 + nchar(variableList)), minmax[2])
    )

    scrollbar <-
        ttkscrollbar(
            frame,
            command = function(...) tkyview(listbox, ...))

    tkconfigure(listbox,
                yscrollcommand = function(...) tkset(scrollbar,  ...))

    # for (var in variableList)  tkinsert(listbox, "end", var)
    listbox_set_values(listbox, variableList)

    if (is.numeric(initialSelection))
        for (sel in initialSelection)
            tkselection.set(listbox, sel)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    firstChar <- tolower(substr(variableList, 1, 1))
    len <- length(variableList)
    onLetter <- function(letter) {
        letter <- tolower(letter)
        current <-
            1 + round(as.numeric(
                unlist(strsplit(tclvalue(tkyview(listbox)), " "))[1]) * len)

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
        tkbind(listbox,
               str_glue("<{letter}>"),
               get(str_glue("on{toupper(letter)}")))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onClick <- function() {
        tkfocus(listbox)
        onClick_fun()
    }
    tkbind(listbox, "<ButtonPress-1>",   onClick)

    onDoubleClick <- function() {
        tkfocus(listbox)
        onDoubleClick_fun()
    }
    tkbind(listbox, "<Double-Button-1>", onDoubleClick)

    onTripleClick <- function() {
        tkfocus(listbox)
        onTripleClick_fun()
    }
    tkbind(listbox, "<Triple-Button-1>", onTripleClick)


    onRelease <- function() {
        tkfocus(listbox)
        onRelease_fun()
    }
    tkbind(listbox, "<ButtonRelease-1>", onRelease)


    onClick3 <- function() {
        tkfocus(listbox)
        onClick3_fun()
    }
    tkbind(listbox, "<ButtonPress-3>", onClick3)

    onDoubleClick3 <- function() {
        tkfocus(listbox)
        onDoubleClick3_fun()
    }
    tkbind(listbox, "<Double-Button-3>", onDoubleClick3)

    onTripleClick3 <- function() {
        tkfocus(listbox)
        onTripleClick3_fun()
    }
    tkbind(listbox, "<Triple-Button-3>", onTripleClick3)

    onRelease3 <- function() {
        tkfocus(listbox)
        onRelease3_fun()
    }
    tkbind(listbox, "<ButtonRelease-3>", onRelease3)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    toggleSelection <- function() {
        active   <- tclvalue(tkindex(listbox, "active"))
        selected <- tclvalue(tkcurselection(listbox))
        if (selected == active) {
            tkselection.clear(listbox, "active")
        } else {
            tkselection.set(listbox, "active")
        }
    }

    if (selectmode == "single")
        tkbind(listbox, "<Control-ButtonPress-1>", toggleSelection)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkgrid(labelRcmdr(frame, text = title, fg = getRcmdr("title.color"),
                      font = "RcmdrTitleFont"),
           columnspan = 2, sticky = "w")

    tkgrid(listbox, scrollbar,  sticky = "nw")
    tkgrid.configure(scrollbar, sticky = "wns")
    tkgrid.configure(listbox,   sticky = "ewns")


    structure(
        list(frame      = frame,
             listbox    = listbox,
             scrollbar  = scrollbar,
             selectmode = selectmode,
             varlist    = variableList),
        class = "listbox"
    )
}