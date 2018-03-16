inputComboBox <- function(parentWindow,
                          variableList = Variables(),
                          export = "FALSE",
                          state = "readonly",
                          # default_text = "<no variable selected>",
                          # initialSelection = gettextRcmdr(default_text),
                          initialSelection = NULL,
                          title = NULL,
                          title_sticky = "w",
                          combobox_sticky = "nw",
                          onSelect_fun = function(){},
                          onClick_fun = function(){},
                          onRelease_fun = function(){},
                          width = 20
                          )
{
        # variableList <- c(gettextRcmdr(default_text), variableList)
        frame <- tkframe(parentWindow)
        combovar <- tclVar()
        tclvalue(combovar) <- initialSelection
        combobox <- ttkcombobox(frame,
                                values = variableList,
                                textvariable = combovar,
                                state = state,
                                export = export,
                                width = width)

        firstChar <- tolower(substr(variableList, 1, 1))

        onLetter <- function(letter) {
            letter <- tolower(letter)
            current <- as.numeric(tcl(combobox, "current"))
            current <- if (current == -1) 1 else current + 1
            mat <- match(letter, firstChar[-(1:current)])
            if (is.na(mat)) return()
            tcl(combobox, "current", current + mat - 1)
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
            tkbind(combobox, glue("<{letter}>"), get(glue("on{toupper(letter)}")))
        }


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        onClick <- function() {
            tkfocus(combobox)
            onClick_fun()
        }

        onRelease <- function() {
            tkfocus(combobox)
            onRelease_fun()
        }

        onSelect <- function() {
            tkfocus(combobox)
            onSelect_fun()
        }

        tkbind(combobox, "<ButtonPress-1>",   onClick)
        tkbind(combobox, "<ButtonRelease-1>", onRelease)
        tkbind(combobox, "<<ComboboxSelected>>", onSelect) # on change of selected value
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if (!is.null(title)) {
            tkgrid(labelRcmdr(frame,
                              text = title,
                              fg = getRcmdr("title.color"),
                              font = "RcmdrTitleFont"),
                   sticky = title_sticky)
        }

        tkgrid(combobox, sticky = combobox_sticky)

        result <- list(frame = frame,
                       combobox = combobox,
                       varlist = variableList,
                       combovar = combovar)

        class(result) <- "combobox"
        result
}


# set_selection <- function(object, val) {
#     tclvalue(object$combovar) <- val
# }
#
# getSelection.combobox <- function(object){
#     tclvalue(object$combovar)
# }