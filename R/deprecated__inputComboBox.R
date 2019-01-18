# soft DEPRECATED

inputComboBox <- function(parentWindow,
                          variableList       = Variables(),
                          export             = "FALSE",
                          state              = "readonly",
                          # default_text     = "<no variable selected>",
                          # initialSelection = gettext_bs(default_text),
                          initialSelection   = NULL,
                          title              = NULL,
                          title_sticky       = "w",
                          combobox_sticky    = "nw",
                          onSelect_fun       = function(){},
                          onClick_fun        = function(){},
                          onDoubleClick      = function(){},
                          onRelease_fun      = function(){},
                          width              = 20) {

# variableList <- c(gettext_bs(default_text), variableList)
frame <- tkframe(parentWindow)
combovar <- tclVar()
tclvalue(combovar) <- initialSelection
combobox <- ttkcombobox(
        parent = frame,
        values       = variableList,
        textvariable = combovar,
        state        = state,
        export       = export,
        width        = width)

firstChar <- tolower(substr(variableList, 1, 1))

onLetter <- function(letter) {
        letter  <- tolower(letter)
        current <- as.numeric(tcl(combobox, "current"))
        current <- if (current == -1) 1 else current + 1
        mat     <- match(letter, firstChar[-(1:current)])
        if (is.na(mat)) return()
        tcl(combobox, "current", current + mat - 1)
}

# eval_glue('on{LETTERS} <- function() onLetter{"letters"}')
# eval_glue('tkbind(combobox, "<{letters}>", get("on{LETTERS}"))')
# eval_glue('tkbind(combobox, "<{LETTERS}>", get("on{LETTERS}"))')

eval_glue('tkbind(combobox, "<{letters}>", function() onLetter("{letters}"))')
eval_glue('tkbind(combobox, "<{LETTERS}>", function() onLetter("{letters}"))')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
onClick <- function() {
        tkfocus(combobox)
        onClick_fun()
}

onDoubleClick <- function() {
        tkfocus(combobox)
        onDoubleClick_fun()
}

onRelease <- function() {
        tkfocus(combobox)
        onRelease_fun()
}

onSelect <- function() {
        tkfocus(combobox)
        onSelect_fun()
}

tkbind(combobox, "<ButtonPress-1>",      onClick)
tkbind(combobox, "<Double-Button-1>",    onDoubleClick)
tkbind(combobox, "<ButtonRelease-1>",    onRelease)

tkbind(combobox, "<<ComboboxSelected>>", onSelect) # on change of selected value
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!is.null(title)) {
        tkgrid(labelRcmdr(frame,
                          text = title,
                          fg   = getRcmdr("title.color"),
                          font = "RcmdrTitleFont"),
               sticky = title_sticky)
}

tkgrid(combobox, sticky = combobox_sticky)

result <- list(frame    = frame,
               combobox = combobox,
               varlist  = variableList,
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
