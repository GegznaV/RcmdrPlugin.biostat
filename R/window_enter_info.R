library(tcltk2)


window_enter_info <-
    function(
        # parent,
             title = "",
             text_1a = "",
             text_1b = "",
             text_1c = "",
             text_2a = "",
             entry_2b = "",
             text_2c = "",
             entryWidth = 30,
             text_choose = "Choose file...",
             returnValOnCancel = NULL,
             returnValOnChose = NA) {
        dlg <- tktoplevel()
        tkwm.deiconify(dlg)
        tkgrab.set(dlg)
        tkfocus(dlg)
        tkwm.title(dlg, title)
        textEntryVarTcl <- tclVar(paste(entry_2b))
        textEntryWidget <- tk2entry(dlg,
                                    width = paste(entryWidth),
                                    textvariable = textEntryVarTcl)

        tkgrid(tklabel(dlg, text = text_1a, fg = getRcmdr("title.color")),
               tklabel(dlg, text = text_1b),
               tklabel(dlg, text = text_1c),
               padx = 10,
               pady = 5)

        tkgrid(tklabel(dlg, text = text_2a, fg = getRcmdr("title.color")),
               textEntryWidget,
               tklabel(dlg, text = text_2c),
               padx = 10,
               pady = 10)

        returnVal    <- returnValOnCancel
        returnChoose <- returnValOnChose

        onCHOOSE <- function() {
            returnVal <<- returnChoose
            tkgrab.release(dlg)
            tkdestroy(dlg)
            # tkfocus(parent)
        }

        onOK <- function() {
            returnVal <<- tclvalue(textEntryVarTcl)
            tkgrab.release(dlg)
            tkdestroy(dlg)
            # tkfocus(parent)
        }

        onCancel <- function() {
            returnVal <<- returnValOnCancel
            tkgrab.release(dlg)
            tkdestroy(dlg)
            # tkfocus(parent)
        }

        butCHOOSE <- tk2button(dlg,
                               text = text_choose,
                               width = -6,
                               command = onCHOOSE)

        butOK <- tk2button(dlg,
                           text = "OK",
                           width = -6,
                           command = onOK)

        butCancel <- tk2button(dlg,
                               text = "Cancel",
                               width = -6,
                               command = onCancel)

        tkgrid(butCancel, butCHOOSE, butOK,
               padx = 10, pady = c(0, 15))

        tkfocus(dlg)
        tkbind(dlg, "<Destroy>", function() {
            tkgrab.release(dlg)
            # tkfocus(parent)
        })
        tkbind(textEntryWidget, "<Return>", onOK)
        tkwait.window(dlg)

        returnVal
    }
