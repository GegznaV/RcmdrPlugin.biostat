# ok_cancel_help <-
#     function (window      = top,
#               helpSubject = NULL,
#               model       = FALSE,
#               reset       = NULL,
#               apply       = NULL,
#               helpPackage = NULL)
# {
#     tmp <- substitute({
#         on.exit(remove(list = objects(pattern = "^\\.\\.", all.names = TRUE)))
#
#         memory <- getRcmdr("retain.selections")
#
#         button.strings <-
#             c("OK",
#               "Cancel",
#               if (!is.null(helpSubject)) "Help",
#               if (!is.null(reset) && memory) "Reset",
#               if (!is.null(apply)) "Apply")
#
#         width <- max(nchar(gettextRcmdr(button.strings)))
#
#         if (WindowsP())
#             width <- width + 2
#
#         buttonsFrame <- tkframe(window)
#         leftButtonsBox <- tkframe(buttonsFrame)
#         rightButtonsBox <- tkframe(buttonsFrame)
#
#         OnOK <- function() {
#             putRcmdr("restoreTab", FALSE)
#             if (getRcmdr("use.markdown")) {
#                 putRcmdr("startNewCommandBlock", FALSE)
#                 beginRmdBlock()
#             }
#             if (getRcmdr("use.knitr")) {
#                 putRcmdr("startNewKnitrCommandBlock", FALSE)
#                 beginRnwBlock()
#             }
#             setBusyCursor()
#             on.exit(setIdleCursor())
#             onOK()
#             if (model)
#                 putDialog("effectPlots", NULL)
#             if (getRcmdr("use.markdown")) {
#                 removeNullRmdBlocks()
#                 putRcmdr("startNewCommandBlock", TRUE)
#                 if (getRcmdr("rmd.generated")) {
#                     endRmdBlock()
#                     putRcmdr("rmd.generated", FALSE)
#                 }
#                 removeNullRmdBlocks()
#             }
#             if (getRcmdr("use.knitr")) {
#                 removeNullRnwBlocks()
#                 putRcmdr("startNewKnitrCommandBlock", TRUE)
#                 if (getRcmdr("rnw.generated")) {
#                     endRnwBlock()
#                     putRcmdr("rnw.generated", FALSE)
#                 }
#                 removeNullRnwBlocks()
#             }
#             putRcmdr("rgl.command", FALSE)
#         }
#
#         OKbutton <-
#             buttonRcmdr(
#                 rightButtonsBox,
#                 text       = gettextRcmdr("OK"),
#                 foreground = "darkgreen",
#                 width      = width,
#                 command    = OnOK,
#                 default    = "active",
#                 image      = "::image::okIcon",
#                 compound   = "left"
#             )
#
#         onCancel <- function() {
#             if (exists(".exit")) {
#                 result <- .exit()
#                 if (result == "abort") return()
#             }
#             putRcmdr("restoreTab", FALSE)
#             if (model)
#                 putRcmdr("modelNumber", getRcmdr("modelNumber") - 1)
#             if (GrabFocus())
#                 tkgrab.release(window)
#             tkdestroy(window)
#             putRcmdr("rgl.command", FALSE)
#             tkfocus(CommanderWindow())
#         }
#
#         cancelButton <-
#             buttonRcmdr(
#                 rightButtonsBox,
#                 text = gettextRcmdr("Cancel"),
#                 foreground = "red",
#                 width = width,
#                 command = onCancel,
#                 image = "::image::cancelIcon",
#                 compound = "left"
#             )
#
#         if (!is.null(helpSubject)) {
#             onHelp <- function() {
#                 if (GrabFocus() && (!WindowsP()))
#                     tkgrab.release(window)
#                 if (as.numeric(R.Version()$major) >= 2)
#                     print(help(helpSubject, package = helpPackage))
#                 else
#                     help(helpSubject, package = helpPackage)
#             }
#
#             helpButton <-
#                 buttonRcmdr(
#                     leftButtonsBox,
#                     text = gettextRcmdr("Help"),
#                     width = width,
#                     command = onHelp,
#                     image = "::image::helpIcon",
#                     compound = "left"
#                 )
#         }
#
#         if (!is.null(reset) && memory) {
#
#             onReset <- function() {
#                 ID <- window$ID
#
#                 putRcmdr("cancelDialogReopen", TRUE)
#                 putRcmdr("open.dialog.here", as.character(.Tcl(paste("winfo geometry", ID))))
#
#                 if (model) putRcmdr("modelNumber", getRcmdr("modelNumber") - 1)
#
#                 putDialog(reset, NULL)
#                 putDialog(reset, NULL, resettable = FALSE)
#                 closeDialog()
#
#                 eval(parse(text = paste(reset, "()")))
#                 putRcmdr("open.dialog.here", NULL)
#                 putRcmdr("restoreTab", FALSE)
#             }
#
#             resetButton <-
#                 buttonRcmdr(
#                     leftButtonsBox,
#                     text = gettextRcmdr("Reset"),
#                     width = width,
#                     command = onReset,
#                     image = "::image::resetIcon",
#                     compound = "left"
#                 )
#         }
#
#         if (!is.null(apply)) {
#             onApply <- function() {
#                 putRcmdr("restoreTab", TRUE)
#                 putRcmdr("cancelDialogReopen", FALSE)
#
#                 ID <- window$ID
#                 putRcmdr("open.dialog.here",
#                          as.character(.Tcl(paste("winfo geometry", ID))))
#
#                 if (getRcmdr("use.markdown")) {
#                     putRcmdr("startNewCommandBlock", FALSE)
#                     beginRmdBlock()
#                 }
#
#                 if (getRcmdr("use.knitr")) {
#                     putRcmdr("startNewKnitrCommandBlock", FALSE)
#                     beginRnwBlock()
#                 }
#
#                 setBusyCursor()
#                 on.exit(setIdleCursor())
#
#                 onOK()
#
#                 putRcmdr("rgl.command", FALSE)
#
#                 if (getRcmdr("use.markdown")) {
#                     removeNullRmdBlocks()
#                     putRcmdr("startNewCommandBlock", TRUE)
#                     if (getRcmdr("rmd.generated")) {
#                         endRmdBlock()
#                         putRcmdr("rmd.generated", FALSE)
#                     }
#                     removeNullRmdBlocks()
#                 }
#
#                 if (getRcmdr("use.knitr")) {
#                     removeNullRnwBlocks()
#                     putRcmdr("startNewKnitrCommandBlock", TRUE)
#                     if (getRcmdr("rnw.generated")) {
#                         endRnwBlock()
#                         putRcmdr("rnw.generated", FALSE)
#                     }
#                     removeNullRnwBlocks()
#                 }
#
#                 if (getRcmdr("cancelDialogReopen")) {
#                     putRcmdr("cancelDialogReopen", FALSE)
#                 } else {
#
#                     # ???
#                     eval(parse(text = paste(apply, "()")))
#
#                     putRcmdr("open.dialog.here", NULL)
#                 }
#
#             }
#
#             applyButton <-
#                 buttonRcmdr(
#                     rightButtonsBox,
#                     text       = gettextRcmdr("Apply"),
#                     foreground = "yellow",
#                     width      = width,
#                     command    = onApply,
#                     image      = "::image::applyIcon",
#                     compound   = "left"
#                 )
#         }
#         if (!WindowsP()) {
#             if (!is.null(apply)) {
#                 tkgrid(applyButton, cancelButton, OKbutton, sticky = "w")
#                 tkgrid.configure(OKbutton, padx = c(6, 0))
#             } else {
#                 tkgrid(cancelButton, OKbutton, sticky = "w")
#             }
#             tkgrid.configure(cancelButton, padx = c(6, 6))
#
#         } else {
#             if (!is.null(apply)) {
#                 tkgrid(OKbutton, cancelButton, applyButton,
#                        sticky = "w")
#                 tkgrid.configure(applyButton, padx = c(6, 0))
#             } else {
#                 tkgrid(OKbutton, cancelButton, sticky = "w")
#             }
#             tkgrid.configure(OKbutton, padx = c(6, 6))
#         }
#         if (!is.null(reset) && memory) {
#             if (!is.null(helpSubject)) {
#                 tkgrid(helpButton, resetButton, pady = 6)
#             } else
#                 tkgrid(resetButton, pady = 6)
#             if (!WindowsP())
#                 tkgrid.configure(resetButton, padx = c(0, 6))
#
#         } else if (!is.null(helpSubject)) {
#             tkgrid(helpButton, pady = 6)
#         }
#
#         tkgrid(leftButtonsBox, rightButtonsBox, pady = 6, sticky = "ew")
#         if (!is.null(helpSubject))
#             tkgrid.configure(helpButton, padx = c(0, 18))
#         else if (!is.null(reset) && memory)
#             tkgrid.configure(resetButton, padx = c(0, 18))
#
#         tkgrid.columnconfigure(buttonsFrame, 0, weight = 1)
#         tkgrid.columnconfigure(buttonsFrame, 1, weight = 1)
#         tkgrid.configure(leftButtonsBox, sticky = "w")
#         tkgrid.configure(rightButtonsBox, sticky = "e")
#     })
#     eval(tmp, parent.frame())
# }
