#' @rdname Helper-functions
#' @export
#' @keywords internal
ok_cancel_help <- defmacro(
    window = top,
    helpSubject = NULL,
    model = FALSE,
    reset = NULL,
    apply = NULL,
    helpPackage = NULL,
    expr = {
        memory <- getRcmdr("retain.selections")

        button.strings <- c(
            "OK",
            "Cancel",
            if (!is.null(helpSubject))     "Help",
            if (!is.null(reset) && memory) "Reset",
            if (!is.null(apply))           "Apply"
        )

        width <- max(nchar(gettextRcmdr(button.strings)))
        if (WindowsP()) width <- width + 2

        buttonsFrame    <- tkframe(window)
        leftButtonsBox  <- tkframe(buttonsFrame)
        rightButtonsBox <- tkframe(buttonsFrame)

        # Functions ============================================================
        # START: ok ------------------------------------------------------------
        OnOK <- function() {
            putRcmdr("restoreTab", FALSE)

            if (getRcmdr("use.markdown")) {
                putRcmdr("startNewCommandBlock", FALSE)
                beginRmdBlock()
            }

            if (getRcmdr("use.knitr")) {
                putRcmdr("startNewKnitrCommandBlock", FALSE)
                beginRnwBlock()
            }

            cursor_set_busy()
            on.exit(cursor_set_idle())

            onOK()

            if (model) putDialog("effectPlots", NULL)

            if (getRcmdr("use.markdown")) {
                removeNullRmdBlocks()
                putRcmdr("startNewCommandBlock", TRUE)
                if (getRcmdr("rmd.generated")) {
                    endRmdBlock()
                    putRcmdr("rmd.generated", FALSE)
                }
                removeNullRmdBlocks()
            }

            if (getRcmdr("use.knitr")) {
                removeNullRnwBlocks()
                putRcmdr("startNewKnitrCommandBlock", TRUE)
                if (getRcmdr("rnw.generated")) {
                    endRnwBlock()
                    putRcmdr("rnw.generated", FALSE)
                }
                removeNullRnwBlocks()
            }
            putRcmdr("rgl.command", FALSE)
        }

        OKbutton <- buttonRcmdr(rightButtonsBox,
                                text       = gettextRcmdr("OK"),
                                foreground = "darkgreen",
                                width      = width,
                                command    = OnOK,
                                default    = "active",
                                image      = "::image::okIcon",
                                compound   = "left"
        )
        # END: ok ------------------------------------------------------------


        # START: cancel ------------------------------------------------------
        onCancel <- function() {
            if (exists(".exit")) {
                result <- .exit()
                if (result == "abort") return()
            }
            putRcmdr("restoreTab", FALSE)
            if (model) putRcmdr("modelNumber", getRcmdr("modelNumber") - 1)
            if (GrabFocus()) tkgrab.release(window)
            tkdestroy(window)
            putRcmdr("rgl.command", FALSE)
            tkfocus(CommanderWindow())
        }

        cancelButton <- buttonRcmdr(
            rightButtonsBox,
            text         = gettextRcmdr("Cancel"),
            foreground   = "red",
            width        = width,
            command      = onCancel,
            # borderwidth=3,
            image        = "::image::cancelIcon",
            compound     = "left"
        )

        if (!is.null(helpSubject)) {
            onHelp <- function() {
                if (GrabFocus() && (!WindowsP())) tkgrab.release(window)
                if (as.numeric(R.Version()$major) >= 2) {
                    print(help(helpSubject, package = helpPackage))
                } else {
                    help(helpSubject, package = helpPackage)
                }
            }
            helpButton <- buttonRcmdr(
                leftButtonsBox,
                text          = gettextRcmdr("Help"),
                width         = width,
                command       = onHelp,
                # borderwidth = 3,
                image         = "::image::helpIcon",
                compound      = "left"
            )
        }
        # END: cancel --------------------------------------------------------

        # START: reset -------------------------------------------------------
        if (!is.null(reset) && memory) {
            onReset <- function() {
                ID <- window$ID
                putRcmdr("cancelDialogReopen", TRUE)
                putRcmdr("open.dialog.here", as.character(.Tcl(paste("winfo geometry", ID))))
                if (model) putRcmdr("modelNumber", getRcmdr("modelNumber") - 1)
                putDialog(reset, NULL)
                putDialog(reset, NULL, resettable = FALSE)
                closeDialog()
                eval(parse(text = paste(reset, "()")))
                putRcmdr("open.dialog.here", NULL)
                putRcmdr("restoreTab", FALSE)
            }
            resetButton <- buttonRcmdr(
                leftButtonsBox,
                text     = gettextRcmdr("Reset"),
                width    = width,
                command  = onReset,
                image    = "::image::resetIcon",
                compound = "left"
            )
        }
        # END: reset ---------------------------------------------------------

        # START: apply -------------------------------------------------------
        if (!is.null(apply)) {
            onApply <- function() {
                putRcmdr("restoreTab", TRUE)
                putRcmdr("cancelDialogReopen", FALSE)
                ID <- window$ID

                # [???] ------
                # Ar tai iškraipo klaidos pranešimo dėžučių dydį???
                # Kokia kitos eilutės paskirtis?

                # putRcmdr("open.dialog.here", as.character(.Tcl(paste("winfo geometry", ID))))



                wininfo <- as.character(.Tcl(paste("winfo geometry", ID)))



                if (getRcmdr("use.markdown")) {
                    putRcmdr("startNewCommandBlock", FALSE)
                    beginRmdBlock()
                }
                if (getRcmdr("use.knitr")) {
                    putRcmdr("startNewKnitrCommandBlock", FALSE)
                    beginRnwBlock()
                }

                cursor_set_busy()
                on.exit(cursor_set_idle())
                # [???] ------
                # Gal reikia kokio nors rezultato, kuris parodytų, jog funkcija
                # įvykdyta sėkmingai

                res_of_ok <- onOK() # Function should return TRUE,
                                    # if no error occurs.

                # print(res_of_ok)

                if (!isTRUE(res_of_ok)) {
                    putRcmdr("cancelDialogReopen", TRUE)
                }
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                putRcmdr("rgl.command", FALSE)
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                if (getRcmdr("use.markdown")) {
                    removeNullRmdBlocks()
                    putRcmdr("startNewCommandBlock", TRUE)
                    if (getRcmdr("rmd.generated")) {
                        endRmdBlock()
                        putRcmdr("rmd.generated", FALSE)
                    }
                    removeNullRmdBlocks()
                }
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                if (getRcmdr("use.knitr")) {
                    removeNullRnwBlocks()
                    putRcmdr("startNewKnitrCommandBlock", TRUE)
                    if (getRcmdr("rnw.generated")) {
                        endRnwBlock()
                        putRcmdr("rnw.generated", FALSE)
                    }
                    removeNullRnwBlocks()
                }
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                if (getRcmdr("cancelDialogReopen")) {
                    putRcmdr("cancelDialogReopen", FALSE)

                } else {
                    putRcmdr("open.dialog.here", wininfo)
                    # [???] ------
                    eval(parse(text = paste(apply, "()")))
                    putRcmdr("open.dialog.here", NULL)
                }
            }

            applyButton <- buttonRcmdr(
                rightButtonsBox,
                text       = gettextRcmdr("Apply"),
                foreground = "yellow",
                width      = width,
                command    = onApply,
                image      = "::image::applyIcon",
                compound   = "left"
            )
        }
        # END: apply ---------------------------------------------------------

        # Grid ===============================================================
        if (!WindowsP()) {
            if (!is.null(apply)) {
                tkgrid(applyButton, cancelButton, OKbutton, sticky = "w")
                tkgrid.configure(OKbutton, padx = c(6, 0))
            }
            else {
                tkgrid(cancelButton, OKbutton, sticky = "w")
            }
            tkgrid.configure(cancelButton, padx = c(6, 6))
        }
        else {
            if (!is.null(apply)) {
                tkgrid(OKbutton, cancelButton, applyButton, sticky = "w")
                tkgrid.configure(applyButton, padx = c(6, 0))
            }
            else {
                tkgrid(OKbutton, cancelButton, sticky = "w")
            }
            tkgrid.configure(OKbutton, padx = c(6, 6))
        }
        if (!is.null(reset) && memory) {
            if (!is.null(helpSubject)) {
                tkgrid(helpButton, resetButton, pady = 6)
            }
            else {
                tkgrid(resetButton, pady = 6)
            }
            if (!WindowsP()) tkgrid.configure(resetButton, padx = c(0, 6))
        }
        else if (!is.null(helpSubject)) {
            tkgrid(helpButton, pady = 6)
        }
        tkgrid(leftButtonsBox, rightButtonsBox, pady = 6, sticky = "ew")
        if (!is.null(helpSubject)) {
            tkgrid.configure(helpButton, padx = c(0, 18))
        } else if (!is.null(reset) && memory) {
            tkgrid.configure(resetButton, padx = c(0, 18))
        }
        tkgrid.columnconfigure(buttonsFrame, 0, weight = 1)
        tkgrid.columnconfigure(buttonsFrame, 1, weight = 1)
        tkgrid.configure(leftButtonsBox, sticky = "w")
        tkgrid.configure(rightButtonsBox, sticky = "e")
    }
)
