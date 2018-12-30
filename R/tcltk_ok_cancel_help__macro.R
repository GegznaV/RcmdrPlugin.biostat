#' @rdname Helper-functions
#' @export
#' @keywords internal
# This macro is based on function from Rcmdr package
# window = top          -- parent window
# helpSubject = NULL,   -- string with a help subject
# model = FALSE,        --
# reset = NULL,         -- string with a function to recall
# apply = NULL,         -- string with a function to recall
# helpPackage = NULL    -- package to search help topic in

ok_cancel_help <- Rcmdr::defmacro(
    window      = top,
    helpSubject = NULL,
    model       = FALSE,
    reset       = NULL,
    apply       = NULL,
    helpPackage = NULL,
    sticky      = "w",
    ok_label    = "OK",
    expr = {

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Functions that allows parentheses "()" in string with function name.

        # Removes () and everything in it
        extrac_function_name <- function(str) {
            str_extract(str, "^.*?(?=\\()")
        }

        # Make string into form ready to evaluate as function
        make_ready_to_eval <- function(str) {
            if (str_detect(str, "\\(")) {str} else {str_c(str, "()")}
        }

        if (!is.null(reset)) {
            reset_fun <- make_ready_to_eval(reset)
            reset     <- extrac_function_name(reset)
        }

        if (!is.null(apply)) {
            apply_fun <- make_ready_to_eval(apply)
            apply     <- extrac_function_name(apply)
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        memory <- getRcmdr("retain.selections")

        button.strings <- c(
            gettext_bs(ok_label),
            gettext_bs("Cancel"),
            if (!is.null(helpSubject))     gettext_bs("Help"),
            if (!is.null(reset) && memory) gettext_bs("Reset"),
            if (!is.null(apply))           gettext_bs("Apply")
        )

        width <- max(nchar(gettext_bs(button.strings)))
        if (WindowsP()) width <- width + 2

        buttonsFrame    <- tkframe(window)
        leftButtonsBox  <- tkframe(buttonsFrame)
        rightButtonsBox <- tkframe(buttonsFrame)

        # Button call-back functions ==========================================
        # START: ok -----------------------------------------------------------
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
                                text       = gettext_bs(ok_label),
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
            text         = gettext_bs("Cancel"),
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
                text          = gettext_bs("Help"),
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
                eval_text(reset_fun)
                putRcmdr("open.dialog.here", NULL)
                putRcmdr("restoreTab", FALSE)
            }
            resetButton <- buttonRcmdr(
                leftButtonsBox,
                text     = gettext_bs("Reset"),
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

                # Function should return TRUE, if no error occurs:
                res_of_ok <- onOK()

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
                    eval_text(apply_fun)
                    putRcmdr("open.dialog.here", NULL)
                }
            }

            applyButton <- buttonRcmdr(
                rightButtonsBox,
                text       = gettext_bs("Apply"),
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
                tkgrid(applyButton, cancelButton, OKbutton, sticky = sticky)
                tkgrid.configure(OKbutton, padx = c(6, 0))

            } else {
                tkgrid(cancelButton, OKbutton, sticky = sticky)
            }
            tkgrid.configure(cancelButton, padx = c(6, 6))

        } else {
            if (!is.null(apply)) {
                tkgrid(OKbutton, cancelButton, applyButton, sticky = sticky)
                tkgrid.configure(applyButton, padx = c(6, 0))

            } else {
                tkgrid(OKbutton, cancelButton, sticky = sticky)
            }
            tkgrid.configure(OKbutton, padx = c(6, 6))
        }

        if (!is.null(reset) && memory) {
            if (!is.null(helpSubject)) {
                tkgrid(helpButton, resetButton, pady = 6)

            } else {
                tkgrid(resetButton, pady = 6)
            }
            if (!WindowsP()) tkgrid.configure(resetButton, padx = c(0, 6))

        } else if (!is.null(helpSubject)) {
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
