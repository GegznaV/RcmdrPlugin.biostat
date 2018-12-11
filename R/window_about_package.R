#' window_about_package
#'
#' @export
#' @keywords internal
window_about_package <- function() {
    initializeDialog(title = gettext_bs("Information About The Package"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {closeDialog(); tkfocus(CommanderWindow())}
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    descr_file <- system.file("DESCRIPTION", package = "RcmdrPlugin.biostat")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The main title is taken from the description file
    main_title_frame <-
        labelRcmdr(top,
                   fg = "darkblue",
                   font = tkfont.create(family = "Times New Roman", size = 12),
                   text = desc::desc_get("Title", descr_file))

    tkgrid(main_title_frame,
           sticky = "w", pady = c(2, 15), padx = 25, columnspan = 5)

    tkconfigure(main_title_frame, cursor = "heart")       # Mouse cursor changes

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    info_frame <- tkframe(top)
    tkgrid(info_frame, columnspan = 4)

    tkgrid(
        labelRcmdr(info_frame, fg = getRcmdr("title.color"),
                      text = gettext_bs("Package: ")),
        labelRcmdr(info_frame, text = "RcmdrPlugin.biostat"),

           sticky = "w", pady = c(2, 0), padx = 25)

    tkgrid(
        labelRcmdr(info_frame,
                   fg   = getRcmdr("title.color"),
                   text = gettext_bs("Version: ")),
        labelRcmdr(info_frame,
                   text = as.character(packageVersion("RcmdrPlugin.biostat"))),
           sticky = "w", pady = c(2, 0), padx = 25)

    tkgrid(
        labelRcmdr(info_frame,
                   fg   = getRcmdr("title.color"),
                   text = gettext_bs("Date: ")),
        labelRcmdr(info_frame, text = desc::desc_get("Date", descr_file)),
        sticky = "w", pady = c(0, 10), padx = 25
        )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    homepage_link <- "https://gegznav.github.io/RcmdrPlugin.biostat/"

    on_click <- function() {browseURL(homepage_link)}

    hp_frame_1 <- labelRcmdr(top, text = gettext_bs("Homepage: "))
    hp_frame_2 <- labelRcmdr(top, fg = getRcmdr("title.color"), text = homepage_link)

    tkbind(hp_frame_2, "<ButtonPress-1>", on_click) # Link activation
    tkconfigure(hp_frame_2, cursor = "hand2")       # Mouse cursor changes

    tkgrid(hp_frame_1, hp_frame_2,
           sticky = "w",
           padx = 25,
           pady = c(0, 2),
           columnspan = "3")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    issues_link <- "https://github.com/GegznaV/RcmdrPlugin.biostat/issues"

    issues_frame_1 <- labelRcmdr(top, text = gettext_bs("Bug reports: "))
    issues_frame_2 <- labelRcmdr(top, fg = getRcmdr("title.color"), text = "(link)")

    on_click_i <- function() {
        browseURL(issues_link)
    }
    tkbind(issues_frame_2, "<ButtonPress-1>", on_click_i) # Link is activated
    tkconfigure(issues_frame_2, cursor = "hand2")         # Mouse cursor changes

    tkgrid(issues_frame_1, issues_frame_2, pady = c(0, 10), columnspan = "3")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, columnspan = "5", sticky = "")
    dialogSuffix()
}
