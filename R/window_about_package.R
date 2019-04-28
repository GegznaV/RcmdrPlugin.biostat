#' window_about_biostat_version
#'
#' @export
#' @keywords internal
window_about_biostat_version <- function() {
    initializeDialog(title = gettext_bs("Information About The Package"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {closeDialog(); tkfocus(CommanderWindow())}
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    descr_file <- system.file("DESCRIPTION", package = "RcmdrPlugin.biostat")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The main title is taken from the description file
    upper_frame <- tkframe(top)
    tkgrid(upper_frame)

    main_title_frame <-
        labelRcmdr(upper_frame,
                   fg = "darkblue",
                   font = tkfont.create(family = "Times New Roman", size = 14),
                   text = desc::desc_get("Title", descr_file))

    tkgrid(main_title_frame, pady = c(2, 15), padx = 16)

    tkconfigure(main_title_frame, cursor = "heart")       # Mouse cursor changes

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    info_frame <- tkframe(top)
    tkgrid(info_frame)

    tkgrid(
        bs_label_b(info_frame, text = gettext_bs("Package: ")),
        bs_label(  info_frame, text = "RcmdrPlugin.biostat"),
        sticky = "e", pady = c(2, 0), padx = 16)

    tkgrid(
        bs_label_b(info_frame, text = gettext_bs("Version: ")),
        bs_label(info_frame,
                 text = as.character(packageVersion("RcmdrPlugin.biostat"))),
        sticky = "e", pady = c(2, 0), padx = 16)

    tkgrid(
        bs_label_b(info_frame, text = gettext_bs("Date: ")),
        bs_label(info_frame,   text = desc::desc_get("Date", descr_file)),
        sticky = "e", pady = c(0, 10), padx = 16
    )

    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # font_normal  <- tkfont.create(family = "Segoe UI", size = 9, underline = FALSE, weight = "normal")
    # font_over    <- tkfont.create(family = "Segoe UI", size = 9, underline = TRUE,  weight = "normal")
    #
    # set_font_n <- function(W) {
    #     tkconfigure(W, font = font_normal, foreground = "black")
    # }
    #
    # set_font_o <- function(W) {
    #     tkconfigure(W, font = font_over, foreground = "blue")
    # }
    #
    #
    #
    # # homepage_link <- "https://gegznav.github.io/RcmdrPlugin.biostat/"
    # homepage_link <- desc::desc_get("URL", descr_file)
    #
    # on_click <- function() {
    #     tkconfigure(hp_frame_2, cursor = "hand1")
    #     browseURL(homepage_link)
    # }
    # on_release <- function() {
    #     tkconfigure(hp_frame_2, cursor = "hand2")
    # }
    #
    # hp_frame_1 <- bs_label_b(info_frame, text = gettext_bs("Homepage: "))
    # hp_frame_2 <- bs_label(info_frame,   text = homepage_link, font = font_normal)
    #
    # tkbind(hp_frame_2, "<ButtonPress-1>",   on_click)   # Link activation
    # tkbind(hp_frame_2, "<ButtonRelease-1>", on_release)
    # tkbind(hp_frame_2, "<Enter>", set_font_o)
    # tkbind(hp_frame_2, "<Leave>", set_font_n)
    #
    # tkconfigure(hp_frame_2, cursor = "hand2")       # Mouse cursor changes
    #
    # tkgrid(hp_frame_1, hp_frame_2,
    #        sticky = "w",
    #        padx = 16,
    #        pady = c(0, 0))
    #
    # tkgrid.configure(hp_frame_2, sticky = "w")

    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # # issues_link <- "https://github.com/GegznaV/RcmdrPlugin.biostat/issues"
    # issues_link <- desc::desc_get("BugReports", descr_file)
    #
    # issues_frame_1 <- bs_label_b(info_frame, text = gettext_bs("Bug reports: "))
    # issues_frame_2 <- bs_label(info_frame,   text = issues_link, font = font_normal)
    #
    # on_click_i <- function() {
    #     tkconfigure(issues_frame_2, cursor = "hand1")
    #     browseURL(issues_link)
    # }
    #
    # on_release_i <- function() {
    #     tkconfigure(issues_frame_2, cursor = "hand2")
    # }
    #
    #
    # tkbind(issues_frame_2, "<ButtonPress-1>",   on_click_i)   # Link is activated
    # tkbind(issues_frame_2, "<ButtonRelease-1>", on_release_i)
    # tkbind(issues_frame_2, "<Enter>", set_font_o)
    # tkbind(issues_frame_2, "<Leave>", set_font_n)
    #
    # tkconfigure(issues_frame_2, cursor = "hand2")        # Mouse cursor changes
    #
    # tkgrid(issues_frame_1, issues_frame_2,  # pady = c(0, 10), columnspan = "3",
    #        sticky = "w",
    #        padx = 16,
    #        pady = c(0, 10))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help()

    tkgrid(buttonsFrame)
    tkgrid.remove(cancelButton)

    dialogSuffix()
}
