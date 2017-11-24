labeled_frame <- function(parent, label = NULL, ...) {
    ttklabelframe(parent = parent,
                  labelwidget = tklabel(parent,
                                        text = label,
                                        font = "RcmdrTitleFont",
                                        foreground = Rcmdr::getRcmdr("title.color"),
                                        ...)
    )
}