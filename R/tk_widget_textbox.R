
tk_widget_textbox <- function(

    parent_frame = top,
    width    = "28",
    init_val = "",
    label = "",
    label_position = c("left", "above", "right", "none"),
    label_color = getRcmdr("title.color"),
    padx = 0,
    pady = 5,
    sticky = "w",
    main_frame  = tkframe(parent_frame),
    text_frame  = tkframe(main_frame),
    label_frame = tkframe(main_frame),
    ...

) {
    label_position <- match.arg(label_position)

    var_text  <- tclVar(init_val)
    obj_text <- ttkentry(text_frame,
                         width = width,
                         textvariable = var_text,
                         ...)

    if (nchar(label) > 0) {

        label_obj <- label_rcmdr(
            label_frame,
            text = gettext_bs(label),
            foreground = label_color)

        switch(label_position,
               "above" = {
                   if (length(pady) == 1) {
                       pady <- c(pady, pady)
                   }
                   tkgrid(label_frame, sticky = sticky, padx = padx, pady = c(pady[1], 0))
                   tkgrid(text_frame,  sticky = sticky, padx = padx, pady = c(0, pady[2]))
                   tkgrid(obj_text,   sticky = sticky)
               } ,
               "left" = {
                   tkgrid(label_frame, text_frame, sticky = sticky,
                          padx = padx, pady = pady)
                   tkgrid(obj_text,   sticky = sticky, padx = c(5, 0))
               } ,
               "right" = {
                   tkgrid(text_frame, label_frame, sticky = sticky,
                          padx = padx, pady = pady)
                   tkgrid(obj_text,   sticky = sticky, padx = c(0, 5))
               }
        )
        tkgrid(label_obj, sticky = sticky)

    } else {
        tkgrid(text_frame, sticky = sticky, padx = padx, pady = pady)
        tkgrid(obj_text,  sticky = sticky)
    }

    structure(list(
        frame = main_frame,
        frame_text  = text_frame,
        frame_label = label_frame,

        var_text = var_text,
        obj_text = obj_text
    ))
}

