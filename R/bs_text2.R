# f4, width = 80, height = 13, wrap = "none",
#         # autoseparators = TRUE,
#         undo = TRUE,
#         state = "normal",
#         font = font_consolas_regular


bs_text2 <- function(parent, width = 80, ..., label = "", undo = TRUE, k = 2,
  context_menu = FALSE) {

  frame <- tk2frame(parent)

  obj_label <- tk_label_blue(frame, text = label)

  obj_num <- tk2text(frame, wrap = "none", state = "disabled", width = k,        cursor = "", ...)
  obj_txt <- tk2text(frame, wrap = "none", undo = undo,        width = width - k, ...)

  obj_xsc <- tk2scrollbar(
    frame,
    orientation = "horizontal",
    command = function(...) {tkxview(obj_txt, ...)}
  )

  obj_ysc <- tk2scrollbar(
    frame,
    orientation = "vertical",
    command = function(...) {
      tkyview(obj_num, ...)
      tkyview(obj_txt, ...)
    }
  )

  tkconfigure(
    obj_txt,
    xscrollcommand = function(...) {
      tkset(obj_xsc, ...)
    },
    yscrollcommand = function(...) {
      tkset(obj_ysc, ...)
      tkyview.moveto(obj_num, ..1)
    }
  )

  tkconfigure(
    obj_num,
    yscrollcommand = function(...) {
      tkset(obj_ysc, ...)
      tkyview.moveto(obj_txt, ..1)
    }
  )

  # tkgrid(frame, sticky = "news")
  # tkdestroy(frame)

  tkgrid(obj_label)
  tkgrid(obj_num, obj_txt, obj_ysc)
  tkgrid(obj_xsc, "x")


  update_line_numbers <- function() {
    init_pos <- as.numeric(tkyview(obj_txt))[1]
    set_values(obj_num, paste(1:tk_get_n_lines(obj_txt), collapse = "\n"))
    tktag.add(obj_num, "line_numbers", "1.0", "end")
    tkyview.moveto(obj_num, init_pos)
  }

  font_TkTextFont <- tkfont.create(family = "TkTextFont", size = 9)
  tkconfigure(obj_num, font = font_TkTextFont)
  tkconfigure(obj_txt, font = font_TkTextFont)

  tktag.configure(obj_num, "line_numbers",
    background = "",
    foreground = "darkred",
    justify = "right")

  tkgrid.configure(obj_num, sticky = "news", padx = c(10,  0))
  tkgrid.configure(obj_txt, sticky = "news", padx = c(1,  0))
  tkgrid.configure(obj_xsc, sticky = "we",   padx = c(10,  0))
  tkgrid.configure(obj_ysc, sticky = "ns",   padx = c(0,  10))

  tkgrid.columnconfigure(frame, 0, weight = 0)
  tkgrid.columnconfigure(frame, 1, weight = 1)
  tkgrid.columnconfigure(frame, 2, weight = 2)

  tkgrid.rowconfigure(frame, 0, weight = 1,  minsize = 1)
  tkgrid.rowconfigure(frame, 1, weight = 10, minsize = 3)
  tkgrid.rowconfigure(frame, 2, weight = 0)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (context_menu == TRUE) {
    right_click_menu_text(obj_txt, undo = undo)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkbind(obj_txt, "<<Modified>>", update_line_numbers)
  tkbind(obj_txt, "<<Cut>>",      update_line_numbers)
  tkbind(obj_txt, "<<Copy>>",     update_line_numbers)
  tkbind(obj_txt, "<<Paste>>",    update_line_numbers)
  tkbind(obj_txt, "<KeyRelease>", update_line_numbers)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  structure(list(
    frame = frame,
    label    = obj_label,
    text     = obj_txt,
    x_scroll = obj_xsc,
    y_scroll = obj_ysc,
    context_menu_fun =
      purrr::partial(right_click_menu_text, tcl_widget = obj_txt, undo = undo),
    update_line_numbers = update_line_numbers

  ),
    class = c("bs_text2", "bs_text", "bs_tk_widget", "list"))
}
