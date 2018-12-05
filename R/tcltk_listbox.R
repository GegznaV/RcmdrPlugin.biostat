# Listbox functions ==========================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
# Variable list box with constant length (numer of rows)
# onClick_fun - function on mouse click
# onRelease_fun - function on mouse release
bs_listbox <-
  function(parent_window,
           variable_list = variables_all(),
           bg            = "white",
           select_mode   = "single",
           export        = "FALSE",
           initial_selection = NULL,
           height = getRcmdr("variable.list.height"),
           width  = getRcmdr("variable.list.width"),

           on_click          = function() {},
           on_double_click   = function() {},
           on_triple_click   = function() {},
           on_release        = function() {},
           on_click_3        = function() {},
           on_double_click_3 = function() {},
           on_triple_click_3 = function() {},
           on_release_3      = function() {},


           title = NULL,
           title_sticky = "w",
           tip = "",
           ...)
  {

    if (select_mode == "multiple")
      select_mode <- getRcmdr("multiple.select.mode")

    if (length(variable_list) == 1 && is.null(initial_selection))
      initial_selection <- 0

    frame   <- tkframe(parent_window)
    # minmax  <- getRcmdr("variable.list.width")
    minmax  <- width

    listbox <- tklistbox(
      parent     = frame,
      height     = height,
      selectmode = select_mode,
      background = bg,
      exportselection = export,
      width = min(max(minmax[1], 2 + nchar(variable_list)), minmax[2]),
      ...
    )

    scrollbar <-
      ttkscrollbar(
        frame,
        command = function(...) tkyview(listbox, ...))

    tkconfigure(listbox,
                yscrollcommand = function(...) tkset(scrollbar,  ...))

    # for (var in variable_list)  tkinsert(listbox, "end", var)
    set_values_listbox(listbox, variable_list)

    # Initial selection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.numeric(initial_selection))
      for (sel in initial_selection)
        tkselection.set(listbox, sel)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # firstChar <- tolower(substr(variable_list, 1, 1))
    # len <- length(variable_list)
    # onLetter <- function(letter) {
    #   letter <- tolower(letter)
    #   current <-
    #     1 + round(as.numeric(
    #       unlist(strsplit(tclvalue(tkyview(listbox)), " "))[1]) * len)
    #
    #   mat <- match(letter, firstChar[-(1:current)])
    #   if (is.na(mat))
    #     return()
    #   tkyview.scroll(listbox, mat, "units")
    # }

    onLetter <- function(letter) {
      # Letter binding is good for read-only comboboxes only

      get_first_letter <- function(str) {
        tolower(substr(str, 1, 1))
      }

      letter   <- tolower("e")
      all_vals <- get_values_(listbox)
      acceptable_inds <- which(get_first_letter(all_vals) %in% letter)

      if (length(acceptable_ind) == 0) {
        return()
      }

      cur_val <- tclvalue(combovar)
      cur_val_ind <- which(all_vals %in% cur_val)

      if (is.na(cur_val_ind)) return()

      if (isTRUE((cur_val_ind + 1) %in% acceptable_inds)) {
        next_ind <- cur_val_ind + 1
      } else {
        next_ind <- min(acceptable_inds)
      }
      tcl(combobox, "current", next_ind - 1)
    }


    eval_glue('tkbind(listbox, "<{letters}>", function() onLetter("{letters}"))')
    eval_glue('tkbind(listbox, "<{LETTERS}>", function() onLetter("{letters}"))')
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bind_mouse_keys(listbox)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    toggleSelection <- function() {
      active   <- tclvalue(tkindex(listbox, "active"))
      selected <- tclvalue(tkcurselection(listbox))
      if (selected == active) {
        tkselection.clear(listbox, "active")
      } else {
        tkselection.set(listbox, "active")
      }
    }

    if (select_mode == "single")
      tkbind(listbox, "<Control-ButtonPress-1>", toggleSelection)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (!is.null(title)) {
      tkgrid(
        bs_label_b(frame, text = title, font = "RcmdrTitleFont"),
        columnspan = 2, sticky = title_sticky)
    }

    tkgrid(listbox, scrollbar,  sticky = "nw")
    tkgrid.configure(scrollbar, sticky = "wns")
    tkgrid.configure(listbox,   sticky = "ewns")

    # Output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    structure(
      list(frame       = frame,
           listbox     = listbox,
           scrollbar   = scrollbar,
           select_mode = select_mode,
           varlist     = variable_list),
      class = "listbox"
    )
  }
# ~~~~~~~~~~~~~~~~~~~~~ ======================================================


# Helpers and methods ========================================================
#' @rdname Helper-functions
#' @export
#' @keywords internal
get_values_listbox <- function(listbox) {
  n <- tclvalue_int(tksize(listbox))
  vars <-
    (seq_len(n) - 1) %>% # zero based index
    purrr::map_chr(~tclvalue_chr(tkget(listbox, ., .))) %>%
    stringr::str_replace("^\\{(.* .*)\\}$", "\\1") # remove { } if several words are used
  vars
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
set_values_listbox <- function(listbox, values) {
  tkdelete(listbox, 0, "end")
  for (val in values)  tkinsert(listbox, "end", as.character(val))
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_selection_listbox_ind <- function(listbox) {
  as.numeric(tkcurselection(listbox)) + 1
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
# Get selected values
get_selection_listbox <- function(listbox) {
  vals <- get_values_listbox(listbox)
  inds <- get_selection_listbox_ind(listbox)
  vals[inds]
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
# Set selection
# sel - either character values of indices
set_selection_listbox <- function(listbox, sel, clear = TRUE) {

  if (is.null(sel) || length(sel) == 0) {
    # ind <- NULL
    return()

  } else if (is.character(sel)) {
    ind <- which(get_values_listbox(listbox) %in% sel) - 1

  } else if (is.numeric(sel)) {
    ind <- sel - 1

  } else if (is.na(sel)) {
    ind <- -1

  } else {
    stop("Incorrect value of argument `sel`: \n", sel)
  }

  if (isTRUE(clear)) {
    tkselection.clear(listbox, 0, "end")
  }

  # if (is.null(ind)) {
  #   return
  # }

  for (i in ind)
    tkselection.set(listbox, i)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @rdname Helper-functions
#' @export
#' @keywords internal
get_size.listbox <- function(obj, ...) {
  tclvalue_int(tksize(obj$listbox))
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_size.tkwin <- function(obj, ...) {
  tclvalue_int(tksize(obj))
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_selection.listbox <- function(obj, ...) {
  get_selection_listbox(obj$listbox)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_selection_length.listbox <- function(obj, ...) {
  length(get_selection(obj))
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
set_selection.listbox <- function(obj, sel, clear = TRUE, ...) {
  listbox <- obj$listbox
  set_selection_listbox(listbox, sel = sel, clear = clear, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
add_selection.listbox <- function(obj, sel, ...) {
  listbox <- obj$listbox
  set_selection_listbox(listbox, sel = sel, clear = FALSE, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_values.listbox <- function(obj, vals, ...) {
  get_values_listbox(obj$listbox, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
set_values.listbox <- function(obj, values, ...) {
  set_values_listbox(obj$listbox, values = values, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
set_yview.listbox <- function(obj, ind, ...) {
  if (is.numeric(ind)) {
    ind <- ind - 1
  }

  tkyview(obj$listbox, ind, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_disable.listbox <- function(obj, ..., background = "grey95") {
  tk_disable(obj$listbox, background = background, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_normalize.listbox <- function(obj, ..., background = "white") {
  tk_normalize(obj$listbox, background = background, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_get_state.listbox <- function(obj) {
  tk_get_state(obj$listbox)
}

