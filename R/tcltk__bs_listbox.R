# Listbox functions ==========================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
# Variable list box with constant length (numer of rows)
# onClick_fun - function on mouse click
# onRelease_fun - function on mouse release
bs_listbox <-
  function(parent,
           values     = variables_all(), # NULL
           value      = NULL,
           selection  = NULL,
           selectmode = c("single", "extended", "browse", "multiple"),
           title      = NULL,
           subtitle   = NULL,
           tip        = "",
           height     = getRcmdr("variable.list.height"),
           width      = getRcmdr("variable.list.width"),
           enabled    = TRUE,
           scroll     = c("both", "x", "y", "none"),
           autoscroll = c("x", "y", "both", "none"),

           on_click          = function() {},
           on_double_click   = function() {},
           on_triple_click   = function() {},
           on_release        = function() {},
           on_click_3        = function() {},
           on_double_click_3 = function() {},
           on_triple_click_3 = function() {},
           on_release_3      = function() {},

           title_sticky = "w",
           subtitle_sticky = title_sticky,
           bind_keyboard = TRUE

           , ...


           )
  {

    selectmode = match.arg(selectmode)
    scroll     = match.arg(scroll)
    autoscroll = match.arg(autoscroll)


    if (selectmode == "multiple")
      selectmode <- getRcmdr("multiple.select.mode")

    # if (length(values) == 1 && is.null(selection))
    #   selection <- 0


    frame  <- tk2frame(parent)
    if (length(width) == 1) {
      width <- c(width, width)
    }
    width  <- min(max(width[1], 2 + nchar(values)), width[2]) # Set width

    if (is.null(value) & is.null(selection)) {
      listbox <- tk2listbox(
        parent     = frame,
        values     = values,
        # value      = value,
        # selection  = selection,
        selectmode = selectmode,
        height     = height,
        width      = width,
        scroll     = scroll,
        autoscroll = autoscroll,
        enabled    = enabled,
        tip        = tip,
        ...
      )

    } else if (!is.null(selection)) {
      listbox <- tk2listbox(
        parent     = frame,
        values     = values,
        # value      = value,
        selection  = selection,
        selectmode = selectmode,
        height     = height,
        width      = width,
        scroll     = scroll,
        autoscroll = autoscroll,
        enabled    = enabled,
        tip        = tip,

        ...
      )
    } else {
      listbox <- tk2listbox(
        parent     = frame,
        values     = values,
        value      = value,
        # selection  = selection,
        selectmode = selectmode,
        height     = height,
        width      = width,
        scroll     = scroll,
        autoscroll = autoscroll,
        enabled    = enabled,
        tip        = tip,

        ...
      )
    }


    # listbox <- tklistbox(
    #   parent     = frame,
    #   height     = height,
    #   selectmode = selectmode,
    #   background = bg,
    #   exportselection = export,
    #   width = width,
    #   ...
    # )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Adds ability deselect in single-selection boxes
    toggle_single_selection <- function() {
      active   <- tclvalue_int(tkindex(listbox, "active"))
      selected <- tclvalue_int_split(tkcurselection(listbox))

      if (length(selected) == 0) {
        tkselection.set(listbox, "active")

      } else if (isTRUE(active %in% selected)) {
        tkselection.clear(listbox, "active")
      }

    }

    if (selectmode == "single")
      tkbind(listbox, "<Control-ButtonPress-1>", toggle_single_selection)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(bind_keyboard)) {
      onLetter <- function(letter) {
        # Letter binding is good for read-only listboxes only

        get_first_letter <- function(str) {
          tolower(substr(str, 1, 1))
        }

        letter   <- tolower(letter)
        all_vals <- get_values_listbox(listbox)
        acceptable_inds <- which(get_first_letter(all_vals) %in% letter)

        if (length(acceptable_inds) == 0) {
          return()
        }

        cur_val_ind <- get_selection_ind_listbox(listbox)[1]

        if (is.na(cur_val_ind) || length(cur_val_ind) == 0)
          cur_val_ind <- 0

        next_ind <-
          acceptable_inds[which(acceptable_inds %in% cur_val_ind)[1] + 1]

        if (is.na(next_ind)) {
          next_ind <- min(acceptable_inds)
        }
        # Set selection and make selection visible
        set_selection_listbox(listbox, next_ind) # 1 based index
        tkyview(listbox, next_ind - 1)           # 0 based index
      }

      eval_glue('tkbind(listbox, "<{letters}>", function() onLetter("{letters}"))')
      eval_glue('tkbind(listbox, "<{LETTERS}>", function() onLetter("{letters}"))')
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bind_mouse_keys(listbox)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(title)) {
      tkgrid(
        bs_label_b(frame, text = title, font = "RcmdrTitleFont"),
        columnspan = 2, sticky = title_sticky)
    }
    if (!is.null(subtitle)) {
      tkgrid(
        bs_label(frame, text = subtitle, font = "RcmdrTitleFont"),
        columnspan = 2, sticky = subtitle_sticky)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(listbox, sticky = "nw")
    # tkgrid(listbox, scrollbar,  sticky = "nw")
    # tkgrid.configure(scrollbar, sticky = "wns")
    # tkgrid.configure(listbox,   sticky = "ewns")

    # Output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    structure(
      list(frame       = frame,
           listbox     = listbox,
           # scrollbar   = scrollbar,
           selectmode  = selectmode,
           varlist     = values),
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
set_values_listbox <- function(listbox, values, clear = TRUE) {
  if (isTRUE(clear)) {
    tkdelete(listbox, 0, "end")
  }
  for (val in values)  tkinsert(listbox, "end", as.character(val))
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_selection_ind_listbox <- function(listbox) {
  as.numeric(tkcurselection(listbox)) + 1
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
# Get selected values
get_selection_listbox <- function(listbox) {
  vals <- get_values_listbox(listbox)
  inds <- get_selection_ind_listbox(listbox)
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
set_values.listbox <- function(obj, values, ..., clear = TRUE) {
  set_values_listbox(obj$listbox, values = values, ..., clear = clear)
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

