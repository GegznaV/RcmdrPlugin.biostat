# TODO:
#
# 1) for line `values       = variables_all()`  maybe a better default is
#    NULL, as it does not fail if no dataset is selected
# 2) if any value is selected, make it visible
#     if (!is.null(.ds)) {
#           tk_see(ds_1_box, dataSets)
#     }


# Listbox functions ==========================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
# Variable list box with constant length (numer of rows)
# onClick_fun - function on mouse click
# onRelease_fun - function on mouse release
# bind_row_swap - if TRUE, Ctrl/Alt + Up/Down move rows in the listbox

bs_listbox <-
    function(parent,
             values       = variables_all(), # NULL  # [???]
             value        = NULL,
             selection    = NULL,
             selectmode   = c("single", "extended", "browse", "multiple"),
             title        = NULL,
             subtitle     = NULL,
             tip          = "",
             height       = getRcmdr("variable.list.height"),
             width        = getRcmdr("variable.list.width"),
             enabled      = TRUE,
             scroll       = c("both", "x", "y", "none"),
             autoscroll   = c("x", "y", "both", "none"),
             use_filter   = FALSE,
             filter_label = "Filter",
             sticky       = "nw",

             on_select         = do_nothing,
             on_click          = do_nothing,
             on_double_click   = do_nothing,
             on_triple_click   = do_nothing,
             on_release        = do_nothing,
             on_click_3        = do_nothing,
             on_double_click_3 = do_nothing,
             on_triple_click_3 = do_nothing,
             on_release_3      = do_nothing,

             on_keyboard = c("select", "scroll", "ignore"),
             on_keyboard_fun = do_nothing,
             bind_row_swap = FALSE,

             title_sticky = "w",
             subtitle_sticky = title_sticky

             , ...
    )
    {

        selectmode  <- match.arg(selectmode)
        scroll      <- match.arg(scroll)
        autoscroll  <- match.arg(autoscroll)
        on_keyboard <- match.arg(on_keyboard)


        if (selectmode == "multiple")
            selectmode <- getRcmdr("multiple.select.mode")

        # if (length(values) == 1 && is.null(selection))
        #   selection <- 0


        frame  <- tk2frame(parent)

        if (length(width) == 1) {
            width <- c(width, width)
        }

        width  <- min(max(width[1], 2 + nchar(values)), width[2]) # Set width

        selection_code <- if(!is.null(selection)) "selection  = {selection}," else ""
        value_code     <- if(!is.null(value))     "value      = {value},"     else ""

        listbox <-  str_glue_eval("
        tk2listbox(
            parent     = frame,
            values     = values,
            {selection_code}
            {value_code}
            selectmode = selectmode,
            height     = height,
            width      = width,
            scroll     = scroll,
            autoscroll = autoscroll,
            enabled    = enabled,
            tip        = tip,
            ...)
        ")

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Adds ability to deselect in single-selection boxes
        if (selectmode %in% c("single", "browse")) {

            toggle_single_selection <- function() {
                active   <- tclvalue_int(tkindex(listbox, "active"))
                selected <- tclvalue_int_split(tkcurselection(listbox))

                if (length(selected) == 0) {
                    tkselection.set(listbox, "active")

                } else if (isTRUE(active %in% selected)) {
                    tkselection.clear(listbox, "active")
                }

            }

            tkbind(listbox, "<Control-ButtonPress-1>", toggle_single_selection)
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (on_keyboard %in% c("select", "scroll")) {
            onLetter <- function(letter) {
                # Letter binding is good for read-only listboxes only
                #         # Letter binding is good for single-selection comboboxes:
                #        otherwise selecton should not be toggled.

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

                # Make selection visible


                switch(
                    on_keyboard,

                    "select" = {
                        # Reset selection
                        set_selection_listbox(listbox, next_ind) # 1 based index
                        tksee(listbox, next_ind - 1)             # 0 based index
                    },

                    "scroll" = {
                        tkyview(listbox, next_ind - 1)         # 0 based index
                    }
                )
                on_keyboard_fun()
            }

            str_glue_eval('tkbind(listbox, "<{letters}>", function() onLetter("{letters}"))')
            str_glue_eval('tkbind(listbox, "<{LETTERS}>", function() onLetter("{letters}"))')
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        bind_mouse_keys(listbox)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        tkbind(listbox, "<<ListboxSelect>>", on_select)
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
        # if (bind_row_swap) {
        #     bind_row_swap_listbox(listbox)
        # }


        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkgrid(listbox, sticky = sticky)
        # tkgrid(listbox, scrollbar,  sticky = "nw")
        # tkgrid.configure(scrollbar, sticky = "wns")
        # tkgrid.configure(listbox,   sticky = "ewns")


        # Add textbox with filter ------------------------------------------------
        values_env <- new.env()
        values_env$all_values <- values

        if (use_filter == TRUE) {
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            cmd_update_list <- function() {

                s_txt <- get_values(text_box_1)
                if (str_length(s_txt) == 0) {
                    new_list <- values_env$all_values

                } else {
                    ignore_case <- !tclvalue_lgl(options$var$case)

                    filter_fun <- switch(
                        tclvalue_chr(options$var$regex), #
                        "0" = stringr::fixed,
                        "1" = stringr::regex
                    )

                    s_filter <- filter_fun(s_txt, ignore_case = ignore_case)
                    # To prevet from invalid regular exressions `try` is used
                    rez <- try(silent = TRUE, {
                        new_list <- str_subset(values_env$all_values, s_filter)
                    })

                    if (inherits(rez, "try-error")) {
                        return()
                    }
                }

                set_values_listbox(listbox, new_list)
            }

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            clear_textbox <- function() {
                set_values(text_box_1, "")
                cmd_update_list()
            }
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            text_box_1 <- bs_entry(
                parent = frame,
                width = width,
                label = filter_label,
                label_position = "above")

            options <- bs_checkboxes(
                parent = frame,
                boxes  = c("case"  = "Match case",
                           "regex" = "Regex"),
                values = c(0, 0),
                layout = "horizontal",
                commands = list("case"  = cmd_update_list,
                                "regex" = cmd_update_list)
            )

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            tkgrid(text_box_1$frame, sticky = sticky)
            # tkgrid(options_frame,    sticky = sticky)
            tkgrid(options$frame,    sticky = sticky)

            # tkgrid.forget(text_box_1$frames)

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            tkbind(text_box_1$obj_text, "<KeyRelease>",      cmd_update_list)
            tkbind(text_box_1$obj_text, "<Double-Button-3>", clear_textbox)
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            add_class <- "listbox_with_filter"

        } else {
            add_class  <- NULL
            text_box_1 <- NULL
            options    <- NULL
        }

        # Output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        structure(
            list(frame      = frame,
                 listbox    = listbox,
                 # scrollbar = scrollbar,
                 selectmode  = selectmode,
                 varlist     = values,
                 values_env  = values_env,
                 filter  = list(
                     entry = text_box_1,
                     opts  = options)
            ),
            class = c(add_class, "listbox", "bs_tk_widget", "list")
        )
    }
# ~~~~~~~~~~~~~~~~~~~~~ ======================================================


# Helpers and methods ========================================================
# Function
#
# k - string
# i - row index
# n - total number of rows
get_j <- function(k, i, n) {

    k <- match.arg(k, choices = c("top", "-1", "+1", "end"))

    j <- switch(
        k,
        "top"    = 1,
        "-1"     = max(i - 1, 1),
        "+1"     = min(i + 1, n),
        "end"    = n,
        i)
    j
}


## listbox - listbox widget
# move_to - action:
#     "top" - selected row becomes 1-st
#     "-1"  - position decreases by 1
#     "+1"  - position inreases by 1
#     "end" - selected row becomes last
#
# i, j - row numbers
# swap_two_rows_in_listbox
move_selected_row_in_listbox <- function(listbox, move_to = "") {

    if (inherits(listbox,  "listbox")) {
        listbox <- listbox$listbox
    }

    # Get y view
    y_view <- str_split_fixed(tkyview(listbox), " ", n = 2)[1]

    # [???] adapt code according to `move_selected_row_in_tktext()`
    pre_i <- as.integer(tkcurselection(listbox)) + 1

    # Return, if not selected
    if (length(pre_i) == 0) {
        return()
    }

    i   <- min(pre_i) # i -- first selected row
    tmp <- get_values_listbox(listbox) # vals_old_order
    n   <- length(tmp)

    j <- get_j(move_to, i, n)

    i <- correct_row_index(i, n)
    j <- correct_row_index(j, n)

    swapped <- swap(tmp, i, j)

    set_values(listbox, swapped, clear = TRUE)
    tkyview.moveto(listbox, y_view) # reset y view

    tk_see(listbox, j)
    tkselection.set(listbox, j - 1)
}

bind_row_swap_listbox <- function(listbox, ...) {
    tkbind(listbox, "<Control-Up>",   function() {move_selected_row_in_listbox(listbox, "top")})
    tkbind(listbox, "<Alt-Up>",       function() {move_selected_row_in_listbox(listbox, "-1")})
    tkbind(listbox, "<Alt-Down>",     function() {move_selected_row_in_listbox(listbox, "+1")})
    tkbind(listbox, "<Control-Down>", function() {move_selected_row_in_listbox(listbox, "end")})
}


bind_row_swap.listbox <- function(obj, ...) {
    bind_row_swap_listbox(obj$listbox, ...)
}

bind_row_swap.tk2listbox <- function(obj, ...) {
    bind_row_swap_listbox(obj, ...)
}



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
get_values.listbox <- function(obj, ...) {
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
set_values.listbox_with_filter <- function(obj, values, ..., clear = TRUE) {
    # Set new set of possible values
    if (clear == TRUE) {
        obj$values_env$all_values <- values
    } else {
        obj$values_env$all_values <- c(obj$values_env$all_values, values)
    }

    # Set listbox values
    set_values_listbox(obj$listbox, values = values, ..., clear = clear)

    # Clear filter box
    set_values(obj$filter$entry, "")
}


get_0_based_ind <- function(obj, ind) {
    if (is.numeric(ind)) {
        ind <- as.integer(ind) - 1

    } else if (is.character(ind)) {
        ind <- which(get_values(obj) %in% ind) - 1
    }

    ind
}


#' @rdname Helper-functions
#' @export
#' @keywords internal
set_yview.listbox <- function(obj, ind, ...) {
    ind <- get_0_based_ind(obj, ind)
    tkyview(obj$listbox, ind, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
set_yview.tk2listbox <- function(obj, ind, ...) {
    ind <- get_0_based_ind(obj, ind)
    tkyview(obj, ind, ...)
}



#' @rdname Helper-functions
#' @export
#' @keywords internal
# Ind must be either numeric or character.
tk_see.listbox <- function(obj, ind, ...) {
    ind <- get_0_based_ind(obj, ind)
    tksee(obj$listbox, ind, ...)
}


#' @rdname Helper-functions
#' @export
#' @keywords internal
# Ind must be either numeric or character.
tk_see.tk2listbox <- function(obj, ind, ...) {
    ind <- get_0_based_ind(obj, ind)
    tksee(obj, ind, ...)
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
tk_get_state.listbox <- function(obj, ...) {
    tk_get_state(obj$listbox, ...)
}

# ----------------------------------------------------------------------------

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_selection.tk2listbox <- function(obj, ...) {
    get_selection_listbox(obj)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_selection_length.tk2listbox <- function(obj, ...) {
    length(get_selection(obj))
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
set_selection.tk2listbox <- function(obj, sel, clear = TRUE, ...) {
    set_selection_listbox(obj, sel = sel, clear = clear, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
add_selection.tk2listbox <- function(obj, sel, ...) {
    set_selection_listbox(obj, sel = sel, clear = FALSE, ...)
}


#' @rdname Helper-functions
#' @export
#' @keywords internal
get_values.tk2listbox <- function(obj, vals, ...) {
    get_values_listbox(obj, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
set_values.tk2listbox <- function(obj, values, ..., clear = TRUE) {
    set_values_listbox(obj, values = values, ..., clear = clear)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_disable.tk2listbox <- function(obj, ..., background = "grey95") {
    tk_disable.default(obj, background = background, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_normalize.tk2listbox <- function(obj, ..., background = "white") {
    tk_normalize.default(obj, background = background, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_get_state.tk2listbox <- function(obj, ...) {
    tk_get_state.default(obj, ...)
}


