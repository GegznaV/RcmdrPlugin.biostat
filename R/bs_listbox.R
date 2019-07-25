# TODO:
#
# 1) for line `values       = variables_all()`  maybe a better default is
#    NULL, as it does not fail if no dataset is selected


# Listbox functions ==========================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' List box widget (TCL/TK)
#'
#' A list box with constant number of rows.
#'
#' @param parent Parent TCL/TK frame.
#' @param values (character) Vector of possible values.
#' @param value (character or NULL) Values that should be initially selected.
#' @param selection (integer) Numeric indices of initially selected lines.
#'                  Can be used instead of \code{value}.
#' @param selectmode ("single", "extended", "browse", "multiple") Selection mode.
#' @param title (string) Title.
#' @param subtitle (string) Subtitle.
#' @param tip (string) Message that is displayed on muse hover.
#' @param height (integer) Height of list box in lines.
#' @param width (one or two integers) Minimum and maximum width of list box.
#' @param enabled (logical) If \code{TRUE}, widget is enabled by default.
#' @param scroll ("both", "x", "y", "none") Do we add scrollbars?
#'               Indicates scrollbar position.
#' @param autoscroll ("x", "y", "both", "none")
#'                  Do we automatically hide scrollbars if not needed?
#'                  Indicates scrollbar position.
#' @param use_filter (logical) Should list box values filter box be displayed.
#' @param filter_label (string) Label for filter entry box.
#' @param sticky (combination of "n", "e", "w", "s" or "").
#' @param on_select Function that is activated on value selection.
#' @param on_click Function that is activated on single left-click.
#' @param on_double_click  on double left-click.
#' @param on_triple_click Function that is activated on triple left-click.
#' @param on_release Function that is activated on release of left-click.
#' @param on_click_3 Function that is activated on single right-click.
#' @param on_double_click_3 Function that is activated on double right-click.
#' @param on_triple_click_3 Function that is activated on triple right-click.
#' @param on_release_3 Function that is activated on release of right-click.
#' @param on_keyboard ("select", "scroll", "ignore") Action on keyboard letter click:
#' \describe{
#'     \item{"select"}{toggle selection of values that start with the pressed letter.
#'     This letter binding is good for single-selection boxes, otherwise selection should not be toggled (see "scroll")}
#'     \item{"scroll"}{toggle view of values that start with the pressed letter.
#'      Do not toggle selection (i.e., do not select or deselect). }
#'     \item{"ignore"}{Do not bind any action. Letter binding is good for read-only list boxes only.}
#'}
#' @param on_keyboard_fun Function that is activated on keyboard button press, if
#'        \code{on_keyboard} is "select" or "scroll".
#' @param bind_row_swap (Does not work yet!) if TRUE, Ctrl/Alt + Up/Down keys move rows in the list box
#' @param title_sticky (combination of "n", "e", "w", "s" or "")
#' @param subtitle_sticky (combination of "n", "e", "w", "s" or "")
#' @param ... Additional arguments.  \enumerate{
#'    \item For \code{bs_listbox()}: arguments passed to function
#'    \code{tk2listbox()} (see \link[tcltk2]{tk2widgets}).
#'    \item For other functions: arguments passed to further methods.
#' }
#'
#' @param obj Widget.
#' @param listbox List box widget.
#' @param ind Index. Usually either character values or numeric indices.
#' @param sel Selection. Usually either character values or numeric indices.
#' @param vals Values.
#' @param clear (logical) Should values be cleared?
#' @param background (string) background color.
#' @param move_to (string) Action:
#' \itemize{
#'    \item "top" - selected row is moved to the first row.
#'    \item "-1"  - position is decreased by 1 row.
#'    \item "+1"  - position inreased by 1row.
#'    \item "end" - selected row is moved to the last row.
#' }
#'
#' @seealso \code{tk2listbox()} at \link[tcltk2]{tk2widgets}, \cr
#'          \url{https://www.tcl.tk/man/tcl8.4/TkCmd/listbox.htm}
#' @export
#' @examples
#'
#' \dontrun{\donttest{
#'
#' # Active dataset must be selected
#' top <- tktoplevel()
#' lb1 <- bs_listbox(top, values = LETTERS, value = "K")
#' lb2 <- bs_listbox(top, values = LETTERS, selection = c(12, 15), selectmode = "multiple")
#' tkgrid(lb1$frame, lb2$frame)
#'
#' }}
#'

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

        selection_code <- if (!is.null(selection)) "selection  = {selection}," else ""
        value_code     <- if (!is.null(value))     "value      = {value},"     else ""

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
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        bind_mouse_keys(listbox)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkbind(listbox, "<<ListboxSelect>>", on_select)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # if (bind_row_swap) {
        #     bind_row_swap_listbox(listbox)
        # }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkgrid(listbox, sticky = sticky)
        # tkgrid(listbox, scrollbar,  sticky = "nw")
        # tkgrid.configure(scrollbar, sticky = "wns")
        # tkgrid.configure(listbox,   sticky = "ewns")

        # Add textbox with filter ---------------------------------------------
        values_env <- new.env()
        values_env$all_values <- values

        if (use_filter == TRUE) {
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            cmd_update_list <- function() {

                s_txt <- get_values(text_box_1)
                if (str_length(s_txt) == 0) {
                    new_list <- values_env$all_values

                } else {
                    ignore_case <- !tclvalue_lgl(options$var$case)

                    filter_fun <- switch(
                        tclvalue_chr(options$var$regex),
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

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            clear_textbox <- function() {
                set_values(text_box_1, "")
                cmd_update_list()
            }
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            tkgrid(text_box_1$frame, sticky = sticky)
            # tkgrid(options_frame,    sticky = sticky)
            tkgrid(options$frame,    sticky = sticky)

            # tkgrid.forget(text_box_1$frames)

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            tkbind(text_box_1$obj_text, "<Double-Button-3>", clear_textbox)
            tkbind(text_box_1$obj_text, "<KeyRelease>",      cmd_update_list)
            tkbind(text_box_1$obj_text, "<<Cut>>",           cmd_update_list)
            tkbind(text_box_1$obj_text, "<<Copy>>",          cmd_update_list)
            tkbind(text_box_1$obj_text, "<<Paste>>",         cmd_update_list)
            tkbind(text_box_1$obj_text, "<<Clear>>",         cmd_update_list)
            tkbind(text_box_1$obj_text, "<ButtonPress-1>",   cmd_update_list)

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            add_class <- "listbox_with_filter"

        } else {
            add_class  <- NULL
            text_box_1 <- NULL
            options    <- NULL
        }

        # Output object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        output_object <- structure(
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

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Put the first selected value in view
        if (!is.null(value) || length(value) > 0) {
            tk_see(output_object, value[1])

        } else if (!is.null(selection) || length(selection) > 0) {
            tk_see(output_object, selection[1])
        }

        # Output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        output_object
    }

# ~~~~~~~~~~~~~~~~~~~~~~~~~ ==================================================


# Helpers and methods ========================================================

# Size ===================== =================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_listbox
#' @export
get_size.listbox <- function(obj, ...) {
    tclvalue_int(tksize(obj$listbox))
}

#' @rdname bs_listbox
#' @export
get_size.tkwin <- function(obj, ...) {
    tclvalue_int(tksize(obj))
}


# Values =================== ================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_listbox
#' @export
get_values_listbox <- function(listbox) {
    n <- tclvalue_int(tksize(listbox))
    vars <-
        (seq_len(n) - 1) %>% # zero based index
        purrr::map_chr(~tclvalue_chr(tkget(listbox, ., .))) %>%
        stringr::str_replace("^\\{(.* .*)\\}$", "\\1") # removes { } if several words are used
    vars
}

#' @rdname bs_listbox
#' @export
get_values.listbox <- function(obj, ...) {
    get_values_listbox(obj$listbox, ...)
}

#' @rdname bs_listbox
#' @export
get_values.tk2listbox <- function(obj, vals, ...) {
    get_values_listbox(obj, ...)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_listbox
#' @export
set_values_listbox <- function(listbox, values, clear = TRUE) {
    if (isTRUE(clear)) {
        tkdelete(listbox, 0, "end")
    }
    for (val in values)  tkinsert(listbox, "end", as.character(val))
}

#' @rdname bs_listbox
#' @export
set_values.listbox <- function(obj, values, ..., clear = TRUE) {
    set_values_listbox(obj$listbox, values = values, ..., clear = clear)
}

#' @rdname bs_listbox
#' @export
set_values.tk2listbox <- function(obj, values, ..., clear = TRUE) {
    set_values_listbox(obj, values = values, ..., clear = clear)
}

#' @rdname bs_listbox
#' @export
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


# Selection ================ =================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_listbox
#' @export
get_selection_ind_listbox <- function(listbox) {
    as.numeric(tkcurselection(listbox)) + 1
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_listbox
#' @export
# Get selected values
get_selection_listbox <- function(listbox) {
    vals <- get_values_listbox(listbox)
    inds <- get_selection_ind_listbox(listbox)
    vals[inds]
}

#' @rdname bs_listbox
#' @export
get_selection.listbox <- function(obj, ...) {
    get_selection_listbox(obj$listbox)
}

#' @rdname bs_listbox
#' @export
get_selection.tk2listbox <- function(obj, ...) {
    get_selection_listbox(obj)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_listbox
#' @export
get_selection_length.listbox <- function(obj, ...) {
    length(get_selection(obj))
}

#' @rdname bs_listbox
#' @export
get_selection_length.tk2listbox <- function(obj, ...) {
    length(get_selection(obj))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_listbox
#' @export
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

#' @rdname bs_listbox
#' @export
set_selection.listbox <- function(obj, sel, clear = TRUE, ...) {
    listbox <- obj$listbox
    set_selection_listbox(listbox, sel = sel, clear = clear, ...)
}

#' @rdname bs_listbox
#' @export
set_selection.tk2listbox <- function(obj, sel, clear = TRUE, ...) {
    set_selection_listbox(obj, sel = sel, clear = clear, ...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_listbox
#' @export
add_selection.listbox <- function(obj, sel, ...) {
    listbox <- obj$listbox
    set_selection_listbox(listbox, sel = sel, clear = FALSE, ...)
}

#' @rdname bs_listbox
#' @export
add_selection.tk2listbox <- function(obj, sel, ...) {
    set_selection_listbox(obj, sel = sel, clear = FALSE, ...)
}

# Move rows =============== ==================================================

# k - string
# i - row index
# n - total number of rows
#
# i, j - row numbers
# swap_two_rows_in_listbox
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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_0_based_ind <- function(obj, ind) {
    if (is.numeric(ind)) {
        ind <- as.integer(ind) - 1

    } else if (is.character(ind)) {
        ind <- which(get_values(obj) %in% ind) - 1
    }

    ind
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_listbox
#' @export
## listbox - listbox widget
# move_to - action:
#     "top" - selected row becomes 1-st
#     "-1"  - position decreases by 1
#     "+1"  - position inreases by 1
#     "end" - selected row becomes last
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bind_row_swap_listbox <- function(listbox, ...) {
    tkbind(listbox, "<Control-Up>",   function() {move_selected_row_in_listbox(listbox, "top")})
    tkbind(listbox, "<Alt-Up>",       function() {move_selected_row_in_listbox(listbox, "-1")})
    tkbind(listbox, "<Alt-Down>",     function() {move_selected_row_in_listbox(listbox, "+1")})
    tkbind(listbox, "<Control-Down>", function() {move_selected_row_in_listbox(listbox, "end")})
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_listbox
#' @export
bind_row_swap.listbox <- function(obj, ...) {
    bind_row_swap_listbox(obj$listbox, ...)
}

#' @rdname bs_listbox
#' @export
bind_row_swap.tk2listbox <- function(obj, ...) {
    bind_row_swap_listbox(obj, ...)
}

# Visibility ================== ==============================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_listbox
#' @export
set_yview.listbox <- function(obj, ind, ...) {
    ind <- get_0_based_ind(obj, ind)
    tkyview(obj$listbox, ind, ...)
}

#' @rdname bs_listbox
#' @export
set_yview.tk2listbox <- function(obj, ind, ...) {
    ind <- get_0_based_ind(obj, ind)
    tkyview(obj, ind, ...)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_listbox
#' @export
# Ind must be either numeric or character.
tk_see.listbox <- function(obj, ind, ...) {
    ind <- get_0_based_ind(obj, ind)
    tksee(obj$listbox, ind, ...)
}

#' @rdname bs_listbox
#' @export
# Ind must be either numeric or character.
tk_see.tk2listbox <- function(obj, ind, ...) {
    ind <- get_0_based_ind(obj, ind)
    tksee(obj, ind, ...)
}

# State of widget ============== =============================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_listbox
#' @export
tk_get_state.listbox <- function(obj, ...) {
    tk_get_state(obj$listbox, ...)
}

#' @rdname bs_listbox
#' @export
tk_get_state.tk2listbox <- function(obj, ...) {
    tk_get_state.default(obj, ...)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_listbox
#' @export
tk_disable.listbox <- function(obj, ..., background = "grey95") {
    tk_disable(obj$listbox, background = background, ...)
}

#' @rdname bs_listbox
#' @export
tk_disable.tk2listbox <- function(obj, ..., background = "grey95") {
    tk_disable.default(obj, background = background, ...)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_listbox
#' @export
tk_normalize.listbox <- function(obj, ..., background = "white") {
    tk_normalize(obj$listbox, background = background, ...)
}

#' @rdname bs_listbox
#' @export
tk_normalize.tk2listbox <- function(obj, ..., background = "white") {
    tk_normalize.default(obj, background = background, ...)
}

