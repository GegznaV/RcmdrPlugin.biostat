# TCL/TK Combo Box
#
# https://www.tcl.tk/man/tcl/TkCmd/ttk_combobox.htm
# https://core.tcl.tk/bwidget/doc/bwidget/BWman/ComboBox.html#-values
#
# @param parent parent Tcl/Tk frame
# @param values
# @param export
# @param state
# @param selection
# @param title
#
# @param title_sticky
# @param combobox_sticky
#
# @param width     Specifies an integer value indicating the desired width of the entry window, in average-size characters of the widget's font.
# @param height    Specifies the height of the pop-down listbox, in rows.

#
# @param on_select      function
# @param on_click
# @param on_double_click
# @param on_triple_click
# @param on_release
# @param on_click_3
# @param on_double_click_3
# @param on_triple_click_3
# @param on_release_3
# @param postcommand A Tcl script to evaluate immediately before displaying the
#                    listbox. The -postcommand script may specify the -values
#                    to display.

# @param ...
#
# @return
# @export

bs_combobox <- function(
    parent       = top,
    values       = variables_all(),
    export              = "FALSE",
    state               = c("readonly", "normal", "disabled"),
    # default_text      = "<no variable selected>",
    # selection = gettext_bs(default_text),
    value               = NULL,
    selection           = NULL,
    title               = NULL,
    title_sticky        = "w",
    combobox_sticky     = "nw",

    on_select          = function(){},
    postcommand        = function(){},
    on_click           = function() {},
    on_double_click    = function() {},
    on_triple_click    = function() {},
    on_release         = function() {},
    on_click_3         = function() {},
    on_double_click_3  = function() {},
    on_triple_click_3  = function() {},
    on_release_3       = function() {},
    tip                = gettext_bs(
        "For quicker search, press the \nfirst letter of variable name\non your keyboard."),
    width              = 20,
    height             = 8,
    on_keyboard        = c("select", "ignore"),

    ...)
{

    state       <- match.arg(state)
    on_keyboard <- match.arg(on_keyboard)


    checkmate::assert_integerish(selection, lower = 1, len = 1, null.ok = TRUE)
    checkmate::assert_string(value, null.ok = TRUE)

    # Check this ---- START ==================================================
    if (is.null(values) || length(values) == 0) {
        values    <- NULL
        selection <- NULL
        value     <- NULL

    } else {
        values <- as.character(values)

        if (length(value) == 1 && (value[1] %in% values)) {
            value <- value[1]
        } else if (length(selection) != 0) {
            value <-  values[selection[1]]
        } else {
            value <- NULL
        }
    }

    # Check this ---- END ====================================================


    # values     <- c(gettext_bs(default_text), values)
    frame              <- tkframe(parent)
    combovar           <- tclVar()
    tclvalue(combovar) <- value

    # Main -------------------------------------------------------------------
    combobox <- tk2combobox(
        parent       = frame,
        values       = values,
        textvariable = combovar,
        state        = state,
        export       = export,
        width        = width,
        height       = height,
        postcommand  = postcommand,
        tip = tip
        , ...
    )


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (on_keyboard %in% c("select")) {

        onLetter <- function(letter) {
            # Letter binding is good for read-only comboboxes only

            get_first_letter <- function(str) {
                tolower(substr(str, 1, 1))
            }

            letter   <- tolower(letter)
            all_vals <- tk2list.get(combobox)
            acceptable_inds <- which(get_first_letter(all_vals) %in% letter)

            if (length(acceptable_inds) == 0) {
                return()
            }

            cur_val     <- tclvalue_chr(tkget(combobox))
            cur_val_ind <- which(all_vals %in% cur_val)

            if (length(cur_val_ind) == 0)
                cur_val_ind <- 0

            next_ind <-
                acceptable_inds[which(acceptable_inds %in% cur_val_ind)[1] + 1]

            if (is.na(next_ind)) {
                next_ind <- min(acceptable_inds)
            }

            tcl(combobox, "current", next_ind - 1) # 0 based index
        }
        eval_glue('tkbind(combobox, "<{letters}>", function() onLetter("{letters}"))')
        eval_glue('tkbind(combobox, "<{LETTERS}>", function() onLetter("{letters}"))')


        # TODO:
        #    -  bind event "on state change"
        #    -  on state change: bind letters only in read-only mode
        #    -  on state change: in other modes - remove binding
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bind_mouse_keys(combobox)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkbind(combobox, "<<ComboboxSelected>>", function() {
        tkfocus(combobox)
        on_select()
    }) # on change of selected value
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (!is.null(title)) {
        tkgrid(labelRcmdr(frame,
                          text = title,
                          fg   = getRcmdr("title.color"),
                          font = "RcmdrTitleFont"),
               sticky = title_sticky)
    }

    tkgrid(combobox, sticky = combobox_sticky)

    result <- list(
        frame    = frame,
        obj      = combobox,
        var      = combovar,

        combobox = combobox,
        combovar = combovar,


        varlist  = values
    )

    class(result) <- "combobox"
    result
}

# Helpers and methods ========================================================
#' @rdname Helper-functions
#' @export
#' @keywords internal
get_values_combobox <- function(combobox) {
    tk2list.get(combobox)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
set_values_combobox <- function(combobox, values, ...) {
    tkconfigure(combobox, values = as.character(values), ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_selection_ind_combobox <- function(combobox) {
    which(trimws(tk2list.get(combobox)) %in% tclvalue_chr(tkget(combobox)))
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
# Get selected values
get_selection_combobox <- function(combobox) {
    tclvalue_chr(tkget(combobox))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @rdname Helper-functions
#' @export
#' @keywords internal
get_size.combobox <- function(obj, ...) {
    tk2list.size(obj$combobox)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_size.tk2combobox <- function(obj, ...) {
    tk2list.size(obj)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_selection.combobox <- function(obj, ...) {
    get_selection_combobox(obj$combobox)
}


#' @rdname Helper-functions
#' @export
#' @keywords internal
set_selection.combobox <- function(obj, sel, ...) {
    if (is.numeric(sel)) {
        new_val <- get_values(obj)[sel]
    } else {
        new_val <- sel
    }

    tclvalue(obj$combovar) <- sel
}


#' @rdname Helper-functions
#' @export
#' @keywords internal
get_values.combobox <- function(obj, ...) {
    get_values_combobox(obj$combobox, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
set_values.combobox <- function(obj, values, ...) {
    set_values_combobox(obj$combobox, values = values, ...)
    set_selection(obj, 1)
}


#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_activate.combobox <- function(obj, ...) {
    tk_activate(obj$combobox, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_normalize.combobox <- function(obj, ...) {
    tk_normalize(obj$combobox, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_read_only.combobox <- function(obj, ...) {
    tk_read_only(obj$combobox, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_disable.combobox <- function(obj, ...) {
    tk_disable(obj$combobox, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_get_state.combobox <- function(obj) {
    tk_get_state(obj$combobox)
}



