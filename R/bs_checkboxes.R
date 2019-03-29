#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Checkboxes widget.
#'
#' Checkboxes widget with command functions and tips when mouse is over the box.
#'
#' @param parent Parent frame.
#' @param boxes (vector of strings) Variable names for each checkbox.
#' @param labels (vector of strings) Labels for each checkbox.
#' @param title Title for the set of checkboxes.
#' @param values  A vector of values ("0" or "1"). Overrides the `default_value`.
#' @param default_value (string) Default value ("0" or "1").
#' @param commands  A named list of commands (functions) for checkbox. The names must match the values of "boxes".
#' @param default_command (function) A default command.
#' @param tips   A named list of strings to be used as tips for checkbox.
#' The names must match the values of "boxes".
#' @param default_tip (string) a default tip.
#' @param border (logical) Flag if the frame should have a border.
#' @param layout (string) One of "vertical" (default) and "horizontal".
#' @param sticky_buttons (string) `sticky` option for buttons.
#' @param sticky_title (string) `sticky` option for title (if no border is used).
#'
#' @return A named list with fields `frame` (frame with the checkboxes),
#'  `var` (tcl/tk variables for each box),
#'  and `obj` (tcl/tk objects for each box).
#'
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#'
#' library(RcmdrPlugin.biostat)
#'
#' top <- tcltk::tktoplevel()
#'
#' boxes_1 <- bs_checkboxes(top, c("A", "B", "C"))
#' tcltk::tkgrid(boxes_1$frame)
#'
#'
#' top <- tcltk::tktoplevel()
#' boxes_2 <- bs_checkboxes(top, boxes = c("A", "B", "C"), border = TRUE)
#' tcltk::tkgrid(boxes_2$frame)
#'
#'
#' top <- tcltk::tktoplevel()
#' boxes_3 <- bs_checkboxes(top, c("A", "B", "C"),
#'                         layout = "h", title = "Buttons")
#' tcltk::tkgrid(boxes_3$frame)
#'
#'
#' set_values(boxes_3, B = TRUE, C = TRUE)
#'
#' new_vals <- c(A = TRUE, B = FALSE)
#' set_values(boxes_3, new_vals)
#'
#'
#' get_values(boxes_3)
#' get_values(boxes_3, "B")
#' get_values(boxes_3, simplify = FALSE)
#'
#'
#'}}

bs_checkboxes <- function(
    parent          = top,
    boxes,
    labels          = NULL,
    title           = NULL,
    values          = NULL,
    default_value   = "0",
    commands        = list(),          # named list of functions
    default_command = function() {},
    tips            = list(), # named list of strings
    default_tip     = "",
    border          = FALSE,
    layout          = c("vertical", "horizontal"),
    sticky_buttons  = "w",
    sticky_title    = "w"
)
{
    checkmate::assert_character(boxes)
    checkmate::assert_character(labels, null.ok = TRUE)
    checkmate::assert_string(title, null.ok = TRUE)
    checkmate::assert_list(commands)
    checkmate::assert_function(default_command)
    checkmate::assert_list(tips)
    checkmate::assert_string(default_tip)
    checkmate::assert_flag(border)

    layout <- match.arg(layout)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.null(names(boxes))) {
        if (is.null(labels)) {
            labels <- boxes
        }

    } else {
        # If 'boxes' is a named vector,
        # values are treated as 'labels' and
        # names as 'boxes'
        if (!is.null(labels)) {
            warning("Values of 'labels' are ignored as 'boxes' is a named vector.")
        }

        labels <- unname(boxes)
        boxes <- names(boxes)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    boxes_list <- structure(boxes, names = boxes)

    # Manage commands` ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(commands) > 0 && !all(names(commands) %in% boxes)) {
        stop(
            "Argument `commands` must be a named list of functions. ",
            "The element names must be a subset of: ",
            paste(boxes, collapse = ", "), ". Unrecognized names: ",
            paste(setdiff(names(commands), boxes), collapse = ", "), "."
        )
    }

    commands <- modifyList(
        map(boxes_list, function(x) default_command),
        commands)

    # Manage `tips` ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.null(names(tips)) && (length(tips) == length(boxes))) {
        tips <- as.list(tips)

    } else if (length(tips) > 0 && !all(names(tips) %in% boxes)) {
        stop(
            "Argument `tips` must be a named list of strings.  ",
            "The element names must be a subset of: ",
            paste(boxes, collapse = ", "), ". Unrecognized names: ",
            paste(setdiff(names(tips), boxes), collapse = ", "), "."
        )

    } else {
        tips <- modifyList(map(boxes_list, function(x) default_tip), tips)
    }


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    values <-
        if (is.null(values)) {
            rep(default_value, length(boxes))

        } else if (length(values) == length(boxes)) {
            values

        } else {
            stop("The length of `values` must be ", length(boxes), ", not ",
                 length(values), ".")
        }

    vars <- map(values, tclVar)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    frame <-
        if (border) {
            if (is.null(title)) {
                tk2labelframe(parent)

            } else {
                tk2labelframe(
                    parent,
                    labelwidget = bs_label_b(parent, text = title))
            }

        } else {
            tk2frame(parent)
        }

    if (!is.null(title) && !border) {
        tkgrid(bs_label_b(frame, text = title), sticky = sticky_title)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    objs <- pmap(
        list(vars, labels, commands, tips),
        ~ tk2checkbutton(frame,
                         variable = ..1,
                         text     = ..2,
                         command  = ..3,
                         tip      = ..4))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    switch(layout,
           vertical = {
               walk(objs, tkgrid, sticky = sticky_buttons)
           },

           horizontal = {
               buttons_str <- paste0("objs[[", seq_along(objs), "]]",
                                     collapse = ", ")
               str_glue_eval('tkgrid({buttons_str}, sticky = sticky_buttons)')
           },

           stop("Unrecognized layout: ", layout)
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    structure(list(
        frame = frame,
        var   = structure(vars, names = boxes),
        obj   = structure(objs, names = boxes)
    ),
    class = c("bs_checkboxes", "bs_tk_buttonset", "bs_tk_widget", "list"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
get_values.bs_checkboxes <- function(obj, ..., simplify = TRUE, rm_names = simplify) {
    opts <- c(...)
    len <- length(opts)

    map_fun <-
        if (isTRUE(simplify)) {
            map_lgl
        } else {
            map
        }

    res <-
        if (len == 0) {
            map_fun(obj$var, tclvalue_lgl)

        } else {
            map_fun(obj$var[opts], tclvalue_lgl)
        }

    if (isTRUE(rm_names)) {
        unname(res)
    } else {
        res
    }

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.set_values <- function(obj_list, values, FUN) {

    if (all(names(values) %in% names(obj_list))) {

        to_modify <- obj_list[names(values)]
        new_vals <- modifyList(map(to_modify, tclvalue_lgl), values)
        pmap(list(to_modify, new_vals), FUN)

    } else {
        stop("Possibly misspelled names: ",
             setdiff(names(values), names(obj_list)) %>% str_c(collapse = ", "),
             ".",
             call. = FALSE
        )
    }
}


#' @rdname Helper-functions
#' @export
#' @keywords internal
set_values.bs_checkboxes <- function(obj, values, ...) {

    if (!missing(values)) {
        values <- as.list(values)

    } else {
        values <- list(...)
    }

    invisible(.set_values(obj$var, values, function(.x, .y) tclvalue(.x) <- .y))
}

