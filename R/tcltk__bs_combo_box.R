# Title
#
# https://www.tcl.tk/man/tcl/TkCmd/ttk_combobox.htm
#
# @param parent_window parent Tcl/Tk frame
# @param variableList
# @param export
# @param state
# @param initial_selection
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

bs_combo_box <- function(
    parent_window       = top,
    variableList        = variables_all(),
    export              = "FALSE",
    state               = c("readonly", "normal", "disabled"),
    # default_text      = "<no variable selected>",
    # initial_selection = gettext_bs(default_text),
    initial_selection   = NULL,
    title               = NULL,
    title_sticky        = "w",
    combobox_sticky     = "nw",

    on_select           = function(){},
    postcommand         = postcommand,

    on_click          = function() {},
    on_double_click   = function() {},
    on_triple_click   = function() {},
    on_release        = function() {},
    on_click_3        = function() {},
    on_double_click_3 = function() {},
    on_triple_click_3 = function() {},
    on_release_3      = function() {},

    width              = 20,
    heihgt             = 1,
    ...)
{

    state <- match.arg(state)

    # variableList     <- c(gettext_bs(default_text), variableList)
    frame              <- tkframe(parent_window)
    combovar           <- tclVar()
    tclvalue(combovar) <- initial_selection

    combobox <- ttkcombobox(
        parent = frame,
        values       = variableList,
        textvariable = combovar,
        state        = state,
        export       = export,
        width        = width,
        postcommand = postcommand,
        ...)

    firstChar <- tolower(substr(variableList, 1, 1))

    onLetter <- function(letter) {
        letter  <- tolower(letter)
        current <- as.numeric(tcl(combobox, "current"))
        current <- if (current == -1) 1 else current + 1
        mat     <- match(letter, firstChar[-(1:current)])
        if (is.na(mat)) return()
        tcl(combobox, "current", current + mat - 1)
    }

    # eval_glue('on{LETTERS} <- function() onLetter{"letters"}')
    # eval_glue('tkbind(combobox, "<{letters}>", get("on{LETTERS}"))')
    # eval_glue('tkbind(combobox, "<{LETTERS}>", get("on{LETTERS}"))')

    eval_glue('tkbind(combobox, "<{letters}>", function() onLetter{"letters"})')
    eval_glue('tkbind(combobox, "<{LETTERS}>", function() onLetter{"letters"})')

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onClick <-

    onDoubleClick <- function()

    onRelease <- function() {
        tkfocus(combobox)
        onRelease_fun()
    }

    onSelect <- function() {
        tkfocus(combobox)
        onSelect_fun()
    }

    bind_mouse_keys(combobox)

    tkbind(combobox, "<<ComboboxSelected>>", onSelect) # on change of selected value
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
        combobox = combobox,
        varlist  = variableList,
        combovar = combovar)

    class(result) <- "combobox"
    result
}


# set_selection <- function(object, val) {
#     tclvalue(object$combovar) <- val
# }
#
# getSelection.combobox <- function(object){
#     tclvalue(object$combovar)
# }
