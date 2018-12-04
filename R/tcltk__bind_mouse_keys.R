bind_mouse_keys <- function(obj, envir = parent.frame()) {

    obj <- deparse(substitute(obj))

    # on_click          = function() {},
    # on_double_click   = function() {},
    # on_triple_click   = function() {},
    # on_release        = function() {},
    # on_click_3        = function() {},
    # on_double_click_3 = function() {},
    # on_triple_click_3 = function() {},
    # on_release_3      = function() {},

    key <- tibble::tribble(
        ~Binding,            ~Function,
        "ButtonPress-1",      "on_click",
        "Double-Button-1" ,   "on_double_click",
        "Triple-Button-1",    "on_triple_click",
        "ButtonRelease-1",    "on_release",
        "ButtonPress-3",      "on_click_3",
        "Double-Button-3",    "on_double_click_3",
        "Triple-Button-3",    "on_triple_click_3",
        "ButtonRelease-3",    "on_release_3"        )

    eval_glue('tkbind({obj}, "<{key$Binding}>", function() {{tkfocus({obj}); {key$Function}}()}',
              envir_eval = envir)
}