# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_image_digitizer_online  <- function(variables) {

    if (!pingr::is_online()) {
        RcmdrTkmessageBox(
            "This feature requires an Internet connection to work properly.",
            icon  = "warning",
            title = "No Internet Connection",
            type  = "ok")
    }

    browseURL("https://apps.automeris.io/wpd/")

}