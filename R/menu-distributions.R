# "Distributions" menu related functions ===========================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Commands and windows for Rcmdr
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_distributions_descriptr <- function() {
    Library("descriptr")

    command <- style_cmd(glue::glue(
        '## The application (app) will open in your default Internet browser.\n',
        '## You will not be able to use R and R Commander correctly\n',
        '## until you close the app in the browser.\n\n',

        '## In the app, go to "Analyze" -> "Distributions".\n\n',

        'shiny::runApp(\n',
        '    system.file(package = "descriptr", "application"),\n',
        '    launch.browser = TRUE)'
    ))
    doItAndPrint(command)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
