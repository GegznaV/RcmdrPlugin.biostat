#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_pander <- function() {

    # [!!!]
    object <- ActiveDataSet()
    caption <- glue::glue("Object `{object}`")


    # [!!!]

    Library("pander")

    doItAndPrint(style_cmd(glue::glue(
        '# Object printed for an R Markdown report\n',
        'pander::pander({object}, caption = "{caption}")')))
}

