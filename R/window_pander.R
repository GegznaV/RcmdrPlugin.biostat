#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_pander <- function() {

    # [!!!]
    object <- ActiveDataSet()
    caption <- str_glue("Object `{object}`")


    # [!!!]

    Library("pander")

    doItAndPrint(style_cmd(str_glue(
        '# Object printed for an R Markdown report\n',
        'pander::pander({object}, caption = "{caption}")')))
}

