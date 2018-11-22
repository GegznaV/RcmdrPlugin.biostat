# TODO:
# 1. Convert into window.
# 2. Add ability to add custom caption;
# 3. Add ability to choose style of pander table ;
# 4. Add other parameters (???)
#
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_print_as_md <- function() {

    tbl_style   <- "simple"
    use_caption <- TRUE

    if (use_caption) {
        caption_text <- str_glue("Dataset `{ActiveDataSet()}`")
        tbl_caption  <- str_glue(',\n caption = "{caption_text}" ')

    } else {
        tbl_caption  <- ""
    }



    Library("tidyverse")
    Library("pander")


    command <- str_glue(
        '## Dataset as Markdown table \n',
        # '# The dataset printed in a from that will be converted \n',
        # '# to a table in an R Markdown report. \n',
        '{ActiveDataSet()} %>% \n',
        '    pander::pander(missing = "", style = "{tbl_style}"',
        '    {tbl_caption}',
        '    )') %>%
        style_cmd()

    doItAndPrint(command)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~